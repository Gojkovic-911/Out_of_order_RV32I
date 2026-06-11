from playwright.sync_api import sync_playwright
import time
import sys

def run_interpreter(asm_file: str, out_regs: str):
    """Run the RISC-V interpreter on a local .s file, download registers, and save memory."""
    # Read assembly code from file
    asm_code = open(asm_file, 'r').read()
    with sync_playwright() as p:
        # Launch headless Chromium
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()
        # Navigate to the RISC-V interpreter page
        page.goto("https://www.cs.cornell.edu/courses/cs3410/2019sp/riscv/interpreter/")
        page.evaluate("setFrequency(256)")

        # Fill the code textarea. This waits for the element and then types the code.
        page.fill('#code', '')           # clear any existing code
        page.fill('#code', asm_code)     # input our assembly code

        # Click Reset and Run buttons to execute the code
        page.click('text=Reset')
        page.click('text=Run')

        time.sleep(5) 

        # Wait up to 5 seconds for the Stop button to appear (signaling completion).
        try:
            page.wait_for_selector('#stop', timeout=5000)
        except Exception:
            print("Stop button not found (maybe execution was instant).")
        time.sleep(0.5)  # small delay to ensure final updates

        # Download the registers: wrap the click in expect_download to catch the file.
        with page.expect_download() as download_info:
            page.get_by_text("Download Registers!").click()
        download = download_info.value
        # Save the downloaded registers file to the specified path.
        download.save_as(out_regs)
        
        # Close the browser
        browser.close()

if __name__ == "__main__":
    testname = sys.argv[1]
    run_interpreter( f"../RISCV_tb/{testname}/{testname}.s", f"../RISCV_tb/{testname}/golden_vector_regs.txt")
