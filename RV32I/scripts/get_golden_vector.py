from playwright.sync_api import sync_playwright
import time
import sys

def wait_for_completion(page, timeout=50000):
    """Wait for 'No more instructions to run!' text to appear."""
    print("Waiting for execution to complete...")
    try:
        # Wait for the completion message to appear in the recent-instruction div
        page.wait_for_function(
            '''() => {
                const div = document.querySelector('#recent-instruction');
                if (div && div.innerText.includes('No more instructions to run!')) {
                    return true;
                }
                return false;
            }''',
            timeout=timeout
        )
        print("Execution completed!")
        return True
    except Exception as e:
        print(f"Timeout: Execution did not complete within {timeout/1000} seconds")
        return False

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
        
        # Fill the code textarea
        page.fill('#code', '')           # clear any existing code
        page.fill('#code', asm_code)     # input our assembly code
        
        # Click Reset and Run buttons to execute the code
        page.click('text=Reset')
        page.click('text=Run')
        
        # Wait for completion using the "No more instructions to run!" message
        if not wait_for_completion(page, timeout=50000):
            print("Warning: Execution may not have completed successfully")
        
        # Small delay to ensure everything is stable
        time.sleep(0.5)
        
        # Download the registers
        with page.expect_download() as download_info:
            page.get_by_text("Download Registers!").click()
        download = download_info.value
        download.save_as(out_regs)
        
        # Close the browser
        browser.close()

if __name__ == "__main__":
    testname = sys.argv[1]
    run_interpreter(
        f"../RISCV_tb/{testname}/{testname}.s", 
        f"../RISCV_tb/{testname}/golden_vector_regs.txt"
    )