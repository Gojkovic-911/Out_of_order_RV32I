from playwright.sync_api import sync_playwright
import hashlib
import os
import time
import sys

def wait_for_completion(page, timeout=300000):
    """Wait for 'No more instructions to run!' text to appear."""
    print("Waiting for execution to complete...")
    try:
        # Wait for completion message
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
    # Read assembly code
    asm_code = open(asm_file, 'r').read()
    
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()
        
        # Run on web interpreter
        page.goto("https://www.cs.cornell.edu/courses/cs3410/2019sp/riscv/interpreter/")
        page.evaluate("setFrequency(256)")
        
        page.fill('#code', '')
        page.fill('#code', asm_code)
        
        page.click('text=Reset')
        page.click('text=Run')
        
        if not wait_for_completion(page, timeout=300000):
            print("Warning: Execution may not have completed successfully")
        
        time.sleep(0.5)
        
        # Download the registers
        with page.expect_download() as download_info:
            page.get_by_text("Download Registers!").click()
        download = download_info.value
        download.save_as(out_regs)
        
        browser.close()

if __name__ == "__main__":
    testname = sys.argv[1]
    root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
    asm = f"{root}/RV32I/tb/tests/{testname}/{testname}.s"
    regs = f"{root}/out/tests/{testname}/golden_vector_regs.txt"
    os.makedirs(os.path.dirname(regs), exist_ok=True)
    # Reuse golden of unchanged source
    digest = hashlib.sha256(open(asm, 'rb').read()).hexdigest()
    sha = regs.replace('.txt', '.sha')
    if os.path.exists(regs) and os.path.exists(sha) and open(sha).read() == digest:
        print("Golden vector up to date")
        sys.exit(0)
    run_interpreter(asm, regs)
    open(sha, 'w').write(digest)