/* 4-bit counter on GPIO LD0-LD3, about 0.66 s per step */
#include <stdint.h>
#include "platform.h"

#define GPIO  (*(volatile uint32_t *)GPIO_BASE)
#define DELAY (CPU_CLK_MHZ * 125000u)

int main(void)
{
    for (uint32_t count = 0;; count++) {
        GPIO = count;
        /* Empty asm keeps the delay loop */
        for (uint32_t i = DELAY; i; i--)
            __asm__ volatile ("");
    }
}
