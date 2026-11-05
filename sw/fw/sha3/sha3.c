/* SHA3-256 of three known messages through DMA and the accelerator; LD0-LD2 per pass, LD3 all */
#include <stdint.h>
#include "platform.h"
#include "axi_dma.h"
#include "sha3.h"

#define GPIO     (*(volatile uint32_t *)GPIO_BASE)
#define MAX_LEN  200u
#define TIMEOUT  100000u

static const uint8_t abc[3] = {'a', 'b', 'c'};

/* SHA3-256 of "", "abc" and bytes i & 0xff, i < 200 (hashlib) */
static const uint8_t expected[3][SHA3_DIGEST] = {
    {0xa7, 0xff, 0xc6, 0xf8, 0xbf, 0x1e, 0xd7, 0x66, 0x51, 0xc1, 0x47, 0x56, 0xa0, 0x61, 0xd6, 0x62,
     0xf5, 0x80, 0xff, 0x4d, 0xe4, 0x3b, 0x49, 0xfa, 0x82, 0xd8, 0x0a, 0x4b, 0x80, 0xf8, 0x43, 0x4a},
    {0x3a, 0x98, 0x5d, 0xa7, 0x4f, 0xe2, 0x25, 0xb2, 0x04, 0x5c, 0x17, 0x2d, 0x6b, 0xd3, 0x90, 0xbd,
     0x85, 0x5f, 0x08, 0x6e, 0x3e, 0x9d, 0x52, 0x5b, 0x46, 0xbf, 0xe2, 0x45, 0x11, 0x43, 0x15, 0x32},
    {0x5f, 0x72, 0x8f, 0x63, 0xbf, 0x5e, 0xe4, 0x8c, 0x77, 0xf4, 0x53, 0xc0, 0x49, 0x03, 0x98, 0xfa,
     0x64, 0x5b, 0x8d, 0x4c, 0x4e, 0x56, 0xbe, 0x9a, 0x41, 0xcf, 0xec, 0x34, 0x4d, 0x6c, 0xa8, 0x99},
};

static uint8_t msg[MAX_LEN];
static uint8_t padded[SHA3_BLOCKS(MAX_LEN) * SHA3_RATE] __attribute__((aligned(8)));
static volatile uint8_t digest[SHA3_DIGEST] __attribute__((aligned(8)));

/* One hash: S2MM ready for the digest, SHA3 started, MM2S feeds the blocks */
static int hash_check(const uint8_t *data, uint32_t len, const uint8_t *exp)
{
    uint32_t nb = sha3_pad(padded, data, len);
    dma_s2mm_start((void *)digest, SHA3_DIGEST);
    sha3_start(nb);
    dma_mm2s_start(padded, nb * SHA3_RATE);
    if (dma_s2mm_wait(TIMEOUT))
        return 0;
    for (uint32_t i = 0; i < SHA3_DIGEST; i++)
        if (digest[i] != exp[i])
            return 0;
    return 1;
}

int main(void)
{
    uint32_t pass = 0;
    for (uint32_t i = 0; i < MAX_LEN; i++)
        msg[i] = (uint8_t)i;
    GPIO = 0;
    dma_init();
    pass |= hash_check(abc, 0, expected[0]) << 0;
    pass |= hash_check(abc, 3, expected[1]) << 1;
    pass |= hash_check(msg, MAX_LEN, expected[2]) << 2;
    GPIO = pass | (pass == 7 ? 8 : 0);
    return 0;
}
