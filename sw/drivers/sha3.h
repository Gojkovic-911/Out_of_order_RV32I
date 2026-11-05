/* SHA3-256 accelerator: padding in software, blocks over the DMA stream */
#ifndef SHA3_H
#define SHA3_H

#include <stdint.h>

#define SHA3_REG(off)    (*(volatile uint32_t *)(SHA3_BASE + (off)))
#define SHA3_NB          SHA3_REG(0x0)
#define SHA3_START       SHA3_REG(0x4)
#define SHA3_STATUS      SHA3_REG(0x8)

#define SHA3_ST_READY    0x1u
#define SHA3_ST_DONE     0x2u
#define SHA3_RATE        136u
#define SHA3_DIGEST      32u

/* Blocks needed for len message bytes */
#define SHA3_BLOCKS(len) ((len) / SHA3_RATE + 1u)

/* Message with SHA3 padding into dst (8-byte aligned, SHA3_BLOCKS(len) * SHA3_RATE bytes); returns the blocks */
static inline uint32_t sha3_pad(uint8_t *dst, const uint8_t *msg, uint32_t len)
{
    uint32_t nb = SHA3_BLOCKS(len);
    uint32_t i;
    for (i = 0; i < len; i++)
        dst[i] = msg[i];
    dst[i++] = 0x06;
    for (; i < nb * SHA3_RATE; i++)
        dst[i] = 0;
    dst[nb * SHA3_RATE - 1] |= 0x80;
    return nb;
}

/* Start a hash of nb blocks; data follows on the stream */
static inline void sha3_start(uint32_t nb)
{
    while (!(SHA3_STATUS & SHA3_ST_READY))
        ;
    SHA3_NB    = nb;
    SHA3_START = 1;
}

#endif
