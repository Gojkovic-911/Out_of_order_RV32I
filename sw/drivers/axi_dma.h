/* AXI DMA simple (direct register) mode: MM2S memory to stream, S2MM stream to memory */
#ifndef AXI_DMA_H
#define AXI_DMA_H

#include <stdint.h>

#define DMA_REG(off)     (*(volatile uint32_t *)(DMA_BASE + (off)))
#define DMA_MM2S_DMACR   DMA_REG(0x00)
#define DMA_MM2S_DMASR   DMA_REG(0x04)
#define DMA_MM2S_SA      DMA_REG(0x18)
#define DMA_MM2S_LENGTH  DMA_REG(0x28)
#define DMA_S2MM_DMACR   DMA_REG(0x30)
#define DMA_S2MM_DMASR   DMA_REG(0x34)
#define DMA_S2MM_DA      DMA_REG(0x48)
#define DMA_S2MM_LENGTH  DMA_REG(0x58)

#define DMA_CR_RS        0x1u
#define DMA_SR_ERR       0x70u
#define DMA_SR_IOC       0x1000u

/* Both channels running, once after reset */
static inline void dma_init(void)
{
    DMA_MM2S_DMACR = DMA_CR_RS;
    DMA_S2MM_DMACR = DMA_CR_RS;
}

/* The length write starts the transfer */
static inline void dma_mm2s_start(const void *src, uint32_t len)
{
    DMA_MM2S_SA     = (uint32_t)src;
    DMA_MM2S_LENGTH = len;
}

static inline void dma_s2mm_start(void *dst, uint32_t len)
{
    DMA_S2MM_DA     = (uint32_t)dst;
    DMA_S2MM_LENGTH = len;
}

/* Wait for IOC on S2MM and clear both; 0 on success, -1 on error or timeout */
static inline int dma_s2mm_wait(uint32_t timeout)
{
    uint32_t sr;
    do {
        sr = DMA_S2MM_DMASR;
        if (sr & DMA_SR_ERR)
            return -1;
    } while (!(sr & DMA_SR_IOC) && --timeout);
    DMA_S2MM_DMASR = DMA_SR_IOC;
    DMA_MM2S_DMASR = DMA_SR_IOC;
    return (sr & DMA_SR_IOC) && !(DMA_MM2S_DMASR & DMA_SR_ERR) ? 0 : -1;
}

#endif
