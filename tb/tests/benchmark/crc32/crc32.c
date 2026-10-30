/* CRC-32 of a constant message, table built at run time */
#include <stdint.h>

static const char msg[] = "Out-of-order RV32I core, CRC-32 test message";
static uint32_t table[256];
uint32_t scale = 3;

int main(void)
{
    for (uint32_t i = 0; i < 256; i++) {
        uint32_t c = i;
        for (int k = 0; k < 8; k++)
            c = (c & 1) ? 0xEDB88320u ^ (c >> 1) : c >> 1;
        table[i] = c;
    }
    uint32_t crc = 0xFFFFFFFFu;
    for (const char *p = msg; *p; p++)
        crc = table[(crc ^ (uint8_t)*p) & 0xFF] ^ (crc >> 8);
    return (int)((crc ^ 0xFFFFFFFFu) * scale);
}
