/* Harvard CPU: code in IMEM, everything else in DMEM.
   memory.ld (generated per target) sets the sizes and the base. */
OUTPUT_ARCH(riscv)
ENTRY(_start)

INCLUDE memory.ld

SECTIONS
{
    /* Code, crt0 first at address 0 */
    .text : {
        KEEP(*(.text.start))
        *(.text .text.*)
        . = ALIGN(4);
        __text_end = .;
    } > IMEM

    /* Loads cannot read IMEM, constants go to DMEM */
    .rodata : {
        *(.rodata .rodata.* .srodata .srodata.*)
    } > DMEM

    .data : {
        *(.data .data.* .sdata .sdata.*)
        . = ALIGN(4);
    } > DMEM

    /* Zeroed by crt0 */
    .bss (NOLOAD) : {
        __bss_start = .;
        *(.sbss .sbss.* .bss .bss.* COMMON)
        . = ALIGN(4);
        __bss_end = .;
    } > DMEM

    __global_pointer$ = ADDR(.data) + 0x800;
    __stack_top       = ORIGIN(DMEM) + LENGTH(DMEM);

    /DISCARD/ : { *(.comment) *(.riscv.attributes) *(.note*) *(.eh_frame*) }
}
