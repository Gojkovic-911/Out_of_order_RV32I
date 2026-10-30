/* Runs an application on the host, its return value is the expected result */
#include <stdio.h>

int app_main(void);

int main(void)
{
    printf("0x%08x\n", (unsigned)app_main());
    return 0;
}
