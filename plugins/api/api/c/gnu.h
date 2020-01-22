int __libc_start_main(int (*main) (int, char **, char **), int, char **, void *auxv);

void __stack_chk_fail(void)  __attribute__((noreturn));
