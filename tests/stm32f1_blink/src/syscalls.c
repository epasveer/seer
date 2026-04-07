// Create dummy syscalls as we don't need them but the linker still needs them.
void _close(void) {}
void _lseek(void) {}
void _read(void) {}
void _write(void) {}
