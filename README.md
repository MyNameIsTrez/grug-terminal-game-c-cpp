# grug-example-game

## Running the game

Clone or download this repository, and `cd` into it with a terminal.

### Windows

TODO: Modify this command to use the DLL approach
`clear && cc main.c grug.c data.c mod.c game/human.c mods/magic/potions/health.c -Wall -Wextra -Werror -Wpedantic -g -I. && ./a.exe`

### Linux

#### With fsanitize

`clear && cc main.c grug.c data.c mod.c game/human.c mods/magic/potions/health.c libtcc.c tccpp.c tccgen.c tccdbg.c tccelf.c tccasm.c tccrun.c x86_64-gen.c x86_64-link.c i386-asm.c -Wall -Wextra -Werror -Wpedantic -Wfatal-errors -g -I. -rdynamic -fsanitize=address,undefined -DCONFIG_TRIPLET="\"x86_64-linux-gnu\"" -DTCC_TARGET_X86_64 -DONE_SOURCE=0 -DTCC_GITHASH="\"2024-03-03 mob@9d2068c6*\"" -fno-strict-aliasing -Wno-pointer-sign -Wno-sign-compare -Wno-unused-result -Wno-format-truncation -Wno-stringop-truncation -Wno-old-style-declaration -Wno-overlength-strings -Wno-implicit-fallthrough -Wno-missing-field-initializers && ./a.out`

#### valgrind

`valgrind --leak-check=full ./a.out`

### TODO

- Figure out whether some files like `tccrun.c` are optional, and can be left out
- Get rid of all the `#pragma GCC` I temporarily added, which are all in `tccrun.c`
