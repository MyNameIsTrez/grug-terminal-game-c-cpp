#pragma once

#include <stddef.h>
#include <typedefs.h>

typedef void (*grug_error_handler_fn)(char *error_msg, char *filename, int line_number);
extern grug_error_handler_fn grug_error_handler;

typedef struct grug_file grug_file;
typedef struct mod_directory mod_directory;

struct grug_file {
	char *name;
	void *dll;
};

struct mod_directory {
	char *name;

	mod_directory *dirs;
	size_t dirs_size;
	size_t dirs_capacity;

	grug_file *files;
	size_t files_size;
	size_t files_capacity;
};

void grug_free_mods(mod_directory dir);
mod_directory grug_reload_modified_mods(char *mods_dir_path, char *mods_dir_name, char *dll_dir_path);
void grug_print_mods(mod_directory mods);
void *grug_get_fn_address(void *dll, char *fn_name);

/*

 Relicensing TinyCC
 ------------------

 The authors listed below hereby confirm their agreement to relicense TinyCC
 including their past contributions under the following terms:


 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.


 Author (name)              I agree (YES/NO)    Files/Features (optional)
 ------------------------------------------------------------------------------
 Adam Sampson               YES                 makefiles
 Daniel Glöckner            NO                  arm-gen.c
 Daniel Glöckner            YES                 not arm-gen.c
 Danny Milosavljevic        YES                 arm-asm.c riscv64-asm.c
 Edmund Grimley Evans       YES                 arm64
 Fabrice Bellard            YES                 original author
 Frédéric Féret             YES                 x86 64/16 bit asm
 grischka                   YES                 tccpe.c
 Henry Kroll                YES
 Herman ten Brugge	    YES
 Joe Soroka                 YES
 Kirill Smelkov             YES
 mingodad                   YES
 Pip Cet                    YES
 Shinichiro Hamaji          YES                 x86_64-gen.c
 Steffen Nurpmeso           YES
 Vincent Lefèvre            YES
 Thomas Preud'homme         YES                 arm-gen.c
 Timo VJ Lähde (Timppa)     ?                   tiny_libmaker.c
 TK                         ?                   tcccoff.c c67-gen.c
 Tyge Løvset                YES                 tgmath.h, Windows tcc_libm.h math.h
 Urs Janssen                YES
 waddlesplash               YES
 Christian Jullien          YES                 Windows Cygwin build and tests
 Reimar Döffinger           YES

*/
