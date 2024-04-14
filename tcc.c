//// tcc: i386-asm.c

/*
 *  i386 specific functions for TCC assembler
 *
 *  Copyright (c) 2001, 2002 Fabrice Bellard
 *  Copyright (c) 2009 Frédéric Feret (x86_64 support)
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#define USING_GLOBALS
#include "tcc.h"

#define MAX_OPERANDS 3

#define TOK_ASM_first TOK_ASM_clc
#define TOK_ASM_last TOK_ASM_emms
#define TOK_ASM_alllast TOK_ASM_subps

#define OPC_B          0x01  /* only used with OPC_WL */
#define OPC_WL         0x02  /* accepts w, l or no suffix */
#define OPC_BWL        (OPC_B | OPC_WL) /* accepts b, w, l or no suffix */
#define OPC_REG        0x04 /* register is added to opcode */
#define OPC_MODRM      0x08 /* modrm encoding */

#define OPCT_MASK      0x70
#define OPC_FWAIT      0x10 /* add fwait opcode */
#define OPC_SHIFT      0x20 /* shift opcodes */
#define OPC_ARITH      0x30 /* arithmetic opcodes */
#define OPC_FARITH     0x40 /* FPU arithmetic opcodes */
#define OPC_TEST       0x50 /* test opcodes */
#define OPC_0F01       0x60 /* 0x0f01XX (group 7, XX is 2nd opcode,
                               no operands and unstructured mod/rm) */
#define OPCT_IS(v,i) (((v) & OPCT_MASK) == (i))

#define OPC_0F        0x100 /* Is secondary map (0x0f prefix) */
#define OPC_48        0x200 /* Always has REX prefix */
#ifdef TCC_TARGET_X86_64
# define OPC_WLQ     0x1000  /* accepts w, l, q or no suffix */
# define OPC_BWLQ    (OPC_B | OPC_WLQ) /* accepts b, w, l, q or no suffix */
# define OPC_WLX     OPC_WLQ
# define OPC_BWLX    OPC_BWLQ
#else
# define OPC_WLX     OPC_WL
# define OPC_BWLX    OPC_BWL
#endif

#define OPC_GROUP_SHIFT 13

/* in order to compress the operand type, we use specific operands and
   we or only with EA  */
enum {
    OPT_REG8=0, /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_REG16,  /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_REG32,  /* warning: value is hardcoded from TOK_ASM_xxx */
#ifdef TCC_TARGET_X86_64
    OPT_REG64,  /* warning: value is hardcoded from TOK_ASM_xxx */
#endif
    OPT_MMX,    /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_SSE,    /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_CR,     /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_TR,     /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_DB,     /* warning: value is hardcoded from TOK_ASM_xxx */
    OPT_SEG,
    OPT_ST,
#ifdef TCC_TARGET_X86_64
    OPT_REG8_LOW, /* %spl,%bpl,%sil,%dil, encoded like ah,ch,dh,bh, but
		     with REX prefix, not used in insn templates */
#endif
    OPT_IM8,
    OPT_IM8S,
    OPT_IM16,
    OPT_IM32,
#ifdef TCC_TARGET_X86_64
    OPT_IM64,
#endif
    OPT_EAX,    /* %al, %ax, %eax or %rax register */
    OPT_ST0,    /* %st(0) register */
    OPT_CL,     /* %cl register */
    OPT_DX,     /* %dx register */
    OPT_ADDR,   /* OP_EA with only offset */
    OPT_INDIR,  /* *(expr) */
    /* composite types */
    OPT_COMPOSITE_FIRST,
    OPT_IM,     /* IM8 | IM16 | IM32 */
    OPT_REG,    /* REG8 | REG16 | REG32 | REG64 */
    OPT_REGW,   /* REG16 | REG32 | REG64 */
    OPT_IMW,    /* IM16 | IM32 */
    OPT_MMXSSE, /* MMX | SSE */
    OPT_DISP,   /* Like OPT_ADDR, but emitted as displacement (for jumps) */
    OPT_DISP8,  /* Like OPT_ADDR, but only 8bit (short jumps) */
    /* can be ored with any OPT_xxx */
    OPT_EA = 0x80
};

#define OP_REG8   (1 << OPT_REG8)
#define OP_REG16  (1 << OPT_REG16)
#define OP_REG32  (1 << OPT_REG32)
#define OP_MMX    (1 << OPT_MMX)
#define OP_SSE    (1 << OPT_SSE)
#define OP_CR     (1 << OPT_CR)
#define OP_TR     (1 << OPT_TR)
#define OP_DB     (1 << OPT_DB)
#define OP_SEG    (1 << OPT_SEG)
#define OP_ST     (1 << OPT_ST)
#define OP_IM8    (1 << OPT_IM8)
#define OP_IM8S   (1 << OPT_IM8S)
#define OP_IM16   (1 << OPT_IM16)
#define OP_IM32   (1 << OPT_IM32)
#define OP_EAX    (1 << OPT_EAX)
#define OP_ST0    (1 << OPT_ST0)
#define OP_CL     (1 << OPT_CL)
#define OP_DX     (1 << OPT_DX)
#define OP_ADDR   (1 << OPT_ADDR)
#define OP_INDIR  (1 << OPT_INDIR)
#ifdef TCC_TARGET_X86_64
# define OP_REG64 (1 << OPT_REG64)
# define OP_REG8_LOW (1 << OPT_REG8_LOW)
# define OP_IM64  (1 << OPT_IM64)
# define OP_EA32  (OP_EA << 1)
#else
# define OP_REG64 0
# define OP_REG8_LOW 0
# define OP_IM64  0
# define OP_EA32  0
#endif

#define OP_EA     0x40000000
#define OP_REG    (OP_REG8 | OP_REG16 | OP_REG32 | OP_REG64)

#ifdef TCC_TARGET_X86_64
# define TREG_XAX   TREG_RAX
# define TREG_XCX   TREG_RCX
# define TREG_XDX   TREG_RDX
#else
# define TREG_XAX   TREG_EAX
# define TREG_XCX   TREG_ECX
# define TREG_XDX   TREG_EDX
#endif

typedef struct ASMInstr {
    uint16_t sym;
    uint16_t opcode;
    uint16_t instr_type;
    uint8_t nb_ops;
    uint8_t op_type[MAX_OPERANDS]; /* see OP_xxx */
} ASMInstr;

typedef struct Operand {
    uint32_t type;
    int8_t  reg; /* register, -1 if none */
    int8_t  reg2; /* second register, -1 if none */
    uint8_t shift;
    ExprValue e;
} Operand;

static const uint8_t reg_to_size[9] = {
/*
    [OP_REG8] = 0,
    [OP_REG16] = 1,
    [OP_REG32] = 2,
#ifdef TCC_TARGET_X86_64
    [OP_REG64] = 3,
#endif
*/
    0, 0, 1, 0, 2, 0, 0, 0, 3
};

#define NB_TEST_OPCODES 30

static const uint8_t test_bits[NB_TEST_OPCODES] = {
 0x00, /* o */
 0x01, /* no */
 0x02, /* b */
 0x02, /* c */
 0x02, /* nae */
 0x03, /* nb */
 0x03, /* nc */
 0x03, /* ae */
 0x04, /* e */
 0x04, /* z */
 0x05, /* ne */
 0x05, /* nz */
 0x06, /* be */
 0x06, /* na */
 0x07, /* nbe */
 0x07, /* a */
 0x08, /* s */
 0x09, /* ns */
 0x0a, /* p */
 0x0a, /* pe */
 0x0b, /* np */
 0x0b, /* po */
 0x0c, /* l */
 0x0c, /* nge */
 0x0d, /* nl */
 0x0d, /* ge */
 0x0e, /* le */
 0x0e, /* ng */
 0x0f, /* nle */
 0x0f, /* g */
};

static const uint8_t segment_prefixes[] = {
 0x26, /* es */
 0x2e, /* cs */
 0x36, /* ss */
 0x3e, /* ds */
 0x64, /* fs */
 0x65  /* gs */
};

static const ASMInstr asm_instrs[] = {
#define ALT(x) x
/* This removes a 0x0f in the second byte */
#define O(o) ((uint64_t) ((((o) & 0xff00) == 0x0f00) ? ((((o) >> 8) & ~0xff) | ((o) & 0xff)) : (o)))
/* This constructs instr_type from opcode, type and group.  */
#define T(o,i,g) ((i) | ((g) << OPC_GROUP_SHIFT) | ((((o) & 0xff00) == 0x0f00) ? OPC_0F : 0))
#define DEF_ASM_OP0(name, opcode)
#define DEF_ASM_OP0L(name, opcode, group, instr_type) { TOK_ASM_ ## name, O(opcode), T(opcode, instr_type, group), 0, { 0 } },
#define DEF_ASM_OP1(name, opcode, group, instr_type, op0) { TOK_ASM_ ## name, O(opcode), T(opcode, instr_type, group), 1, { op0 }},
#define DEF_ASM_OP2(name, opcode, group, instr_type, op0, op1) { TOK_ASM_ ## name, O(opcode), T(opcode, instr_type, group), 2, { op0, op1 }},
#define DEF_ASM_OP3(name, opcode, group, instr_type, op0, op1, op2) { TOK_ASM_ ## name, O(opcode), T(opcode, instr_type, group), 3, { op0, op1, op2 }},
#ifdef TCC_TARGET_X86_64
# include "x86_64-asm.h"
#else
# include "i386-asm.h"
#endif
    /* last operation */
    { 0, },
};

static const uint16_t op0_codes[] = {
#define ALT(x)
#define DEF_ASM_OP0(x, opcode) opcode,
#define DEF_ASM_OP0L(name, opcode, group, instr_type)
#define DEF_ASM_OP1(name, opcode, group, instr_type, op0)
#define DEF_ASM_OP2(name, opcode, group, instr_type, op0, op1)
#define DEF_ASM_OP3(name, opcode, group, instr_type, op0, op1, op2)
#ifdef TCC_TARGET_X86_64
# include "x86_64-asm.h"
#else
# include "i386-asm.h"
#endif
};

static inline int get_reg_shift(TCCState *s1)
{
    int shift, v;
    v = asm_int_expr(s1);
    switch(v) {
    case 1:
        shift = 0;
        break;
    case 2:
        shift = 1;
        break;
    case 4:
        shift = 2;
        break;
    case 8:
        shift = 3;
        break;
    default:
        expect("1, 2, 4 or 8 constant");
        shift = 0;
        break;
    }
    return shift;
}

#ifdef TCC_TARGET_X86_64
static int asm_parse_numeric_reg(int t, unsigned int *type)
{
    int reg = -1;
    if (t >= TOK_IDENT && t < tok_ident) {
	const char *s = table_ident[t - TOK_IDENT]->str;
	char c;
	*type = OP_REG64;
	if (*s == 'c') {
	    s++;
	    *type = OP_CR;
	}
	if (*s++ != 'r')
	  return -1;
	/* Don't allow leading '0'.  */
	if ((c = *s++) >= '1' && c <= '9')
	  reg = c - '0';
	else
	  return -1;
	if ((c = *s) >= '0' && c <= '5')
	  s++, reg = reg * 10 + c - '0';
	if (reg > 15)
	  return -1;
	if ((c = *s) == 0)
	  ;
	else if (*type != OP_REG64)
	  return -1;
	else if (c == 'b' && !s[1])
	  *type = OP_REG8;
	else if (c == 'w' && !s[1])
	  *type = OP_REG16;
	else if (c == 'd' && !s[1])
	  *type = OP_REG32;
	else
	  return -1;
    }
    return reg;
}
#endif

static int asm_parse_reg(unsigned int *type)
{
    int reg = 0;
    *type = 0;
    if (tok != '%')
        goto error_32;
    next();
    if (tok >= TOK_ASM_eax && tok <= TOK_ASM_edi) {
        reg = tok - TOK_ASM_eax;
	*type = OP_REG32;
#ifdef TCC_TARGET_X86_64
    } else if (tok >= TOK_ASM_rax && tok <= TOK_ASM_rdi) {
        reg = tok - TOK_ASM_rax;
	*type = OP_REG64;
    } else if (tok == TOK_ASM_rip) {
        reg = -2; /* Probably should use different escape code. */
	*type = OP_REG64;
    } else if ((reg = asm_parse_numeric_reg(tok, type)) >= 0
	       && (*type == OP_REG32 || *type == OP_REG64)) {
	;
#endif
    } else {
    error_32:
        expect("register");
    }
    next();
    return reg;
}

static void parse_operand(TCCState *s1, Operand *op)
{
    ExprValue e;
    int reg, indir;
    const char *p;

    indir = 0;
    if (tok == '*') {
        next();
        indir = OP_INDIR;
    }

    if (tok == '%') {
        next();
        if (tok >= TOK_ASM_al && tok <= TOK_ASM_db7) {
            reg = tok - TOK_ASM_al;
            op->type = 1 << (reg >> 3); /* WARNING: do not change constant order */
            op->reg = reg & 7;
            if ((op->type & OP_REG) && op->reg == TREG_XAX)
                op->type |= OP_EAX;
            else if (op->type == OP_REG8 && op->reg == TREG_XCX)
                op->type |= OP_CL;
            else if (op->type == OP_REG16 && op->reg == TREG_XDX)
                op->type |= OP_DX;
        } else if (tok >= TOK_ASM_dr0 && tok <= TOK_ASM_dr7) {
            op->type = OP_DB;
            op->reg = tok - TOK_ASM_dr0;
        } else if (tok >= TOK_ASM_es && tok <= TOK_ASM_gs) {
            op->type = OP_SEG;
            op->reg = tok - TOK_ASM_es;
        } else if (tok == TOK_ASM_st) {
            op->type = OP_ST;
            op->reg = 0;
            next();
            if (tok == '(') {
                next();
                if (tok != TOK_PPNUM)
                    goto reg_error;
                p = tokc.str.data;
                reg = p[0] - '0';
                if ((unsigned)reg >= 8 || p[1] != '\0')
                    goto reg_error;
                op->reg = reg;
                next();
                skip(')');
            }
            if (op->reg == 0)
                op->type |= OP_ST0;
            goto no_skip;
#ifdef TCC_TARGET_X86_64
	} else if (tok >= TOK_ASM_spl && tok <= TOK_ASM_dil) {
	    op->type = OP_REG8 | OP_REG8_LOW;
	    op->reg = 4 + tok - TOK_ASM_spl;
        } else if ((op->reg = asm_parse_numeric_reg(tok, &op->type)) >= 0) {
	    ;
#endif
        } else {
        reg_error:
            tcc_error("unknown register %%%s", get_tok_str(tok, &tokc));
        }
        next();
    no_skip: ;
    } else if (tok == '$') {
        /* constant value */
        next();
        asm_expr(s1, &e);
        op->type = OP_IM32;
        op->e = e;
        if (!op->e.sym) {
            if (op->e.v == (uint8_t)op->e.v)
                op->type |= OP_IM8;
            if (op->e.v == (int8_t)op->e.v)
                op->type |= OP_IM8S;
            if (op->e.v == (uint16_t)op->e.v)
                op->type |= OP_IM16;
#ifdef TCC_TARGET_X86_64
            if (op->e.v != (int32_t)op->e.v && op->e.v != (uint32_t)op->e.v)
                op->type = OP_IM64;
#endif
        }
    } else {
        /* address(reg,reg2,shift) with all variants */
        op->type = OP_EA;
        op->reg = -1;
        op->reg2 = -1;
        op->shift = 0;
        if (tok != '(') {
            asm_expr(s1, &e);
            op->e = e;
        } else {
            next();
            if (tok == '%') {
                unget_tok('(');
                op->e.v = 0;
                op->e.sym = NULL;
            } else {
                /* bracketed offset expression */
                asm_expr(s1, &e);
                if (tok != ')')
                    expect(")");
                next();
                op->e.v = e.v;
                op->e.sym = e.sym;
            }
	    op->e.pcrel = 0;
        }
        if (tok == '(') {
	    unsigned int type = 0;
            next();
            if (tok != ',') {
                op->reg = asm_parse_reg(&type);
            }
            if (tok == ',') {
                next();
                if (tok != ',') {
                    op->reg2 = asm_parse_reg(&type);
                }
                if (tok == ',') {
                    next();
                    op->shift = get_reg_shift(s1);
                }
            }
	    if (type & OP_REG32)
	        op->type |= OP_EA32;
            skip(')');
        }
        if (op->reg == -1 && op->reg2 == -1)
            op->type |= OP_ADDR;
    }
    op->type |= indir;
}

/* XXX: unify with C code output ? */
ST_FUNC void gen_expr32(ExprValue *pe)
{
    if (pe->pcrel)
        /* If PC-relative, always set VT_SYM, even without symbol,
	   so as to force a relocation to be emitted.  */
	gen_addrpc32(VT_SYM, pe->sym, pe->v + (ind + 4));
    else
	gen_addr32(pe->sym ? VT_SYM : 0, pe->sym, pe->v);
}

#ifdef TCC_TARGET_X86_64
ST_FUNC void gen_expr64(ExprValue *pe)
{
    gen_addr64(pe->sym ? VT_SYM : 0, pe->sym, pe->v);
}
#endif

/* XXX: unify with C code output ? */
static void gen_disp32(ExprValue *pe)
{
    Sym *sym = pe->sym;
    ElfSym *esym = elfsym(sym);
    if (esym && esym->st_shndx == cur_text_section->sh_num) {
        /* same section: we can output an absolute value. Note
           that the TCC compiler behaves differently here because
           it always outputs a relocation to ease (future) code
           elimination in the linker */
        gen_le32(pe->v + esym->st_value - ind - 4);
    } else {
        if (sym && sym->type.t == VT_VOID) {
            sym->type.t = VT_FUNC;
            sym->type.ref = NULL;
        }
#ifdef TCC_TARGET_X86_64
        greloca(cur_text_section, sym, ind, R_X86_64_PLT32, pe->v - 4);
        gen_le32(0);
#else
        gen_addrpc32(VT_SYM, sym, pe->v);
#endif

    }
}

/* generate the modrm operand */
static inline int asm_modrm(int reg, Operand *op)
{
    int mod, reg1, reg2, sib_reg1;

    if (op->type & (OP_REG | OP_MMX | OP_SSE)) {
        g(0xc0 + (reg << 3) + op->reg);
    } else if (op->reg == -1 && op->reg2 == -1) {
        /* displacement only */
#ifdef TCC_TARGET_X86_64
	g(0x04 + (reg << 3));
	g(0x25);
#else
	g(0x05 + (reg << 3));
#endif
	gen_expr32(&op->e);
#ifdef TCC_TARGET_X86_64
    } else if (op->reg == -2) {
        ExprValue *pe = &op->e;
        g(0x05 + (reg << 3));
        gen_addrpc32(pe->sym ? VT_SYM : 0, pe->sym, pe->v);
        return ind;
#endif
    } else {
        sib_reg1 = op->reg;
        /* fist compute displacement encoding */
        if (sib_reg1 == -1) {
            sib_reg1 = 5;
            mod = 0x00;
        } else if (op->e.v == 0 && !op->e.sym && op->reg != 5) {
            mod = 0x00;
        } else if (op->e.v == (int8_t)op->e.v && !op->e.sym) {
            mod = 0x40;
        } else {
            mod = 0x80;
        }
        /* compute if sib byte needed */
        reg1 = op->reg;
        if (op->reg2 != -1)
            reg1 = 4;
        g(mod + (reg << 3) + reg1);
        if (reg1 == 4) {
            /* add sib byte */
            reg2 = op->reg2;
            if (reg2 == -1)
                reg2 = 4; /* indicate no index */
            g((op->shift << 6) + (reg2 << 3) + sib_reg1);
        }
        /* add offset */
        if (mod == 0x40) {
            g(op->e.v);
        } else if (mod == 0x80 || op->reg == -1) {
	    gen_expr32(&op->e);
        }
    }
    return 0;
}

#ifdef TCC_TARGET_X86_64
#define REX_W 0x48
#define REX_R 0x44
#define REX_X 0x42
#define REX_B 0x41

static void asm_rex(int width64, Operand *ops, int nb_ops, int *op_type,
		    int regi, int rmi)
{
  unsigned char rex = width64 ? 0x48 : 0;
  int saw_high_8bit = 0;
  int i;
  if (rmi == -1) {
      /* No mod/rm byte, but we might have a register op nevertheless
         (we will add it to the opcode later).  */
      for(i = 0; i < nb_ops; i++) {
	  if (op_type[i] & (OP_REG | OP_ST)) {
	      if (ops[i].reg >= 8) {
		  rex |= REX_B;
		  ops[i].reg -= 8;
	      } else if (ops[i].type & OP_REG8_LOW)
		  rex |= 0x40;
	      else if (ops[i].type & OP_REG8 && ops[i].reg >= 4)
		  /* An 8 bit reg >= 4 without REG8 is ah/ch/dh/bh */
		  saw_high_8bit = ops[i].reg;
	      break;
	  }
      }
  } else {
      if (regi != -1) {
	  if (ops[regi].reg >= 8) {
	      rex |= REX_R;
	      ops[regi].reg -= 8;
	  } else if (ops[regi].type & OP_REG8_LOW)
	      rex |= 0x40;
	  else if (ops[regi].type & OP_REG8 && ops[regi].reg >= 4)
	      /* An 8 bit reg >= 4 without REG8 is ah/ch/dh/bh */
	      saw_high_8bit = ops[regi].reg;
      }
      if (ops[rmi].type & (OP_REG | OP_MMX | OP_SSE | OP_CR | OP_EA)) {
	  if (ops[rmi].reg >= 8) {
	      rex |= REX_B;
	      ops[rmi].reg -= 8;
	  } else if (ops[rmi].type & OP_REG8_LOW)
	      rex |= 0x40;
	  else if (ops[rmi].type & OP_REG8 && ops[rmi].reg >= 4)
	      /* An 8 bit reg >= 4 without REG8 is ah/ch/dh/bh */
	      saw_high_8bit = ops[rmi].reg;
      }
      if (ops[rmi].type & OP_EA && ops[rmi].reg2 >= 8) {
	  rex |= REX_X;
	  ops[rmi].reg2 -= 8;
      }
  }
  if (rex) {
      if (saw_high_8bit)
	  tcc_error("can't encode register %%%ch when REX prefix is required",
		    "acdb"[saw_high_8bit-4]);
      g(rex);
  }
}
#endif


static void maybe_print_stats (void)
{
    static int already;

    if (0 && !already)
    /* print stats about opcodes */
    {
        const struct ASMInstr *pa;
        int freq[4];
        int op_vals[500];
        int nb_op_vals, i, j;

	already = 1;
        nb_op_vals = 0;
        memset(freq, 0, sizeof(freq));
        for(pa = asm_instrs; pa->sym != 0; pa++) {
            freq[pa->nb_ops]++;
            //for(i=0;i<pa->nb_ops;i++) {
                for(j=0;j<nb_op_vals;j++) {
                    //if (pa->op_type[i] == op_vals[j])
                    if (pa->instr_type == op_vals[j])
                        goto found;
                }
                //op_vals[nb_op_vals++] = pa->op_type[i];
                op_vals[nb_op_vals++] = pa->instr_type;
            found: ;
            //}
        }
        for(i=0;i<nb_op_vals;i++) {
            int v = op_vals[i];
            //if ((v & (v - 1)) != 0)
                printf("%3d: %08x\n", i, v);
        }
        printf("size=%d nb=%d f0=%d f1=%d f2=%d f3=%d\n",
               (int)sizeof(asm_instrs),
	       (int)sizeof(asm_instrs) / (int)sizeof(ASMInstr),
               freq[0], freq[1], freq[2], freq[3]);
    }
}

ST_FUNC void asm_opcode(TCCState *s1, int opcode)
{
    const ASMInstr *pa;
    int i, modrm_index, modreg_index, reg, v, op1, seg_prefix, pc, p;
    int nb_ops, s;
    Operand ops[MAX_OPERANDS], *pop;
    int op_type[3]; /* decoded op type */
    int alltypes;   /* OR of all operand types */
    int autosize;
    int p66;
#ifdef TCC_TARGET_X86_64
    int rex64;
#endif

    maybe_print_stats();
    /* force synthetic ';' after prefix instruction, so we can handle */
    /* one-line things like "rep stosb" instead of only "rep\nstosb" */
    if (opcode >= TOK_ASM_wait && opcode <= TOK_ASM_repnz)
        unget_tok(';');

    /* get operands */
    pop = ops;
    nb_ops = 0;
    seg_prefix = 0;
    alltypes = 0;
    for(;;) {
        if (tok == ';' || tok == TOK_LINEFEED)
            break;
        if (nb_ops >= MAX_OPERANDS) {
            tcc_error("incorrect number of operands");
        }
        parse_operand(s1, pop);
        if (tok == ':') {
           if (pop->type != OP_SEG || seg_prefix)
               tcc_error("incorrect prefix");
           seg_prefix = segment_prefixes[pop->reg];
           next();
           parse_operand(s1, pop);
           if (!(pop->type & OP_EA)) {
               tcc_error("segment prefix must be followed by memory reference");
           }
        }
        pop++;
        nb_ops++;
        if (tok != ',')
            break;
        next();
    }

    s = 0; /* avoid warning */

again:
    /* optimize matching by using a lookup table (no hashing is needed
       !) */
    for(pa = asm_instrs; pa->sym != 0; pa++) {
	int it = pa->instr_type & OPCT_MASK;
        s = 0;
        if (it == OPC_FARITH) {
            v = opcode - pa->sym;
            if (!((unsigned)v < 8 * 6 && (v % 6) == 0))
                continue;
        } else if (it == OPC_ARITH) {
            if (!(opcode >= pa->sym && opcode < pa->sym + 8*NBWLX))
                continue;
            s = (opcode - pa->sym) % NBWLX;
	    if ((pa->instr_type & OPC_BWLX) == OPC_WLX)
	      {
		/* We need to reject the xxxb opcodes that we accepted above.
		   Note that pa->sym for WLX opcodes is the 'w' token,
		   to get the 'b' token subtract one.  */
		if (((opcode - pa->sym + 1) % NBWLX) == 0)
		    continue;
	        s++;
	      }
        } else if (it == OPC_SHIFT) {
            if (!(opcode >= pa->sym && opcode < pa->sym + 7*NBWLX))
                continue;
            s = (opcode - pa->sym) % NBWLX;
        } else if (it == OPC_TEST) {
            if (!(opcode >= pa->sym && opcode < pa->sym + NB_TEST_OPCODES))
                continue;
	    /* cmovxx is a test opcode but accepts multiple sizes.
	       The suffixes aren't encoded in the table, instead we
	       simply force size autodetection always and deal with suffixed
	       variants below when we don't find e.g. "cmovzl".  */
	    if (pa->instr_type & OPC_WLX)
	        s = NBWLX - 1;
        } else if (pa->instr_type & OPC_B) {
#ifdef TCC_TARGET_X86_64
	    /* Some instructions don't have the full size but only
	       bwl form.  insb e.g. */
	    if ((pa->instr_type & OPC_WLQ) != OPC_WLQ
		&& !(opcode >= pa->sym && opcode < pa->sym + NBWLX-1))
	        continue;
#endif
            if (!(opcode >= pa->sym && opcode < pa->sym + NBWLX))
                continue;
            s = opcode - pa->sym;
        } else if (pa->instr_type & OPC_WLX) {
            if (!(opcode >= pa->sym && opcode < pa->sym + NBWLX-1))
                continue;
            s = opcode - pa->sym + 1;
        } else {
            if (pa->sym != opcode)
                continue;
        }
        if (pa->nb_ops != nb_ops)
            continue;
#ifdef TCC_TARGET_X86_64
	/* Special case for moves.  Selecting the IM64->REG64 form
	   should only be done if we really have an >32bit imm64, and that
	   is hardcoded.  Ignore it here.  */
	if (pa->opcode == 0xb0 && ops[0].type != OP_IM64
	    && (ops[1].type & OP_REG) == OP_REG64
	    && !(pa->instr_type & OPC_0F))
	    continue;
#endif
        /* now decode and check each operand */
	alltypes = 0;
        for(i = 0; i < nb_ops; i++) {
            int op1, op2;
            op1 = pa->op_type[i];
            op2 = op1 & 0x1f;
            switch(op2) {
            case OPT_IM:
                v = OP_IM8 | OP_IM16 | OP_IM32;
                break;
            case OPT_REG:
                v = OP_REG8 | OP_REG16 | OP_REG32 | OP_REG64;
                break;
            case OPT_REGW:
                v = OP_REG16 | OP_REG32 | OP_REG64;
                break;
            case OPT_IMW:
                v = OP_IM16 | OP_IM32;
                break;
	    case OPT_MMXSSE:
		v = OP_MMX | OP_SSE;
		break;
	    case OPT_DISP:
	    case OPT_DISP8:
		v = OP_ADDR;
		break;
            default:
                v = 1 << op2;
                break;
            }
            if (op1 & OPT_EA)
                v |= OP_EA;
	    op_type[i] = v;
            if ((ops[i].type & v) == 0)
                goto next;
	    alltypes |= ops[i].type;
        }
        (void)alltypes; /* maybe unused */
        /* all is matching ! */
        break;
    next: ;
    }
    if (pa->sym == 0) {
        if (opcode >= TOK_ASM_first && opcode <= TOK_ASM_last) {
            int b;
            b = op0_codes[opcode - TOK_ASM_first];
            if (b & 0xff00) 
                g(b >> 8);
            g(b);
            return;
        } else if (opcode <= TOK_ASM_alllast) {
            tcc_error("bad operand with opcode '%s'",
                  get_tok_str(opcode, NULL));
        } else {
	    /* Special case for cmovcc, we accept size suffixes but ignore
	       them, but we don't want them to blow up our tables.  */
	    TokenSym *ts = table_ident[opcode - TOK_IDENT];
	    if (ts->len >= 6
		&& strchr("wlq", ts->str[ts->len-1])
		&& !memcmp(ts->str, "cmov", 4)) {
		opcode = tok_alloc(ts->str, ts->len-1)->tok;
		goto again;
	    }
            tcc_error("unknown opcode '%s'", ts->str);
        }
    }
    /* if the size is unknown, then evaluate it (OPC_B or OPC_WL case) */
    autosize = NBWLX-1;
#ifdef TCC_TARGET_X86_64
    /* XXX the autosize should rather be zero, to not have to adjust this
       all the time.  */
    if ((pa->instr_type & OPC_BWLQ) == OPC_B)
        autosize = NBWLX-2;
#endif
    if (s == autosize) {
	/* Check for register operands providing hints about the size.
	   Start from the end, i.e. destination operands.  This matters
	   only for opcodes accepting different sized registers, lar and lsl
	   are such opcodes.  */
        for(i = nb_ops - 1; s == autosize && i >= 0; i--) {
            if ((ops[i].type & OP_REG) && !(op_type[i] & (OP_CL | OP_DX)))
                s = reg_to_size[ops[i].type & OP_REG];
        }
        if (s == autosize) {
            if ((opcode == TOK_ASM_push || opcode == TOK_ASM_pop) &&
                (ops[0].type & (OP_SEG | OP_IM8S | OP_IM32)))
                s = 2;
	    else if ((opcode == TOK_ASM_push || opcode == TOK_ASM_pop) &&
		     (ops[0].type & OP_EA))
	        s = NBWLX - 2;
            else
                tcc_error("cannot infer opcode suffix");
        }
    }

#ifdef TCC_TARGET_X86_64
    rex64 = 0;
    if (pa->instr_type & OPC_48)
        rex64 = 1;
    else if (s == 3 || (alltypes & OP_REG64)) {
        /* generate REX prefix */
	int default64 = 0;
	for(i = 0; i < nb_ops; i++) {
	    if (op_type[i] == OP_REG64 && pa->opcode != 0xb8) {
		/* If only 64bit regs are accepted in one operand
		   this is a default64 instruction without need for
		   REX prefixes, except for movabs(0xb8).  */
		default64 = 1;
		break;
	    }
	}
	/* XXX find better encoding for the default64 instructions.  */
        if (((opcode != TOK_ASM_push && opcode != TOK_ASM_pop
	      && opcode != TOK_ASM_pushw && opcode != TOK_ASM_pushl
	      && opcode != TOK_ASM_pushq && opcode != TOK_ASM_popw
	      && opcode != TOK_ASM_popl && opcode != TOK_ASM_popq
	      && opcode != TOK_ASM_call && opcode != TOK_ASM_jmp))
	    && !default64)
            rex64 = 1;
    }
#endif

    /* now generates the operation */
    if (OPCT_IS(pa->instr_type, OPC_FWAIT))
        g(0x9b);
    if (seg_prefix)
        g(seg_prefix);
#ifdef TCC_TARGET_X86_64
    /* Generate addr32 prefix if needed */
    for(i = 0; i < nb_ops; i++) {
        if (ops[i].type & OP_EA32) {
	    g(0x67);
	    break;
        }
    }
#endif
    /* generate data16 prefix if needed */
    p66 = 0;
    if (s == 1)
        p66 = 1;
    else {
	/* accepting mmx+sse in all operands --> needs 0x66 to
	   switch to sse mode.  Accepting only sse in an operand --> is
	   already SSE insn and needs 0x66/f2/f3 handling.  */
        for (i = 0; i < nb_ops; i++)
            if ((op_type[i] & (OP_MMX | OP_SSE)) == (OP_MMX | OP_SSE)
	        && ops[i].type & OP_SSE)
	        p66 = 1;
    }
    if (p66)
        g(0x66);

    v = pa->opcode;
    p = v >> 8;  /* possibly prefix byte(s) */
    switch (p) {
        case 0: break;  /* no prefix */
        case 0x48: break; /* REX, handled elsewhere */
        case 0x66:
        case 0x67:
        case 0xf2:
        case 0xf3: v = v & 0xff; g(p); break;
        case 0xd4: case 0xd5: break; /* aam and aad, not prefix, but hardcoded immediate argument "10" */
        case 0xd8: case 0xd9: case 0xda: case 0xdb: /* x87, no normal prefix */
        case 0xdc: case 0xdd: case 0xde: case 0xdf: break;
        default: tcc_error("bad prefix 0x%2x in opcode table", p); break;
    }
    if (pa->instr_type & OPC_0F)
        v = ((v & ~0xff) << 8) | 0x0f00 | (v & 0xff);
    if ((v == 0x69 || v == 0x6b) && nb_ops == 2) {
        /* kludge for imul $im, %reg */
        nb_ops = 3;
        ops[2] = ops[1];
        op_type[2] = op_type[1];
    } else if (v == 0xcd && ops[0].e.v == 3 && !ops[0].e.sym) {
        v--; /* int $3 case */
        nb_ops = 0;
    } else if ((v == 0x06 || v == 0x07)) {
        if (ops[0].reg >= 4) {
            /* push/pop %fs or %gs */
            v = 0x0fa0 + (v - 0x06) + ((ops[0].reg - 4) << 3);
        } else {
            v += ops[0].reg << 3;
        }
        nb_ops = 0;
    } else if (v <= 0x05) {
        /* arith case */
        v += ((opcode - TOK_ASM_addb) / NBWLX) << 3;
    } else if ((pa->instr_type & (OPCT_MASK | OPC_MODRM)) == OPC_FARITH) {
        /* fpu arith case */
        v += ((opcode - pa->sym) / 6) << 3;
    }

    /* search which operand will be used for modrm */
    modrm_index = -1;
    modreg_index = -1;
    if (pa->instr_type & OPC_MODRM) {
	if (!nb_ops) {
	    /* A modrm opcode without operands is a special case (e.g. mfence).
	       It has a group and acts as if there's an register operand 0
	       (ax).  */
	    i = 0;
	    ops[i].type = OP_REG;
	    ops[i].reg = 0;
	    goto modrm_found;
	}
        /* first look for an ea operand */
        for(i = 0;i < nb_ops; i++) {
            if (op_type[i] & OP_EA)
                goto modrm_found;
        }
        /* then if not found, a register or indirection (shift instructions) */
        for(i = 0;i < nb_ops; i++) {
            if (op_type[i] & (OP_REG | OP_MMX | OP_SSE | OP_INDIR))
                goto modrm_found;
        }
#ifdef ASM_DEBUG
        tcc_error("bad op table");
#endif
    modrm_found:
        modrm_index = i;
        /* if a register is used in another operand then it is
           used instead of group */
        for(i = 0;i < nb_ops; i++) {
            int t = op_type[i];
            if (i != modrm_index &&
                (t & (OP_REG | OP_MMX | OP_SSE | OP_CR | OP_TR | OP_DB | OP_SEG))) {
                modreg_index = i;
                break;
            }
        }
    }
#ifdef TCC_TARGET_X86_64
    asm_rex (rex64, ops, nb_ops, op_type, modreg_index, modrm_index);
#endif

    if (pa->instr_type & OPC_REG) {
        /* mov $im, %reg case */
        if (v == 0xb0 && s >= 1)
            v += 7;
        for(i = 0; i < nb_ops; i++) {
            if (op_type[i] & (OP_REG | OP_ST)) {
                v += ops[i].reg;
                break;
            }
        }
    }
    if (pa->instr_type & OPC_B)
        v += s >= 1;
    if (nb_ops == 1 && pa->op_type[0] == OPT_DISP8) {
	ElfSym *esym;
        int jmp_disp;

        /* see if we can really generate the jump with a byte offset */
	esym = elfsym(ops[0].e.sym);
        if (!esym || esym->st_shndx != cur_text_section->sh_num)
            goto no_short_jump;
        jmp_disp = ops[0].e.v + esym->st_value - ind - 2 - (v >= 0xff);
        if (jmp_disp == (int8_t)jmp_disp) {
            /* OK to generate jump */
	    ops[0].e.sym = 0;
            ops[0].e.v = jmp_disp;
	    op_type[0] = OP_IM8S;
        } else {
        no_short_jump:
	    /* long jump will be allowed. need to modify the
	       opcode slightly */
	    if (v == 0xeb) /* jmp */
	        v = 0xe9;
	    else if (v == 0x70) /* jcc */
	        v += 0x0f10;
	    else
	        tcc_error("invalid displacement");
        }
    }
    if (OPCT_IS(pa->instr_type, OPC_TEST))
        v += test_bits[opcode - pa->sym];
    else if (OPCT_IS(pa->instr_type, OPC_0F01))
        v |= 0x0f0100;
    op1 = v >> 16;
    if (op1)
        g(op1);
    op1 = (v >> 8) & 0xff;
    if (op1)
        g(op1);
    g(v);

    if (OPCT_IS(pa->instr_type, OPC_SHIFT)) {
        reg = (opcode - pa->sym) / NBWLX;
        if (reg == 6)
            reg = 7;
    } else if (OPCT_IS(pa->instr_type, OPC_ARITH)) {
        reg = (opcode - pa->sym) / NBWLX;
    } else if (OPCT_IS(pa->instr_type, OPC_FARITH)) {
        reg = (opcode - pa->sym) / 6;
    } else {
        reg = (pa->instr_type >> OPC_GROUP_SHIFT) & 7;
    }

    pc = 0;
    if (pa->instr_type & OPC_MODRM) {
        /* if a register is used in another operand then it is
           used instead of group */
	if (modreg_index >= 0)
	    reg = ops[modreg_index].reg;
        pc = asm_modrm(reg, &ops[modrm_index]);
    }

    /* emit constants */
#ifndef TCC_TARGET_X86_64
    if (!(pa->instr_type & OPC_0F)
	&& (pa->opcode == 0x9a || pa->opcode == 0xea)) {
        /* ljmp or lcall kludge */
	gen_expr32(&ops[1].e);
        if (ops[0].e.sym)
            tcc_error("cannot relocate");
        gen_le16(ops[0].e.v);
        return;
    }
#endif
    for(i = 0;i < nb_ops; i++) {
        v = op_type[i];
        if (v & (OP_IM8 | OP_IM16 | OP_IM32 | OP_IM64 | OP_IM8S | OP_ADDR)) {
            /* if multiple sizes are given it means we must look
               at the op size */
            if ((v | OP_IM8 | OP_IM64) == (OP_IM8 | OP_IM16 | OP_IM32 | OP_IM64)) {
                if (s == 0)
                    v = OP_IM8;
                else if (s == 1)
                    v = OP_IM16;
                else if (s == 2 || (v & OP_IM64) == 0)
                    v = OP_IM32;
                else
                    v = OP_IM64;
            }

            if ((v & (OP_IM8 | OP_IM8S | OP_IM16)) && ops[i].e.sym)
                tcc_error("cannot relocate");

            if (v & (OP_IM8 | OP_IM8S)) {
                g(ops[i].e.v);
            } else if (v & OP_IM16) {
                gen_le16(ops[i].e.v);
#ifdef TCC_TARGET_X86_64
            } else if (v & OP_IM64) {
                gen_expr64(&ops[i].e);
#endif
	    } else if (pa->op_type[i] == OPT_DISP || pa->op_type[i] == OPT_DISP8) {
                gen_disp32(&ops[i].e);
            } else {
                gen_expr32(&ops[i].e);
            }
        }
    }

    /* after immediate operands, adjust pc-relative address */
    if (pc)
        add32le(cur_text_section->data + pc - 4, pc - ind);
}

/* return the constraint priority (we allocate first the lowest
   numbered constraints) */
static inline int constraint_priority(const char *str)
{
    int priority, c, pr;

    /* we take the lowest priority */
    priority = 0;
    for(;;) {
        c = *str;
        if (c == '\0')
            break;
        str++;
        switch(c) {
        case 'A':
            pr = 0;
            break;
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'S':
        case 'D':
            pr = 1;
            break;
        case 'q':
            pr = 2;
            break;
        case 'r':
	case 'R':
	case 'p':
            pr = 3;
            break;
        case 'N':
        case 'M':
        case 'I':
	case 'e':
        case 'i':
        case 'm':
        case 'g':
            pr = 4;
            break;
        default:
            tcc_error("unknown constraint '%c'", c);
            pr = 0;
        }
        if (pr > priority)
            priority = pr;
    }
    return priority;
}

static const char *skip_constraint_modifiers(const char *p)
{
    while (*p == '=' || *p == '&' || *p == '+' || *p == '%')
        p++;
    return p;
}

/* If T (a token) is of the form "%reg" returns the register
   number and type, otherwise return -1.  */
ST_FUNC int asm_parse_regvar (int t)
{
    const char *s;
    Operand op;
    if (t < TOK_IDENT || (t & SYM_FIELD))
        return -1;
    s = table_ident[t - TOK_IDENT]->str;
    if (s[0] != '%')
        return -1;
    t = tok_alloc_const(s + 1);
    unget_tok(t);
    unget_tok('%');
    parse_operand(tcc_state, &op);
    /* Accept only integer regs for now.  */
    if (op.type & OP_REG)
        return op.reg;
    else
        return -1;
}

#define REG_OUT_MASK 0x01
#define REG_IN_MASK  0x02

#define is_reg_allocated(reg) (regs_allocated[reg] & reg_mask)

ST_FUNC void asm_compute_constraints(ASMOperand *operands,
                                    int nb_operands, int nb_outputs,
                                    const uint8_t *clobber_regs,
                                    int *pout_reg)
{
    ASMOperand *op;
    int sorted_op[MAX_ASM_OPERANDS];
    int i, j, k, p1, p2, tmp, reg, c, reg_mask;
    const char *str;
    uint8_t regs_allocated[NB_ASM_REGS];

    /* init fields */
    for(i=0;i<nb_operands;i++) {
        op = &operands[i];
        op->input_index = -1;
        op->ref_index = -1;
        op->reg = -1;
        op->is_memory = 0;
        op->is_rw = 0;
    }
    /* compute constraint priority and evaluate references to output
       constraints if input constraints */
    for(i=0;i<nb_operands;i++) {
        op = &operands[i];
        str = op->constraint;
        str = skip_constraint_modifiers(str);
        if (isnum(*str) || *str == '[') {
            /* this is a reference to another constraint */
            k = find_constraint(operands, nb_operands, str, NULL);
            if ((unsigned)k >= i || i < nb_outputs)
                tcc_error("invalid reference in constraint %d ('%s')",
                      i, str);
            op->ref_index = k;
            if (operands[k].input_index >= 0)
                tcc_error("cannot reference twice the same operand");
            operands[k].input_index = i;
            op->priority = 5;
	} else if ((op->vt->r & VT_VALMASK) == VT_LOCAL
		   && op->vt->sym
		   && (reg = op->vt->sym->r & VT_VALMASK) < VT_CONST) {
	    op->priority = 1;
	    op->reg = reg;
        } else {
            op->priority = constraint_priority(str);
        }
    }

    /* sort operands according to their priority */
    for(i=0;i<nb_operands;i++)
        sorted_op[i] = i;
    for(i=0;i<nb_operands - 1;i++) {
        for(j=i+1;j<nb_operands;j++) {
            p1 = operands[sorted_op[i]].priority;
            p2 = operands[sorted_op[j]].priority;
            if (p2 < p1) {
                tmp = sorted_op[i];
                sorted_op[i] = sorted_op[j];
                sorted_op[j] = tmp;
            }
        }
    }

    for(i = 0;i < NB_ASM_REGS; i++) {
        if (clobber_regs[i])
            regs_allocated[i] = REG_IN_MASK | REG_OUT_MASK;
        else
            regs_allocated[i] = 0;
    }
    /* esp cannot be used */
    regs_allocated[4] = REG_IN_MASK | REG_OUT_MASK;
    /* ebp cannot be used yet */
    regs_allocated[5] = REG_IN_MASK | REG_OUT_MASK;

    /* allocate registers and generate corresponding asm moves */
    for(i=0;i<nb_operands;i++) {
        j = sorted_op[i];
        op = &operands[j];
        str = op->constraint;
        /* no need to allocate references */
        if (op->ref_index >= 0)
            continue;
        /* select if register is used for output, input or both */
        if (op->input_index >= 0) {
            reg_mask = REG_IN_MASK | REG_OUT_MASK;
        } else if (j < nb_outputs) {
            reg_mask = REG_OUT_MASK;
        } else {
            reg_mask = REG_IN_MASK;
        }
	if (op->reg >= 0) {
	    if (is_reg_allocated(op->reg))
	        tcc_error("asm regvar requests register that's taken already");
	    reg = op->reg;
	    goto reg_found;
	}
    try_next:
        c = *str++;
        switch(c) {
        case '=':
            goto try_next;
        case '+':
            op->is_rw = 1;
            /* FALL THRU */
        case '&':
            if (j >= nb_outputs)
                tcc_error("'%c' modifier can only be applied to outputs", c);
            reg_mask = REG_IN_MASK | REG_OUT_MASK;
            goto try_next;
        case 'A':
            /* allocate both eax and edx */
            if (is_reg_allocated(TREG_XAX) ||
                is_reg_allocated(TREG_XDX))
                goto try_next;
            op->is_llong = 1;
            op->reg = TREG_XAX;
            regs_allocated[TREG_XAX] |= reg_mask;
            regs_allocated[TREG_XDX] |= reg_mask;
            break;
        case 'a':
            reg = TREG_XAX;
            goto alloc_reg;
        case 'b':
            reg = 3;
            goto alloc_reg;
        case 'c':
            reg = TREG_XCX;
            goto alloc_reg;
        case 'd':
            reg = TREG_XDX;
            goto alloc_reg;
        case 'S':
            reg = 6;
            goto alloc_reg;
        case 'D':
            reg = 7;
        alloc_reg:
            if (is_reg_allocated(reg))
                goto try_next;
            goto reg_found;
        case 'q':
            /* eax, ebx, ecx or edx */
            for(reg = 0; reg < 4; reg++) {
                if (!is_reg_allocated(reg))
                    goto reg_found;
            }
            goto try_next;
        case 'r':
	case 'R':
	case 'p': /* A general address, for x86(64) any register is acceptable*/
            /* any general register */
            for(reg = 0; reg < 8; reg++) {
                if (!is_reg_allocated(reg))
                    goto reg_found;
            }
            goto try_next;
        reg_found:
            /* now we can reload in the register */
            op->is_llong = 0;
            op->reg = reg;
            regs_allocated[reg] |= reg_mask;
            break;
	case 'e':
        case 'i':
            if (!((op->vt->r & (VT_VALMASK | VT_LVAL)) == VT_CONST))
                goto try_next;
            break;
        case 'I':
        case 'N':
        case 'M':
            if (!((op->vt->r & (VT_VALMASK | VT_LVAL | VT_SYM)) == VT_CONST))
                goto try_next;
            break;
        case 'm':
        case 'g':
            /* nothing special to do because the operand is already in
               memory, except if the pointer itself is stored in a
               memory variable (VT_LLOCAL case) */
            /* XXX: fix constant case */
            /* if it is a reference to a memory zone, it must lie
               in a register, so we reserve the register in the
               input registers and a load will be generated
               later */
            if (j < nb_outputs || c == 'm') {
                if ((op->vt->r & VT_VALMASK) == VT_LLOCAL) {
                    /* any general register */
                    for(reg = 0; reg < 8; reg++) {
                        if (!(regs_allocated[reg] & REG_IN_MASK))
                            goto reg_found1;
                    }
                    goto try_next;
                reg_found1:
                    /* now we can reload in the register */
                    regs_allocated[reg] |= REG_IN_MASK;
                    op->reg = reg;
                    op->is_memory = 1;
                }
            }
            break;
        default:
            tcc_error("asm constraint %d ('%s') could not be satisfied",
                  j, op->constraint);
            break;
        }
        /* if a reference is present for that operand, we assign it too */
        if (op->input_index >= 0) {
            operands[op->input_index].reg = op->reg;
            operands[op->input_index].is_llong = op->is_llong;
        }
    }

    /* compute out_reg. It is used to store outputs registers to memory
       locations references by pointers (VT_LLOCAL case) */
    *pout_reg = -1;
    for(i=0;i<nb_operands;i++) {
        op = &operands[i];
        if (op->reg >= 0 &&
            (op->vt->r & VT_VALMASK) == VT_LLOCAL  &&
            !op->is_memory) {
            for(reg = 0; reg < 8; reg++) {
                if (!(regs_allocated[reg] & REG_OUT_MASK))
                    goto reg_found2;
            }
            tcc_error("could not find free output register for reloading");
        reg_found2:
            *pout_reg = reg;
            break;
        }
    }

    /* print sorted constraints */
#ifdef ASM_DEBUG
    for(i=0;i<nb_operands;i++) {
        j = sorted_op[i];
        op = &operands[j];
        printf("%%%d [%s]: \"%s\" r=0x%04x reg=%d\n",
               j,
               op->id ? get_tok_str(op->id, NULL) : "",
               op->constraint,
               op->vt->r,
               op->reg);
    }
    if (*pout_reg >= 0)
        printf("out_reg=%d\n", *pout_reg);
#endif
}

ST_FUNC void subst_asm_operand(CString *add_str,
                              SValue *sv, int modifier)
{
    int r, reg, size, val;
    char buf[64];

    r = sv->r;
    if ((r & VT_VALMASK) == VT_CONST) {
        if (!(r & VT_LVAL) && modifier != 'c' && modifier != 'n' &&
	    modifier != 'P')
            cstr_ccat(add_str, '$');
        if (r & VT_SYM) {
	    const char *name = get_tok_str(sv->sym->v, NULL);
	    if (sv->sym->v >= SYM_FIRST_ANOM) {
		/* In case of anonymous symbols ("L.42", used
		   for static data labels) we can't find them
		   in the C symbol table when later looking up
		   this name.  So enter them now into the asm label
		   list when we still know the symbol.  */
		get_asm_sym(tok_alloc_const(name), sv->sym);
	    }
            if (tcc_state->leading_underscore)
              cstr_ccat(add_str, '_');
            cstr_cat(add_str, name, -1);
            if ((uint32_t)sv->c.i == 0)
                goto no_offset;
	    cstr_ccat(add_str, '+');
        }
        val = sv->c.i;
        if (modifier == 'n')
            val = -val;
        snprintf(buf, sizeof(buf), "%d", (int)sv->c.i);
        cstr_cat(add_str, buf, -1);
    no_offset:;
#ifdef TCC_TARGET_X86_64
        if (r & VT_LVAL)
            cstr_cat(add_str, "(%rip)", -1);
#endif
    } else if ((r & VT_VALMASK) == VT_LOCAL) {
#ifdef TCC_TARGET_X86_64
        snprintf(buf, sizeof(buf), "%d(%%rbp)", (int)sv->c.i);
#else
        snprintf(buf, sizeof(buf), "%d(%%ebp)", (int)sv->c.i);
#endif
        cstr_cat(add_str, buf, -1);
    } else if (r & VT_LVAL) {
        reg = r & VT_VALMASK;
        if (reg >= VT_CONST)
            tcc_internal_error("");
        snprintf(buf, sizeof(buf), "(%%%s)",
#ifdef TCC_TARGET_X86_64
                 get_tok_str(TOK_ASM_rax + reg, NULL)
#else
                 get_tok_str(TOK_ASM_eax + reg, NULL)
#endif
		 );
        cstr_cat(add_str, buf, -1);
    } else {
        /* register case */
        reg = r & VT_VALMASK;
        if (reg >= VT_CONST)
            tcc_internal_error("");

        /* choose register operand size */
        if ((sv->type.t & VT_BTYPE) == VT_BYTE ||
	    (sv->type.t & VT_BTYPE) == VT_BOOL)
            size = 1;
        else if ((sv->type.t & VT_BTYPE) == VT_SHORT)
            size = 2;
#ifdef TCC_TARGET_X86_64
        else if ((sv->type.t & VT_BTYPE) == VT_LLONG ||
		 (sv->type.t & VT_BTYPE) == VT_PTR)
            size = 8;
#endif
        else
            size = 4;
        if (size == 1 && reg >= 4)
            size = 4;

        if (modifier == 'b') {
            if (reg >= 4)
                tcc_error("cannot use byte register");
            size = 1;
        } else if (modifier == 'h') {
            if (reg >= 4)
                tcc_error("cannot use byte register");
            size = -1;
        } else if (modifier == 'w') {
            size = 2;
        } else if (modifier == 'k') {
            size = 4;
#ifdef TCC_TARGET_X86_64
        } else if (modifier == 'q') {
            size = 8;
#endif
        }

        switch(size) {
        case -1:
            reg = TOK_ASM_ah + reg;
            break;
        case 1:
            reg = TOK_ASM_al + reg;
            break;
        case 2:
            reg = TOK_ASM_ax + reg;
            break;
        default:
            reg = TOK_ASM_eax + reg;
            break;
#ifdef TCC_TARGET_X86_64
        case 8:
            reg = TOK_ASM_rax + reg;
            break;
#endif
        }
        snprintf(buf, sizeof(buf), "%%%s", get_tok_str(reg, NULL));
        cstr_cat(add_str, buf, -1);
    }
}

/* generate prolog and epilog code for asm statement */
ST_FUNC void asm_gen_code(ASMOperand *operands, int nb_operands,
                         int nb_outputs, int is_output,
                         uint8_t *clobber_regs,
                         int out_reg)
{
    uint8_t regs_allocated[NB_ASM_REGS];
    ASMOperand *op;
    int i, reg;

    /* Strictly speaking %Xbp and %Xsp should be included in the
       call-preserved registers, but currently it doesn't matter.  */
#ifdef TCC_TARGET_X86_64
#ifdef TCC_TARGET_PE
    static const uint8_t reg_saved[] = { 3, 6, 7, 12, 13, 14, 15 };
#else
    static const uint8_t reg_saved[] = { 3, 12, 13, 14, 15 };
#endif
#else
    static const uint8_t reg_saved[] = { 3, 6, 7 };
#endif

    /* mark all used registers */
    memcpy(regs_allocated, clobber_regs, sizeof(regs_allocated));
    for(i = 0; i < nb_operands;i++) {
        op = &operands[i];
        if (op->reg >= 0)
            regs_allocated[op->reg] = 1;
    }
    if (!is_output) {
        /* generate reg save code */
        for(i = 0; i < sizeof(reg_saved)/sizeof(reg_saved[0]); i++) {
            reg = reg_saved[i];
            if (regs_allocated[reg]) {
		if (reg >= 8)
		  g(0x41), reg-=8;
                g(0x50 + reg);
            }
        }

        /* generate load code */
        for(i = 0; i < nb_operands; i++) {
            op = &operands[i];
            if (op->reg >= 0) {
                if ((op->vt->r & VT_VALMASK) == VT_LLOCAL &&
                    op->is_memory) {
                    /* memory reference case (for both input and
                       output cases) */
                    SValue sv;
                    sv = *op->vt;
                    sv.r = (sv.r & ~VT_VALMASK) | VT_LOCAL | VT_LVAL;
                    sv.type.t = VT_PTR;
                    load(op->reg, &sv);
                } else if (i >= nb_outputs || op->is_rw) {
                    /* load value in register */
                    load(op->reg, op->vt);
                    if (op->is_llong) {
                        SValue sv;
                        sv = *op->vt;
                        sv.c.i += 4;
                        load(TREG_XDX, &sv);
                    }
                }
            }
        }
    } else {
        /* generate save code */
        for(i = 0 ; i < nb_outputs; i++) {
            op = &operands[i];
            if (op->reg >= 0) {
                if ((op->vt->r & VT_VALMASK) == VT_LLOCAL) {
                    if (!op->is_memory) {
                        SValue sv;
                        sv = *op->vt;
                        sv.r = (sv.r & ~VT_VALMASK) | VT_LOCAL;
			sv.type.t = VT_PTR;
                        load(out_reg, &sv);

			sv = *op->vt;
                        sv.r = (sv.r & ~VT_VALMASK) | out_reg;
                        store(op->reg, &sv);
                    }
                } else {
                    store(op->reg, op->vt);
                    if (op->is_llong) {
                        SValue sv;
                        sv = *op->vt;
                        sv.c.i += 4;
                        store(TREG_XDX, &sv);
                    }
                }
            }
        }
        /* generate reg restore code */
        for(i = sizeof(reg_saved)/sizeof(reg_saved[0]) - 1; i >= 0; i--) {
            reg = reg_saved[i];
            if (regs_allocated[reg]) {
		if (reg >= 8)
		  g(0x41), reg-=8;
                g(0x58 + reg);
            }
        }
    }
}

ST_FUNC void asm_clobber(uint8_t *clobber_regs, const char *str)
{
    int reg;
#ifdef TCC_TARGET_X86_64
    unsigned int type;
#endif

    if (!strcmp(str, "memory") ||
        !strcmp(str, "cc") ||
	!strcmp(str, "flags"))
        return;
    reg = tok_alloc_const(str);
    if (reg >= TOK_ASM_eax && reg <= TOK_ASM_edi) {
        reg -= TOK_ASM_eax;
    } else if (reg >= TOK_ASM_ax && reg <= TOK_ASM_di) {
        reg -= TOK_ASM_ax;
#ifdef TCC_TARGET_X86_64
    } else if (reg >= TOK_ASM_rax && reg <= TOK_ASM_rdi) {
        reg -= TOK_ASM_rax;
    } else if ((reg = asm_parse_numeric_reg(reg, &type)) >= 0) {
	;
#endif
    } else {
        tcc_error("invalid clobber register '%s'", str);
    }
    clobber_regs[reg] = 1;
}

//// tcc: libtcc.c

/*
 *  TCC - Tiny C Compiler
 *
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#if !defined ONE_SOURCE || ONE_SOURCE
#include "tccpp.c"
#include "tccgen.c"
#include "tccdbg.c"
#include "tccasm.c"
#include "tccelf.c"
#include "tccrun.c"
#ifdef TCC_TARGET_I386
#include "i386-gen.c"
#include "i386-link.c"
#include "i386-asm.c"
#elif defined(TCC_TARGET_ARM)
#include "arm-gen.c"
#include "arm-link.c"
#include "arm-asm.c"
#elif defined(TCC_TARGET_ARM64)
#include "arm64-gen.c"
#include "arm64-link.c"
#include "arm-asm.c"
#elif defined(TCC_TARGET_C67)
#include "c67-gen.c"
#include "c67-link.c"
#include "tcccoff.c"
#elif defined(TCC_TARGET_X86_64)
#include "x86_64-gen.c"
#include "x86_64-link.c"
#include "i386-asm.c"
#elif defined(TCC_TARGET_RISCV64)
#include "riscv64-gen.c"
#include "riscv64-link.c"
#include "riscv64-asm.c"
#else
#error unknown target
#endif
#ifdef TCC_TARGET_PE
#include "tccpe.c"
#endif
#ifdef TCC_TARGET_MACHO
#include "tccmacho.c"
#endif
#endif /* ONE_SOURCE */

#define TCC_SEM_IMPL 1
#include "tcc.h"

/********************************************************/
/* global variables */

/* XXX: get rid of this ASAP (or maybe not) */
ST_DATA struct TCCState *tcc_state;
TCC_SEM(static tcc_compile_sem);
/* an array of pointers to memory to be free'd after errors */
ST_DATA void** stk_data;
ST_DATA int nb_stk_data;

/********************************************************/
#ifdef _WIN32
ST_FUNC char *normalize_slashes(char *path)
{
    char *p;
    for (p = path; *p; ++p)
        if (*p == '\\')
            *p = '/';
    return path;
}

#if defined LIBTCC_AS_DLL && !defined CONFIG_TCCDIR
static HMODULE tcc_module;
BOOL WINAPI DllMain (HINSTANCE hDll, DWORD dwReason, LPVOID lpReserved)
{
    if (DLL_PROCESS_ATTACH == dwReason)
        tcc_module = hDll;
    return TRUE;
}
#else
#define tcc_module NULL /* NULL means executable itself */
#endif

#ifndef CONFIG_TCCDIR
/* on win32, we suppose the lib and includes are at the location of 'tcc.exe' */
static inline char *config_tccdir_w32(char *path)
{
    char *p;
    GetModuleFileName(tcc_module, path, MAX_PATH);
    p = tcc_basename(normalize_slashes(strlwr(path)));
    if (p > path)
        --p;
    *p = 0;
    return path;
}
#define CONFIG_TCCDIR config_tccdir_w32(alloca(MAX_PATH))
#endif

#ifdef TCC_IS_NATIVE
static void tcc_add_systemdir(TCCState *s)
{
    char buf[1000];
    GetSystemDirectory(buf, sizeof buf);
    tcc_add_library_path(s, normalize_slashes(buf));
}
#endif
#endif

/********************************************************/

PUB_FUNC void tcc_enter_state(TCCState *s1)
{
    if (s1->error_set_jmp_enabled)
        return;
    WAIT_SEM(&tcc_compile_sem);
    tcc_state = s1;
}

PUB_FUNC void tcc_exit_state(TCCState *s1)
{
    if (s1->error_set_jmp_enabled)
        return;
    tcc_state = NULL;
    POST_SEM(&tcc_compile_sem);
}

/********************************************************/
/* copy a string and truncate it. */
ST_FUNC char *pstrcpy(char *buf, size_t buf_size, const char *s)
{
    char *q, *q_end;
    int c;

    if (buf_size > 0) {
        q = buf;
        q_end = buf + buf_size - 1;
        while (q < q_end) {
            c = *s++;
            if (c == '\0')
                break;
            *q++ = c;
        }
        *q = '\0';
    }
    return buf;
}

/* strcat and truncate. */
ST_FUNC char *pstrcat(char *buf, size_t buf_size, const char *s)
{
    size_t len;
    len = strlen(buf);
    if (len < buf_size)
        pstrcpy(buf + len, buf_size - len, s);
    return buf;
}

ST_FUNC char *pstrncpy(char *out, const char *in, size_t num)
{
    memcpy(out, in, num);
    out[num] = '\0';
    return out;
}

/* extract the basename of a file */
PUB_FUNC char *tcc_basename(const char *name)
{
    char *p = strchr(name, 0);
    while (p > name && !IS_DIRSEP(p[-1]))
        --p;
    return p;
}

/* extract extension part of a file
 *
 * (if no extension, return pointer to end-of-string)
 */
PUB_FUNC char *tcc_fileextension (const char *name)
{
    char *b = tcc_basename(name);
    char *e = strrchr(b, '.');
    return e ? e : strchr(b, 0);
}

ST_FUNC char *tcc_load_text(int fd)
{
    int len = lseek(fd, 0, SEEK_END);
    char *buf = load_data(fd, 0, len + 1);
    buf[len] = 0;
    return buf;
}

/********************************************************/
/* memory management */

/* we'll need the actual versions for a minute */
#undef free
#undef realloc

static void *default_reallocator(void *ptr, unsigned long size)
{
    void *ptr1;
    if (size == 0) {
        free(ptr);
        ptr1 = NULL;
    }
    else {
        ptr1 = realloc(ptr, size);
        if (!ptr1) {
            fprintf(stderr, "memory full\n");
            exit (1);
        }
    }
    return ptr1;
}

ST_FUNC void libc_free(void *ptr)
{
    free(ptr);
}

#define free(p) use_tcc_free(p)
#define realloc(p, s) use_tcc_realloc(p, s)

/* global so that every tcc_alloc()/tcc_free() call doesn't need to be changed */
static void *(*reallocator)(void*, unsigned long) = default_reallocator;

LIBTCCAPI void tcc_set_realloc(TCCReallocFunc *realloc)
{
    reallocator = realloc ? realloc : default_reallocator;
}

/* in case MEM_DEBUG is #defined */
#undef tcc_free
#undef tcc_malloc
#undef tcc_realloc
#undef tcc_mallocz
#undef tcc_strdup

PUB_FUNC void tcc_free(void *ptr)
{
    reallocator(ptr, 0);
}

PUB_FUNC void *tcc_malloc(unsigned long size)
{
    return reallocator(0, size);
}

PUB_FUNC void *tcc_realloc(void *ptr, unsigned long size)
{
    return reallocator(ptr, size);
}

PUB_FUNC void *tcc_mallocz(unsigned long size)
{
    void *ptr;
    ptr = tcc_malloc(size);
    if (size)
        memset(ptr, 0, size);
    return ptr;
}

PUB_FUNC char *tcc_strdup(const char *str)
{
    char *ptr;
    ptr = tcc_malloc(strlen(str) + 1);
    strcpy(ptr, str);
    return ptr;
}

#ifdef MEM_DEBUG

#define MEM_DEBUG_MAGIC1 0xFEEDDEB1
#define MEM_DEBUG_MAGIC2 0xFEEDDEB2
#define MEM_DEBUG_MAGIC3 0xFEEDDEB3
#define MEM_DEBUG_FILE_LEN 40
#define MEM_DEBUG_CHECK3(header) \
    ((mem_debug_header_t*)((char*)header + header->size))->magic3
#define MEM_USER_PTR(header) \
    ((char *)header + offsetof(mem_debug_header_t, magic3))
#define MEM_HEADER_PTR(ptr) \
    (mem_debug_header_t *)((char*)ptr - offsetof(mem_debug_header_t, magic3))

struct mem_debug_header {
    unsigned magic1;
    unsigned size;
    struct mem_debug_header *prev;
    struct mem_debug_header *next;
    int line_num;
    char file_name[MEM_DEBUG_FILE_LEN + 1];
    unsigned magic2;
    ALIGNED(16) unsigned char magic3[4];
};

typedef struct mem_debug_header mem_debug_header_t;

TCC_SEM(static mem_sem);
static mem_debug_header_t *mem_debug_chain;
static unsigned mem_cur_size;
static unsigned mem_max_size;
static int nb_states;

static mem_debug_header_t *malloc_check(void *ptr, const char *msg)
{
    mem_debug_header_t * header = MEM_HEADER_PTR(ptr);
    if (header->magic1 != MEM_DEBUG_MAGIC1 ||
        header->magic2 != MEM_DEBUG_MAGIC2 ||
        read32le(MEM_DEBUG_CHECK3(header)) != MEM_DEBUG_MAGIC3 ||
        header->size == (unsigned)-1) {
        fprintf(stderr, "%s check failed\n", msg);
        if (header->magic1 == MEM_DEBUG_MAGIC1)
            fprintf(stderr, "%s:%u: block allocated here.\n",
                header->file_name, header->line_num);
        exit(1);
    }
    return header;
}

PUB_FUNC void *tcc_malloc_debug(unsigned long size, const char *file, int line)
{
    int ofs;
    mem_debug_header_t *header;

    header = tcc_malloc(sizeof(mem_debug_header_t) + size);
    header->magic1 = MEM_DEBUG_MAGIC1;
    header->magic2 = MEM_DEBUG_MAGIC2;
    header->size = size;
    write32le(MEM_DEBUG_CHECK3(header), MEM_DEBUG_MAGIC3);
    header->line_num = line;
    ofs = strlen(file) - MEM_DEBUG_FILE_LEN;
    strncpy(header->file_name, file + (ofs > 0 ? ofs : 0), MEM_DEBUG_FILE_LEN);
    header->file_name[MEM_DEBUG_FILE_LEN] = 0;

    WAIT_SEM(&mem_sem);
    header->next = mem_debug_chain;
    header->prev = NULL;
    if (header->next)
        header->next->prev = header;
    mem_debug_chain = header;
    mem_cur_size += size;
    if (mem_cur_size > mem_max_size)
        mem_max_size = mem_cur_size;
    POST_SEM(&mem_sem);

    return MEM_USER_PTR(header);
}

PUB_FUNC void tcc_free_debug(void *ptr)
{
    mem_debug_header_t *header;
    if (!ptr)
        return;
    header = malloc_check(ptr, "tcc_free");

    WAIT_SEM(&mem_sem);
    mem_cur_size -= header->size;
    header->size = (unsigned)-1;
    if (header->next)
        header->next->prev = header->prev;
    if (header->prev)
        header->prev->next = header->next;
    if (header == mem_debug_chain)
        mem_debug_chain = header->next;
    POST_SEM(&mem_sem);
    tcc_free(header);
}

PUB_FUNC void *tcc_mallocz_debug(unsigned long size, const char *file, int line)
{
    void *ptr;
    ptr = tcc_malloc_debug(size,file,line);
    memset(ptr, 0, size);
    return ptr;
}

PUB_FUNC void *tcc_realloc_debug(void *ptr, unsigned long size, const char *file, int line)
{
    mem_debug_header_t *header;
    int mem_debug_chain_update = 0;
    if (!ptr)
        return tcc_malloc_debug(size, file, line);
    header = malloc_check(ptr, "tcc_realloc");

    WAIT_SEM(&mem_sem);
    mem_cur_size -= header->size;
    mem_debug_chain_update = (header == mem_debug_chain);
    header = tcc_realloc(header, sizeof(mem_debug_header_t) + size);
    header->size = size;
    write32le(MEM_DEBUG_CHECK3(header), MEM_DEBUG_MAGIC3);
    if (header->next)
        header->next->prev = header;
    if (header->prev)
        header->prev->next = header;
    if (mem_debug_chain_update)
        mem_debug_chain = header;
    mem_cur_size += size;
    if (mem_cur_size > mem_max_size)
        mem_max_size = mem_cur_size;
    POST_SEM(&mem_sem);

    return MEM_USER_PTR(header);
}

PUB_FUNC char *tcc_strdup_debug(const char *str, const char *file, int line)
{
    char *ptr;
    ptr = tcc_malloc_debug(strlen(str) + 1, file, line);
    strcpy(ptr, str);
    return ptr;
}

PUB_FUNC void tcc_memcheck(int d)
{
    WAIT_SEM(&mem_sem);
    nb_states += d;
    if (0 == nb_states && mem_cur_size) {
        mem_debug_header_t *header = mem_debug_chain;
        fflush(stdout);
        fprintf(stderr, "MEM_DEBUG: mem_leak= %d bytes, mem_max_size= %d bytes\n",
            mem_cur_size, mem_max_size);
        while (header) {
            fprintf(stderr, "%s:%u: error: %u bytes leaked\n",
                header->file_name, header->line_num, header->size);
            header = header->next;
        }
        fflush(stderr);
        mem_cur_size = 0;
        mem_debug_chain = NULL;
#if MEM_DEBUG-0 == 2
        exit(2);
#endif
    }
    POST_SEM(&mem_sem);
}

/* restore the debug versions */
#define tcc_free(ptr)           tcc_free_debug(ptr)
#define tcc_malloc(size)        tcc_malloc_debug(size, __FILE__, __LINE__)
#define tcc_mallocz(size)       tcc_mallocz_debug(size, __FILE__, __LINE__)
#define tcc_realloc(ptr,size)   tcc_realloc_debug(ptr, size, __FILE__, __LINE__)
#define tcc_strdup(str)         tcc_strdup_debug(str, __FILE__, __LINE__)

#endif /* MEM_DEBUG */

#ifdef _WIN32
# define realpath(file, buf) _fullpath(buf, file, 260)
#endif

/* for #pragma once */
ST_FUNC int normalized_PATHCMP(const char *f1, const char *f2)
{
    char *p1, *p2;
    int ret = 1;
    if (!!(p1 = realpath(f1, NULL))) {
        if (!!(p2 = realpath(f2, NULL))) {
            ret = PATHCMP(p1, p2);
            libc_free(p2); /* realpath() requirement */
        }
        libc_free(p1);
    }
    return ret;
}

/********************************************************/
/* dynarrays */

ST_FUNC void dynarray_add(void *ptab, int *nb_ptr, void *data)
{
    int nb, nb_alloc;
    void **pp;

    nb = *nb_ptr;
    pp = *(void ***)ptab;
    /* every power of two we double array size */
    if ((nb & (nb - 1)) == 0) {
        if (!nb)
            nb_alloc = 1;
        else
            nb_alloc = nb * 2;
        pp = tcc_realloc(pp, nb_alloc * sizeof(void *));
        *(void***)ptab = pp;
    }
    pp[nb++] = data;
    *nb_ptr = nb;
}

ST_FUNC void dynarray_reset(void *pp, int *n)
{
    void **p;
    for (p = *(void***)pp; *n; ++p, --*n)
        if (*p)
            tcc_free(*p);
    tcc_free(*(void**)pp);
    *(void**)pp = NULL;
}

static void tcc_split_path(TCCState *s, void *p_ary, int *p_nb_ary, const char *in)
{
    const char *p;
    do {
        int c;
        CString str;

        cstr_new(&str);
        for (p = in; c = *p, c != '\0' && c != PATHSEP[0]; ++p) {
            if (c == '{' && p[1] && p[2] == '}') {
                c = p[1], p += 2;
                if (c == 'B')
                    cstr_cat(&str, s->tcc_lib_path, -1);
                if (c == 'R')
                    cstr_cat(&str, CONFIG_SYSROOT, -1);
                if (c == 'f' && file) {
                    /* substitute current file's dir */
                    const char *f = file->true_filename;
                    const char *b = tcc_basename(f);
                    if (b > f)
                        cstr_cat(&str, f, b - f - 1);
                    else
                        cstr_cat(&str, ".", 1);
                }
            } else {
                cstr_ccat(&str, c);
            }
        }
        if (str.size) {
            cstr_ccat(&str, '\0');
            dynarray_add(p_ary, p_nb_ary, tcc_strdup(str.data));
        }
        cstr_free(&str);
        in = p+1;
    } while (*p);
}

/********************************************************/
/* warning / error */

/* warn_... option bits */
#define WARN_ON  1 /* warning is on (-Woption) */
#define WARN_ERR 2 /* warning is an error (-Werror=option) */
#define WARN_NOE 4 /* warning is not an error (-Wno-error=option) */

/* error1() modes */
enum { ERROR_WARN, ERROR_NOABORT, ERROR_ERROR };

static void error1(int mode, const char *fmt, va_list ap)
{
    BufferedFile **pf, *f;
    TCCState *s1 = tcc_state;
    CString cs;

    tcc_exit_state(s1);

    if (mode == ERROR_WARN) {
        if (s1->warn_error)
            mode = ERROR_ERROR;
        if (s1->warn_num) {
            /* handle tcc_warning_c(warn_option)(fmt, ...) */
            int wopt = *(&s1->warn_none + s1->warn_num);
            s1->warn_num = 0;
            if (0 == (wopt & WARN_ON))
                return;
            if (wopt & WARN_ERR)
                mode = ERROR_ERROR;
            if (wopt & WARN_NOE)
                mode = ERROR_WARN;
        }
        if (s1->warn_none)
            return;
    }

    cstr_new(&cs);
    f = NULL;
    if (s1->error_set_jmp_enabled) { /* we're called while parsing a file */
        /* use upper file if inline ":asm:" or token ":paste:" */
        for (f = file; f && f->filename[0] == ':'; f = f->prev)
            ;
    }
    if (f) {
        for(pf = s1->include_stack; pf < s1->include_stack_ptr; pf++)
            cstr_printf(&cs, "In file included from %s:%d:\n",
                (*pf)->filename, (*pf)->line_num - 1);
        cstr_printf(&cs, "%s:%d: ",
            f->filename, f->line_num - ((tok_flags & TOK_FLAG_BOL) && !macro_ptr));
    } else if (s1->current_filename) {
        cstr_printf(&cs, "%s: ", s1->current_filename);
    } else {
        cstr_printf(&cs, "tcc: ");
    }
    cstr_printf(&cs, mode == ERROR_WARN ? "warning: " : "error: ");
    if (pp_expr > 1)
        pp_error(&cs); /* special handler for preprocessor expression errors */
    else
        cstr_vprintf(&cs, fmt, ap);
    if (!s1->error_func) {
        /* default case: stderr */
        if (s1 && s1->output_type == TCC_OUTPUT_PREPROCESS && s1->ppfp == stdout)
            printf("\n"); /* print a newline during tcc -E */
        fflush(stdout); /* flush -v output */
        fprintf(stderr, "%s\n", (char*)cs.data);
        fflush(stderr); /* print error/warning now (win32) */
    } else {
        s1->error_func(s1->error_opaque, (char*)cs.data);
    }
    cstr_free(&cs);
    if (mode != ERROR_WARN)
        s1->nb_errors++;
    if (mode == ERROR_ERROR && s1->error_set_jmp_enabled) {
        while (nb_stk_data)
            tcc_free(*(void**)stk_data[--nb_stk_data]);
        longjmp(s1->error_jmp_buf, 1);
    }
}

LIBTCCAPI void tcc_set_error_func(TCCState *s, void *error_opaque, TCCErrorFunc *error_func)
{
    s->error_opaque = error_opaque;
    s->error_func = error_func;
}

/* error without aborting current compilation */
PUB_FUNC int _tcc_error_noabort(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    error1(ERROR_NOABORT, fmt, ap);
    va_end(ap);
    return -1;
}

#undef _tcc_error
PUB_FUNC void _tcc_error(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    error1(ERROR_ERROR, fmt, ap);
    exit(1);
}
#define _tcc_error use_tcc_error_noabort

PUB_FUNC void _tcc_warning(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    error1(ERROR_WARN, fmt, ap);
    va_end(ap);
}


/********************************************************/
/* I/O layer */

ST_FUNC void tcc_open_bf(TCCState *s1, const char *filename, int initlen)
{
    BufferedFile *bf;
    int buflen = initlen ? initlen : IO_BUF_SIZE;

    bf = tcc_mallocz(sizeof(BufferedFile) + buflen);
    bf->buf_ptr = bf->buffer;
    bf->buf_end = bf->buffer + initlen;
    bf->buf_end[0] = CH_EOB; /* put eob symbol */
    pstrcpy(bf->filename, sizeof(bf->filename), filename);
#ifdef _WIN32
    normalize_slashes(bf->filename);
#endif
    bf->true_filename = bf->filename;
    bf->line_num = 1;
    bf->ifdef_stack_ptr = s1->ifdef_stack_ptr;
    bf->fd = -1;
    bf->prev = file;
    bf->prev_tok_flags = tok_flags;
    file = bf;
    tok_flags = TOK_FLAG_BOL | TOK_FLAG_BOF;
}

ST_FUNC void tcc_close(void)
{
    TCCState *s1 = tcc_state;
    BufferedFile *bf = file;
    if (bf->fd > 0) {
        close(bf->fd);
        total_lines += bf->line_num - 1;
    }
    if (bf->true_filename != bf->filename)
        tcc_free(bf->true_filename);
    file = bf->prev;
    tok_flags = bf->prev_tok_flags;
    tcc_free(bf);
}

static int _tcc_open(TCCState *s1, const char *filename)
{
    int fd;
    if (strcmp(filename, "-") == 0)
        fd = 0, filename = "<stdin>";
    else
        fd = open(filename, O_RDONLY | O_BINARY);
    if ((s1->verbose == 2 && fd >= 0) || s1->verbose == 3)
        printf("%s %*s%s\n", fd < 0 ? "nf":"->",
               (int)(s1->include_stack_ptr - s1->include_stack), "", filename);
    return fd;
}

ST_FUNC int tcc_open(TCCState *s1, const char *filename)
{
    int fd = _tcc_open(s1, filename);
    if (fd < 0)
        return -1;
    tcc_open_bf(s1, filename, 0);
    file->fd = fd;
    return 0;
}

/* compile the file opened in 'file'. Return non zero if errors. */
static int tcc_compile(TCCState *s1, int filetype, const char *str, int fd)
{
    /* Here we enter the code section where we use the global variables for
       parsing and code generation (tccpp.c, tccgen.c, <target>-gen.c).
       Other threads need to wait until we're done.

       Alternatively we could use thread local storage for those global
       variables, which may or may not have advantages */

    tcc_enter_state(s1);
    s1->error_set_jmp_enabled = 1;

    if (setjmp(s1->error_jmp_buf) == 0) {
        s1->nb_errors = 0;

        if (fd == -1) {
            int len = strlen(str);
            tcc_open_bf(s1, "<string>", len);
            memcpy(file->buffer, str, len);
        } else {
            tcc_open_bf(s1, str, 0);
            file->fd = fd;
        }

        preprocess_start(s1, filetype);
        tccgen_init(s1);

        if (s1->output_type == TCC_OUTPUT_PREPROCESS) {
            tcc_preprocess(s1);
        } else {
            tccelf_begin_file(s1);
            if (filetype & (AFF_TYPE_ASM | AFF_TYPE_ASMPP)) {
                tcc_assemble(s1, !!(filetype & AFF_TYPE_ASMPP));
            } else {
                tccgen_compile(s1);
            }
            tccelf_end_file(s1);
        }
    }
    tccgen_finish(s1);
    preprocess_end(s1);
    s1->error_set_jmp_enabled = 0;
    tcc_exit_state(s1);
    return s1->nb_errors != 0 ? -1 : 0;
}

LIBTCCAPI int tcc_compile_string(TCCState *s, const char *str)
{
    return tcc_compile(s, s->filetype, str, -1);
}

/* define a preprocessor symbol. value can be NULL, sym can be "sym=val" */
LIBTCCAPI void tcc_define_symbol(TCCState *s1, const char *sym, const char *value)
{
    const char *eq;
    if (NULL == (eq = strchr(sym, '=')))
        eq = strchr(sym, 0);
    if (NULL == value)
        value = *eq ? eq + 1 : "1";
    cstr_printf(&s1->cmdline_defs, "#define %.*s %s\n", (int)(eq-sym), sym, value);
}

/* undefine a preprocessor symbol */
LIBTCCAPI void tcc_undefine_symbol(TCCState *s1, const char *sym)
{
    cstr_printf(&s1->cmdline_defs, "#undef %s\n", sym);
}


LIBTCCAPI TCCState *tcc_new(void)
{
    TCCState *s;

    s = tcc_mallocz(sizeof(TCCState));
    if (!s)
        return NULL;
#ifdef MEM_DEBUG
    tcc_memcheck(1);
#endif

#undef gnu_ext

    s->gnu_ext = 1;
    s->tcc_ext = 1;
    s->nocommon = 1;
    s->dollars_in_identifiers = 1; /*on by default like in gcc/clang*/
    s->cversion = 199901; /* default unless -std=c11 is supplied */
    s->warn_implicit_function_declaration = 1;
    s->warn_discarded_qualifiers = 1;
    s->ms_extensions = 1;

#ifdef CHAR_IS_UNSIGNED
    s->char_is_unsigned = 1;
#endif
#ifdef TCC_TARGET_I386
    s->seg_size = 32;
#endif
    /* enable this if you want symbols with leading underscore on windows: */
#if defined TCC_TARGET_MACHO /* || defined TCC_TARGET_PE */
    s->leading_underscore = 1;
#endif
#ifdef TCC_TARGET_ARM
    s->float_abi = ARM_FLOAT_ABI;
#endif
#ifdef CONFIG_NEW_DTAGS
    s->enable_new_dtags = 1;
#endif
    s->ppfp = stdout;
    /* might be used in error() before preprocess_start() */
    s->include_stack_ptr = s->include_stack;

    tcc_set_lib_path(s, CONFIG_TCCDIR);
    return s;
}

LIBTCCAPI void tcc_delete(TCCState *s1)
{
    /* free sections */
    tccelf_delete(s1);

    /* free library paths */
    dynarray_reset(&s1->library_paths, &s1->nb_library_paths);
    dynarray_reset(&s1->crt_paths, &s1->nb_crt_paths);

    /* free include paths */
    dynarray_reset(&s1->include_paths, &s1->nb_include_paths);
    dynarray_reset(&s1->sysinclude_paths, &s1->nb_sysinclude_paths);

    tcc_free(s1->tcc_lib_path);
    tcc_free(s1->soname);
    tcc_free(s1->rpath);
    tcc_free(s1->elf_entryname);
    tcc_free(s1->init_symbol);
    tcc_free(s1->fini_symbol);
    tcc_free(s1->mapfile);
    tcc_free(s1->outfile);
    tcc_free(s1->deps_outfile);
#if defined TCC_TARGET_MACHO
    tcc_free(s1->install_name);
#endif
    dynarray_reset(&s1->files, &s1->nb_files);
    dynarray_reset(&s1->target_deps, &s1->nb_target_deps);
    dynarray_reset(&s1->pragma_libs, &s1->nb_pragma_libs);
    dynarray_reset(&s1->argv, &s1->argc);
    cstr_free(&s1->cmdline_defs);
    cstr_free(&s1->cmdline_incl);
    cstr_free(&s1->linker_arg);
#ifdef TCC_IS_NATIVE
    /* free runtime memory */
    tcc_run_free(s1);
#endif
    tcc_free(s1->dState);
    tcc_free(s1);
#ifdef MEM_DEBUG
    tcc_memcheck(-1);
#endif
}

LIBTCCAPI int tcc_set_output_type(TCCState *s, int output_type)
{
#ifdef CONFIG_TCC_PIE
    if (output_type == TCC_OUTPUT_EXE)
        output_type |= TCC_OUTPUT_DYN;
#endif
    s->output_type = output_type;

    if (!s->nostdinc) {
        /* default include paths */
        /* -isystem paths have already been handled */
        tcc_add_sysinclude_path(s, CONFIG_TCC_SYSINCLUDEPATHS);
    }

    if (output_type == TCC_OUTPUT_PREPROCESS) {
        s->do_debug = 0;
        return 0;
    }

    /* add sections */
    tccelf_new(s);

    if (output_type == TCC_OUTPUT_OBJ) {
        /* always elf for objects */
        s->output_format = TCC_OUTPUT_FORMAT_ELF;
        return 0;
    }

    tcc_add_library_path(s, CONFIG_TCC_LIBPATHS);

#ifdef TCC_TARGET_PE
# ifdef TCC_IS_NATIVE
    /* allow linking with system dll's directly */
    tcc_add_systemdir(s);
# endif
#elif defined TCC_TARGET_MACHO
# ifdef TCC_IS_NATIVE
    tcc_add_macos_sdkpath(s);
# endif
#else
    /* paths for crt objects */
    tcc_split_path(s, &s->crt_paths, &s->nb_crt_paths, CONFIG_TCC_CRTPREFIX);
    if (output_type != TCC_OUTPUT_MEMORY && !s->nostdlib)
        tccelf_add_crtbegin(s);
#endif
    return 0;
}

LIBTCCAPI int tcc_add_include_path(TCCState *s, const char *pathname)
{
    tcc_split_path(s, &s->include_paths, &s->nb_include_paths, pathname);
    return 0;
}

LIBTCCAPI int tcc_add_sysinclude_path(TCCState *s, const char *pathname)
{
    tcc_split_path(s, &s->sysinclude_paths, &s->nb_sysinclude_paths, pathname);
    return 0;
}

/* add/update a 'DLLReference', Just find if level == -1  */
ST_FUNC DLLReference *tcc_add_dllref(TCCState *s1, const char *dllname, int level)
{
    DLLReference *ref = NULL;
    int i;
    for (i = 0; i < s1->nb_loaded_dlls; i++)
        if (0 == strcmp(s1->loaded_dlls[i]->name, dllname)) {
            ref = s1->loaded_dlls[i];
            break;
        }
    if (level == -1)
        return ref;
    if (ref) {
        if (level < ref->level)
            ref->level = level;
        ref->found = 1;
        return ref;
    }
    ref = tcc_mallocz(sizeof(DLLReference) + strlen(dllname));
    strcpy(ref->name, dllname);
    dynarray_add(&s1->loaded_dlls, &s1->nb_loaded_dlls, ref);
    ref->level = level;
    ref->index = s1->nb_loaded_dlls;
    return ref;
}

/* OpenBSD: choose latest from libxxx.so.x.y versions */
#if defined TARGETOS_OpenBSD && !defined _WIN32
#include <glob.h>
static int tcc_glob_so(TCCState *s1, const char *pattern, char *buf, int size)
{
    const char *star;
    glob_t g;
    char *p;
    int i, v, v1, v2, v3;

    star = strchr(pattern, '*');
    if (!star || glob(pattern, 0, NULL, &g))
        return -1;
    for (v = -1, i = 0; i < g.gl_pathc; ++i) {
        p = g.gl_pathv[i];
        if (2 != sscanf(p + (star - pattern), "%d.%d.%d", &v1, &v2, &v3))
            continue;
        if ((v1 = v1 * 1000 + v2) > v)
            v = v1, pstrcpy(buf, size, p);
    }
    globfree(&g);
    return v;
}
#endif

ST_FUNC int tcc_add_file_internal(TCCState *s1, const char *filename, int flags)
{
    int fd, ret = -1;

#if defined TARGETOS_OpenBSD && !defined _WIN32
    char buf[1024];
    if (tcc_glob_so(s1, filename, buf, sizeof buf) >= 0)
        filename = buf;
#endif

    /* ignore binary files with -E */
    if (s1->output_type == TCC_OUTPUT_PREPROCESS
        && (flags & AFF_TYPE_BIN))
        return 0;

    /* open the file */
    fd = _tcc_open(s1, filename);
    if (fd < 0) {
        if (flags & AFF_PRINT_ERROR)
            tcc_error_noabort("file '%s' not found", filename);
        return FILE_NOT_FOUND;
    }

    s1->current_filename = filename;
    if (flags & AFF_TYPE_BIN) {
        ElfW(Ehdr) ehdr;
        int obj_type;

        obj_type = tcc_object_type(fd, &ehdr);
        lseek(fd, 0, SEEK_SET);

        switch (obj_type) {

        case AFF_BINTYPE_REL:
            ret = tcc_load_object_file(s1, fd, 0);
            break;

        case AFF_BINTYPE_AR:
            ret = tcc_load_archive(s1, fd, !(flags & AFF_WHOLE_ARCHIVE));
            break;

#ifdef TCC_TARGET_PE
        default:
            ret = pe_load_file(s1, fd, filename);
            goto check_success;

#elif defined TCC_TARGET_MACHO
        case AFF_BINTYPE_DYN:
        case_dyn_or_tbd:
            if (s1->output_type == TCC_OUTPUT_MEMORY) {
#ifdef TCC_IS_NATIVE
                void* dl;
                const char* soname = filename;
                if (obj_type != AFF_BINTYPE_DYN)
                    soname = macho_tbd_soname(filename);
                dl = dlopen(soname, RTLD_GLOBAL | RTLD_LAZY);
                if (dl)
                    tcc_add_dllref(s1, soname, 0)->handle = dl, ret = 0;
	        if (filename != soname)
		    tcc_free((void *)soname);
#endif
            } else if (obj_type == AFF_BINTYPE_DYN) {
                ret = macho_load_dll(s1, fd, filename, (flags & AFF_REFERENCED_DLL) != 0);
            } else {
                ret = macho_load_tbd(s1, fd, filename, (flags & AFF_REFERENCED_DLL) != 0);
            }
            goto check_success;
        default:
        {
            const char *ext = tcc_fileextension(filename);
            if (!strcmp(ext, ".tbd"))
                goto case_dyn_or_tbd;
            if (!strcmp(ext, ".dylib")) {
                obj_type = AFF_BINTYPE_DYN;
                goto case_dyn_or_tbd;
            }
            goto check_success;
        }

#else /* unix */
        case AFF_BINTYPE_DYN:
            if (s1->output_type == TCC_OUTPUT_MEMORY) {
#ifdef TCC_IS_NATIVE
                void* dl = dlopen(filename, RTLD_GLOBAL | RTLD_LAZY);
                if (dl)
                    tcc_add_dllref(s1, filename, 0)->handle = dl, ret = 0;
#endif
            } else
                ret = tcc_load_dll(s1, fd, filename, (flags & AFF_REFERENCED_DLL) != 0);
            break;

        default:
            /* as GNU ld, consider it is an ld script if not recognized */
            ret = tcc_load_ldscript(s1, fd);
            goto check_success;

#endif /* pe / macos / unix */

check_success:
            if (ret < 0)
                tcc_error_noabort("%s: unrecognized file type", filename);
            break;

#ifdef TCC_TARGET_COFF
        case AFF_BINTYPE_C67:
            ret = tcc_load_coff(s1, fd);
            break;
#endif
        }
        close(fd);
    } else {
        /* update target deps */
        dynarray_add(&s1->target_deps, &s1->nb_target_deps, tcc_strdup(filename));
        ret = tcc_compile(s1, flags, filename, fd);
    }
    s1->current_filename = NULL;
    return ret;
}

LIBTCCAPI int tcc_add_file(TCCState *s, const char *filename)
{
    int filetype = s->filetype;
    if (0 == (filetype & AFF_TYPE_MASK)) {
        /* use a file extension to detect a filetype */
        const char *ext = tcc_fileextension(filename);
        if (ext[0]) {
            ext++;
            if (!strcmp(ext, "S"))
                filetype = AFF_TYPE_ASMPP;
            else if (!strcmp(ext, "s"))
                filetype = AFF_TYPE_ASM;
            else if (!PATHCMP(ext, "c")
                     || !PATHCMP(ext, "h")
                     || !PATHCMP(ext, "i"))
                filetype = AFF_TYPE_C;
            else
                filetype |= AFF_TYPE_BIN;
        } else {
            filetype = AFF_TYPE_C;
        }
    }
    return tcc_add_file_internal(s, filename, filetype | AFF_PRINT_ERROR);
}

LIBTCCAPI int tcc_add_library_path(TCCState *s, const char *pathname)
{
    tcc_split_path(s, &s->library_paths, &s->nb_library_paths, pathname);
    return 0;
}

static int tcc_add_library_internal(TCCState *s1, const char *fmt,
    const char *filename, int flags, char **paths, int nb_paths)
{
    char buf[1024];
    int i, ret;

    for(i = 0; i < nb_paths; i++) {
        snprintf(buf, sizeof(buf), fmt, paths[i], filename);
        ret = tcc_add_file_internal(s1, buf, (flags & ~AFF_PRINT_ERROR) | AFF_TYPE_BIN);
        if (ret != FILE_NOT_FOUND)
            return ret;
    }
    if (flags & AFF_PRINT_ERROR)
        tcc_error_noabort("file '%s' not found", filename);
    return FILE_NOT_FOUND;
}

/* find and load a dll. Return non zero if not found */
ST_FUNC int tcc_add_dll(TCCState *s, const char *filename, int flags)
{
    return tcc_add_library_internal(s, "%s/%s", filename, flags,
        s->library_paths, s->nb_library_paths);
}

/* find [cross-]libtcc1.a and tcc helper objects in library path */
ST_FUNC void tcc_add_support(TCCState *s1, const char *filename)
{
    char buf[100];
    if (CONFIG_TCC_CROSSPREFIX[0])
        filename = strcat(strcpy(buf, CONFIG_TCC_CROSSPREFIX), filename);
    tcc_add_dll(s1, filename, AFF_PRINT_ERROR);
}

#if !defined TCC_TARGET_PE && !defined TCC_TARGET_MACHO
ST_FUNC int tcc_add_crt(TCCState *s1, const char *filename)
{
    return tcc_add_library_internal(s1, "%s/%s",
        filename, AFF_PRINT_ERROR, s1->crt_paths, s1->nb_crt_paths);
}
#endif

/* the library name is the same as the argument of the '-l' option */
LIBTCCAPI int tcc_add_library(TCCState *s, const char *libraryname)
{
#if defined TCC_TARGET_PE
    static const char * const libs[] = { "%s/%s.def", "%s/lib%s.def", "%s/%s.dll", "%s/lib%s.dll", "%s/lib%s.a", NULL };
    const char * const *pp = s->static_link ? libs + 4 : libs;
#elif defined TCC_TARGET_MACHO
    static const char * const libs[] = { "%s/lib%s.dylib", "%s/lib%s.tbd", "%s/lib%s.a", NULL };
    const char * const *pp = s->static_link ? libs + 2 : libs;
#elif defined TARGETOS_OpenBSD
    static const char * const libs[] = { "%s/lib%s.so.*", "%s/lib%s.a", NULL };
    const char * const *pp = s->static_link ? libs + 1 : libs;
#else
    static const char * const libs[] = { "%s/lib%s.so", "%s/lib%s.a", NULL };
    const char * const *pp = s->static_link ? libs + 1 : libs;
#endif
    int flags = s->filetype & AFF_WHOLE_ARCHIVE;
    while (*pp) {
        int ret = tcc_add_library_internal(s, *pp,
            libraryname, flags, s->library_paths, s->nb_library_paths);
        if (ret != FILE_NOT_FOUND)
            return ret;
        ++pp;
    }
    return FILE_NOT_FOUND;
}

PUB_FUNC int tcc_add_library_err(TCCState *s1, const char *libname)
{
    int ret = tcc_add_library(s1, libname);
    if (ret == FILE_NOT_FOUND)
        tcc_error_noabort("library '%s' not found", libname);
    return ret;
}

/* handle #pragma comment(lib,) */
ST_FUNC void tcc_add_pragma_libs(TCCState *s1)
{
    int i;
    for (i = 0; i < s1->nb_pragma_libs; i++)
        tcc_add_library_err(s1, s1->pragma_libs[i]);
}

LIBTCCAPI int tcc_add_symbol(TCCState *s1, const char *name, const void *val)
{
#ifdef TCC_TARGET_PE
    /* On x86_64 'val' might not be reachable with a 32bit offset.
       So it is handled here as if it were in a DLL. */
    pe_putimport(s1, 0, name, (uintptr_t)val);
#else
    char buf[256];
    if (s1->leading_underscore) {
        buf[0] = '_';
        pstrcpy(buf + 1, sizeof(buf) - 1, name);
        name = buf;
    }
    set_global_sym(s1, name, NULL, (addr_t)(uintptr_t)val); /* NULL: SHN_ABS */
#endif
    return 0;
}

LIBTCCAPI void tcc_set_lib_path(TCCState *s, const char *path)
{
    tcc_free(s->tcc_lib_path);
    s->tcc_lib_path = tcc_strdup(path);
}

/********************************************************/
/* options parser */

static int strstart(const char *val, const char **str)
{
    const char *p, *q;
    p = *str;
    q = val;
    while (*q) {
        if (*p != *q)
            return 0;
        p++;
        q++;
    }
    *str = p;
    return 1;
}

/* Like strstart, but automatically takes into account that ld options can
 *
 * - start with double or single dash (e.g. '--soname' or '-soname')
 * - arguments can be given as separate or after '=' (e.g. '-Wl,-soname,x.so'
 *   or '-Wl,-soname=x.so')
 *
 * you provide `val` always in 'option[=]' form (no leading -)
 */
static int link_option(const char *str, const char *val, const char **ptr)
{
    const char *p, *q;
    int ret;

    /* there should be 1 or 2 dashes */
    if (*str++ != '-')
        return 0;
    if (*str == '-')
        str++;

    /* then str & val should match (potentially up to '=') */
    p = str;
    q = val;

    ret = 1;
    if (q[0] == '?') {
        ++q;
        if (strstart("no-", &p))
            ret = -1;
    }

    while (*q != '\0' && *q != '=') {
        if (*p != *q)
            return 0;
        p++;
        q++;
    }

    /* '=' near eos means ',' or '=' is ok */
    if (*q == '=') {
        if (*p == 0)
            *ptr = p;
        if (*p != ',' && *p != '=')
            return 0;
        p++;
    } else if (*p) {
        return 0;
    }
    *ptr = p;
    return ret;
}

static const char *skip_linker_arg(const char **str)
{
    const char *s1 = *str;
    const char *s2 = strchr(s1, ',');
    *str = s2 ? s2++ : (s2 = s1 + strlen(s1));
    return s2;
}

static void copy_linker_arg(char **pp, const char *s, int sep)
{
    const char *q = s;
    char *p = *pp;
    int l = 0;
    if (p && sep)
        p[l = strlen(p)] = sep, ++l;
    skip_linker_arg(&q);
    pstrncpy(l + (*pp = tcc_realloc(p, q - s + l + 1)), s, q - s);
}

static void args_parser_add_file(TCCState *s, const char* filename, int filetype)
{
    struct filespec *f = tcc_malloc(sizeof *f + strlen(filename));
    f->type = filetype;
    strcpy(f->name, filename);
    dynarray_add(&s->files, &s->nb_files, f);
}

/* set linker options */
static int tcc_set_linker(TCCState *s, const char *option)
{
    TCCState *s1 = s;
    while (*option) {

        const char *p = NULL;
        char *end = NULL;
        int ignoring = 0;
        int ret;

        if (link_option(option, "Bsymbolic", &p)) {
            s->symbolic = 1;
        } else if (link_option(option, "nostdlib", &p)) {
            s->nostdlib = 1;
        } else if (link_option(option, "e=", &p)
               ||  link_option(option, "entry=", &p)) {
            copy_linker_arg(&s->elf_entryname, p, 0);
        } else if (link_option(option, "fini=", &p)) {
            copy_linker_arg(&s->fini_symbol, p, 0);
            ignoring = 1;
        } else if (link_option(option, "image-base=", &p)
                || link_option(option, "Ttext=", &p)) {
            s->text_addr = strtoull(p, &end, 16);
            s->has_text_addr = 1;
        } else if (link_option(option, "init=", &p)) {
            copy_linker_arg(&s->init_symbol, p, 0);
            ignoring = 1;
        } else if (link_option(option, "Map=", &p)) {
            copy_linker_arg(&s->mapfile, p, 0);
            ignoring = 1;
        } else if (link_option(option, "oformat=", &p)) {
#if defined(TCC_TARGET_PE)
            if (strstart("pe-", &p)) {
#elif PTR_SIZE == 8
            if (strstart("elf64-", &p)) {
#else
            if (strstart("elf32-", &p)) {
#endif
                s->output_format = TCC_OUTPUT_FORMAT_ELF;
            } else if (!strcmp(p, "binary")) {
                s->output_format = TCC_OUTPUT_FORMAT_BINARY;
#ifdef TCC_TARGET_COFF
            } else if (!strcmp(p, "coff")) {
                s->output_format = TCC_OUTPUT_FORMAT_COFF;
#endif
            } else
                goto err;

        } else if (link_option(option, "as-needed", &p)) {
            ignoring = 1;
        } else if (link_option(option, "O", &p)) {
            ignoring = 1;
        } else if (link_option(option, "export-all-symbols", &p)) {
            s->rdynamic = 1;
        } else if (link_option(option, "export-dynamic", &p)) {
            s->rdynamic = 1;
        } else if (link_option(option, "rpath=", &p)) {
            copy_linker_arg(&s->rpath, p, ':');
        } else if (link_option(option, "enable-new-dtags", &p)) {
            s->enable_new_dtags = 1;
        } else if (link_option(option, "section-alignment=", &p)) {
            s->section_align = strtoul(p, &end, 16);
        } else if (link_option(option, "soname=", &p)) {
            copy_linker_arg(&s->soname, p, 0);
        } else if (link_option(option, "install_name=", &p)) {
            copy_linker_arg(&s->soname, p, 0);
#ifdef TCC_TARGET_PE
        } else if (link_option(option, "large-address-aware", &p)) {
            s->pe_characteristics |= 0x20;
        } else if (link_option(option, "file-alignment=", &p)) {
            s->pe_file_align = strtoul(p, &end, 16);
        } else if (link_option(option, "stack=", &p)) {
            s->pe_stack_size = strtoul(p, &end, 10);
        } else if (link_option(option, "subsystem=", &p)) {
#if defined(TCC_TARGET_I386) || defined(TCC_TARGET_X86_64)
            if (!strcmp(p, "native")) {
                s->pe_subsystem = 1;
            } else if (!strcmp(p, "console")) {
                s->pe_subsystem = 3;
            } else if (!strcmp(p, "gui") || !strcmp(p, "windows")) {
                s->pe_subsystem = 2;
            } else if (!strcmp(p, "posix")) {
                s->pe_subsystem = 7;
            } else if (!strcmp(p, "efiapp")) {
                s->pe_subsystem = 10;
            } else if (!strcmp(p, "efiboot")) {
                s->pe_subsystem = 11;
            } else if (!strcmp(p, "efiruntime")) {
                s->pe_subsystem = 12;
            } else if (!strcmp(p, "efirom")) {
                s->pe_subsystem = 13;
#elif defined(TCC_TARGET_ARM)
            if (!strcmp(p, "wince")) {
                s->pe_subsystem = 9;
#endif
            } else
                goto err;
#endif
#ifdef TCC_TARGET_MACHO
        } else if (link_option(option, "all_load", &p)) {
	    s->filetype |= AFF_WHOLE_ARCHIVE;
        } else if (link_option(option, "force_load", &p)) {
	    s->filetype |= AFF_WHOLE_ARCHIVE;
            args_parser_add_file(s, p, AFF_TYPE_LIB | (s->filetype & ~AFF_TYPE_MASK));
            s->nb_libraries++;
        } else if (link_option(option, "single_module", &p)) {
            ignoring = 1;
#endif
        } else if (ret = link_option(option, "?whole-archive", &p), ret) {
            if (ret > 0)
                s->filetype |= AFF_WHOLE_ARCHIVE;
            else
                s->filetype &= ~AFF_WHOLE_ARCHIVE;
        } else if (link_option(option, "z=", &p)) {
            ignoring = 1;
        } else if (p) {
            return 0;
        } else {
    err:
            return tcc_error_noabort("unsupported linker option '%s'", option);
        }
        if (ignoring)
            tcc_warning_c(warn_unsupported)("unsupported linker option '%s'", option);
        option = skip_linker_arg(&p);
    }
    return 1;
}

typedef struct TCCOption {
    const char *name;
    uint16_t index;
    uint16_t flags;
} TCCOption;

enum {
    TCC_OPTION_ignored = 0,
    TCC_OPTION_HELP,
    TCC_OPTION_HELP2,
    TCC_OPTION_v,
    TCC_OPTION_I,
    TCC_OPTION_D,
    TCC_OPTION_U,
    TCC_OPTION_P,
    TCC_OPTION_L,
    TCC_OPTION_B,
    TCC_OPTION_l,
    TCC_OPTION_bench,
    TCC_OPTION_bt,
    TCC_OPTION_b,
    TCC_OPTION_ba,
    TCC_OPTION_g,
    TCC_OPTION_c,
    TCC_OPTION_dumpmachine,
    TCC_OPTION_dumpversion,
    TCC_OPTION_d,
    TCC_OPTION_static,
    TCC_OPTION_std,
    TCC_OPTION_shared,
    TCC_OPTION_soname,
    TCC_OPTION_o,
    TCC_OPTION_r,
    TCC_OPTION_Wl,
    TCC_OPTION_Wp,
    TCC_OPTION_W,
    TCC_OPTION_O,
    TCC_OPTION_mfloat_abi,
    TCC_OPTION_m,
    TCC_OPTION_f,
    TCC_OPTION_isystem,
    TCC_OPTION_iwithprefix,
    TCC_OPTION_include,
    TCC_OPTION_nostdinc,
    TCC_OPTION_nostdlib,
    TCC_OPTION_print_search_dirs,
    TCC_OPTION_rdynamic,
    TCC_OPTION_pthread,
    TCC_OPTION_run,
    TCC_OPTION_w,
    TCC_OPTION_E,
    TCC_OPTION_M,
    TCC_OPTION_MD,
    TCC_OPTION_MF,
    TCC_OPTION_MM,
    TCC_OPTION_MMD,
    TCC_OPTION_MP,
    TCC_OPTION_x,
    TCC_OPTION_ar,
    TCC_OPTION_impdef,
    TCC_OPTION_dynamiclib,
    TCC_OPTION_flat_namespace,
    TCC_OPTION_two_levelnamespace,
    TCC_OPTION_undefined,
    TCC_OPTION_install_name,
    TCC_OPTION_compatibility_version ,
    TCC_OPTION_current_version,
};

#define TCC_OPTION_HAS_ARG 0x0001
#define TCC_OPTION_NOSEP   0x0002 /* cannot have space before option and arg */

static const TCCOption tcc_options[] = {
    { "h", TCC_OPTION_HELP, 0 },
    { "-help", TCC_OPTION_HELP, 0 },
    { "?", TCC_OPTION_HELP, 0 },
    { "hh", TCC_OPTION_HELP2, 0 },
    { "v", TCC_OPTION_v, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "-version", TCC_OPTION_v, 0 }, /* handle as verbose, also prints version*/
    { "I", TCC_OPTION_I, TCC_OPTION_HAS_ARG },
    { "D", TCC_OPTION_D, TCC_OPTION_HAS_ARG },
    { "U", TCC_OPTION_U, TCC_OPTION_HAS_ARG },
    { "P", TCC_OPTION_P, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "L", TCC_OPTION_L, TCC_OPTION_HAS_ARG },
    { "B", TCC_OPTION_B, TCC_OPTION_HAS_ARG },
    { "l", TCC_OPTION_l, TCC_OPTION_HAS_ARG },
    { "bench", TCC_OPTION_bench, 0 },
#ifdef CONFIG_TCC_BACKTRACE
    { "bt", TCC_OPTION_bt, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
#endif
#ifdef CONFIG_TCC_BCHECK
    { "b", TCC_OPTION_b, 0 },
#endif
    { "g", TCC_OPTION_g, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
#ifdef TCC_TARGET_MACHO
    { "compatibility_version", TCC_OPTION_compatibility_version, TCC_OPTION_HAS_ARG },
    { "current_version", TCC_OPTION_current_version, TCC_OPTION_HAS_ARG },
#endif
    { "c", TCC_OPTION_c, 0 },
#ifdef TCC_TARGET_MACHO
    { "dynamiclib", TCC_OPTION_dynamiclib, 0 },
#endif
    { "dumpmachine", TCC_OPTION_dumpmachine, 0},
    { "dumpversion", TCC_OPTION_dumpversion, 0},
    { "d", TCC_OPTION_d, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "static", TCC_OPTION_static, 0 },
    { "std", TCC_OPTION_std, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "shared", TCC_OPTION_shared, 0 },
    { "soname", TCC_OPTION_soname, TCC_OPTION_HAS_ARG },
    { "o", TCC_OPTION_o, TCC_OPTION_HAS_ARG },
    { "pthread", TCC_OPTION_pthread, 0},
    { "run", TCC_OPTION_run, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "rdynamic", TCC_OPTION_rdynamic, 0 },
    { "r", TCC_OPTION_r, 0 },
    { "Wl,", TCC_OPTION_Wl, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "Wp,", TCC_OPTION_Wp, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "W", TCC_OPTION_W, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "O", TCC_OPTION_O, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
#ifdef TCC_TARGET_ARM
    { "mfloat-abi", TCC_OPTION_mfloat_abi, TCC_OPTION_HAS_ARG },
#endif
    { "m", TCC_OPTION_m, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
#ifdef TCC_TARGET_MACHO
    { "flat_namespace", TCC_OPTION_flat_namespace, 0 },
#endif
    { "f", TCC_OPTION_f, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "isystem", TCC_OPTION_isystem, TCC_OPTION_HAS_ARG },
    { "include", TCC_OPTION_include, TCC_OPTION_HAS_ARG },
    { "nostdinc", TCC_OPTION_nostdinc, 0 },
    { "nostdlib", TCC_OPTION_nostdlib, 0 },
    { "print-search-dirs", TCC_OPTION_print_search_dirs, 0 },
    { "w", TCC_OPTION_w, 0 },
    { "E", TCC_OPTION_E, 0},
    { "M", TCC_OPTION_M, 0},
    { "MD", TCC_OPTION_MD, 0},
    { "MF", TCC_OPTION_MF, TCC_OPTION_HAS_ARG },
    { "MM", TCC_OPTION_MM, 0},
    { "MMD", TCC_OPTION_MMD, 0},
    { "MP", TCC_OPTION_MP, 0},
    { "x", TCC_OPTION_x, TCC_OPTION_HAS_ARG },
    { "ar", TCC_OPTION_ar, 0},
#ifdef TCC_TARGET_PE
    { "impdef", TCC_OPTION_impdef, 0},
#endif
#ifdef TCC_TARGET_MACHO
    { "install_name", TCC_OPTION_install_name, TCC_OPTION_HAS_ARG },
    { "two_levelnamespace", TCC_OPTION_two_levelnamespace, 0 },
    { "undefined", TCC_OPTION_undefined, TCC_OPTION_HAS_ARG },
#endif
    /* ignored (silently, except after -Wunsupported) */
    { "arch", 0, TCC_OPTION_HAS_ARG},
    { "C", 0, 0 },
    { "-param", 0, TCC_OPTION_HAS_ARG },
    { "pedantic", 0, 0 },
    { "pipe", 0, 0 },
    { "s", 0, 0 },
    { "traditional", 0, 0 },
    { NULL, 0, 0 },
};

typedef struct FlagDef {
    uint16_t offset;
    uint16_t flags;
    const char *name;
} FlagDef;

#define WD_ALL    0x0001 /* warning is activated when using -Wall */
#define FD_INVERT 0x0002 /* invert value before storing */

static const FlagDef options_W[] = {
    { offsetof(TCCState, warn_all), WD_ALL, "all" },
    { offsetof(TCCState, warn_error), 0, "error" },
    { offsetof(TCCState, warn_write_strings), 0, "write-strings" },
    { offsetof(TCCState, warn_unsupported), 0, "unsupported" },
    { offsetof(TCCState, warn_implicit_function_declaration), WD_ALL, "implicit-function-declaration" },
    { offsetof(TCCState, warn_discarded_qualifiers), WD_ALL, "discarded-qualifiers" },
    { 0, 0, NULL }
};

static const FlagDef options_f[] = {
    { offsetof(TCCState, char_is_unsigned), 0, "unsigned-char" },
    { offsetof(TCCState, char_is_unsigned), FD_INVERT, "signed-char" },
    { offsetof(TCCState, nocommon), FD_INVERT, "common" },
    { offsetof(TCCState, leading_underscore), 0, "leading-underscore" },
    { offsetof(TCCState, ms_extensions), 0, "ms-extensions" },
    { offsetof(TCCState, dollars_in_identifiers), 0, "dollars-in-identifiers" },
    { offsetof(TCCState, test_coverage), 0, "test-coverage" },
    { 0, 0, NULL }
};

static const FlagDef options_m[] = {
    { offsetof(TCCState, ms_bitfields), 0, "ms-bitfields" },
#ifdef TCC_TARGET_X86_64
    { offsetof(TCCState, nosse), FD_INVERT, "sse" },
#endif
    { 0, 0, NULL }
};

static int set_flag(TCCState *s, const FlagDef *flags, const char *name)
{
    int value, mask, ret;
    const FlagDef *p;
    const char *r;
    unsigned char *f;

    r = name, value = !strstart("no-", &r), mask = 0;

    /* when called with options_W, look for -W[no-]error=<option> */
    if ((flags->flags & WD_ALL) && strstart("error=", &r))
        value = value ? WARN_ON|WARN_ERR : WARN_NOE, mask = WARN_ON;

    for (ret = -1, p = flags; p->name; ++p) {
        if (ret) {
            if (strcmp(r, p->name))
                continue;
        } else {
            if (0 == (p->flags & WD_ALL))
                continue;
        }

        f = (unsigned char *)s + p->offset;
        *f = (*f & mask) | (value ^ !!(p->flags & FD_INVERT));

        if (ret) {
            ret = 0;
            if (strcmp(r, "all"))
                break;
        }
    }
    return ret;
}

static const char dumpmachine_str[] =
/* this is a best guess, please refine as necessary */
#ifdef TCC_TARGET_I386
    "i386-pc"
#elif defined TCC_TARGET_X86_64
    "x86_64-pc"
#elif defined TCC_TARGET_C67
    "c67"
#elif defined TCC_TARGET_ARM
    "arm"
#elif defined TCC_TARGET_ARM64
    "aarch64"
#elif defined TCC_TARGET_RISCV64
    "riscv64"
#endif
    "-"
#ifdef TCC_TARGET_PE
    "mingw32"
#elif defined(TCC_TARGET_MACHO)
    "apple-darwin"
#elif TARGETOS_FreeBSD || TARGETOS_FreeBSD_kernel
    "freebsd"
#elif TARGETOS_OpenBSD
    "openbsd"
#elif TARGETOS_NetBSD
    "netbsd"
#else
    "linux-gnu"
#endif
;

static int args_parser_make_argv(const char *r, int *argc, char ***argv)
{
    int ret = 0, q, c;
    CString str;
    for(;;) {
        while (c = (unsigned char)*r, c && c <= ' ')
          ++r;
        if (c == 0)
            break;
        q = 0;
        cstr_new(&str);
        while (c = (unsigned char)*r, c) {
            ++r;
            if (c == '\\' && (*r == '"' || *r == '\\')) {
                c = *r++;
            } else if (c == '"') {
                q = !q;
                continue;
            } else if (q == 0 && c <= ' ') {
                break;
            }
            cstr_ccat(&str, c);
        }
        cstr_ccat(&str, 0);
        //printf("<%s>\n", str.data), fflush(stdout);
        dynarray_add(argv, argc, tcc_strdup(str.data));
        cstr_free(&str);
        ++ret;
    }
    return ret;
}

/* read list file */
static int args_parser_listfile(TCCState *s,
    const char *filename, int optind, int *pargc, char ***pargv)
{
    TCCState *s1 = s;
    int fd, i;
    char *p;
    int argc = 0;
    char **argv = NULL;

    fd = open(filename, O_RDONLY | O_BINARY);
    if (fd < 0)
        return tcc_error_noabort("listfile '%s' not found", filename);

    p = tcc_load_text(fd);
    for (i = 0; i < *pargc; ++i)
        if (i == optind)
            args_parser_make_argv(p, &argc, &argv);
        else
            dynarray_add(&argv, &argc, tcc_strdup((*pargv)[i]));

    tcc_free(p);
    dynarray_reset(&s->argv, &s->argc);
    *pargc = s->argc = argc, *pargv = s->argv = argv;
    return 0;
}

#if defined TCC_TARGET_MACHO
static uint32_t parse_version(TCCState *s1, const char *version)
{
    uint32_t a = 0;
    uint32_t b = 0;
    uint32_t c = 0;
    char* last;

    a = strtoul(version, &last, 10);
    if (*last == '.') {
        b = strtoul(&last[1], &last, 10);
        if (*last == '.')
             c = strtoul(&last[1], &last, 10);
    }
    if (*last || a > 0xffff || b > 0xff || c > 0xff)
        tcc_error_noabort("version a.b.c not correct: %s", version);
    return (a << 16) | (b << 8) | c;
}
#endif

PUB_FUNC int tcc_parse_args(TCCState *s, int *pargc, char ***pargv, int optind)
{
    TCCState *s1 = s;
    const TCCOption *popt;
    const char *optarg, *r;
    const char *run = NULL;
    int x;
    int tool = 0, arg_start = 0, noaction = optind;
    char **argv = *pargv;
    int argc = *pargc;

    cstr_reset(&s->linker_arg);

    while (optind < argc) {
        r = argv[optind];
        if (r[0] == '@' && r[1] != '\0') {
            if (args_parser_listfile(s, r + 1, optind, &argc, &argv))
                return -1;
            continue;
        }
        optind++;
        if (tool) {
            if (r[0] == '-' && r[1] == 'v' && r[2] == 0)
                ++s->verbose;
            continue;
        }
reparse:
        if (r[0] != '-' || r[1] == '\0') {
            args_parser_add_file(s, r, s->filetype);
            if (run) {
dorun:
                if (tcc_set_options(s, run))
                    return -1;
                arg_start = optind - 1;
                break;
            }
            continue;
        }

        /* allow "tcc files... -run -- args ..." */
        if (r[1] == '-' && r[2] == '\0' && run)
            goto dorun;

        /* find option in table */
        for(popt = tcc_options; ; ++popt) {
            const char *p1 = popt->name;
            const char *r1 = r + 1;
            if (p1 == NULL)
                return tcc_error_noabort("invalid option -- '%s'", r);
            if (!strstart(p1, &r1))
                continue;
            optarg = r1;
            if (popt->flags & TCC_OPTION_HAS_ARG) {
                if (*r1 == '\0' && !(popt->flags & TCC_OPTION_NOSEP)) {
                    if (optind >= argc)
                arg_err:
                        return tcc_error_noabort("argument to '%s' is missing", r);
                    optarg = argv[optind++];
                }
            } else if (*r1 != '\0')
                continue;
            break;
        }

        switch(popt->index) {
        case TCC_OPTION_HELP:
            x = OPT_HELP;
            goto extra_action;
        case TCC_OPTION_HELP2:
            x = OPT_HELP2;
            goto extra_action;
        case TCC_OPTION_I:
            tcc_add_include_path(s, optarg);
            break;
        case TCC_OPTION_D:
            tcc_define_symbol(s, optarg, NULL);
            break;
        case TCC_OPTION_U:
            tcc_undefine_symbol(s, optarg);
            break;
        case TCC_OPTION_L:
            tcc_add_library_path(s, optarg);
            break;
        case TCC_OPTION_B:
            /* set tcc utilities path (mainly for tcc development) */
            tcc_set_lib_path(s, optarg);
            ++noaction;
            break;
        case TCC_OPTION_l:
            args_parser_add_file(s, optarg, AFF_TYPE_LIB | (s->filetype & ~AFF_TYPE_MASK));
            s->nb_libraries++;
            break;
        case TCC_OPTION_pthread:
            s->option_pthread = 1;
            break;
        case TCC_OPTION_bench:
            s->do_bench = 1;
            break;
#ifdef CONFIG_TCC_BACKTRACE
        case TCC_OPTION_bt:
            s->rt_num_callers = atoi(optarg); /* zero = default (6) */
            goto enable_backtrace;
        enable_backtrace:
            s->do_backtrace = 1;
            s->do_debug = s->do_debug ? s->do_debug : 1;
	    s->dwarf = DWARF_VERSION;
            break;
#ifdef CONFIG_TCC_BCHECK
        case TCC_OPTION_b:
            s->do_bounds_check = 1;
            goto enable_backtrace;
#endif
#endif
        case TCC_OPTION_g:
            s->do_debug = 2;
            s->dwarf = DWARF_VERSION;
            if (strstart("dwarf", &optarg)) {
                s->dwarf = (*optarg) ? (0 - atoi(optarg)) : DEFAULT_DWARF_VERSION;
            } else if (isnum(*optarg)) {
                x = *optarg - '0';
                /* -g0 = no info, -g1 = lines/functions only, -g2 = full info */
                s->do_debug = x > 2 ? 2 : x == 0 && s->do_backtrace ? 1 : x;
#ifdef TCC_TARGET_PE
            } else if (0 == strcmp(".pdb", optarg)) {
                s->dwarf = 5, s->do_debug |= 16;
#endif
            }
            break;
        case TCC_OPTION_c:
            x = TCC_OUTPUT_OBJ;
        set_output_type:
            if (s->output_type)
                tcc_warning("-%s: overriding compiler action already specified", popt->name);
            s->output_type = x;
            break;
        case TCC_OPTION_d:
            if (*optarg == 'D')
                s->dflag = 3;
            else if (*optarg == 'M')
                s->dflag = 7;
            else if (*optarg == 't')
                s->dflag = 16;
            else if (isnum(*optarg))
                s->g_debug |= atoi(optarg);
            else
                goto unsupported_option;
            break;
        case TCC_OPTION_static:
            s->static_link = 1;
            break;
        case TCC_OPTION_std:
            if (strcmp(optarg, "=c11") == 0)
                s->cversion = 201112;
            break;
        case TCC_OPTION_shared:
            x = TCC_OUTPUT_DLL;
            goto set_output_type;
        case TCC_OPTION_soname:
            s->soname = tcc_strdup(optarg);
            break;
        case TCC_OPTION_o:
            if (s->outfile) {
                tcc_warning("multiple -o option");
                tcc_free(s->outfile);
            }
            s->outfile = tcc_strdup(optarg);
            break;
        case TCC_OPTION_r:
            /* generate a .o merging several output files */
            s->option_r = 1;
            x = TCC_OUTPUT_OBJ;
            goto set_output_type;
        case TCC_OPTION_isystem:
            tcc_add_sysinclude_path(s, optarg);
            break;
        case TCC_OPTION_include:
            cstr_printf(&s->cmdline_incl, "#include \"%s\"\n", optarg);
            break;
        case TCC_OPTION_nostdinc:
            s->nostdinc = 1;
            break;
        case TCC_OPTION_nostdlib:
            s->nostdlib = 1;
            break;
        case TCC_OPTION_run:
#ifndef TCC_IS_NATIVE
            return tcc_error_noabort("-run is not available in a cross compiler");
#else
            run = optarg;
            x = TCC_OUTPUT_MEMORY;
            goto set_output_type;
#endif
        case TCC_OPTION_v:
            do ++s->verbose; while (*optarg++ == 'v');
            ++noaction;
            break;
        case TCC_OPTION_f:
            if (set_flag(s, options_f, optarg) < 0)
                goto unsupported_option;
            break;
#ifdef TCC_TARGET_ARM
        case TCC_OPTION_mfloat_abi:
            /* tcc doesn't support soft float yet */
            if (!strcmp(optarg, "softfp")) {
                s->float_abi = ARM_SOFTFP_FLOAT;
            } else if (!strcmp(optarg, "hard"))
                s->float_abi = ARM_HARD_FLOAT;
            else
                return tcc_error_noabort("unsupported float abi '%s'", optarg);
            break;
#endif
        case TCC_OPTION_m:
            if (set_flag(s, options_m, optarg) < 0) {
                if (x = atoi(optarg), x != 32 && x != 64)
                    goto unsupported_option;
                if (PTR_SIZE != x/8)
                    return x;
                ++noaction;
            }
            break;
        case TCC_OPTION_W:
            s->warn_none = 0;
            if (optarg[0] && set_flag(s, options_W, optarg) < 0)
                goto unsupported_option;
            break;
        case TCC_OPTION_w:
            s->warn_none = 1;
            break;
        case TCC_OPTION_rdynamic:
            s->rdynamic = 1;
            break;
        case TCC_OPTION_Wl:
            if (s->linker_arg.size)
                ((char*)s->linker_arg.data)[s->linker_arg.size - 1] = ',';
            cstr_cat(&s->linker_arg, optarg, 0);
            x = tcc_set_linker(s, s->linker_arg.data);
            if (x)
                cstr_reset(&s->linker_arg);
            if (x < 0)
                return -1;
            break;
        case TCC_OPTION_Wp:
            r = optarg;
            goto reparse;
        case TCC_OPTION_E:
            x = TCC_OUTPUT_PREPROCESS;
            goto set_output_type;
        case TCC_OPTION_P:
            s->Pflag = atoi(optarg) + 1;
            break;
        case TCC_OPTION_M:
            s->include_sys_deps = 1;
            // fall through
        case TCC_OPTION_MM:
            s->just_deps = 1;
            if(!s->deps_outfile)
                s->deps_outfile = tcc_strdup("-");
            // fall through
        case TCC_OPTION_MMD:
            s->gen_deps = 1;
            break;
        case TCC_OPTION_MD:
            s->gen_deps = 1;
            s->include_sys_deps = 1;
            break;
        case TCC_OPTION_MF:
            s->deps_outfile = tcc_strdup(optarg);
            break;
        case TCC_OPTION_MP:
            s->gen_phony_deps = 1;
            break;
        case TCC_OPTION_dumpmachine:
            printf("%s\n", dumpmachine_str);
            exit(0);
        case TCC_OPTION_dumpversion:
            printf ("%s\n", TCC_VERSION);
            exit(0);
        case TCC_OPTION_x:
            x = 0;
            if (*optarg == 'c')
                x = AFF_TYPE_C;
            else if (*optarg == 'a')
                x = AFF_TYPE_ASMPP;
            else if (*optarg == 'b')
                x = AFF_TYPE_BIN;
            else if (*optarg == 'n')
                x = AFF_TYPE_NONE;
            else
                tcc_warning("unsupported language '%s'", optarg);
            s->filetype = x | (s->filetype & ~AFF_TYPE_MASK);
            break;
        case TCC_OPTION_O:
            s->optimize = atoi(optarg);
            break;
        case TCC_OPTION_print_search_dirs:
            x = OPT_PRINT_DIRS;
            goto extra_action;
        case TCC_OPTION_impdef:
            x = OPT_IMPDEF;
            goto extra_action;
#if defined TCC_TARGET_MACHO
        case TCC_OPTION_dynamiclib:
            x = TCC_OUTPUT_DLL;
            goto set_output_type;
        case TCC_OPTION_flat_namespace:
	     break;
        case TCC_OPTION_two_levelnamespace:
	     break;
        case TCC_OPTION_undefined:
	     break;
        case TCC_OPTION_install_name:
	    s->install_name = tcc_strdup(optarg);
            break;
        case TCC_OPTION_compatibility_version:
	    s->compatibility_version = parse_version(s, optarg);
            break;
        case TCC_OPTION_current_version:
	    s->current_version = parse_version(s, optarg);;
            break;
#endif
        case TCC_OPTION_ar:
            x = OPT_AR;
        extra_action:
            arg_start = optind - 1;
            if (arg_start != noaction)
                return tcc_error_noabort("cannot parse %s here", r);
            tool = x;
            break;
        default:
unsupported_option:
            tcc_warning_c(warn_unsupported)("unsupported option '%s'", r);
            break;
        }
    }
    if (s->linker_arg.size) {
        r = s->linker_arg.data;
        goto arg_err;
    }
    *pargc = argc - arg_start;
    *pargv = argv + arg_start;
    if (tool)
        return tool;
    if (optind != noaction)
        return 0;
    if (s->verbose == 2)
        return OPT_PRINT_DIRS;
    if (s->verbose)
        return OPT_V;
    return OPT_HELP;
}

LIBTCCAPI int tcc_set_options(TCCState *s, const char *r)
{
    char **argv = NULL;
    int argc = 0, ret;
    args_parser_make_argv(r, &argc, &argv);
    ret = tcc_parse_args(s, &argc, &argv, 0);
    dynarray_reset(&argv, &argc);
    return ret < 0 ? ret : 0;
}

PUB_FUNC void tcc_print_stats(TCCState *s1, unsigned total_time)
{
    if (!total_time)
        total_time = 1;
    fprintf(stderr, "# %d idents, %d lines, %u bytes\n"
                    "# %0.3f s, %u lines/s, %0.1f MB/s\n",
           total_idents, total_lines, total_bytes,
           (double)total_time/1000,
           (unsigned)total_lines*1000/total_time,
           (double)total_bytes/1000/total_time);
    fprintf(stderr, "# text %u, data.rw %u, data.ro %u, bss %u bytes\n",
           s1->total_output[0],
           s1->total_output[1],
           s1->total_output[2],
           s1->total_output[3]
           );
#ifdef MEM_DEBUG
    fprintf(stderr, "# memory usage");
#ifdef TCC_IS_NATIVE
    if (s1->run_size) {
        Section *s = s1->symtab;
        unsigned ms = s->data_offset + s->link->data_offset + s->hash->data_offset;
        unsigned rs = s1->run_size;
        fprintf(stderr, ": %d to run, %d symbols, %d other,",
            rs, ms, mem_cur_size - rs - ms);
    }
#endif
    fprintf(stderr, " %d max (bytes)\n", mem_max_size);
#endif
}

//// tcc: tccasm.c

/*
 *  GAS like assembler for TCC
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#define USING_GLOBALS
#include "tcc.h"
#ifdef CONFIG_TCC_ASM

static Section *last_text_section; /* to handle .previous asm directive */
static int asmgoto_n;

static int asm_get_prefix_name(TCCState *s1, const char *prefix, unsigned int n)
{
    (void)s1;
    char buf[64];
    snprintf(buf, sizeof(buf), "%s%u", prefix, n);
    return tok_alloc_const(buf);
}

ST_FUNC int asm_get_local_label_name(TCCState *s1, unsigned int n)
{
    return asm_get_prefix_name(s1, "L..", n);
}

static int tcc_assemble_internal(TCCState *s1, int do_preprocess, int global);
static Sym* asm_new_label(TCCState *s1, int label, int is_local);
static Sym* asm_new_label1(TCCState *s1, int label, int is_local, int sh_num, int value);

/* If a C name has an _ prepended then only asm labels that start
   with _ are representable in C, by removing the first _.  ASM names
   without _ at the beginning don't correspond to C names, but we use
   the global C symbol table to track ASM names as well, so we need to
   transform those into ones that don't conflict with a C name,
   so prepend a '.' for them, but force the ELF asm name to be set.  */
static int asm2cname(int v, int *addeddot)
{
    const char *name;
    *addeddot = 0;
    if (!tcc_state->leading_underscore)
      return v;
    name = get_tok_str(v, NULL);
    if (!name)
      return v;
    if (name[0] == '_') {
        v = tok_alloc_const(name + 1);
    } else if (!strchr(name, '.')) {
        char newname[256];
        snprintf(newname, sizeof newname, ".%s", name);
        v = tok_alloc_const(newname);
        *addeddot = 1;
    }
    return v;
}

static Sym *asm_label_find(int v)
{
    Sym *sym;
    int addeddot;
    v = asm2cname(v, &addeddot);
    sym = sym_find(v);
    while (sym && sym->sym_scope && !(sym->type.t & VT_STATIC))
        sym = sym->prev_tok;
    return sym;
}

static Sym *asm_label_push(int v)
{
    int addeddot, v2 = asm2cname(v, &addeddot);
    /* We always add VT_EXTERN, for sym definition that's tentative
       (for .set, removed for real defs), for mere references it's correct
       as is.  */
    Sym *sym = global_identifier_push(v2, VT_ASM | VT_EXTERN | VT_STATIC, 0);
    if (addeddot)
        sym->asm_label = v;
    return sym;
}

/* Return a symbol we can use inside the assembler, having name NAME.
   Symbols from asm and C source share a namespace.  If we generate
   an asm symbol it's also a (file-global) C symbol, but it's
   either not accessible by name (like "L.123"), or its type information
   is such that it's not usable without a proper C declaration.

   Sometimes we need symbols accessible by name from asm, which
   are anonymous in C, in this case CSYM can be used to transfer
   all information from that symbol to the (possibly newly created)
   asm symbol.  */
ST_FUNC Sym* get_asm_sym(int name, Sym *csym)
{
    Sym *sym = asm_label_find(name);
    if (!sym) {
	sym = asm_label_push(name);
	if (csym)
	  sym->c = csym->c;
    }
    return sym;
}

static Sym* asm_section_sym(TCCState *s1, Section *sec)
{
    char buf[100]; int label; Sym *sym;
    snprintf(buf, sizeof buf, "L.%s", sec->name);
    label = tok_alloc_const(buf);
    sym = asm_label_find(label);
    return sym ? sym : asm_new_label1(s1, label, 1, sec->sh_num, 0);
}

/* We do not use the C expression parser to handle symbols. Maybe the
   C expression parser could be tweaked to do so. */

static void asm_expr_unary(TCCState *s1, ExprValue *pe)
{
    Sym *sym;
    int op, label;
    uint64_t n;
    const char *p;

    switch(tok) {
    case TOK_PPNUM:
        p = tokc.str.data;
        n = strtoull(p, (char **)&p, 0);
        if (*p == 'b' || *p == 'f') {
            /* backward or forward label */
            label = asm_get_local_label_name(s1, n);
            sym = asm_label_find(label);
            if (*p == 'b') {
                /* backward : find the last corresponding defined label */
                if (sym && (!sym->c || elfsym(sym)->st_shndx == SHN_UNDEF))
                    sym = sym->prev_tok;
                if (!sym)
                    tcc_error("local label '%d' not found backward", (int)n);
            } else {
                /* forward */
                if (!sym || (sym->c && elfsym(sym)->st_shndx != SHN_UNDEF)) {
                    /* if the last label is defined, then define a new one */
		    sym = asm_label_push(label);
                }
            }
	    pe->v = 0;
	    pe->sym = sym;
	    pe->pcrel = 0;
        } else if (*p == '\0') {
            pe->v = n;
            pe->sym = NULL;
	    pe->pcrel = 0;
        } else {
            tcc_error("invalid number syntax");
        }
        next();
        break;
    case '+':
        next();
        asm_expr_unary(s1, pe);
        break;
    case '-':
    case '~':
        op = tok;
        next();
        asm_expr_unary(s1, pe);
        if (pe->sym)
            tcc_error("invalid operation with label");
        if (op == '-')
            pe->v = -pe->v;
        else
            pe->v = ~pe->v;
        break;
    case TOK_CCHAR:
    case TOK_LCHAR:
	pe->v = tokc.i;
	pe->sym = NULL;
	pe->pcrel = 0;
	next();
	break;
    case '(':
        next();
        asm_expr(s1, pe);
        skip(')');
        break;
    case '.':
        pe->v = ind;
        pe->sym = asm_section_sym(s1, cur_text_section);
        pe->pcrel = 0;
        next();
        break;
    default:
        if (tok >= TOK_IDENT) {
	    ElfSym *esym;
            /* label case : if the label was not found, add one */
	    sym = get_asm_sym(tok, NULL);
	    esym = elfsym(sym);
            if (esym && esym->st_shndx == SHN_ABS) {
                /* if absolute symbol, no need to put a symbol value */
                pe->v = esym->st_value;
                pe->sym = NULL;
		pe->pcrel = 0;
            } else {
                pe->v = 0;
                pe->sym = sym;
		pe->pcrel = 0;
            }
            next();
        } else {
            tcc_error("bad expression syntax [%s]", get_tok_str(tok, &tokc));
        }
        break;
    }
}
    
static void asm_expr_prod(TCCState *s1, ExprValue *pe)
{
    int op;
    ExprValue e2;

    asm_expr_unary(s1, pe);
    for(;;) {
        op = tok;
        if (op != '*' && op != '/' && op != '%' && 
            op != TOK_SHL && op != TOK_SAR)
            break;
        next();
        asm_expr_unary(s1, &e2);
        if (pe->sym || e2.sym)
            tcc_error("invalid operation with label");
        switch(op) {
        case '*':
            pe->v *= e2.v;
            break;
        case '/':  
            if (e2.v == 0) {
            div_error:
                tcc_error("division by zero");
            }
            pe->v /= e2.v;
            break;
        case '%':  
            if (e2.v == 0)
                goto div_error;
            pe->v %= e2.v;
            break;
        case TOK_SHL:
            pe->v <<= e2.v;
            break;
        default:
        case TOK_SAR:
            pe->v >>= e2.v;
            break;
        }
    }
}

static void asm_expr_logic(TCCState *s1, ExprValue *pe)
{
    int op;
    ExprValue e2;

    asm_expr_prod(s1, pe);
    for(;;) {
        op = tok;
        if (op != '&' && op != '|' && op != '^')
            break;
        next();
        asm_expr_prod(s1, &e2);
        if (pe->sym || e2.sym)
            tcc_error("invalid operation with label");
        switch(op) {
        case '&':
            pe->v &= e2.v;
            break;
        case '|':  
            pe->v |= e2.v;
            break;
        default:
        case '^':
            pe->v ^= e2.v;
            break;
        }
    }
}

static inline void asm_expr_sum(TCCState *s1, ExprValue *pe)
{
    int op;
    ExprValue e2;

    asm_expr_logic(s1, pe);
    for(;;) {
        op = tok;
        if (op != '+' && op != '-')
            break;
        next();
        asm_expr_logic(s1, &e2);
        if (op == '+') {
            if (pe->sym != NULL && e2.sym != NULL)
                goto cannot_relocate;
            pe->v += e2.v;
            if (pe->sym == NULL && e2.sym != NULL)
                pe->sym = e2.sym;
        } else {
            pe->v -= e2.v;
            /* NOTE: we are less powerful than gas in that case
               because we store only one symbol in the expression */
	    if (!e2.sym) {
		/* OK */
	    } else if (pe->sym == e2.sym) { 
		/* OK */
		pe->sym = NULL; /* same symbols can be subtracted to NULL */
	    } else {
		ElfSym *esym1, *esym2;
		esym1 = elfsym(pe->sym);
		esym2 = elfsym(e2.sym);
		if (!esym2)
		    goto cannot_relocate;
		if (esym1 && esym1->st_shndx == esym2->st_shndx
		    && esym1->st_shndx != SHN_UNDEF) {
		    /* we also accept defined symbols in the same section */
		    pe->v += esym1->st_value - esym2->st_value;
		    pe->sym = NULL;
		} else if (esym2->st_shndx == cur_text_section->sh_num) {
		    /* When subtracting a defined symbol in current section
		       this actually makes the value PC-relative.  */
		    pe->v += 0 - esym2->st_value;
		    pe->pcrel = 1;
		    e2.sym = NULL;
		} else {
cannot_relocate:
		    tcc_error("invalid operation with label");
		}
	    }
        }
    }
}

static inline void asm_expr_cmp(TCCState *s1, ExprValue *pe)
{
    int op;
    ExprValue e2;

    asm_expr_sum(s1, pe);
    for(;;) {
        op = tok;
	if (op != TOK_EQ && op != TOK_NE
	    && (op > TOK_GT || op < TOK_ULE))
            break;
        next();
        asm_expr_sum(s1, &e2);
        if (pe->sym || e2.sym)
            tcc_error("invalid operation with label");
        switch(op) {
	case TOK_EQ:
	    pe->v = pe->v == e2.v;
	    break;
	case TOK_NE:
	    pe->v = pe->v != e2.v;
	    break;
	case TOK_LT:
	    pe->v = (int64_t)pe->v < (int64_t)e2.v;
	    break;
	case TOK_GE:
	    pe->v = (int64_t)pe->v >= (int64_t)e2.v;
	    break;
	case TOK_LE:
	    pe->v = (int64_t)pe->v <= (int64_t)e2.v;
	    break;
	case TOK_GT:
	    pe->v = (int64_t)pe->v > (int64_t)e2.v;
	    break;
        default:
            break;
        }
	/* GAS compare results are -1/0 not 1/0.  */
	pe->v = -(int64_t)pe->v;
    }
}

ST_FUNC void asm_expr(TCCState *s1, ExprValue *pe)
{
    asm_expr_cmp(s1, pe);
}

ST_FUNC int asm_int_expr(TCCState *s1)
{
    ExprValue e;
    asm_expr(s1, &e);
    if (e.sym)
        expect("constant");
    return e.v;
}

static Sym* asm_new_label1(TCCState *s1, int label, int is_local,
                           int sh_num, int value)
{
    Sym *sym;
    ElfSym *esym;

    (void)s1;
    sym = asm_label_find(label);
    if (sym) {
	esym = elfsym(sym);
	/* A VT_EXTERN symbol, even if it has a section is considered
	   overridable.  This is how we "define" .set targets.  Real
	   definitions won't have VT_EXTERN set.  */
        if (esym && esym->st_shndx != SHN_UNDEF) {
            /* the label is already defined */
            if (IS_ASM_SYM(sym)
                && (is_local == 1 || (sym->type.t & VT_EXTERN)))
                goto new_label;
            if (!(sym->type.t & VT_EXTERN))
                tcc_error("assembler label '%s' already defined",
                          get_tok_str(label, NULL));
        }
    } else {
    new_label:
        sym = asm_label_push(label);
    }
    if (!sym->c)
      put_extern_sym2(sym, SHN_UNDEF, 0, 0, 1);
    esym = elfsym(sym);
    esym->st_shndx = sh_num;
    esym->st_value = value;
    if (is_local != 2)
        sym->type.t &= ~VT_EXTERN;
    return sym;
}

static Sym* asm_new_label(TCCState *s1, int label, int is_local)
{
    return asm_new_label1(s1, label, is_local, cur_text_section->sh_num, ind);
}

/* Set the value of LABEL to that of some expression (possibly
   involving other symbols).  LABEL can be overwritten later still.  */
static Sym* set_symbol(TCCState *s1, int label)
{
    long n;
    ExprValue e;
    Sym *sym;
    ElfSym *esym;
    next();
    asm_expr(s1, &e);
    n = e.v;
    esym = elfsym(e.sym);
    if (esym)
	n += esym->st_value;
    sym = asm_new_label1(s1, label, 2, esym ? esym->st_shndx : SHN_ABS, n);
    elfsym(sym)->st_other |= ST_ASM_SET;
    return sym;
}

static void use_section1(TCCState *s1, Section *sec)
{
    (void)s1;
    cur_text_section->data_offset = ind;
    cur_text_section = sec;
    ind = cur_text_section->data_offset;
}

static void use_section(TCCState *s1, const char *name)
{
    Section *sec;
    sec = find_section(s1, name);
    use_section1(s1, sec);
}

static void push_section(TCCState *s1, const char *name)
{
    Section *sec = find_section(s1, name);
    sec->prev = cur_text_section;
    use_section1(s1, sec);
}

static void pop_section(TCCState *s1)
{
    Section *prev = cur_text_section->prev;
    if (!prev)
        tcc_error(".popsection without .pushsection");
    cur_text_section->prev = NULL;
    use_section1(s1, prev);
}

static void asm_parse_directive(TCCState *s1, int global)
{
    int n, offset, v, size, tok1;
    Section *sec;
    uint8_t *ptr;

    /* assembler directive */
    sec = cur_text_section;
    switch(tok) {
    case TOK_ASMDIR_align:
    case TOK_ASMDIR_balign:
    case TOK_ASMDIR_p2align:
    case TOK_ASMDIR_skip:
    case TOK_ASMDIR_space:
        tok1 = tok;
        next();
        n = asm_int_expr(s1);
        if (tok1 == TOK_ASMDIR_p2align)
        {
            if (n < 0 || n > 30)
                tcc_error("invalid p2align, must be between 0 and 30");
            n = 1 << n;
            tok1 = TOK_ASMDIR_align;
        }
        if (tok1 == TOK_ASMDIR_align || tok1 == TOK_ASMDIR_balign) {
            if (n < 0 || (n & (n-1)) != 0)
                tcc_error("alignment must be a positive power of two");
            offset = (ind + n - 1) & -n;
            size = offset - ind;
            /* the section must have a compatible alignment */
            if (sec->sh_addralign < n)
                sec->sh_addralign = n;
        } else {
	    if (n < 0)
	        n = 0;
            size = n;
        }
        v = 0;
        if (tok == ',') {
            next();
            v = asm_int_expr(s1);
        }
    zero_pad:
        if (sec->sh_type != SHT_NOBITS) {
            sec->data_offset = ind;
            ptr = section_ptr_add(sec, size);
            memset(ptr, v, size);
        }
        ind += size;
        break;
    case TOK_ASMDIR_quad:
#ifdef TCC_TARGET_X86_64
	size = 8;
	goto asm_data;
#else
        next();
        for(;;) {
            uint64_t vl;
            const char *p;

            p = tokc.str.data;
            if (tok != TOK_PPNUM) {
            error_constant:
                tcc_error("64 bit constant");
            }
            vl = strtoll(p, (char **)&p, 0);
            if (*p != '\0')
                goto error_constant;
            next();
            if (sec->sh_type != SHT_NOBITS) {
                /* XXX: endianness */
                gen_le32(vl);
                gen_le32(vl >> 32);
            } else {
                ind += 8;
            }
            if (tok != ',')
                break;
            next();
        }
        break;
#endif
    case TOK_ASMDIR_byte:
        size = 1;
        goto asm_data;
    case TOK_ASMDIR_word:
    case TOK_ASMDIR_short:
        size = 2;
        goto asm_data;
    case TOK_ASMDIR_long:
    case TOK_ASMDIR_int:
        size = 4;
    asm_data:
        next();
        for(;;) {
            ExprValue e;
            asm_expr(s1, &e);
            if (sec->sh_type != SHT_NOBITS) {
                if (size == 4) {
                    gen_expr32(&e);
#ifdef TCC_TARGET_X86_64
		} else if (size == 8) {
		    gen_expr64(&e);
#endif
                } else {
                    if (e.sym)
                        expect("constant");
                    if (size == 1)
                        g(e.v);
                    else
                        gen_le16(e.v);
                }
            } else {
                ind += size;
            }
            if (tok != ',')
                break;
            next();
        }
        break;
    case TOK_ASMDIR_fill:
        {
            int repeat, size, val, i, j;
            uint8_t repeat_buf[8];
            next();
            repeat = asm_int_expr(s1);
            if (repeat < 0) {
                tcc_error("repeat < 0; .fill ignored");
                break;
            }
            size = 1;
            val = 0;
            if (tok == ',') {
                next();
                size = asm_int_expr(s1);
                if (size < 0) {
                    tcc_error("size < 0; .fill ignored");
                    break;
                }
                if (size > 8)
                    size = 8;
                if (tok == ',') {
                    next();
                    val = asm_int_expr(s1);
                }
            }
            /* XXX: endianness */
            repeat_buf[0] = val;
            repeat_buf[1] = val >> 8;
            repeat_buf[2] = val >> 16;
            repeat_buf[3] = val >> 24;
            repeat_buf[4] = 0;
            repeat_buf[5] = 0;
            repeat_buf[6] = 0;
            repeat_buf[7] = 0;
            for(i = 0; i < repeat; i++) {
                for(j = 0; j < size; j++) {
                    g(repeat_buf[j]);
                }
            }
        }
        break;
    case TOK_ASMDIR_rept:
        {
            int repeat;
            TokenString *init_str;
            next();
            repeat = asm_int_expr(s1);
            init_str = tok_str_alloc();
            while (next(), tok != TOK_ASMDIR_endr) {
                if (tok == CH_EOF)
                    tcc_error("we at end of file, .endr not found");
                tok_str_add_tok(init_str);
            }
            tok_str_add(init_str, TOK_EOF);
            begin_macro(init_str, 1);
            while (repeat-- > 0) {
                tcc_assemble_internal(s1, (parse_flags & PARSE_FLAG_PREPROCESS),
				      global);
                macro_ptr = init_str->str;
            }
            end_macro();
            next();
            break;
        }
    case TOK_ASMDIR_org:
        {
            unsigned long n;
	    ExprValue e;
	    ElfSym *esym;
            next();
	    asm_expr(s1, &e);
	    n = e.v;
	    esym = elfsym(e.sym);
	    if (esym) {
		if (esym->st_shndx != cur_text_section->sh_num)
		  expect("constant or same-section symbol");
		n += esym->st_value;
	    }
            if (n < ind)
                tcc_error("attempt to .org backwards");
            v = 0;
            size = n - ind;
            goto zero_pad;
        }
        break;
    case TOK_ASMDIR_set:
	next();
	tok1 = tok;
	next();
	/* Also accept '.set stuff', but don't do anything with this.
	   It's used in GAS to set various features like '.set mips16'.  */
	if (tok == ',')
	    set_symbol(s1, tok1);
	break;
    case TOK_ASMDIR_globl:
    case TOK_ASMDIR_global:
    case TOK_ASMDIR_weak:
    case TOK_ASMDIR_hidden:
	tok1 = tok;
	do { 
            Sym *sym;
            next();
            sym = get_asm_sym(tok, NULL);
	    if (tok1 != TOK_ASMDIR_hidden)
                sym->type.t &= ~VT_STATIC;
            if (tok1 == TOK_ASMDIR_weak)
                sym->a.weak = 1;
	    else if (tok1 == TOK_ASMDIR_hidden)
	        sym->a.visibility = STV_HIDDEN;
            update_storage(sym);
            next();
	} while (tok == ',');
	break;
    case TOK_ASMDIR_string:
    case TOK_ASMDIR_ascii:
    case TOK_ASMDIR_asciz:
        {
            const uint8_t *p;
            int i, size, t;

            t = tok;
            next();
            for(;;) {
                if (tok != TOK_STR)
                    expect("string constant");
                p = tokc.str.data;
                size = tokc.str.size;
                if (t == TOK_ASMDIR_ascii && size > 0)
                    size--;
                for(i = 0; i < size; i++)
                    g(p[i]);
                next();
                if (tok == ',') {
                    next();
                } else if (tok != TOK_STR) {
                    break;
                }
            }
	}
	break;
    case TOK_ASMDIR_text:
    case TOK_ASMDIR_data:
    case TOK_ASMDIR_bss:
	{ 
            char sname[64];
            tok1 = tok;
            n = 0;
            next();
            if (tok != ';' && tok != TOK_LINEFEED) {
		n = asm_int_expr(s1);
		next();
            }
            if (n)
                sprintf(sname, "%s%d", get_tok_str(tok1, NULL), n);
            else
                sprintf(sname, "%s", get_tok_str(tok1, NULL));
            use_section(s1, sname);
	}
	break;
    case TOK_ASMDIR_file:
        {
            char filename[512];

            filename[0] = '\0';
            next();
            if (tok == TOK_STR)
                pstrcat(filename, sizeof(filename), tokc.str.data);
            else
                pstrcat(filename, sizeof(filename), get_tok_str(tok, NULL));
            tcc_warning_c(warn_unsupported)("ignoring .file %s", filename);
            next();
        }
        break;
    case TOK_ASMDIR_ident:
        {
            char ident[256];

            ident[0] = '\0';
            next();
            if (tok == TOK_STR)
                pstrcat(ident, sizeof(ident), tokc.str.data);
            else
                pstrcat(ident, sizeof(ident), get_tok_str(tok, NULL));
            tcc_warning_c(warn_unsupported)("ignoring .ident %s", ident);
            next();
        }
        break;
    case TOK_ASMDIR_size:
        { 
            Sym *sym;

            next();
            sym = asm_label_find(tok);
            if (!sym) {
                tcc_error("label not found: %s", get_tok_str(tok, NULL));
            }
            /* XXX .size name,label2-label1 */
            tcc_warning_c(warn_unsupported)("ignoring .size %s,*", get_tok_str(tok, NULL));
            next();
            skip(',');
            while (tok != TOK_LINEFEED && tok != ';' && tok != CH_EOF) {
                next();
            }
        }
        break;
    case TOK_ASMDIR_type:
        { 
            Sym *sym;
            const char *newtype;

            next();
            sym = get_asm_sym(tok, NULL);
            next();
            skip(',');
            if (tok == TOK_STR) {
                newtype = tokc.str.data;
            } else {
                if (tok == '@' || tok == '%')
                    next();
                newtype = get_tok_str(tok, NULL);
            }

            if (!strcmp(newtype, "function") || !strcmp(newtype, "STT_FUNC")) {
                sym->type.t = (sym->type.t & ~VT_BTYPE) | VT_FUNC;
                if (sym->c) {
                    ElfSym *esym = elfsym(sym);
                    esym->st_info = ELFW(ST_INFO)(ELFW(ST_BIND)(esym->st_info), STT_FUNC);
                }
            } else
                tcc_warning_c(warn_unsupported)("change type of '%s' from 0x%x to '%s' ignored",
                    get_tok_str(sym->v, NULL), sym->type.t, newtype);

            next();
        }
        break;
    case TOK_ASMDIR_pushsection:
    case TOK_ASMDIR_section:
        {
            char sname[256];
	    int old_nb_section = s1->nb_sections;

	    tok1 = tok;
            /* XXX: support more options */
            next();
            sname[0] = '\0';
            while (tok != ';' && tok != TOK_LINEFEED && tok != ',') {
                if (tok == TOK_STR)
                    pstrcat(sname, sizeof(sname), tokc.str.data);
                else
                    pstrcat(sname, sizeof(sname), get_tok_str(tok, NULL));
                next();
            }
            if (tok == ',') {
                /* skip section options */
                next();
                if (tok != TOK_STR)
                    expect("string constant");
                next();
                if (tok == ',') {
                    next();
                    if (tok == '@' || tok == '%')
                        next();
                    next();
                }
            }
            last_text_section = cur_text_section;
	    if (tok1 == TOK_ASMDIR_section)
	        use_section(s1, sname);
	    else
	        push_section(s1, sname);
	    /* If we just allocated a new section reset its alignment to
	       1.  new_section normally acts for GCC compatibility and
	       sets alignment to PTR_SIZE.  The assembler behaves different. */
	    if (old_nb_section != s1->nb_sections)
	        cur_text_section->sh_addralign = 1;
        }
        break;
    case TOK_ASMDIR_previous:
        { 
            Section *sec;
            next();
            if (!last_text_section)
                tcc_error("no previous section referenced");
            sec = cur_text_section;
            use_section1(s1, last_text_section);
            last_text_section = sec;
        }
        break;
    case TOK_ASMDIR_popsection:
	next();
	pop_section(s1);
	break;
#ifdef TCC_TARGET_I386
    case TOK_ASMDIR_code16:
        {
            next();
            s1->seg_size = 16;
        }
        break;
    case TOK_ASMDIR_code32:
        {
            next();
            s1->seg_size = 32;
        }
        break;
#endif
#ifdef TCC_TARGET_X86_64
    /* added for compatibility with GAS */
    case TOK_ASMDIR_code64:
        next();
        break;
#endif
    default:
        tcc_error("unknown assembler directive '.%s'", get_tok_str(tok, NULL));
        break;
    }
}


/* assemble a file */
static int tcc_assemble_internal(TCCState *s1, int do_preprocess, int global)
{
    int opcode;
    int saved_parse_flags = parse_flags;

    parse_flags = PARSE_FLAG_ASM_FILE | PARSE_FLAG_TOK_STR;
    if (do_preprocess)
        parse_flags |= PARSE_FLAG_PREPROCESS;
    for(;;) {
        next();
        if (tok == TOK_EOF)
            break;
        parse_flags |= PARSE_FLAG_LINEFEED; /* XXX: suppress that hack */
    redo:
        if (tok == '#') {
            /* horrible gas comment */
            while (tok != TOK_LINEFEED)
                next();
        } else if (tok >= TOK_ASMDIR_FIRST && tok <= TOK_ASMDIR_LAST) {
            asm_parse_directive(s1, global);
        } else if (tok == TOK_PPNUM) {
            const char *p;
            int n;
            p = tokc.str.data;
            n = strtoul(p, (char **)&p, 10);
            if (*p != '\0')
                expect("':'");
            /* new local label */
            asm_new_label(s1, asm_get_local_label_name(s1, n), 1);
            next();
            skip(':');
            goto redo;
        } else if (tok >= TOK_IDENT) {
            /* instruction or label */
            opcode = tok;
            next();
            if (tok == ':') {
                /* new label */
                asm_new_label(s1, opcode, 0);
                next();
                goto redo;
            } else if (tok == '=') {
		set_symbol(s1, opcode);
                goto redo;
            } else {
                asm_opcode(s1, opcode);
            }
        }
        /* end of line */
        if (tok != ';' && tok != TOK_LINEFEED)
            expect("end of line");
        parse_flags &= ~PARSE_FLAG_LINEFEED; /* XXX: suppress that hack */
    }

    parse_flags = saved_parse_flags;
    return 0;
}

/* Assemble the current file */
ST_FUNC int tcc_assemble(TCCState *s1, int do_preprocess)
{
    int ret;
    tcc_debug_start(s1);
    /* default section is text */
    cur_text_section = text_section;
    ind = cur_text_section->data_offset;
    nocode_wanted = 0;
    ret = tcc_assemble_internal(s1, do_preprocess, 1);
    cur_text_section->data_offset = ind;
    tcc_debug_end(s1);
    return ret;
}

/********************************************************************/
/* GCC inline asm support */

/* assemble the string 'str' in the current C compilation unit without
   C preprocessing. NOTE: str is modified by modifying the '\0' at the
   end */
static void tcc_assemble_inline(TCCState *s1, char *str, int len, int global)
{
    const int *saved_macro_ptr = macro_ptr;
    int dotid = set_idnum('.', IS_ID);
    int dolid = set_idnum('$', 0);

    tcc_open_bf(s1, ":asm:", len);
    memcpy(file->buffer, str, len);
    macro_ptr = NULL;
    tcc_assemble_internal(s1, 0, global);
    tcc_close();

    set_idnum('$', dolid);
    set_idnum('.', dotid);
    macro_ptr = saved_macro_ptr;
}

/* find a constraint by its number or id (gcc 3 extended
   syntax). return -1 if not found. Return in *pp in char after the
   constraint */
ST_FUNC int find_constraint(ASMOperand *operands, int nb_operands, 
                           const char *name, const char **pp)
{
    int index;
    TokenSym *ts;
    const char *p;

    if (isnum(*name)) {
        index = 0;
        while (isnum(*name)) {
            index = (index * 10) + (*name) - '0';
            name++;
        }
        if ((unsigned)index >= nb_operands)
            index = -1;
    } else if (*name == '[') {
        name++;
        p = strchr(name, ']');
        if (p) {
            ts = tok_alloc(name, p - name);
            for(index = 0; index < nb_operands; index++) {
                if (operands[index].id == ts->tok)
                    goto found;
            }
            index = -1;
        found:
            name = p + 1;
        } else {
            index = -1;
        }
    } else {
        index = -1;
    }
    if (pp)
        *pp = name;
    return index;
}

static void subst_asm_operands(ASMOperand *operands, int nb_operands, 
                               CString *out_str, const char *str)
{
    int c, index, modifier;
    ASMOperand *op;
    SValue sv;

    for(;;) {
        c = *str++;
        if (c == '%') {
            if (*str == '%') {
                str++;
                goto add_char;
            }
            modifier = 0;
            if (*str == 'c' || *str == 'n' ||
                *str == 'b' || *str == 'w' || *str == 'h' || *str == 'k' ||
		*str == 'q' || *str == 'l' ||
		/* P in GCC would add "@PLT" to symbol refs in PIC mode,
		   and make literal operands not be decorated with '$'.  */
		*str == 'P')
                modifier = *str++;
            index = find_constraint(operands, nb_operands, str, &str);
            if (index < 0)
                tcc_error("invalid operand reference after %%");
            op = &operands[index];
            if (modifier == 'l') {
                cstr_cat(out_str, get_tok_str(op->is_label, NULL), -1);
            } else {
                sv = *op->vt;
                if (op->reg >= 0) {
                    sv.r = op->reg;
                    if ((op->vt->r & VT_VALMASK) == VT_LLOCAL && op->is_memory)
                      sv.r |= VT_LVAL;
                }
                subst_asm_operand(out_str, &sv, modifier);
            }
        } else {
        add_char:
            cstr_ccat(out_str, c);
            if (c == '\0')
                break;
        }
    }
}


static void parse_asm_operands(ASMOperand *operands, int *nb_operands_ptr,
                               int is_output)
{
    ASMOperand *op;
    int nb_operands;
    char* astr;

    if (tok != ':') {
        nb_operands = *nb_operands_ptr;
        for(;;) {
            if (nb_operands >= MAX_ASM_OPERANDS)
                tcc_error("too many asm operands");
            op = &operands[nb_operands++];
            op->id = 0;
            if (tok == '[') {
                next();
                if (tok < TOK_IDENT)
                    expect("identifier");
                op->id = tok;
                next();
                skip(']');
            }
	    astr = parse_mult_str("string constant")->data;
            pstrcpy(op->constraint, sizeof op->constraint, astr);
            skip('(');
            gexpr();
            if (is_output) {
                if (!(vtop->type.t & VT_ARRAY))
                    test_lvalue();
            } else {
                /* we want to avoid LLOCAL case, except when the 'm'
                   constraint is used. Note that it may come from
                   register storage, so we need to convert (reg)
                   case */
                if ((vtop->r & VT_LVAL) &&
                    ((vtop->r & VT_VALMASK) == VT_LLOCAL ||
                     (vtop->r & VT_VALMASK) < VT_CONST) &&
                    !strchr(op->constraint, 'm')) {
                    gv(RC_INT);
                }
            }
            op->vt = vtop;
            skip(')');
            if (tok == ',') {
                next();
            } else {
                break;
            }
        }
        *nb_operands_ptr = nb_operands;
    }
}

/* parse the GCC asm() instruction */
ST_FUNC void asm_instr(void)
{
    CString astr, *astr1;

    ASMOperand operands[MAX_ASM_OPERANDS];
    int nb_outputs, nb_operands, i, must_subst, out_reg, nb_labels;
    uint8_t clobber_regs[NB_ASM_REGS];
    Section *sec;

    /* since we always generate the asm() instruction, we can ignore
       volatile */
    while (tok == TOK_VOLATILE1 || tok == TOK_VOLATILE2 || tok == TOK_VOLATILE3
           || tok == TOK_GOTO) {
        next();
    }

    astr1 = parse_asm_str();
    cstr_new_s(&astr);
    cstr_cat(&astr, astr1->data, astr1->size);

    nb_operands = 0;
    nb_outputs = 0;
    nb_labels = 0;
    must_subst = 0;
    memset(clobber_regs, 0, sizeof(clobber_regs));
    if (tok == ':') {
        next();
        must_subst = 1;
        /* output args */
        parse_asm_operands(operands, &nb_operands, 1);
        nb_outputs = nb_operands;
        if (tok == ':') {
            next();
            if (tok != ')') {
                /* input args */
                parse_asm_operands(operands, &nb_operands, 0);
                if (tok == ':') {
                    /* clobber list */
                    /* XXX: handle registers */
                    next();
                    for(;;) {
                        if (tok == ':')
                          break;
                        if (tok != TOK_STR)
                            expect("string constant");
                        asm_clobber(clobber_regs, tokc.str.data);
                        next();
                        if (tok == ',') {
                            next();
                        } else {
                            break;
                        }
                    }
                }
                if (tok == ':') {
                    /* goto labels */
                    next();
                    for (;;) {
                        Sym *csym;
                        int asmname;
                        if (nb_operands + nb_labels >= MAX_ASM_OPERANDS)
                          tcc_error("too many asm operands");
                        if (tok < TOK_UIDENT)
                          expect("label identifier");
                        operands[nb_operands + nb_labels++].id = tok;

                        csym = label_find(tok);
                        if (!csym) {
                            csym = label_push(&global_label_stack, tok,
                                              LABEL_FORWARD);
                        } else {
                            if (csym->r == LABEL_DECLARED)
                              csym->r = LABEL_FORWARD;
                        }
                        next();
                        asmname = asm_get_prefix_name(tcc_state, "LG.",
                                                      ++asmgoto_n);
                        if (!csym->c)
                          put_extern_sym2(csym, SHN_UNDEF, 0, 0, 1);
                        get_asm_sym(asmname, csym);
                        operands[nb_operands + nb_labels - 1].is_label = asmname;

                        if (tok != ',')
                          break;
                        next();
                    }
                }
            }
        }
    }
    skip(')');
    /* NOTE: we do not eat the ';' so that we can restore the current
       token after the assembler parsing */
    if (tok != ';')
        expect("';'");
    
    /* save all values in the memory */
    save_regs(0);

    /* compute constraints */
    asm_compute_constraints(operands, nb_operands, nb_outputs, 
                            clobber_regs, &out_reg);

    /* substitute the operands in the asm string. No substitution is
       done if no operands (GCC behaviour) */
#ifdef ASM_DEBUG
    printf("asm: \"%s\"\n", (char *)astr.data);
#endif
    if (must_subst) {
        cstr_reset(astr1);
        cstr_cat(astr1, astr.data, astr.size);
        cstr_reset(&astr);
        subst_asm_operands(operands, nb_operands + nb_labels, &astr, astr1->data);
    }

#ifdef ASM_DEBUG
    printf("subst_asm: \"%s\"\n", (char *)astr.data);
#endif

    /* generate loads */
    asm_gen_code(operands, nb_operands, nb_outputs, 0, 
                 clobber_regs, out_reg);    

    /* We don't allow switching section within inline asm to
       bleed out to surrounding code.  */
    sec = cur_text_section;
    /* assemble the string with tcc internal assembler */
    tcc_assemble_inline(tcc_state, astr.data, astr.size - 1, 0);
    cstr_free_s(&astr);
    if (sec != cur_text_section) {
        tcc_warning("inline asm tries to change current section");
        use_section1(tcc_state, sec);
    }

    /* restore the current C token */
    next();

    /* store the output values if needed */
    asm_gen_code(operands, nb_operands, nb_outputs, 1, 
                 clobber_regs, out_reg);
    
    /* free everything */
    for(i=0;i<nb_operands;i++) {
        vpop();
    }

}

ST_FUNC void asm_global_instr(void)
{
    CString *astr;
    int saved_nocode_wanted = nocode_wanted;

    /* Global asm blocks are always emitted.  */
    nocode_wanted = 0;
    next();
    astr = parse_asm_str();
    skip(')');
    /* NOTE: we do not eat the ';' so that we can restore the current
       token after the assembler parsing */
    if (tok != ';')
        expect("';'");
    
#ifdef ASM_DEBUG
    printf("asm_global: \"%s\"\n", (char *)astr.data);
#endif
    cur_text_section = text_section;
    ind = cur_text_section->data_offset;

    /* assemble the string with tcc internal assembler */
    tcc_assemble_inline(tcc_state, astr->data, astr->size - 1, 1);
    
    cur_text_section->data_offset = ind;

    /* restore the current C token */
    next();

    nocode_wanted = saved_nocode_wanted;
}

/********************************************************/
#else
ST_FUNC int tcc_assemble(TCCState *s1, int do_preprocess)
{
    tcc_error("asm not supported");
}

ST_FUNC void asm_instr(void)
{
    tcc_error("inline asm() not supported");
}

ST_FUNC void asm_global_instr(void)
{
    tcc_error("inline asm() not supported");
}
#endif /* CONFIG_TCC_ASM */

//// tcc: tccdbg.c

/*
 *  TCC - Tiny C Compiler
 *
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "tcc.h"

/* stab debug support */

static const struct {
  int type;
  int size;
  int encoding;
  const char *name;
} default_debug[] = {
    {   VT_INT, 4, DW_ATE_signed, "int:t1=r1;-2147483648;2147483647;" },
    {   VT_BYTE, 1, DW_ATE_signed_char, "char:t2=r2;0;127;" },
#if LONG_SIZE == 4
    {   VT_LONG | VT_INT, 4, DW_ATE_signed, "long int:t3=r3;-2147483648;2147483647;" },
#else
    {   VT_LLONG | VT_LONG, 8, DW_ATE_signed, "long int:t3=r3;-9223372036854775808;9223372036854775807;" },
#endif
    {   VT_INT | VT_UNSIGNED, 4, DW_ATE_unsigned, "unsigned int:t4=r4;0;037777777777;" },
#if LONG_SIZE == 4
    {   VT_LONG | VT_INT | VT_UNSIGNED, 4, DW_ATE_unsigned, "long unsigned int:t5=r5;0;037777777777;" },
#else
    /* use octal instead of -1 so size_t works (-gstabs+ in gcc) */
    {   VT_LLONG | VT_LONG | VT_UNSIGNED, 8, DW_ATE_unsigned, "long unsigned int:t5=r5;0;01777777777777777777777;" },
#endif
    {   VT_QLONG, 16, DW_ATE_signed, "__int128:t6=r6;0;-1;" },
    {   VT_QLONG | VT_UNSIGNED, 16, DW_ATE_unsigned, "__int128 unsigned:t7=r7;0;-1;" },
    {   VT_LLONG, 8, DW_ATE_signed, "long long int:t8=r8;-9223372036854775808;9223372036854775807;" },
    {   VT_LLONG | VT_UNSIGNED, 8, DW_ATE_unsigned, "long long unsigned int:t9=r9;0;01777777777777777777777;" },
    {   VT_SHORT, 2, DW_ATE_signed, "short int:t10=r10;-32768;32767;" },
    {   VT_SHORT | VT_UNSIGNED, 2, DW_ATE_unsigned, "short unsigned int:t11=r11;0;65535;" },
    {   VT_BYTE | VT_DEFSIGN, 1, DW_ATE_signed_char, "signed char:t12=r12;-128;127;" },
    {   VT_BYTE | VT_DEFSIGN | VT_UNSIGNED, 1, DW_ATE_unsigned_char, "unsigned char:t13=r13;0;255;" },
    {   VT_FLOAT, 4, DW_ATE_float, "float:t14=r1;4;0;" },
    {   VT_DOUBLE, 8, DW_ATE_float, "double:t15=r1;8;0;" },
#ifdef TCC_USING_DOUBLE_FOR_LDOUBLE
    {   VT_DOUBLE | VT_LONG, 8, DW_ATE_float, "long double:t16=r1;8;0;" },
#else
    {   VT_LDOUBLE, 16, DW_ATE_float, "long double:t16=r1;16;0;" },
#endif
    {   -1, -1, -1, "_Float32:t17=r1;4;0;" },
    {   -1, -1, -1, "_Float64:t18=r1;8;0;" },
    {   -1, -1, -1, "_Float128:t19=r1;16;0;" },
    {   -1, -1, -1, "_Float32x:t20=r1;8;0;" },
    {   -1, -1, -1, "_Float64x:t21=r1;16;0;" },
    {   -1, -1, -1, "_Decimal32:t22=r1;4;0;" },
    {   -1, -1, -1, "_Decimal64:t23=r1;8;0;" },
    {   -1, -1, -1, "_Decimal128:t24=r1;16;0;" },
    /* if default char is unsigned */
    {   VT_BYTE | VT_UNSIGNED, 1, DW_ATE_unsigned_char, "unsigned char:t25=r25;0;255;" },
    /* boolean type */
    {   VT_BOOL, 1, DW_ATE_boolean, "bool:t26=r26;0;255;" },
#if LONG_SIZE == 4
    {   VT_VOID, 1, DW_ATE_unsigned_char, "void:t27=27" },
#else 
    /* bitfields use these */
    {   VT_LONG | VT_INT, 8, DW_ATE_signed, "long int:t27=r27;-9223372036854775808;9223372036854775807;" },
    {   VT_LONG | VT_INT | VT_UNSIGNED, 8, DW_ATE_unsigned, "long unsigned int:t28=r28;0;01777777777777777777777;" },
    {   VT_VOID, 1, DW_ATE_unsigned_char, "void:t29=29" },
#endif
};

#define	N_DEFAULT_DEBUG	(sizeof (default_debug) / sizeof (default_debug[0]))

/* dwarf debug */

#define	DWARF_LINE_BASE				-5
#define	DWARF_LINE_RANGE			14
#define	DWARF_OPCODE_BASE			13

#if defined TCC_TARGET_ARM64
#define	DWARF_MIN_INSTR_LEN			4
#elif defined TCC_TARGET_ARM
#define	DWARF_MIN_INSTR_LEN			2
#else
#define	DWARF_MIN_INSTR_LEN			1
#endif

#define	DWARF_ABBREV_COMPILE_UNIT		1
#define	DWARF_ABBREV_BASE_TYPE			2
#define	DWARF_ABBREV_VARIABLE_EXTERNAL		3
#define	DWARF_ABBREV_VARIABLE_STATIC		4
#define	DWARF_ABBREV_VARIABLE_LOCAL		5
#define	DWARF_ABBREV_FORMAL_PARAMETER		6
#define	DWARF_ABBREV_POINTER			7
#define	DWARF_ABBREV_ARRAY_TYPE			8
#define	DWARF_ABBREV_SUBRANGE_TYPE		9
#define	DWARF_ABBREV_TYPEDEF			10
#define	DWARF_ABBREV_ENUMERATOR_SIGNED		11
#define	DWARF_ABBREV_ENUMERATOR_UNSIGNED	12
#define	DWARF_ABBREV_ENUMERATION_TYPE		13
#define	DWARF_ABBREV_MEMBER			14
#define	DWARF_ABBREV_MEMBER_BF			15
#define	DWARF_ABBREV_STRUCTURE_TYPE		16
#define	DWARF_ABBREV_STRUCTURE_EMPTY_TYPE	17
#define	DWARF_ABBREV_UNION_TYPE			18
#define	DWARF_ABBREV_UNION_EMPTY_TYPE		19
#define	DWARF_ABBREV_SUBPROGRAM_EXTERNAL	20
#define	DWARF_ABBREV_SUBPROGRAM_STATIC		21
#define	DWARF_ABBREV_LEXICAL_BLOCK		22
#define	DWARF_ABBREV_LEXICAL_EMPTY_BLOCK	23
#define	DWARF_ABBREV_SUBROUTINE_TYPE		24
#define	DWARF_ABBREV_SUBROUTINE_EMPTY_TYPE	25
#define	DWARF_ABBREV_FORMAL_PARAMETER2		26

/* all entries should have been generated with dwarf_uleb128 except
   has_children. All values are currently below 128 so this currently
   works.  */
static const unsigned char dwarf_abbrev_init[] = {
    DWARF_ABBREV_COMPILE_UNIT, DW_TAG_compile_unit, 1,
          DW_AT_producer, DW_FORM_strp,
          DW_AT_language, DW_FORM_data1,
          DW_AT_name, DW_FORM_line_strp,
          DW_AT_comp_dir, DW_FORM_line_strp,
          DW_AT_low_pc, DW_FORM_addr,
#if PTR_SIZE == 4
          DW_AT_high_pc, DW_FORM_data4,
#else
          DW_AT_high_pc, DW_FORM_data8,
#endif
          DW_AT_stmt_list, DW_FORM_sec_offset,
          0, 0,
    DWARF_ABBREV_BASE_TYPE, DW_TAG_base_type, 0,
          DW_AT_byte_size, DW_FORM_udata,
          DW_AT_encoding, DW_FORM_data1,
          DW_AT_name, DW_FORM_strp,
          0, 0,
    DWARF_ABBREV_VARIABLE_EXTERNAL, DW_TAG_variable, 0,
          DW_AT_name, DW_FORM_strp,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_external, DW_FORM_flag,
          DW_AT_location, DW_FORM_exprloc,
          0, 0,
    DWARF_ABBREV_VARIABLE_STATIC, DW_TAG_variable, 0,
          DW_AT_name, DW_FORM_strp,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_location, DW_FORM_exprloc,
          0, 0,
    DWARF_ABBREV_VARIABLE_LOCAL, DW_TAG_variable, 0,
          DW_AT_name, DW_FORM_strp,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_location, DW_FORM_exprloc,
          0, 0,
    DWARF_ABBREV_FORMAL_PARAMETER, DW_TAG_formal_parameter, 0,
          DW_AT_name, DW_FORM_strp,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_location, DW_FORM_exprloc,
          0, 0,
    DWARF_ABBREV_POINTER, DW_TAG_pointer_type, 0,
          DW_AT_byte_size, DW_FORM_data1,
          DW_AT_type, DW_FORM_ref4,
          0, 0,
    DWARF_ABBREV_ARRAY_TYPE, DW_TAG_array_type, 1,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_sibling, DW_FORM_ref4,
          0, 0,
    DWARF_ABBREV_SUBRANGE_TYPE, DW_TAG_subrange_type, 0,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_upper_bound, DW_FORM_udata,
          0, 0,
    DWARF_ABBREV_TYPEDEF, DW_TAG_typedef, 0,
          DW_AT_name, DW_FORM_strp,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          DW_AT_type, DW_FORM_ref4,
          0, 0,
    DWARF_ABBREV_ENUMERATOR_SIGNED, DW_TAG_enumerator, 0,
          DW_AT_name, DW_FORM_strp,
          DW_AT_const_value, DW_FORM_sdata,
          0, 0,
    DWARF_ABBREV_ENUMERATOR_UNSIGNED, DW_TAG_enumerator, 0,
          DW_AT_name, DW_FORM_strp,
          DW_AT_const_value, DW_FORM_udata,
          0, 0,
    DWARF_ABBREV_ENUMERATION_TYPE, DW_TAG_enumeration_type, 1,
          DW_AT_name, DW_FORM_strp,
          DW_AT_encoding, DW_FORM_data1,
          DW_AT_byte_size, DW_FORM_data1,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          DW_AT_sibling, DW_FORM_ref4,
          0, 0,
    DWARF_ABBREV_MEMBER, DW_TAG_member, 0,
          DW_AT_name, DW_FORM_strp,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_data_member_location, DW_FORM_udata,
          0, 0,
    DWARF_ABBREV_MEMBER_BF, DW_TAG_member, 0,
          DW_AT_name, DW_FORM_strp,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_bit_size, DW_FORM_udata,
          DW_AT_data_bit_offset, DW_FORM_udata,
          0, 0,
    DWARF_ABBREV_STRUCTURE_TYPE, DW_TAG_structure_type, 1,
          DW_AT_name, DW_FORM_strp,
          DW_AT_byte_size, DW_FORM_udata,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          DW_AT_sibling, DW_FORM_ref4,
          0, 0,
    DWARF_ABBREV_STRUCTURE_EMPTY_TYPE, DW_TAG_structure_type, 0,
          DW_AT_name, DW_FORM_strp,
          DW_AT_byte_size, DW_FORM_udata,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          0, 0,
    DWARF_ABBREV_UNION_TYPE, DW_TAG_union_type, 1,
          DW_AT_name, DW_FORM_strp,
          DW_AT_byte_size, DW_FORM_udata,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          DW_AT_sibling, DW_FORM_ref4,
          0, 0,
    DWARF_ABBREV_UNION_EMPTY_TYPE, DW_TAG_union_type, 0,
          DW_AT_name, DW_FORM_strp,
          DW_AT_byte_size, DW_FORM_udata,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          0, 0,
    DWARF_ABBREV_SUBPROGRAM_EXTERNAL, DW_TAG_subprogram, 1,
          DW_AT_external, DW_FORM_flag,
          DW_AT_name, DW_FORM_strp,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_low_pc, DW_FORM_addr,
#if PTR_SIZE == 4
          DW_AT_high_pc, DW_FORM_data4,
#else
          DW_AT_high_pc, DW_FORM_data8,
#endif
          DW_AT_sibling, DW_FORM_ref4,
	  DW_AT_frame_base, DW_FORM_exprloc,
          0, 0,
    DWARF_ABBREV_SUBPROGRAM_STATIC, DW_TAG_subprogram, 1,
          DW_AT_name, DW_FORM_strp,
          DW_AT_decl_file, DW_FORM_udata,
          DW_AT_decl_line, DW_FORM_udata,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_low_pc, DW_FORM_addr,
#if PTR_SIZE == 4
          DW_AT_high_pc, DW_FORM_data4,
#else
          DW_AT_high_pc, DW_FORM_data8,
#endif
          DW_AT_sibling, DW_FORM_ref4,
	  DW_AT_frame_base, DW_FORM_exprloc,
          0, 0,
    DWARF_ABBREV_LEXICAL_BLOCK, DW_TAG_lexical_block, 1,
          DW_AT_low_pc, DW_FORM_addr,
#if PTR_SIZE == 4
          DW_AT_high_pc, DW_FORM_data4,
#else
          DW_AT_high_pc, DW_FORM_data8,
#endif
          0, 0,
    DWARF_ABBREV_LEXICAL_EMPTY_BLOCK, DW_TAG_lexical_block, 0,
          DW_AT_low_pc, DW_FORM_addr,
#if PTR_SIZE == 4
          DW_AT_high_pc, DW_FORM_data4,
#else
          DW_AT_high_pc, DW_FORM_data8,
#endif
          0, 0,
    DWARF_ABBREV_SUBROUTINE_TYPE, DW_TAG_subroutine_type, 1,
          DW_AT_type, DW_FORM_ref4,
          DW_AT_sibling, DW_FORM_ref4,
          0, 0,
    DWARF_ABBREV_SUBROUTINE_EMPTY_TYPE, DW_TAG_subroutine_type, 0,
          DW_AT_type, DW_FORM_ref4,
          0, 0,
    DWARF_ABBREV_FORMAL_PARAMETER2, DW_TAG_formal_parameter, 0,
          DW_AT_type, DW_FORM_ref4,
          0, 0,
  0
};

static const unsigned char dwarf_line_opcodes[] = {
    0 ,1 ,1 ,1 ,1 ,0 ,0 ,0 ,1 ,0 ,0 ,1
};

/* ------------------------------------------------------------------------- */
/* debug state */

struct _tccdbg {

    int last_line_num, new_file;
    int section_sym;

    int debug_next_type;

    struct _debug_hash {
        int debug_type;
        Sym *type;
    } *debug_hash;

    struct _debug_anon_hash {
        Sym *type;
        int n_debug_type;
        int *debug_type;
    } *debug_anon_hash;

    int n_debug_hash;
    int n_debug_anon_hash;

    struct _debug_info {
        int start;
        int end;
        int n_sym;
        struct debug_sym {
            int type;
            unsigned long value;
            char *str;
            Section *sec;
            int sym_index;
            int info;
            int file;
            int line;
        } *sym;
        struct _debug_info *child, *next, *last, *parent;
    } *debug_info, *debug_info_root;

    struct {
        int info;
        int abbrev;
        int line;
        int str;
        int line_str;
    } dwarf_sym;

    struct {
        int start;
        int dir_size;
        char **dir_table;
        int filename_size;
        struct dwarf_filename_struct {
            int dir_entry;
            char *name;
        } *filename_table;
        int line_size;
        int line_max_size;
        unsigned char *line_data;
        int cur_file;
        int last_file;
        int last_pc;
        int last_line;
    } dwarf_line;

    struct {
        int start;
        Sym *func;
        int line;
        int base_type_used[N_DEFAULT_DEBUG];
    } dwarf_info;

    /* test coverage */
    struct {
        unsigned long offset;
        unsigned long last_file_name;
        unsigned long last_func_name;
        int ind;
        int line;
    } tcov_data;

};

#define last_line_num       s1->dState->last_line_num
#define new_file            s1->dState->new_file
#define section_sym         s1->dState->section_sym
#define debug_next_type     s1->dState->debug_next_type
#define debug_hash          s1->dState->debug_hash
#define debug_anon_hash     s1->dState->debug_anon_hash
#define n_debug_hash        s1->dState->n_debug_hash
#define n_debug_anon_hash   s1->dState->n_debug_anon_hash
#define debug_info          s1->dState->debug_info
#define debug_info_root     s1->dState->debug_info_root
#define dwarf_sym           s1->dState->dwarf_sym
#define dwarf_line          s1->dState->dwarf_line
#define dwarf_info          s1->dState->dwarf_info
#define tcov_data           s1->dState->tcov_data

/* ------------------------------------------------------------------------- */
static void put_stabs(TCCState *s1, const char *str, int type, int other,
    int desc, unsigned long value);

ST_FUNC void tcc_debug_new(TCCState *s1)
{
    int shf = 0;
    if (!s1->dState)
        s1->dState = tcc_mallocz(sizeof *s1->dState);

#ifdef CONFIG_TCC_BACKTRACE
    /* include stab info with standalone backtrace support */
    if (s1->do_debug && s1->output_type == TCC_OUTPUT_MEMORY)
        s1->do_backtrace = 1;
    if (s1->do_backtrace)
        shf = SHF_ALLOC | SHF_WRITE; // SHF_WRITE needed for musl/SELINUX
#endif

    if (s1->dwarf) {
        s1->dwlo = s1->nb_sections;
        dwarf_info_section =
	    new_section(s1, ".debug_info", SHT_PROGBITS, shf);
        dwarf_abbrev_section =
	    new_section(s1, ".debug_abbrev", SHT_PROGBITS, shf);
        dwarf_line_section =
	    new_section(s1, ".debug_line", SHT_PROGBITS, shf);
        dwarf_aranges_section =
	    new_section(s1, ".debug_aranges", SHT_PROGBITS, shf);
	shf |= SHF_MERGE | SHF_STRINGS;
        dwarf_str_section =
	    new_section(s1, ".debug_str", SHT_PROGBITS, shf);
	dwarf_str_section->sh_entsize = 1;
	dwarf_info_section->sh_addralign =
	dwarf_abbrev_section->sh_addralign =
	dwarf_line_section->sh_addralign =
	dwarf_aranges_section->sh_addralign =
	dwarf_str_section->sh_addralign = 1;
	if (s1->dwarf >= 5) {
            dwarf_line_str_section =
	        new_section(s1, ".debug_line_str", SHT_PROGBITS, shf);
	    dwarf_line_str_section->sh_entsize = 1;
	    dwarf_line_str_section->sh_addralign = 1;
	}
        s1->dwhi = s1->nb_sections;
    }
    else
    {
        stab_section = new_section(s1, ".stab", SHT_PROGBITS, shf);
        stab_section->sh_entsize = sizeof(Stab_Sym);
        stab_section->sh_addralign = sizeof ((Stab_Sym*)0)->n_value;
        stab_section->link = new_section(s1, ".stabstr", SHT_STRTAB, shf);
        /* put first entry */
        put_stabs(s1, "", 0, 0, 0, 0);
    }
}

/* put stab debug information */
static void put_stabs(TCCState *s1, const char *str, int type, int other, int desc,
                      unsigned long value)
{
    Stab_Sym *sym;

    unsigned offset;
    if (type == N_SLINE
        && (offset = stab_section->data_offset)
        && (sym = (Stab_Sym*)(stab_section->data + offset) - 1)
        && sym->n_type == type
        && sym->n_value == value) {
        /* just update line_number in previous entry */
        sym->n_desc = desc;
        return;
    }

    sym = section_ptr_add(stab_section, sizeof(Stab_Sym));
    if (str) {
        sym->n_strx = put_elf_str(stab_section->link, str);
    } else {
        sym->n_strx = 0;
    }
    sym->n_type = type;
    sym->n_other = other;
    sym->n_desc = desc;
    sym->n_value = value;
}

static void put_stabs_r(TCCState *s1, const char *str, int type, int other, int desc,
                        unsigned long value, Section *sec, int sym_index)
{
    (void)sec;
    put_elf_reloc(symtab_section, stab_section,
                  stab_section->data_offset + 8,
                  sizeof ((Stab_Sym*)0)->n_value == PTR_SIZE ? R_DATA_PTR : R_DATA_32,
                  sym_index);
    put_stabs(s1, str, type, other, desc, value);
}

static void put_stabn(TCCState *s1, int type, int other, int desc, int value)
{
    put_stabs(s1, NULL, type, other, desc, value);
}

/* ------------------------------------------------------------------------- */
#define	dwarf_data1(s,data) \
	{ unsigned char *p = section_ptr_add((s), 1); *p = (data); }
#define	dwarf_data2(s,data) \
	write16le(section_ptr_add((s), 2), (data))
#define	dwarf_data4(s,data) \
	write32le(section_ptr_add((s), 4), (data))
#define	dwarf_data8(s,data) \
	write64le(section_ptr_add((s), 8), (data))

static int dwarf_get_section_sym(Section *s)
{
    TCCState *s1 = s->s1;
    return put_elf_sym(symtab_section, 0, 0,
                       ELFW(ST_INFO)(STB_LOCAL, STT_SECTION), 0,
                       s->sh_num, NULL);
}

static void dwarf_reloc(Section *s, int sym, int rel)
{
    TCCState *s1 = s->s1;
    put_elf_reloca(symtab_section, s, s->data_offset, rel, sym, 0);
}

static void dwarf_string(Section *s, Section *dw, int sym, const char *str)
{
    TCCState *s1 = s->s1;
    int offset, len;
    char *ptr;

    len = strlen(str) + 1;
    offset = dw->data_offset;
    ptr = section_ptr_add(dw, len);
    memmove(ptr, str, len);
    put_elf_reloca(symtab_section, s, s->data_offset, R_DATA_32DW, sym,
                   PTR_SIZE == 4 ? 0 : offset);
    dwarf_data4(s, PTR_SIZE == 4 ? offset : 0);
}

static void dwarf_strp(Section *s, const char *str)
{
    TCCState *s1 = s->s1;
    dwarf_string(s, dwarf_str_section, dwarf_sym.str, str);
}

static void dwarf_line_strp(Section *s, const char *str)
{
    TCCState *s1 = s->s1;
    dwarf_string(s, dwarf_line_str_section, dwarf_sym.line_str, str);
}

static void dwarf_line_op(TCCState *s1, unsigned char op)
{
    if (dwarf_line.line_size >= dwarf_line.line_max_size) {
	dwarf_line.line_max_size += 1024;
	dwarf_line.line_data =
	    (unsigned char *)tcc_realloc(dwarf_line.line_data,
					 dwarf_line.line_max_size);
    }
    dwarf_line.line_data[dwarf_line.line_size++] = op;
}

static void dwarf_file(TCCState *s1)
{
    int i, j;
    char *filename;
    int index_offset = s1->dwarf < 5;

    if (!strcmp(file->filename, "<command line>")) {
        dwarf_line.cur_file = 1;
	return;
    }
    filename = strrchr(file->filename, '/');
    if (filename == NULL) {
        for (i = 1; i < dwarf_line.filename_size; i++)
            if (dwarf_line.filename_table[i].dir_entry == 0 &&
		strcmp(dwarf_line.filename_table[i].name,
		       file->filename) == 0) {
		    dwarf_line.cur_file = i + index_offset;
	            return;
		}
	i = -index_offset;
	filename = file->filename;
    }
    else {
	char *undo = filename;
	char *dir = file->filename;

	*filename++ = '\0';
        for (i = 0; i < dwarf_line.dir_size; i++)
	    if (strcmp(dwarf_line.dir_table[i], dir) == 0) {
		for (j = 1; j < dwarf_line.filename_size; j++)
		    if (dwarf_line.filename_table[j].dir_entry - index_offset
			== i &&
			strcmp(dwarf_line.filename_table[j].name,
			       filename) == 0) {
			*undo = '/';
		        dwarf_line.cur_file = j + index_offset;
			return;
		    }
		break;
	    }
	if (i == dwarf_line.dir_size) {
	    dwarf_line.dir_size++;
	    dwarf_line.dir_table = 
                (char **) tcc_realloc(dwarf_line.dir_table,
                                      dwarf_line.dir_size *
                                      sizeof (char *));
            dwarf_line.dir_table[i] = tcc_strdup(dir);
	}
	*undo = '/';
    }
    dwarf_line.filename_table =
        (struct dwarf_filename_struct *)
        tcc_realloc(dwarf_line.filename_table,
                    (dwarf_line.filename_size + 1) *
                    sizeof (struct dwarf_filename_struct));
    dwarf_line.filename_table[dwarf_line.filename_size].dir_entry =
	i + index_offset;
    dwarf_line.filename_table[dwarf_line.filename_size].name =
        tcc_strdup(filename);
    dwarf_line.cur_file = dwarf_line.filename_size++ + index_offset;
    return;
}

#if 0
static int dwarf_uleb128_size (unsigned long long value)
{
    int size =  0;

    do {
        value >>= 7;
        size++;
    } while (value != 0);
    return size;
}
#endif

static int dwarf_sleb128_size (long long value)
{
    int size =  0;
    long long end = value >> 63;
    unsigned char last = end & 0x40;
    unsigned char byte;

    do {
        byte = value & 0x7f;
        value >>= 7;
        size++;
    } while (value != end || (byte & 0x40) != last);
    return size;
}

static void dwarf_uleb128 (Section *s, unsigned long long value)
{
    do {
        unsigned char byte = value & 0x7f;

        value >>= 7;
        dwarf_data1(s, byte | (value ? 0x80 : 0));
    } while (value != 0);
}

static void dwarf_sleb128 (Section *s, long long value)
{
    int more;
    long long end = value >> 63;
    unsigned char last = end & 0x40;

    do {
        unsigned char byte = value & 0x7f;

        value >>= 7;
	more = value != end || (byte & 0x40) != last;
        dwarf_data1(s, byte | (0x80 * more));
    } while (more);
}

static void dwarf_uleb128_op (TCCState *s1, unsigned long long value)
{
    do {
        unsigned char byte = value & 0x7f;

        value >>= 7;
        dwarf_line_op(s1, byte | (value ? 0x80 : 0));
    } while (value != 0);
}

static void dwarf_sleb128_op (TCCState *s1, long long value)
{
    int more;
    long long end = value >> 63;
    unsigned char last = end & 0x40;

    do {
        unsigned char byte = value & 0x7f;

        value >>= 7;
        more = value != end || (byte & 0x40) != last;
        dwarf_line_op(s1, byte | (0x80 * more));
    } while (more);
}

/* start of translation unit info */
ST_FUNC void tcc_debug_start(TCCState *s1)
{
    int i;
    char buf[512];
    char *filename;

    /* we might currently #include the <command-line> */
    filename = file->prev ? file->prev->filename : file->filename;

    /* an elf symbol of type STT_FILE must be put so that STB_LOCAL
       symbols can be safely used */
    put_elf_sym(symtab_section, 0, 0,
                ELFW(ST_INFO)(STB_LOCAL, STT_FILE), 0,
                SHN_ABS, filename);

    if (s1->do_debug) {

        new_file = last_line_num = 0;
        debug_next_type = N_DEFAULT_DEBUG;
        debug_hash = NULL;
        debug_anon_hash = NULL;
        n_debug_hash = 0;
        n_debug_anon_hash = 0;

        getcwd(buf, sizeof(buf));
#ifdef _WIN32
        normalize_slashes(buf);
#endif

        if (s1->dwarf) {
            int start_abbrev;
            unsigned char *ptr;
	    char *undo;

            /* dwarf_abbrev */
            start_abbrev = dwarf_abbrev_section->data_offset;
            ptr = section_ptr_add(dwarf_abbrev_section, sizeof(dwarf_abbrev_init));
            memcpy(ptr, dwarf_abbrev_init, sizeof(dwarf_abbrev_init));

            if (s1->dwarf < 5) {
    	        while (*ptr) {
    	            ptr += 3;
    	            while (*ptr) {
    	                if (ptr[1] == DW_FORM_line_strp)
    		            ptr[1] = DW_FORM_strp;
		        if (s1->dwarf < 4) {
			    /* These are compatable for DW_TAG_compile_unit
			       DW_AT_stmt_list. */
			    if  (ptr[1] == DW_FORM_sec_offset)
			         ptr[1] = DW_FORM_data4;
			    /* This code uses only size < 0x80 so these are
			       compatible. */
			    if  (ptr[1] == DW_FORM_exprloc)
			         ptr[1] = DW_FORM_block1;
			}
    	                ptr += 2;
    	            }
		    ptr += 2;
    	        }
            }

            dwarf_sym.info = dwarf_get_section_sym(dwarf_info_section);
            dwarf_sym.abbrev = dwarf_get_section_sym(dwarf_abbrev_section);
            dwarf_sym.line = dwarf_get_section_sym(dwarf_line_section);
            dwarf_sym.str = dwarf_get_section_sym(dwarf_str_section);
            if (tcc_state->dwarf >= 5)
    	        dwarf_sym.line_str = dwarf_get_section_sym(dwarf_line_str_section);
            else {
    	        dwarf_line_str_section = dwarf_str_section;
                dwarf_sym.line_str = dwarf_sym.str;
            }
            section_sym = dwarf_get_section_sym(text_section);

            /* dwarf_info */
            dwarf_info.start = dwarf_info_section->data_offset;
            dwarf_data4(dwarf_info_section, 0); // size
            dwarf_data2(dwarf_info_section, s1->dwarf); // version
            if (s1->dwarf >= 5) {
                dwarf_data1(dwarf_info_section, DW_UT_compile); // unit type
                dwarf_data1(dwarf_info_section, PTR_SIZE);
                dwarf_reloc(dwarf_info_section, dwarf_sym.abbrev, R_DATA_32DW);
                dwarf_data4(dwarf_info_section, start_abbrev);
            }
            else {
                dwarf_reloc(dwarf_info_section, dwarf_sym.abbrev, R_DATA_32DW);
                dwarf_data4(dwarf_info_section, start_abbrev);
                dwarf_data1(dwarf_info_section, PTR_SIZE);
            }

            dwarf_data1(dwarf_info_section, DWARF_ABBREV_COMPILE_UNIT);
            dwarf_strp(dwarf_info_section, "tcc " TCC_VERSION);
            dwarf_data1(dwarf_info_section, DW_LANG_C11);
            dwarf_line_strp(dwarf_info_section, filename);
            dwarf_line_strp(dwarf_info_section, buf);
            dwarf_reloc(dwarf_info_section, section_sym, R_DATA_PTR);
#if PTR_SIZE == 4
            dwarf_data4(dwarf_info_section, ind); // low pc
            dwarf_data4(dwarf_info_section, 0); // high pc
#else
            dwarf_data8(dwarf_info_section, ind); // low pc
            dwarf_data8(dwarf_info_section, 0); // high pc
#endif
            dwarf_reloc(dwarf_info_section, dwarf_sym.line, R_DATA_32DW);
            dwarf_data4(dwarf_info_section, dwarf_line_section->data_offset); // stmt_list

            /* dwarf_line */
            dwarf_line.start = dwarf_line_section->data_offset;
            dwarf_data4(dwarf_line_section, 0); // length
            dwarf_data2(dwarf_line_section, s1->dwarf); // version
            if (s1->dwarf >= 5) {
                dwarf_data1(dwarf_line_section, PTR_SIZE); // address size
                dwarf_data1(dwarf_line_section, 0); // segment selector
            }
            dwarf_data4(dwarf_line_section, 0); // prologue Length
            dwarf_data1(dwarf_line_section, DWARF_MIN_INSTR_LEN);
            if (s1->dwarf >= 4)
                dwarf_data1(dwarf_line_section, 1); // maximum ops per instruction
            dwarf_data1(dwarf_line_section, 1); // Initial value of 'is_stmt'
            dwarf_data1(dwarf_line_section, DWARF_LINE_BASE);
            dwarf_data1(dwarf_line_section, DWARF_LINE_RANGE);
            dwarf_data1(dwarf_line_section, DWARF_OPCODE_BASE);
            ptr = section_ptr_add(dwarf_line_section, sizeof(dwarf_line_opcodes));
            memcpy(ptr, dwarf_line_opcodes, sizeof(dwarf_line_opcodes));
	    undo = strrchr(filename, '/');
	    if (undo)
		*undo = 0;
            dwarf_line.dir_size = 1 + (undo != NULL);
            dwarf_line.dir_table = (char **) tcc_malloc(sizeof (char *) *
							dwarf_line.dir_size);
            dwarf_line.dir_table[0] = tcc_strdup(buf);
	    if (undo)
                dwarf_line.dir_table[1] = tcc_strdup(filename);
            dwarf_line.filename_size = 2;
            dwarf_line.filename_table =
    	        (struct dwarf_filename_struct *)
    	        tcc_malloc(2*sizeof (struct dwarf_filename_struct));
            dwarf_line.filename_table[0].dir_entry = 0;
	    if (undo) {
                dwarf_line.filename_table[0].name = tcc_strdup(undo + 1);
                dwarf_line.filename_table[1].dir_entry = 1;
                dwarf_line.filename_table[1].name = tcc_strdup(undo + 1);
		*undo = '/';
	    }
	    else {
                dwarf_line.filename_table[0].name = tcc_strdup(filename);
                dwarf_line.filename_table[1].dir_entry = 0;
                dwarf_line.filename_table[1].name = tcc_strdup(filename);
	    }
            dwarf_line.line_size = dwarf_line.line_max_size = 0;
            dwarf_line.line_data = NULL;
            dwarf_line.cur_file = 1;
            dwarf_line.last_file = 0;
            dwarf_line.last_pc = 0;
            dwarf_line.last_line = 1;
            dwarf_line_op(s1, 0); // extended
            dwarf_uleb128_op(s1, 1 + PTR_SIZE); // extended size
            dwarf_line_op(s1, DW_LNE_set_address);
            for (i = 0; i < PTR_SIZE; i++)
    	        dwarf_line_op(s1, 0);
            memset(&dwarf_info.base_type_used, 0, sizeof(dwarf_info.base_type_used));
        }
        else
        {
            /* file info: full path + filename */
            pstrcat(buf, sizeof(buf), "/");
            section_sym = put_elf_sym(symtab_section, 0, 0,
                                      ELFW(ST_INFO)(STB_LOCAL, STT_SECTION), 0,
                                      text_section->sh_num, NULL);
            put_stabs_r(s1, buf, N_SO, 0, 0,
                        text_section->data_offset, text_section, section_sym);
            put_stabs_r(s1, filename, N_SO, 0, 0,
                        text_section->data_offset, text_section, section_sym);
            for (i = 0; i < N_DEFAULT_DEBUG; i++)
                put_stabs(s1, default_debug[i].name, N_LSYM, 0, 0, 0);
        }
        /* we're currently 'including' the <command line> */
        tcc_debug_bincl(s1);
    }
}

/* put end of translation unit info */
ST_FUNC void tcc_debug_end(TCCState *s1)
{
    if (!s1->do_debug || debug_next_type == 0)
        return;

    if (debug_info_root)
        tcc_debug_funcend(s1, 0); /* free stuff in case of errors */

    if (s1->dwarf) {
	int i, j;
	int start_aranges;
	unsigned char *ptr;
	int text_size = text_section->data_offset;

	/* dwarf_info */
	for (i = 0; i < n_debug_anon_hash; i++) {
	    Sym *t = debug_anon_hash[i].type;
	    int pos = dwarf_info_section->data_offset;

	    dwarf_data1(dwarf_info_section,
                        IS_UNION (t->type.t) ? DWARF_ABBREV_UNION_EMPTY_TYPE
                                             : DWARF_ABBREV_STRUCTURE_EMPTY_TYPE);
            dwarf_strp(dwarf_info_section,
                       (t->v & ~SYM_STRUCT) >= SYM_FIRST_ANOM
                       ? "" : get_tok_str(t->v, NULL));
            dwarf_uleb128(dwarf_info_section, 0);
            dwarf_uleb128(dwarf_info_section, dwarf_line.cur_file);
            dwarf_uleb128(dwarf_info_section, file->line_num);
	    for (j = 0; j < debug_anon_hash[i].n_debug_type; j++)
		write32le(dwarf_info_section->data +
			  debug_anon_hash[i].debug_type[j],
			  pos - dwarf_info.start);
	    tcc_free (debug_anon_hash[i].debug_type);
	}
	tcc_free (debug_anon_hash);
	dwarf_data1(dwarf_info_section, 0);
	ptr = dwarf_info_section->data + dwarf_info.start;
	write32le(ptr, dwarf_info_section->data_offset - dwarf_info.start - 4);
	write32le(ptr + 25 + (s1->dwarf >= 5) + PTR_SIZE, text_size);

	/* dwarf_aranges */
	start_aranges = dwarf_aranges_section->data_offset;
	dwarf_data4(dwarf_aranges_section, 0); // size
	dwarf_data2(dwarf_aranges_section, 2); // version
	dwarf_reloc(dwarf_aranges_section, dwarf_sym.info, R_DATA_32DW);
	dwarf_data4(dwarf_aranges_section, 0); // dwarf_info
#if PTR_SIZE == 4
	dwarf_data1(dwarf_aranges_section, 4); // address size
#else
	dwarf_data1(dwarf_aranges_section, 8); // address size
#endif
	dwarf_data1(dwarf_aranges_section, 0); // segment selector size
	dwarf_data4(dwarf_aranges_section, 0); // padding
	dwarf_reloc(dwarf_aranges_section, section_sym, R_DATA_PTR);
#if PTR_SIZE == 4
	dwarf_data4(dwarf_aranges_section, 0); // Begin
	dwarf_data4(dwarf_aranges_section, text_size); // End
	dwarf_data4(dwarf_aranges_section, 0); // End list
	dwarf_data4(dwarf_aranges_section, 0); // End list
#else
	dwarf_data8(dwarf_aranges_section, 0); // Begin
	dwarf_data8(dwarf_aranges_section, text_size); // End
	dwarf_data8(dwarf_aranges_section, 0); // End list
	dwarf_data8(dwarf_aranges_section, 0); // End list
#endif
	ptr = dwarf_aranges_section->data + start_aranges;
	write32le(ptr, dwarf_aranges_section->data_offset - start_aranges - 4);

	/* dwarf_line */
	if (s1->dwarf >= 5) {
	    dwarf_data1(dwarf_line_section, 1); /* col */
	    dwarf_uleb128(dwarf_line_section, DW_LNCT_path);
	    dwarf_uleb128(dwarf_line_section, DW_FORM_line_strp);
	    dwarf_uleb128(dwarf_line_section, dwarf_line.dir_size);
	    for (i = 0; i < dwarf_line.dir_size; i++)
	        dwarf_line_strp(dwarf_line_section, dwarf_line.dir_table[i]);
	    dwarf_data1(dwarf_line_section, 2); /* col */
	    dwarf_uleb128(dwarf_line_section, DW_LNCT_path);
	    dwarf_uleb128(dwarf_line_section, DW_FORM_line_strp);
	    dwarf_uleb128(dwarf_line_section, DW_LNCT_directory_index);
	    dwarf_uleb128(dwarf_line_section, DW_FORM_udata);
	    dwarf_uleb128(dwarf_line_section, dwarf_line.filename_size);
	    for (i = 0; i < dwarf_line.filename_size; i++) {
	        dwarf_line_strp(dwarf_line_section,
				dwarf_line.filename_table[i].name);
	        dwarf_uleb128(dwarf_line_section,
			      dwarf_line.filename_table[i].dir_entry);
	    }
	}
	else {
	    int len;

	    for (i = 0; i < dwarf_line.dir_size; i++) {
	        len = strlen(dwarf_line.dir_table[i]) + 1;
	        ptr = section_ptr_add(dwarf_line_section, len);
	        memmove(ptr, dwarf_line.dir_table[i], len);
	    }
	    dwarf_data1(dwarf_line_section, 0); /* end dir */
	    for (i = 0; i < dwarf_line.filename_size; i++) {
	        len = strlen(dwarf_line.filename_table[i].name) + 1;
	        ptr = section_ptr_add(dwarf_line_section, len);
	        memmove(ptr, dwarf_line.filename_table[i].name, len);
	        dwarf_uleb128(dwarf_line_section,
			      dwarf_line.filename_table[i].dir_entry);
	        dwarf_uleb128(dwarf_line_section, 0); /* time */
	        dwarf_uleb128(dwarf_line_section, 0); /* size */
	    }
	    dwarf_data1(dwarf_line_section, 0); /* end file */
	}
	for (i = 0; i < dwarf_line.dir_size; i++)
	    tcc_free(dwarf_line.dir_table[i]);
	tcc_free(dwarf_line.dir_table);
	for (i = 0; i < dwarf_line.filename_size; i++)
	    tcc_free(dwarf_line.filename_table[i].name);
	tcc_free(dwarf_line.filename_table);

	dwarf_line_op(s1, 0); // extended
	dwarf_uleb128_op(s1, 1); // extended size
	dwarf_line_op(s1, DW_LNE_end_sequence);
	i = (s1->dwarf >= 5) * 2;
	write32le(&dwarf_line_section->data[dwarf_line.start + 6 + i],
		  dwarf_line_section->data_offset - dwarf_line.start - (10 + i));
	section_ptr_add(dwarf_line_section, 3);
	dwarf_reloc(dwarf_line_section, section_sym, R_DATA_PTR);
	ptr = section_ptr_add(dwarf_line_section, dwarf_line.line_size - 3);
	memmove(ptr - 3, dwarf_line.line_data, dwarf_line.line_size);
	tcc_free(dwarf_line.line_data);
	write32le(dwarf_line_section->data + dwarf_line.start,
		  dwarf_line_section->data_offset - dwarf_line.start - 4);
    }
    else
    {
        put_stabs_r(s1, NULL, N_SO, 0, 0,
                    text_section->data_offset, text_section, section_sym);
    }
    tcc_free(debug_hash);
    debug_next_type = 0;
}

static BufferedFile* put_new_file(TCCState *s1)
{
    BufferedFile *f = file;
    /* use upper file if from inline ":asm:" */
    if (f->filename[0] == ':')
        f = f->prev;
    if (f && new_file) {
        new_file = last_line_num = 0;
        if (s1->dwarf)
            dwarf_file(s1);
        else
            put_stabs_r(s1, f->filename, N_SOL, 0, 0, ind, text_section, section_sym);
    }
    return f;
}

/* put alternative filename */
ST_FUNC void tcc_debug_putfile(TCCState *s1, const char *filename)
{
    if (0 == strcmp(file->filename, filename))
        return;
    pstrcpy(file->filename, sizeof(file->filename), filename);
    if (!s1->do_debug)
        return;
    if (s1->dwarf)
        dwarf_file(s1);
    new_file = 1;
}

/* begin of #include */
ST_FUNC void tcc_debug_bincl(TCCState *s1)
{
    if (!s1->do_debug)
        return;
    if (s1->dwarf)
        dwarf_file(s1);
    else
        put_stabs(s1, file->filename, N_BINCL, 0, 0, 0);
    new_file = 1;
}

/* end of #include */
ST_FUNC void tcc_debug_eincl(TCCState *s1)
{
    if (!s1->do_debug)
        return;
    if (s1->dwarf)
        dwarf_file(s1);
    else
        put_stabn(s1, N_EINCL, 0, 0, 0);
    new_file = 1;
}

/* generate line number info */
ST_FUNC void tcc_debug_line(TCCState *s1)
{
    BufferedFile *f;

    if (!s1->do_debug)
        return;
    if (cur_text_section != text_section || nocode_wanted)
        return;
    f = put_new_file(s1);
    if (!f)
        return;
    if (last_line_num == f->line_num)
        return;
    last_line_num = f->line_num;

    if (s1->dwarf) {
	int len_pc = (ind - dwarf_line.last_pc) / DWARF_MIN_INSTR_LEN;
	int len_line = f->line_num - dwarf_line.last_line;
	int n = len_pc * DWARF_LINE_RANGE + len_line + DWARF_OPCODE_BASE - DWARF_LINE_BASE;

	if (dwarf_line.cur_file != dwarf_line.last_file) {
	    dwarf_line.last_file = dwarf_line.cur_file;
	    dwarf_line_op(s1, DW_LNS_set_file);
	    dwarf_uleb128_op(s1, dwarf_line.cur_file);
	}
	if (len_pc &&
	    len_line >= DWARF_LINE_BASE && len_line <= (DWARF_OPCODE_BASE + DWARF_LINE_BASE) &&
	    n >= DWARF_OPCODE_BASE && n <= 255)
            dwarf_line_op(s1, n);
	else {
	    if (len_pc) {
	        n = len_pc * DWARF_LINE_RANGE + 0 + DWARF_OPCODE_BASE - DWARF_LINE_BASE;
	        if (n >= DWARF_OPCODE_BASE && n <= 255)
                    dwarf_line_op(s1, n);
		else {
	            dwarf_line_op(s1, DW_LNS_advance_pc);
		    dwarf_uleb128_op(s1, len_pc);
		}
	    }
	    if (len_line) {
	        n = 0 * DWARF_LINE_RANGE + len_line + DWARF_OPCODE_BASE - DWARF_LINE_BASE;
	        if (len_line >= DWARF_LINE_BASE && len_line <= (DWARF_OPCODE_BASE + DWARF_LINE_BASE) &&
		    n >= DWARF_OPCODE_BASE && n <= 255)
	            dwarf_line_op(s1, n);
		else {
	            dwarf_line_op(s1, DW_LNS_advance_line);
		    dwarf_sleb128_op(s1, len_line);
		}
	    }
	}
	dwarf_line.last_pc = ind;
	dwarf_line.last_line = f->line_num;
    }
    else
    {
	if (func_ind != -1) {
            put_stabn(s1, N_SLINE, 0, f->line_num, ind - func_ind);
        } else {
            /* from tcc_assemble */
            put_stabs_r(s1, NULL, N_SLINE, 0, f->line_num, ind, text_section, section_sym);
        }
    }
}

static void tcc_debug_stabs (TCCState *s1, const char *str, int type, unsigned long value,
                             Section *sec, int sym_index, int info)
{
    struct debug_sym *s;

    if (debug_info) {
        debug_info->sym =
            (struct debug_sym *)tcc_realloc (debug_info->sym,
                                             sizeof(struct debug_sym) *
                                             (debug_info->n_sym + 1));
        s = debug_info->sym + debug_info->n_sym++;
        s->type = type;
        s->value = value;
        s->str = tcc_strdup(str);
        s->sec = sec;
        s->sym_index = sym_index;
        s->info = info;
        s->file = dwarf_line.cur_file;
        s->line = file->line_num;
    }
    else if (sec)
        put_stabs_r (s1, str, type, 0, 0, value, sec, sym_index);
    else
        put_stabs (s1, str, type, 0, 0, value);
}

ST_FUNC void tcc_debug_stabn(TCCState *s1, int type, int value)
{
    if (!s1->do_debug)
        return;
    if (type == N_LBRAC) {
        struct _debug_info *info =
            (struct _debug_info *) tcc_mallocz(sizeof (*info));

        info->start = value;
        info->parent = debug_info;
        if (debug_info) {
            if (debug_info->child) {
                if (debug_info->child->last)
                    debug_info->child->last->next = info;
                else
                    debug_info->child->next = info;
                debug_info->child->last = info;
            }
            else
                debug_info->child = info;
        }
        else
            debug_info_root = info;
        debug_info = info;
    }
    else {
        debug_info->end = value;
        debug_info = debug_info->parent;
    }
}

static int tcc_debug_find(TCCState *s1, Sym *t, int dwarf)
{
    int i;

    if (!debug_info && dwarf &&
	(t->type.t & VT_BTYPE) == VT_STRUCT && t->c == -1) {
	for (i = 0; i < n_debug_anon_hash; i++)
            if (t == debug_anon_hash[i].type)
		return 0;
	debug_anon_hash = (struct _debug_anon_hash *)
            tcc_realloc (debug_anon_hash,
                         (n_debug_anon_hash + 1) * sizeof(*debug_anon_hash));
        debug_anon_hash[n_debug_anon_hash].n_debug_type = 0;
        debug_anon_hash[n_debug_anon_hash].debug_type = NULL;
        debug_anon_hash[n_debug_anon_hash++].type = t;
	return 0;
    }
    for (i = 0; i < n_debug_hash; i++)
        if (t == debug_hash[i].type)
	    return debug_hash[i].debug_type;
    return -1;
}

static int tcc_get_dwarf_info(TCCState *s1, Sym *s);

static void tcc_debug_check_anon(TCCState *s1, Sym *t, int debug_type)
{
    int i;

    if (!debug_info && (t->type.t & VT_BTYPE) == VT_STRUCT && t->type.ref->c == -1)
	for (i = 0; i < n_debug_anon_hash; i++)
            if (t->type.ref == debug_anon_hash[i].type) {
		debug_anon_hash[i].debug_type =
		    tcc_realloc(debug_anon_hash[i].debug_type,
				(debug_anon_hash[i].n_debug_type + 1) * sizeof(int));
		debug_anon_hash[i].debug_type[debug_anon_hash[i].n_debug_type++] =
		    debug_type;
            }
}

ST_FUNC void tcc_debug_fix_anon(TCCState *s1, CType *t)
{
    int i, j, debug_type;

    if (!(s1->do_debug & 2) || !s1->dwarf || debug_info)
	return;

    if ((t->t & VT_BTYPE) == VT_STRUCT && t->ref->c != -1)
	for (i = 0; i < n_debug_anon_hash; i++)
	    if (t->ref == debug_anon_hash[i].type) {
		Sym sym = {0}; sym .type = *t ;

		/* Trick to not hash this struct */
		debug_info = (struct _debug_info *) t;
		debug_type = tcc_get_dwarf_info(s1, &sym);
		debug_info = NULL;
		for (j = 0; j < debug_anon_hash[i].n_debug_type; j++)
		    write32le(dwarf_info_section->data +
			      debug_anon_hash[i].debug_type[j],
			      debug_type - dwarf_info.start);
		tcc_free(debug_anon_hash[i].debug_type);
		n_debug_anon_hash--;
		for (; i < n_debug_anon_hash; i++)
		    debug_anon_hash[i] = debug_anon_hash[i + 1];
	    }
}

static int tcc_debug_add(TCCState *s1, Sym *t, int dwarf)
{
    int offset = dwarf ? dwarf_info_section->data_offset : ++debug_next_type;
    debug_hash = (struct _debug_hash *)
	tcc_realloc (debug_hash,
		     (n_debug_hash + 1) * sizeof(*debug_hash));
    debug_hash[n_debug_hash].debug_type = offset;
    debug_hash[n_debug_hash++].type = t;
    return offset;
}

static void tcc_debug_remove(TCCState *s1, Sym *t)
{
    int i;

    for (i = 0; i < n_debug_hash; i++)
        if (t == debug_hash[i].type) {
	    n_debug_hash--;
	    for (; i < n_debug_hash; i++)
		debug_hash[i] = debug_hash[i+1];
	}
}

#define	STRUCT_NODEBUG(s) 			       \
    (s->a.nodebug ||                           \
     ((s->v & ~SYM_FIELD) >= SYM_FIRST_ANOM && \
      ((s->type.t & VT_BTYPE) == VT_BYTE ||    \
       (s->type.t & VT_BTYPE) == VT_BOOL ||    \
       (s->type.t & VT_BTYPE) == VT_SHORT ||   \
       (s->type.t & VT_BTYPE) == VT_INT ||     \
       (s->type.t & VT_BTYPE) == VT_LLONG)))

static void tcc_get_debug_info(TCCState *s1, Sym *s, CString *result)
{
    int type;
    int n = 0;
    int debug_type = -1;
    Sym *t = s;
    CString str;

    for (;;) {
        type = t->type.t & ~(VT_STORAGE | VT_CONSTANT | VT_VOLATILE | VT_VLA);
        if ((type & VT_BTYPE) != VT_BYTE)
            type &= ~VT_DEFSIGN;
        if (type == VT_PTR || type == (VT_PTR | VT_ARRAY))
            n++, t = t->type.ref;
        else
            break;
    }
    if ((type & VT_BTYPE) == VT_STRUCT) {
	Sym *e = t;

        t = t->type.ref;
	debug_type = tcc_debug_find(s1, t, 0);
        if (debug_type == -1) {
            debug_type = tcc_debug_add(s1, t, 0);
            cstr_new (&str);
            cstr_printf (&str, "%s:T%d=%c%d",
                         (t->v & ~SYM_STRUCT) >= SYM_FIRST_ANOM
                         ? "" : get_tok_str(t->v, NULL),
                         debug_type,
                         IS_UNION (t->type.t) ? 'u' : 's',
                         t->c);
            while (t->next) {
                int pos, size, align;

                t = t->next;
		if (STRUCT_NODEBUG(t))
		    continue;
                cstr_printf (&str, "%s:",
                             get_tok_str(t->v, NULL));
                tcc_get_debug_info (s1, t, &str);
                if (t->type.t & VT_BITFIELD) {
                    pos = t->c * 8 + BIT_POS(t->type.t);
                    size = BIT_SIZE(t->type.t);
                }
                else {
                    pos = t->c * 8;
                    size = type_size(&t->type, &align) * 8;
                }
                cstr_printf (&str, ",%d,%d;", pos, size);
            }
            cstr_printf (&str, ";");
            tcc_debug_stabs(s1, str.data, N_LSYM, 0, NULL, 0, 0);
            cstr_free (&str);
            if (debug_info)
                tcc_debug_remove(s1, e);
        }
    }
    else if (IS_ENUM(type)) {
        Sym *e = t = t->type.ref;

	debug_type = tcc_debug_find(s1, t, 0);
	if (debug_type == -1) {
	    debug_type = tcc_debug_add(s1, t, 0);
            cstr_new (&str);
            cstr_printf (&str, "%s:T%d=e",
                         (t->v & ~SYM_STRUCT) >= SYM_FIRST_ANOM
                         ? "" : get_tok_str(t->v, NULL),
                         debug_type);
            while (t->next) {
                t = t->next;
                cstr_printf (&str, "%s:",
                             (t->v & ~SYM_FIELD) >= SYM_FIRST_ANOM
                             ? "" : get_tok_str(t->v, NULL));
                cstr_printf (&str, e->type.t & VT_UNSIGNED ? "%u," : "%d,",
                             (int)t->enum_val);
            }
            cstr_printf (&str, ";");
            tcc_debug_stabs(s1, str.data, N_LSYM, 0, NULL, 0, 0);
            cstr_free (&str);
            if (debug_info)
                tcc_debug_remove(s1, e);
	}
    }
    else if ((type & VT_BTYPE) != VT_FUNC) {
        type &= ~VT_STRUCT_MASK;
        for (debug_type = 1; debug_type <= N_DEFAULT_DEBUG; debug_type++)
            if (default_debug[debug_type - 1].type == type)
                break;
        if (debug_type > N_DEFAULT_DEBUG)
            return;
    }
    if (n > 0)
        cstr_printf (result, "%d=", ++debug_next_type);
    t = s;
    for (;;) {
        type = t->type.t & ~(VT_STORAGE | VT_CONSTANT | VT_VOLATILE | VT_VLA);
        if ((type & VT_BTYPE) != VT_BYTE)
            type &= ~VT_DEFSIGN;
        if (type == VT_PTR)
            cstr_printf (result, "%d=*", ++debug_next_type);
        else if (type == (VT_PTR | VT_ARRAY))
            cstr_printf (result, "%d=ar1;0;%d;",
                         ++debug_next_type, t->type.ref->c - 1);
        else if (type == VT_FUNC) {
            cstr_printf (result, "%d=f", ++debug_next_type);
            tcc_get_debug_info (s1, t->type.ref, result);
            return;
        }
        else
            break;
        t = t->type.ref;
    }
    cstr_printf (result, "%d", debug_type);
}

static int tcc_get_dwarf_info(TCCState *s1, Sym *s)
{
    int type;
    int debug_type = -1;
    Sym *e, *t = s;
    int i;
    int last_pos = -1;
    int retval;

    if (new_file)
        put_new_file(s1);
    for (;;) {
        type = t->type.t & ~(VT_STORAGE | VT_CONSTANT | VT_VOLATILE | VT_VLA);
        if ((type & VT_BTYPE) != VT_BYTE)
            type &= ~VT_DEFSIGN;
        if (type == VT_PTR || type == (VT_PTR | VT_ARRAY))
            t = t->type.ref;
        else
            break;
    }
    if ((type & VT_BTYPE) == VT_STRUCT) {
        t = t->type.ref;
	debug_type = tcc_debug_find(s1, t, 1);
	if (debug_type == -1) {
	    int pos_sib = 0, i, *pos_type;

	    debug_type = tcc_debug_add(s1, t, 1);
	    e = t;
	    i = 0;
	    while (e->next) {
		e = e->next;
		if (STRUCT_NODEBUG(e))
		    continue;
		i++;
	    }
	    pos_type = (int *) tcc_malloc(i * sizeof(int));
	    dwarf_data1(dwarf_info_section,
			IS_UNION (t->type.t)
		        ? t->next ? DWARF_ABBREV_UNION_TYPE
				  : DWARF_ABBREV_UNION_EMPTY_TYPE
		        : t->next ? DWARF_ABBREV_STRUCTURE_TYPE
				  : DWARF_ABBREV_STRUCTURE_EMPTY_TYPE);
	    dwarf_strp(dwarf_info_section,
                       (t->v & ~SYM_STRUCT) >= SYM_FIRST_ANOM
                       ? "" : get_tok_str(t->v, NULL));
	    dwarf_uleb128(dwarf_info_section, t->c);
	    dwarf_uleb128(dwarf_info_section, dwarf_line.cur_file);
	    dwarf_uleb128(dwarf_info_section, file->line_num);
	    if (t->next) {
	        pos_sib = dwarf_info_section->data_offset;
	        dwarf_data4(dwarf_info_section, 0);
	    }
	    e = t;
	    i = 0;
            while (e->next) {
                e = e->next;
		if (STRUCT_NODEBUG(e))
		    continue;
	        dwarf_data1(dwarf_info_section,
			    e->type.t & VT_BITFIELD ? DWARF_ABBREV_MEMBER_BF
						    : DWARF_ABBREV_MEMBER);
		dwarf_strp(dwarf_info_section,
			   get_tok_str(e->v, NULL));
		dwarf_uleb128(dwarf_info_section, dwarf_line.cur_file);
		dwarf_uleb128(dwarf_info_section, file->line_num);
		pos_type[i++] = dwarf_info_section->data_offset;
		dwarf_data4(dwarf_info_section, 0);
                if (e->type.t & VT_BITFIELD) {
                    int pos = e->c * 8 + BIT_POS(e->type.t);
                    int size = BIT_SIZE(e->type.t);

		    dwarf_uleb128(dwarf_info_section, size);
		    dwarf_uleb128(dwarf_info_section, pos);
		}
		else
		    dwarf_uleb128(dwarf_info_section, e->c);
	    }
	    if (t->next) {
	        dwarf_data1(dwarf_info_section, 0);
	        write32le(dwarf_info_section->data + pos_sib,
		          dwarf_info_section->data_offset - dwarf_info.start);
	    }
	    e = t;
	    i = 0;
	    while (e->next) {
		e = e->next;
		if (STRUCT_NODEBUG(e))
		    continue;
		type = tcc_get_dwarf_info(s1, e);
		tcc_debug_check_anon(s1, e, pos_type[i]);
		write32le(dwarf_info_section->data + pos_type[i++],
			  type - dwarf_info.start);
	    }
	    tcc_free(pos_type);
	    if (debug_info)
		tcc_debug_remove(s1, t);
        }
    }
    else if (IS_ENUM(type)) {
        t = t->type.ref;
	debug_type = tcc_debug_find(s1, t, 1);
	if (debug_type == -1) {
	    int pos_sib, pos_type;
	    Sym sym = {0}; sym.type.t = VT_INT | (type & VT_UNSIGNED);

	    pos_type = tcc_get_dwarf_info(s1, &sym);
	    debug_type = tcc_debug_add(s1, t, 1);
	    dwarf_data1(dwarf_info_section, DWARF_ABBREV_ENUMERATION_TYPE);
	    dwarf_strp(dwarf_info_section,
                       (t->v & ~SYM_STRUCT) >= SYM_FIRST_ANOM
                       ? "" : get_tok_str(t->v, NULL));
	    dwarf_data1(dwarf_info_section,
		        type & VT_UNSIGNED ? DW_ATE_unsigned : DW_ATE_signed );
	    dwarf_data1(dwarf_info_section, 4);
	    dwarf_data4(dwarf_info_section, pos_type - dwarf_info.start);
	    dwarf_uleb128(dwarf_info_section, dwarf_line.cur_file);
	    dwarf_uleb128(dwarf_info_section, file->line_num);
	    pos_sib = dwarf_info_section->data_offset;
	    dwarf_data4(dwarf_info_section, 0);
	    e = t;
            while (e->next) {
                e = e->next;
	        dwarf_data1(dwarf_info_section,
			type & VT_UNSIGNED ? DWARF_ABBREV_ENUMERATOR_UNSIGNED
					   : DWARF_ABBREV_ENUMERATOR_SIGNED);
	        dwarf_strp(dwarf_info_section,
                           (e->v & ~SYM_FIELD) >= SYM_FIRST_ANOM
                           ? "" : get_tok_str(e->v, NULL));
		if (type & VT_UNSIGNED)
	            dwarf_uleb128(dwarf_info_section, e->enum_val);
		else
	            dwarf_sleb128(dwarf_info_section, e->enum_val);
            }
	    dwarf_data1(dwarf_info_section, 0);
	    write32le(dwarf_info_section->data + pos_sib,
		      dwarf_info_section->data_offset - dwarf_info.start);
	    if (debug_info)
		tcc_debug_remove(s1, t);
	}
    }
    else if ((type & VT_BTYPE) != VT_FUNC) {
        type &= ~VT_STRUCT_MASK;
        for (i = 1; i <= N_DEFAULT_DEBUG; i++)
            if (default_debug[i - 1].type == type)
                break;
        if (i > N_DEFAULT_DEBUG)
            return 0;
	debug_type = dwarf_info.base_type_used[i - 1];
	if (debug_type == 0) {
	    char name[100];

	    debug_type = dwarf_info_section->data_offset;
	    dwarf_data1(dwarf_info_section, DWARF_ABBREV_BASE_TYPE);
	    dwarf_uleb128(dwarf_info_section, default_debug[i - 1].size);
	    dwarf_data1(dwarf_info_section, default_debug[i - 1].encoding);
	    strncpy(name, default_debug[i - 1].name, sizeof(name) -1);
	    *strchr(name, ':') = 0;
	    dwarf_strp(dwarf_info_section, name);
	    dwarf_info.base_type_used[i - 1] = debug_type;
	}
    }
    retval = debug_type;
    e = NULL;
    t = s;
    for (;;) {
        type = t->type.t & ~(VT_STORAGE | VT_CONSTANT | VT_VOLATILE | VT_VLA);
        if ((type & VT_BTYPE) != VT_BYTE)
            type &= ~VT_DEFSIGN;
        if (type == VT_PTR) {
	    i = dwarf_info_section->data_offset;
	    if (retval == debug_type)
		retval = i;
	    dwarf_data1(dwarf_info_section, DWARF_ABBREV_POINTER);
	    dwarf_data1(dwarf_info_section, PTR_SIZE);
	    if (last_pos != -1) {
		tcc_debug_check_anon(s1, e, last_pos);
		write32le(dwarf_info_section->data + last_pos,
			  i - dwarf_info.start);
	    }
	    last_pos = dwarf_info_section->data_offset;
	    e = t->type.ref;
	    dwarf_data4(dwarf_info_section, 0);
	}
        else if (type == (VT_PTR | VT_ARRAY)) {
	    int sib_pos, sub_type;
#if LONG_SIZE == 4
	    Sym sym = {0}; sym.type.t = VT_LONG | VT_INT | VT_UNSIGNED;
#else
	    Sym sym = {0}; sym.type.t = VT_LLONG | VT_LONG | VT_UNSIGNED;
#endif

	    sub_type = tcc_get_dwarf_info(s1, &sym);
	    i = dwarf_info_section->data_offset;
	    if (retval == debug_type)
		retval = i;
	    dwarf_data1(dwarf_info_section, DWARF_ABBREV_ARRAY_TYPE);
	    if (last_pos != -1) {
		tcc_debug_check_anon(s1, e, last_pos);
		write32le(dwarf_info_section->data + last_pos,
			  i - dwarf_info.start);
	    }
	    last_pos = dwarf_info_section->data_offset;
	    e = t->type.ref;
	    dwarf_data4(dwarf_info_section, 0);
	    sib_pos = dwarf_info_section->data_offset;
	    dwarf_data4(dwarf_info_section, 0);
	    for (;;) {
	        dwarf_data1(dwarf_info_section, DWARF_ABBREV_SUBRANGE_TYPE);
	        dwarf_data4(dwarf_info_section, sub_type - dwarf_info.start);
	        dwarf_uleb128(dwarf_info_section, t->type.ref->c - 1);
		s = t->type.ref;
		type = s->type.t & ~(VT_STORAGE | VT_CONSTANT | VT_VOLATILE);
		if (type != (VT_PTR | VT_ARRAY))
		    break;
		t = s;
	    }
	    dwarf_data1(dwarf_info_section, 0);
	    write32le(dwarf_info_section->data + sib_pos,
		      dwarf_info_section->data_offset - dwarf_info.start);
	}
        else if (type == VT_FUNC) {
	    int sib_pos = 0, *pos_type;
	    Sym *f;

	    i = dwarf_info_section->data_offset;
	    debug_type = tcc_get_dwarf_info(s1, t->type.ref);
	    if (retval == debug_type)
		retval = i;
	    dwarf_data1(dwarf_info_section,
			t->type.ref->next ? DWARF_ABBREV_SUBROUTINE_TYPE
					  : DWARF_ABBREV_SUBROUTINE_EMPTY_TYPE);
	    if (last_pos != -1) {
		tcc_debug_check_anon(s1, e, last_pos);
		write32le(dwarf_info_section->data + last_pos,
			  i - dwarf_info.start);
	    }
	    last_pos = dwarf_info_section->data_offset;
	    e = t->type.ref;
	    dwarf_data4(dwarf_info_section, 0);
	    if (t->type.ref->next) {
	        sib_pos = dwarf_info_section->data_offset;
	        dwarf_data4(dwarf_info_section, 0);
	    }
	    f = t->type.ref;
	    i = 0;
	    while (f->next) {
		f = f->next;
		i++;
	    }
	    pos_type = (int *) tcc_malloc(i * sizeof(int));
	    f = t->type.ref;
	    i = 0;
	    while (f->next) {
		f = f->next;
	        dwarf_data1(dwarf_info_section, DWARF_ABBREV_FORMAL_PARAMETER2);
		pos_type[i++] = dwarf_info_section->data_offset;
	        dwarf_data4(dwarf_info_section, 0);
	    }
	    if (t->type.ref->next) {
	        dwarf_data1(dwarf_info_section, 0);
	        write32le(dwarf_info_section->data + sib_pos,
		          dwarf_info_section->data_offset - dwarf_info.start);
	    }
	    f = t->type.ref;
	    i = 0;
	    while (f->next) {
		f = f->next;
		type = tcc_get_dwarf_info(s1, f);
		tcc_debug_check_anon(s1, f, pos_type[i]);
	        write32le(dwarf_info_section->data + pos_type[i++],
                          type - dwarf_info.start);
	    }
	    tcc_free(pos_type);
        }
        else {
	    if (last_pos != -1) {
		tcc_debug_check_anon(s1, e, last_pos);
		write32le(dwarf_info_section->data + last_pos,
			  debug_type - dwarf_info.start);
	    }
            break;
	}
        t = t->type.ref;
    }
    return retval;
}

static void tcc_debug_finish (TCCState *s1, struct _debug_info *cur)
{
    while (cur) {
        struct _debug_info *next = cur->next;
        int i;

        if (s1->dwarf) {

            for (i = cur->n_sym - 1; i >= 0; i--) {
                struct debug_sym *s = &cur->sym[i];

		dwarf_data1(dwarf_info_section,
                            s->type == N_PSYM
			    ? DWARF_ABBREV_FORMAL_PARAMETER
			    : s->type == N_GSYM
                            ? DWARF_ABBREV_VARIABLE_EXTERNAL
                            : s->type == N_STSYM
			    ? DWARF_ABBREV_VARIABLE_STATIC
			    : DWARF_ABBREV_VARIABLE_LOCAL);
                dwarf_strp(dwarf_info_section, s->str);
		if (s->type == N_GSYM || s->type == N_STSYM) {
                    dwarf_uleb128(dwarf_info_section, s->file);
                    dwarf_uleb128(dwarf_info_section, s->line);
		}
                dwarf_data4(dwarf_info_section, s->info - dwarf_info.start);
		if (s->type == N_GSYM || s->type == N_STSYM) {
		    /* global/static */
		    if (s->type == N_GSYM)
                        dwarf_data1(dwarf_info_section, 1);
                    dwarf_data1(dwarf_info_section, PTR_SIZE + 1);
                    dwarf_data1(dwarf_info_section, DW_OP_addr);
		    if (s->type == N_STSYM)
		        dwarf_reloc(dwarf_info_section, section_sym, R_DATA_PTR);
#if PTR_SIZE == 4
                    dwarf_data4(dwarf_info_section, s->value);
#else
                    dwarf_data8(dwarf_info_section, s->value);
#endif
		}
		else {
		    /* param/local */
                    dwarf_data1(dwarf_info_section, dwarf_sleb128_size(s->value) + 1);
                    dwarf_data1(dwarf_info_section, DW_OP_fbreg);
                    dwarf_sleb128(dwarf_info_section, s->value);
		}
		tcc_free (s->str);
            }
            tcc_free (cur->sym);
            dwarf_data1(dwarf_info_section,
			cur->child ? DWARF_ABBREV_LEXICAL_BLOCK
			           : DWARF_ABBREV_LEXICAL_EMPTY_BLOCK);
            dwarf_reloc(dwarf_info_section, section_sym, R_DATA_PTR);
#if PTR_SIZE == 4
            dwarf_data4(dwarf_info_section, func_ind + cur->start);
            dwarf_data4(dwarf_info_section, cur->end - cur->start);
#else
            dwarf_data8(dwarf_info_section, func_ind + cur->start);
            dwarf_data8(dwarf_info_section, cur->end - cur->start);
#endif
            tcc_debug_finish (s1, cur->child);
	    if (cur->child)
                dwarf_data1(dwarf_info_section, 0);
        }
        else
        {
            for (i = 0; i < cur->n_sym; i++) {
                struct debug_sym *s = &cur->sym[i];

                if (s->sec)
                    put_stabs_r(s1, s->str, s->type, 0, 0, s->value,
                                s->sec, s->sym_index);
                else
                    put_stabs(s1, s->str, s->type, 0, 0, s->value);
                tcc_free (s->str);
            }
            tcc_free (cur->sym);
            put_stabn(s1, N_LBRAC, 0, 0, cur->start);
            tcc_debug_finish (s1, cur->child);
            put_stabn(s1, N_RBRAC, 0, 0, cur->end);
        }
        tcc_free (cur);
        cur = next;
    }
}

ST_FUNC void tcc_add_debug_info(TCCState *s1, int param, Sym *s, Sym *e)
{
    CString debug_str;

    if (!(s1->do_debug & 2))
        return;

    cstr_new (&debug_str);
    for (; s != e; s = s->prev) {
        if (!s->v || (s->r & VT_VALMASK) != VT_LOCAL)
            continue;
	if (s1->dwarf) {
	    tcc_debug_stabs(s1, get_tok_str(s->v, NULL),
			    param ? N_PSYM : N_LSYM, s->c, NULL, 0,
			    tcc_get_dwarf_info(s1, s));
	}
	else
        {
            cstr_reset (&debug_str);
            cstr_printf (&debug_str, "%s:%s", get_tok_str(s->v, NULL),
			 param ? "p" : "");
            tcc_get_debug_info(s1, s, &debug_str);
            tcc_debug_stabs(s1, debug_str.data, param ? N_PSYM : N_LSYM,
			    s->c, NULL, 0, 0);
	}
    }
    cstr_free (&debug_str);
}

/* put function symbol */
ST_FUNC void tcc_debug_funcstart(TCCState *s1, Sym *sym)
{
    CString debug_str;
    BufferedFile *f;

    if (!s1->do_debug)
        return;
    debug_info_root = NULL;
    debug_info = NULL;
    tcc_debug_stabn(s1, N_LBRAC, ind - func_ind);
    f = put_new_file(s1);
    if (!f)
	return;

    if (s1->dwarf) {
        tcc_debug_line(s1);
        dwarf_info.func = sym;
        dwarf_info.line = file->line_num;
	if (s1->do_backtrace) {
	    int i, len;

	    dwarf_line_op(s1, 0); // extended
	    dwarf_uleb128_op(s1, strlen(funcname) + 2);
	    dwarf_line_op(s1, DW_LNE_hi_user - 1);
	    len = strlen(funcname) + 1;
	    for (i = 0; i < len; i++)
		dwarf_line_op(s1, funcname[i]);
	}
    }
    else
    {
        cstr_new (&debug_str);
        cstr_printf(&debug_str, "%s:%c", funcname, sym->type.t & VT_STATIC ? 'f' : 'F');
        tcc_get_debug_info(s1, sym->type.ref, &debug_str);
        put_stabs_r(s1, debug_str.data, N_FUN, 0, f->line_num, 0, cur_text_section, sym->c);
        cstr_free (&debug_str);
        tcc_debug_line(s1);
    }
}

ST_FUNC void tcc_debug_prolog_epilog(TCCState *s1, int value)
{
    if (!s1->do_debug)
        return;
    if (s1->dwarf) {
	dwarf_line_op(s1, value == 0 ? DW_LNS_set_prologue_end
				     : DW_LNS_set_epilogue_begin);
    }
}

/* put function size */
ST_FUNC void tcc_debug_funcend(TCCState *s1, int size)
{
    /* lldb does not like function end and next function start at same pc */
    int min_instr_len;

    if (!s1->do_debug)
        return;
    min_instr_len = dwarf_line.last_pc == ind ? 0 : DWARF_MIN_INSTR_LEN;
    ind -= min_instr_len;
    tcc_debug_line(s1);
    ind += min_instr_len;
    tcc_debug_stabn(s1, N_RBRAC, size);
    if (s1->dwarf) {
        int func_sib = 0;
	Sym *sym = dwarf_info.func;
	int n_debug_info = tcc_get_dwarf_info(s1, sym->type.ref);

        dwarf_data1(dwarf_info_section,
	    sym->type.t & VT_STATIC ? DWARF_ABBREV_SUBPROGRAM_STATIC
				    : DWARF_ABBREV_SUBPROGRAM_EXTERNAL);
        if ((sym->type.t & VT_STATIC) == 0)
            dwarf_data1(dwarf_info_section, 1);
        dwarf_strp(dwarf_info_section, funcname);
        dwarf_uleb128(dwarf_info_section, dwarf_line.cur_file);
        dwarf_uleb128(dwarf_info_section, dwarf_info.line);
	tcc_debug_check_anon(s1, sym->type.ref, dwarf_info_section->data_offset);
        dwarf_data4(dwarf_info_section, n_debug_info - dwarf_info.start);
        dwarf_reloc(dwarf_info_section, section_sym, R_DATA_PTR);
#if PTR_SIZE == 4
        dwarf_data4(dwarf_info_section, func_ind); // low_pc
        dwarf_data4(dwarf_info_section, size); // high_pc
#else
        dwarf_data8(dwarf_info_section, func_ind); // low_pc
        dwarf_data8(dwarf_info_section, size); // high_pc
#endif
        func_sib = dwarf_info_section->data_offset;
        dwarf_data4(dwarf_info_section, 0); // sibling
        dwarf_data1(dwarf_info_section, 1);
#if defined(TCC_TARGET_I386)
        dwarf_data1(dwarf_info_section, DW_OP_reg5); // ebp
#elif defined(TCC_TARGET_X86_64)
        dwarf_data1(dwarf_info_section, DW_OP_reg6); // rbp
#elif defined TCC_TARGET_ARM
        dwarf_data1(dwarf_info_section, DW_OP_reg13); // sp
#elif defined TCC_TARGET_ARM64
        dwarf_data1(dwarf_info_section, DW_OP_reg29); // reg 29
#elif defined TCC_TARGET_RISCV64
        dwarf_data1(dwarf_info_section, DW_OP_reg8); // r8(s0)
#else
        dwarf_data1(dwarf_info_section, DW_OP_call_frame_cfa);
#endif
        tcc_debug_finish (s1, debug_info_root);
	dwarf_data1(dwarf_info_section, 0);
        write32le(dwarf_info_section->data + func_sib,
                  dwarf_info_section->data_offset - dwarf_info.start);
    }
    else
    {
        tcc_debug_finish (s1, debug_info_root);
    }
    debug_info_root = 0;
}


ST_FUNC void tcc_debug_extern_sym(TCCState *s1, Sym *sym, int sh_num, int sym_bind, int sym_type)
{
    if (!(s1->do_debug & 2))
        return;

    if (sym_type == STT_FUNC || sym->v >= SYM_FIRST_ANOM)
        return;
    if (s1->dwarf) {
        int debug_type;

        debug_type = tcc_get_dwarf_info(s1, sym);
	dwarf_data1(dwarf_info_section,
		    sym_bind == STB_GLOBAL
		    ? DWARF_ABBREV_VARIABLE_EXTERNAL
		    : DWARF_ABBREV_VARIABLE_STATIC);
	dwarf_strp(dwarf_info_section, get_tok_str(sym->v, NULL));
	dwarf_uleb128(dwarf_info_section, dwarf_line.cur_file);
	dwarf_uleb128(dwarf_info_section, file->line_num);
	tcc_debug_check_anon(s1, sym, dwarf_info_section->data_offset);
	dwarf_data4(dwarf_info_section, debug_type - dwarf_info.start);
	if (sym_bind == STB_GLOBAL)
	    dwarf_data1(dwarf_info_section, 1);
	dwarf_data1(dwarf_info_section, PTR_SIZE + 1);
	dwarf_data1(dwarf_info_section, DW_OP_addr);
	greloca(dwarf_info_section, sym, dwarf_info_section->data_offset,
		R_DATA_PTR, 0);
#if PTR_SIZE == 4
	dwarf_data4(dwarf_info_section, 0);
#else
	dwarf_data8(dwarf_info_section, 0);
#endif
    }
    else
    {
        Section *s = sh_num == SHN_COMMON ? common_section
					  : s1->sections[sh_num];
        CString str;

        cstr_new (&str);
        cstr_printf (&str, "%s:%c",
                get_tok_str(sym->v, NULL),
                sym_bind == STB_GLOBAL ? 'G' : func_ind != -1 ? 'V' : 'S'
                );
        tcc_get_debug_info(s1, sym, &str);
        if (sym_bind == STB_GLOBAL)
            tcc_debug_stabs(s1, str.data, N_GSYM, 0, NULL, 0, 0);
        else
            tcc_debug_stabs(s1, str.data,
                (sym->type.t & VT_STATIC) && data_section == s
                ? N_STSYM : N_LCSYM, 0, s, sym->c, 0);
        cstr_free (&str);
    }
}

ST_FUNC void tcc_debug_typedef(TCCState *s1, Sym *sym)
{
    if (!(s1->do_debug & 2))
        return;

    if (s1->dwarf) {
	int debug_type;

        debug_type = tcc_get_dwarf_info(s1, sym);
	if (debug_type != -1) {
	    dwarf_data1(dwarf_info_section, DWARF_ABBREV_TYPEDEF);
	    dwarf_strp(dwarf_info_section, get_tok_str(sym->v, NULL));
	    dwarf_uleb128(dwarf_info_section, dwarf_line.cur_file);
	    dwarf_uleb128(dwarf_info_section, file->line_num);
	    tcc_debug_check_anon(s1, sym, dwarf_info_section->data_offset);
	    dwarf_data4(dwarf_info_section, debug_type - dwarf_info.start);
	}
    }
    else
    {
        CString str;
        cstr_new (&str);
        cstr_printf (&str, "%s:t",
                     (sym->v & ~SYM_FIELD) >= SYM_FIRST_ANOM
                     ? "" : get_tok_str(sym->v, NULL));
        tcc_get_debug_info(s1, sym, &str);
        tcc_debug_stabs(s1, str.data, N_LSYM, 0, NULL, 0, 0);
        cstr_free (&str);
    }
}

/* ------------------------------------------------------------------------- */
/* for section layout see lib/tcov.c */

ST_FUNC void tcc_tcov_block_end(TCCState *s1, int line);

ST_FUNC void tcc_tcov_block_begin(TCCState *s1)
{
    SValue sv;
    void *ptr;
    unsigned long last_offset = tcov_data.offset;

    tcc_tcov_block_end (tcc_state, 0);
    if (s1->test_coverage == 0 || nocode_wanted)
	return;

    if (tcov_data.last_file_name == 0 ||
	strcmp ((const char *)(tcov_section->data + tcov_data.last_file_name),
		file->true_filename) != 0) {
	char wd[1024];
	CString cstr;

	if (tcov_data.last_func_name)
	    section_ptr_add(tcov_section, 1);
	if (tcov_data.last_file_name)
	    section_ptr_add(tcov_section, 1);
	tcov_data.last_func_name = 0;
	cstr_new (&cstr);
	if (file->true_filename[0] == '/') {
	    tcov_data.last_file_name = tcov_section->data_offset;
	    cstr_printf (&cstr, "%s", file->true_filename);
	}
	else {
	    getcwd (wd, sizeof(wd));
	    tcov_data.last_file_name = tcov_section->data_offset + strlen(wd) + 1;
	    cstr_printf (&cstr, "%s/%s", wd, file->true_filename);
	}
	ptr = section_ptr_add(tcov_section, cstr.size + 1);
	strcpy((char *)ptr, cstr.data);
#ifdef _WIN32
        normalize_slashes((char *)ptr);
#endif
	cstr_free (&cstr);
    }
    if (tcov_data.last_func_name == 0 ||
	strcmp ((const char *)(tcov_section->data + tcov_data.last_func_name),
		funcname) != 0) {
	size_t len;

	if (tcov_data.last_func_name)
	    section_ptr_add(tcov_section, 1);
	tcov_data.last_func_name = tcov_section->data_offset;
	len = strlen (funcname);
	ptr = section_ptr_add(tcov_section, len + 1);
	strcpy((char *)ptr, funcname);
	section_ptr_add(tcov_section, -tcov_section->data_offset & 7);
	ptr = section_ptr_add(tcov_section, 8);
	write64le (ptr, file->line_num);
    }
    if (ind == tcov_data.ind && tcov_data.line == file->line_num)
        tcov_data.offset = last_offset;
    else {
        Sym label = {0};
        label.type.t = VT_LLONG | VT_STATIC;

        ptr = section_ptr_add(tcov_section, 16);
        tcov_data.line = file->line_num;
        write64le (ptr, (tcov_data.line << 8) | 0xff);
        put_extern_sym(&label, tcov_section,
		       ((unsigned char *)ptr - tcov_section->data) + 8, 0);
        sv.type = label.type;
        sv.r = VT_SYM | VT_LVAL | VT_CONST;
        sv.r2 = VT_CONST;
        sv.c.i = 0;
        sv.sym = &label;
#if defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64 || \
    defined TCC_TARGET_ARM || defined TCC_TARGET_ARM64 || \
    defined TCC_TARGET_RISCV64
        gen_increment_tcov (&sv);
#else
        vpushv(&sv);
        inc(0, TOK_INC);
        vpop();
#endif
        tcov_data.offset = (unsigned char *)ptr - tcov_section->data;
        tcov_data.ind = ind;
    }
}

ST_FUNC void tcc_tcov_block_end(TCCState *s1, int line)
{
    if (s1->test_coverage == 0)
	return;
    if (line == -1)
        line = tcov_data.line;
    if (tcov_data.offset) {
	void *ptr = tcov_section->data + tcov_data.offset;
	unsigned long long nline = line ? line : file->line_num;

	write64le (ptr, (read64le (ptr) & 0xfffffffffull) | (nline << 36));
	tcov_data.offset = 0;
    }
}

ST_FUNC void tcc_tcov_check_line(TCCState *s1, int start)
{
    if (s1->test_coverage == 0)
	return;
    if (tcov_data.line != file->line_num) {
        if ((tcov_data.line + 1) != file->line_num) {
	    tcc_tcov_block_end (s1, -1);
	    if (start)
                tcc_tcov_block_begin (s1);
	}
	else
	    tcov_data.line = file->line_num;
    }
}

ST_FUNC void tcc_tcov_start(TCCState *s1)
{
    if (s1->test_coverage == 0)
	return;
    if (!s1->dState)
        s1->dState = tcc_mallocz(sizeof *s1->dState);
    memset (&tcov_data, 0, sizeof (tcov_data));
    if (tcov_section == NULL) {
        tcov_section = new_section(tcc_state, ".tcov", SHT_PROGBITS,
				   SHF_ALLOC | SHF_WRITE);
	section_ptr_add(tcov_section, 4); // pointer to executable name
    }
}

ST_FUNC void tcc_tcov_end(TCCState *s1)
{
    if (s1->test_coverage == 0)
	return;
    if (tcov_data.last_func_name)
        section_ptr_add(tcov_section, 1);
    if (tcov_data.last_file_name)
        section_ptr_add(tcov_section, 1);
}

ST_FUNC void tcc_tcov_reset_ind(TCCState *s1)
{
    tcov_data.ind = 0;
}

/* ------------------------------------------------------------------------- */
#undef last_line_num
#undef new_file
#undef section_sym
#undef debug_next_type
#undef debug_hash
#undef n_debug_hash
#undef debug_anon_hash
#undef n_debug_anon_hash
#undef debug_info
#undef debug_info_root
#undef dwarf_sym
#undef dwarf_line
#undef dwarf_info
#undef tcov_data

//// tcc: tccelf.c

/*
 *  ELF file handling for TCC
 *
 *  Copyright (c) 2001-2004 Fabrice Bellard
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "tcc.h"

/* Define this to get some debug output during relocation processing.  */
#undef DEBUG_RELOC

/********************************************************/
/* global variables */

/* elf version information */
struct sym_version {
    char *lib;
    char *version;
    int out_index;
    int prev_same_lib;
};

#define nb_sym_versions     s1->nb_sym_versions
#define sym_versions        s1->sym_versions
#define nb_sym_to_version   s1->nb_sym_to_version
#define sym_to_version      s1->sym_to_version
#define dt_verneednum       s1->dt_verneednum
#define versym_section      s1->versym_section
#define verneed_section     s1->verneed_section

/* special flag to indicate that the section should not be linked to the other ones */
#define SHF_PRIVATE 0x80000000
/* section is dynsymtab_section */
#define SHF_DYNSYM 0x40000000

#ifdef TCC_TARGET_PE
#define shf_RELRO SHF_ALLOC
static const char rdata[] = ".rdata";
#else
#define shf_RELRO s1->shf_RELRO
static const char rdata[] = ".data.ro";
#endif

/* ------------------------------------------------------------------------- */

ST_FUNC void tccelf_new(TCCState *s)
{
    TCCState *s1 = s;

#ifndef TCC_TARGET_PE
    shf_RELRO = SHF_ALLOC;
    if (s1->output_type != TCC_OUTPUT_MEMORY)
        shf_RELRO |= SHF_WRITE; /* the ELF loader will set it to RO at runtime */
#endif

    /* no section zero */
    dynarray_add(&s->sections, &s->nb_sections, NULL);

    /* create standard sections */
    text_section = new_section(s, ".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR);
    data_section = new_section(s, ".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE);
    /* create ro data section (make ro after relocation done with GNU_RELRO) */
    rodata_section = new_section(s, rdata, SHT_PROGBITS, shf_RELRO);
    bss_section = new_section(s, ".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE);
    common_section = new_section(s, ".common", SHT_NOBITS, SHF_PRIVATE);
    common_section->sh_num = SHN_COMMON;

    /* symbols are always generated for linking stage */
    symtab_section = new_symtab(s, ".symtab", SHT_SYMTAB, 0,
                                ".strtab",
                                ".hashtab", SHF_PRIVATE);

    /* private symbol table for dynamic symbols */
    s->dynsymtab_section = new_symtab(s, ".dynsymtab", SHT_SYMTAB, SHF_PRIVATE|SHF_DYNSYM,
                                      ".dynstrtab",
                                      ".dynhashtab", SHF_PRIVATE);
    get_sym_attr(s, 0, 1);

    if (s->do_debug) {
        /* add debug sections */
        tcc_debug_new(s);
    }

#ifdef CONFIG_TCC_BCHECK
    if (s->do_bounds_check) {
        /* if bound checking, then add corresponding sections */
        /* (make ro after relocation done with GNU_RELRO) */
        bounds_section = new_section(s, ".bounds", SHT_PROGBITS, shf_RELRO);
        lbounds_section = new_section(s, ".lbounds", SHT_PROGBITS, shf_RELRO);
    }
#endif
}

ST_FUNC void free_section(Section *s)
{
    if (!s)
        return;
    tcc_free(s->data);
    s->data = NULL;
    s->data_allocated = s->data_offset = 0;
}

ST_FUNC void tccelf_delete(TCCState *s1)
{
    int i;

#ifndef ELF_OBJ_ONLY
    /* free symbol versions */
    for (i = 0; i < nb_sym_versions; i++) {
        tcc_free(sym_versions[i].version);
        tcc_free(sym_versions[i].lib);
    }
    tcc_free(sym_versions);
    tcc_free(sym_to_version);
#endif

    /* free all sections */
    for(i = 1; i < s1->nb_sections; i++)
        free_section(s1->sections[i]);
    dynarray_reset(&s1->sections, &s1->nb_sections);

    for(i = 0; i < s1->nb_priv_sections; i++)
        free_section(s1->priv_sections[i]);
    dynarray_reset(&s1->priv_sections, &s1->nb_priv_sections);

    tcc_free(s1->sym_attrs);
    symtab_section = NULL; /* for tccrun.c:rt_printline() */
}

/* save section data state */
ST_FUNC void tccelf_begin_file(TCCState *s1)
{
    Section *s; int i;
    for (i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        s->sh_offset = s->data_offset;
    }
    /* disable symbol hashing during compilation */
    s = s1->symtab, s->reloc = s->hash, s->hash = NULL;
#if defined TCC_TARGET_X86_64 && defined TCC_TARGET_PE
    s1->uw_sym = 0;
#endif
}

/* At the end of compilation, convert any UNDEF syms to global, and merge
   with previously existing symbols */
ST_FUNC void tccelf_end_file(TCCState *s1)
{
    Section *s = s1->symtab;
    int first_sym, nb_syms, *tr, i;

    first_sym = s->sh_offset / sizeof (ElfSym);
    nb_syms = s->data_offset / sizeof (ElfSym) - first_sym;
    s->data_offset = s->sh_offset;
    s->link->data_offset = s->link->sh_offset;
    s->hash = s->reloc, s->reloc = NULL;
    tr = tcc_mallocz(nb_syms * sizeof *tr);

    for (i = 0; i < nb_syms; ++i) {
        ElfSym *sym = (ElfSym*)s->data + first_sym + i;

        if (sym->st_shndx == SHN_UNDEF) {
            int sym_bind = ELFW(ST_BIND)(sym->st_info);
            int sym_type = ELFW(ST_TYPE)(sym->st_info);
            if (sym_bind == STB_LOCAL)
                sym_bind = STB_GLOBAL;
#ifndef TCC_TARGET_PE
            if (sym_bind == STB_GLOBAL && s1->output_type == TCC_OUTPUT_OBJ) {
                /* undefined symbols with STT_FUNC are confusing gnu ld when
                   linking statically to STT_GNU_IFUNC */
                sym_type = STT_NOTYPE;
            }
#endif
            sym->st_info = ELFW(ST_INFO)(sym_bind, sym_type);
        }

        tr[i] = set_elf_sym(s, sym->st_value, sym->st_size, sym->st_info,
            sym->st_other, sym->st_shndx, (char*)s->link->data + sym->st_name);
    }
    /* now update relocations */
    for (i = 1; i < s1->nb_sections; i++) {
        Section *sr = s1->sections[i];
        if (sr->sh_type == SHT_RELX && sr->link == s) {
            ElfW_Rel *rel = (ElfW_Rel*)(sr->data + sr->sh_offset);
            ElfW_Rel *rel_end = (ElfW_Rel*)(sr->data + sr->data_offset);
            for (; rel < rel_end; ++rel) {
                int n = ELFW(R_SYM)(rel->r_info) - first_sym;
                if (n < 0) /* zero sym_index in reloc (can happen with asm) */
                    continue;
                rel->r_info = ELFW(R_INFO)(tr[n], ELFW(R_TYPE)(rel->r_info));
            }
        }
    }
    tcc_free(tr);

    /* record text/data/bss output for -bench info */
    for (i = 0; i < 4; ++i) {
        s = s1->sections[i + 1];
        s1->total_output[i] += s->data_offset - s->sh_offset;
    }
}

ST_FUNC Section *new_section(TCCState *s1, const char *name, int sh_type, int sh_flags)
{
    Section *sec;

    sec = tcc_mallocz(sizeof(Section) + strlen(name));
    sec->s1 = s1;
    strcpy(sec->name, name);
    sec->sh_type = sh_type;
    sec->sh_flags = sh_flags;
    switch(sh_type) {
    case SHT_GNU_versym:
        sec->sh_addralign = 2;
        break;
    case SHT_HASH:
    case SHT_GNU_HASH:
    case SHT_REL:
    case SHT_RELA:
    case SHT_DYNSYM:
    case SHT_SYMTAB:
    case SHT_DYNAMIC:
    case SHT_GNU_verneed:
    case SHT_GNU_verdef:
        sec->sh_addralign = PTR_SIZE;
        break;
    case SHT_STRTAB:
        sec->sh_addralign = 1;
        break;
    default:
        sec->sh_addralign =  PTR_SIZE; /* gcc/pcc default alignment */
        break;
    }

    if (sh_flags & SHF_PRIVATE) {
        dynarray_add(&s1->priv_sections, &s1->nb_priv_sections, sec);
    } else {
        sec->sh_num = s1->nb_sections;
        dynarray_add(&s1->sections, &s1->nb_sections, sec);
    }

    return sec;
}

ST_FUNC void init_symtab(Section *s)
{
    int *ptr, nb_buckets = 1;
    put_elf_str(s->link, "");
    section_ptr_add(s, sizeof (ElfW(Sym)));
    ptr = section_ptr_add(s->hash, (2 + nb_buckets + 1) * sizeof(int));
    ptr[0] = nb_buckets;
    ptr[1] = 1;
    memset(ptr + 2, 0, (nb_buckets + 1) * sizeof(int));
}

ST_FUNC Section *new_symtab(TCCState *s1,
                           const char *symtab_name, int sh_type, int sh_flags,
                           const char *strtab_name,
                           const char *hash_name, int hash_sh_flags)
{
    Section *symtab, *strtab, *hash;
    symtab = new_section(s1, symtab_name, sh_type, sh_flags);
    symtab->sh_entsize = sizeof(ElfW(Sym));
    strtab = new_section(s1, strtab_name, SHT_STRTAB, sh_flags);
    symtab->link = strtab;
    hash = new_section(s1, hash_name, SHT_HASH, hash_sh_flags);
    hash->sh_entsize = sizeof(int);
    symtab->hash = hash;
    hash->link = symtab;
    init_symtab(symtab);
    return symtab;
}

/* realloc section and set its content to zero */
ST_FUNC void section_realloc(Section *sec, unsigned long new_size)
{
    unsigned long size;
    unsigned char *data;

    size = sec->data_allocated;
    if (size == 0)
        size = 1;
    while (size < new_size)
        size = size * 2;
    data = tcc_realloc(sec->data, size);
    memset(data + sec->data_allocated, 0, size - sec->data_allocated);
    sec->data = data;
    sec->data_allocated = size;
}

/* reserve at least 'size' bytes aligned per 'align' in section
   'sec' from current offset, and return the aligned offset */
ST_FUNC size_t section_add(Section *sec, addr_t size, int align)
{
    size_t offset, offset1;

    offset = (sec->data_offset + align - 1) & -align;
    offset1 = offset + size;
    if (sec->sh_type != SHT_NOBITS && offset1 > sec->data_allocated)
        section_realloc(sec, offset1);
    sec->data_offset = offset1;
    if (align > sec->sh_addralign)
        sec->sh_addralign = align;
    return offset;
}

/* reserve at least 'size' bytes in section 'sec' from
   sec->data_offset. */
ST_FUNC void *section_ptr_add(Section *sec, addr_t size)
{
    size_t offset = section_add(sec, size, 1);
    if (sec->data == NULL) {
        return sec->data;
    }
    return sec->data + offset;
}

#ifndef ELF_OBJ_ONLY
/* reserve at least 'size' bytes from section start */
static void section_reserve(Section *sec, unsigned long size)
{
    if (size > sec->data_allocated)
        section_realloc(sec, size);
    if (size > sec->data_offset)
        sec->data_offset = size;
}
#endif

static Section *have_section(TCCState *s1, const char *name)
{
    Section *sec;
    int i;
    for(i = 1; i < s1->nb_sections; i++) {
        sec = s1->sections[i];
        if (!strcmp(name, sec->name))
            return sec;
    }
    return NULL;
}

/* return a reference to a section, and create it if it does not
   exists */
ST_FUNC Section *find_section(TCCState *s1, const char *name)
{
    Section *sec = have_section(s1, name);
    if (sec)
        return sec;
    /* sections are created as PROGBITS */
    return new_section(s1, name, SHT_PROGBITS, SHF_ALLOC);
}

/* ------------------------------------------------------------------------- */

ST_FUNC int put_elf_str(Section *s, const char *sym)
{
    int offset, len;
    char *ptr;

    len = strlen(sym) + 1;
    offset = s->data_offset;
    ptr = section_ptr_add(s, len);
    memmove(ptr, sym, len);
    return offset;
}

/* elf symbol hashing function */
static ElfW(Word) elf_hash(const unsigned char *name)
{
    ElfW(Word) h = 0, g;

    while (*name) {
        h = (h << 4) + *name++;
        g = h & 0xf0000000;
        if (g)
            h ^= g >> 24;
        h &= ~g;
    }
    return h;
}

/* rebuild hash table of section s */
/* NOTE: we do factorize the hash table code to go faster */
static void rebuild_hash(Section *s, unsigned int nb_buckets)
{
    ElfW(Sym) *sym;
    int *ptr, *hash, nb_syms, sym_index, h;
    unsigned char *strtab;

    strtab = s->link->data;
    nb_syms = s->data_offset / sizeof(ElfW(Sym));

    if (!nb_buckets)
        nb_buckets = ((int*)s->hash->data)[0];

    s->hash->data_offset = 0;
    ptr = section_ptr_add(s->hash, (2 + nb_buckets + nb_syms) * sizeof(int));
    ptr[0] = nb_buckets;
    ptr[1] = nb_syms;
    ptr += 2;
    hash = ptr;
    memset(hash, 0, (nb_buckets + 1) * sizeof(int));
    ptr += nb_buckets + 1;

    sym = (ElfW(Sym) *)s->data + 1;
    for(sym_index = 1; sym_index < nb_syms; sym_index++) {
        if (ELFW(ST_BIND)(sym->st_info) != STB_LOCAL) {
            h = elf_hash(strtab + sym->st_name) % nb_buckets;
            *ptr = hash[h];
            hash[h] = sym_index;
        } else {
            *ptr = 0;
        }
        ptr++;
        sym++;
    }
}

/* return the symbol number */
ST_FUNC int put_elf_sym(Section *s, addr_t value, unsigned long size,
    int info, int other, int shndx, const char *name)
{
    int name_offset, sym_index;
    int nbuckets, h;
    ElfW(Sym) *sym;
    Section *hs;

    sym = section_ptr_add(s, sizeof(ElfW(Sym)));
    if (name && name[0])
        name_offset = put_elf_str(s->link, name);
    else
        name_offset = 0;
    /* XXX: endianness */
    sym->st_name = name_offset;
    sym->st_value = value;
    sym->st_size = size;
    sym->st_info = info;
    sym->st_other = other;
    sym->st_shndx = shndx;
    sym_index = sym - (ElfW(Sym) *)s->data;
    hs = s->hash;
    if (hs) {
        int *ptr, *base;
        ptr = section_ptr_add(hs, sizeof(int));
        base = (int *)hs->data;
        /* only add global or weak symbols. */
        if (ELFW(ST_BIND)(info) != STB_LOCAL) {
            /* add another hashing entry */
            nbuckets = base[0];
            h = elf_hash((unsigned char *)s->link->data + name_offset) % nbuckets;
            *ptr = base[2 + h];
            base[2 + h] = sym_index;
            base[1]++;
            /* we resize the hash table */
            hs->nb_hashed_syms++;
            if (hs->nb_hashed_syms > 2 * nbuckets) {
                rebuild_hash(s, 2 * nbuckets);
            }
        } else {
            *ptr = 0;
            base[1]++;
        }
    }
    return sym_index;
}

ST_FUNC int find_elf_sym(Section *s, const char *name)
{
    ElfW(Sym) *sym;
    Section *hs;
    int nbuckets, sym_index, h;
    const char *name1;

    hs = s->hash;
    if (!hs)
        return 0;
    nbuckets = ((int *)hs->data)[0];
    h = elf_hash((unsigned char *) name) % nbuckets;
    sym_index = ((int *)hs->data)[2 + h];
    while (sym_index != 0) {
        sym = &((ElfW(Sym) *)s->data)[sym_index];
        name1 = (char *) s->link->data + sym->st_name;
        if (!strcmp(name, name1))
            return sym_index;
        sym_index = ((int *)hs->data)[2 + nbuckets + sym_index];
    }
    return 0;
}

/* return elf symbol value, signal error if 'err' is nonzero, decorate
   name if FORC */
ST_FUNC addr_t get_sym_addr(TCCState *s1, const char *name, int err, int forc)
{
    int sym_index;
    ElfW(Sym) *sym;
    char buf[256];
    if (forc && s1->leading_underscore
#ifdef TCC_TARGET_PE
        /* win32-32bit stdcall symbols always have _ already */
        && !strchr(name, '@')
#endif
        ) {
        buf[0] = '_';
        pstrcpy(buf + 1, sizeof(buf) - 1, name);
        name = buf;
    }
    sym_index = find_elf_sym(s1->symtab, name);
    sym = &((ElfW(Sym) *)s1->symtab->data)[sym_index];
    if (!sym_index || sym->st_shndx == SHN_UNDEF) {
        if (err)
            tcc_error_noabort("%s not defined", name);
        return (addr_t)-1;
    }
    return sym->st_value;
}

/* return elf symbol value */
LIBTCCAPI void *tcc_get_symbol(TCCState *s, const char *name)
{
    addr_t addr = get_sym_addr(s, name, 0, 1);
    return addr == -1 ? NULL : (void*)(uintptr_t)addr;
}

/* list elf symbol names and values */
ST_FUNC void list_elf_symbols(TCCState *s, void *ctx,
    void (*symbol_cb)(void *ctx, const char *name, const void *val))
{
    ElfW(Sym) *sym;
    Section *symtab;
    int sym_index, end_sym;
    const char *name;
    unsigned char sym_vis, sym_bind;

    symtab = s->symtab;
    end_sym = symtab->data_offset / sizeof (ElfSym);
    for (sym_index = 0; sym_index < end_sym; ++sym_index) {
        sym = &((ElfW(Sym) *)symtab->data)[sym_index];
        if (sym->st_value) {
            name = (char *) symtab->link->data + sym->st_name;
            sym_bind = ELFW(ST_BIND)(sym->st_info);
            sym_vis = ELFW(ST_VISIBILITY)(sym->st_other);
            if (sym_bind == STB_GLOBAL && sym_vis == STV_DEFAULT)
                symbol_cb(ctx, name, (void*)(uintptr_t)sym->st_value);
        }
    }
}

/* list elf symbol names and values */
LIBTCCAPI void tcc_list_symbols(TCCState *s, void *ctx,
    void (*symbol_cb)(void *ctx, const char *name, const void *val))
{
    list_elf_symbols(s, ctx, symbol_cb);
}

#ifndef ELF_OBJ_ONLY
static void
version_add (TCCState *s1)
{
    int i;
    ElfW(Sym) *sym;
    ElfW(Verneed) *vn = NULL;
    Section *symtab;
    int sym_index, end_sym, nb_versions = 2, nb_entries = 0;
    ElfW(Half) *versym;
    const char *name;

    if (0 == nb_sym_versions)
        return;
    versym_section = new_section(s1, ".gnu.version", SHT_GNU_versym, SHF_ALLOC);
    versym_section->sh_entsize = sizeof(ElfW(Half));
    versym_section->link = s1->dynsym;

    /* add needed symbols */
    symtab = s1->dynsym;
    end_sym = symtab->data_offset / sizeof (ElfSym);
    versym = section_ptr_add(versym_section, end_sym * sizeof(ElfW(Half)));
    for (sym_index = 1; sym_index < end_sym; ++sym_index) {
        int dllindex, verndx;
        sym = &((ElfW(Sym) *)symtab->data)[sym_index];
        if (sym->st_shndx != SHN_UNDEF)
            continue; /* defined symbol doesn't need library version */
        name = (char *) symtab->link->data + sym->st_name;
        dllindex = find_elf_sym(s1->dynsymtab_section, name);
        verndx = (dllindex && dllindex < nb_sym_to_version)
                 ? sym_to_version[dllindex] : -1;
        if (verndx >= 0) {
            if (!sym_versions[verndx].out_index)
              sym_versions[verndx].out_index = nb_versions++;
            versym[sym_index] = sym_versions[verndx].out_index;
        }
    }
    /* generate verneed section, but not when it will be empty.  Some
       dynamic linkers look at their contents even when DTVERNEEDNUM and
       section size is zero.  */
    if (nb_versions > 2) {
        verneed_section = new_section(s1, ".gnu.version_r",
                                      SHT_GNU_verneed, SHF_ALLOC);
        verneed_section->link = s1->dynsym->link;
        for (i = nb_sym_versions; i-- > 0;) {
            struct sym_version *sv = &sym_versions[i];
            int n_same_libs = 0, prev;
            size_t vnofs;
            ElfW(Vernaux) *vna = 0;
            if (sv->out_index < 1)
              continue;

            /* make sure that a DT_NEEDED tag is put */
            /* abitest-tcc fails on older i386-linux with "ld-linux.so.2" DT_NEEDED
               ret_int_test... Inconsistency detected by ld.so: dl-minimal.c: 148:
               realloc: Assertion `ptr == alloc_last_block' failed! */
            if (strcmp(sv->lib, "ld-linux.so.2"))
                tcc_add_dllref(s1, sv->lib, 0);

            vnofs = section_add(verneed_section, sizeof(*vn), 1);
            vn = (ElfW(Verneed)*)(verneed_section->data + vnofs);
            vn->vn_version = 1;
            vn->vn_file = put_elf_str(verneed_section->link, sv->lib);
            vn->vn_aux = sizeof (*vn);
            do {
                prev = sv->prev_same_lib;
                if (sv->out_index > 0) {
                    vna = section_ptr_add(verneed_section, sizeof(*vna));
                    vna->vna_hash = elf_hash ((const unsigned char *)sv->version);
                    vna->vna_flags = 0;
                    vna->vna_other = sv->out_index;
                    sv->out_index = -2;
                    vna->vna_name = put_elf_str(verneed_section->link, sv->version);
                    vna->vna_next = sizeof (*vna);
                    n_same_libs++;
                }
                if (prev >= 0)
                  sv = &sym_versions[prev];
            } while(prev >= 0);
            vna->vna_next = 0;
            vn = (ElfW(Verneed)*)(verneed_section->data + vnofs);
            vn->vn_cnt = n_same_libs;
            vn->vn_next = sizeof(*vn) + n_same_libs * sizeof(*vna);
            nb_entries++;
        }
        if (vn)
          vn->vn_next = 0;
        verneed_section->sh_info = nb_entries;
    }
    dt_verneednum = nb_entries;
}
#endif /* ndef ELF_OBJ_ONLY */

/* add an elf symbol : check if it is already defined and patch
   it. Return symbol index. NOTE that sh_num can be SHN_UNDEF. */
ST_FUNC int set_elf_sym(Section *s, addr_t value, unsigned long size,
                       int info, int other, int shndx, const char *name)
{
    TCCState *s1 = s->s1;
    ElfW(Sym) *esym;
    int sym_bind, sym_index, sym_type, esym_bind;
    unsigned char sym_vis, esym_vis, new_vis;

    sym_bind = ELFW(ST_BIND)(info);
    sym_type = ELFW(ST_TYPE)(info);
    sym_vis = ELFW(ST_VISIBILITY)(other);

    if (sym_bind != STB_LOCAL) {
        /* we search global or weak symbols */
        sym_index = find_elf_sym(s, name);
        if (!sym_index)
            goto do_def;
        esym = &((ElfW(Sym) *)s->data)[sym_index];
        if (esym->st_value == value && esym->st_size == size && esym->st_info == info
            && esym->st_other == other && esym->st_shndx == shndx)
            return sym_index;
        if (esym->st_shndx != SHN_UNDEF) {
            esym_bind = ELFW(ST_BIND)(esym->st_info);
            /* propagate the most constraining visibility */
            /* STV_DEFAULT(0)<STV_PROTECTED(3)<STV_HIDDEN(2)<STV_INTERNAL(1) */
            esym_vis = ELFW(ST_VISIBILITY)(esym->st_other);
            if (esym_vis == STV_DEFAULT) {
                new_vis = sym_vis;
            } else if (sym_vis == STV_DEFAULT) {
                new_vis = esym_vis;
            } else {
                new_vis = (esym_vis < sym_vis) ? esym_vis : sym_vis;
            }
            esym->st_other = (esym->st_other & ~ELFW(ST_VISIBILITY)(-1))
                             | new_vis;
            if (shndx == SHN_UNDEF) {
                /* ignore adding of undefined symbol if the
                   corresponding symbol is already defined */
            } else if (sym_bind == STB_GLOBAL && esym_bind == STB_WEAK) {
                /* global overrides weak, so patch */
                goto do_patch;
            } else if (sym_bind == STB_WEAK && esym_bind == STB_GLOBAL) {
                /* weak is ignored if already global */
            } else if (sym_bind == STB_WEAK && esym_bind == STB_WEAK) {
                /* keep first-found weak definition, ignore subsequents */
            } else if (sym_vis == STV_HIDDEN || sym_vis == STV_INTERNAL) {
                /* ignore hidden symbols after */
            } else if ((esym->st_shndx == SHN_COMMON
                            || esym->st_shndx == bss_section->sh_num)
                        && (shndx < SHN_LORESERVE
                            && shndx != bss_section->sh_num)) {
                /* data symbol gets precedence over common/bss */
                goto do_patch;
            } else if (shndx == SHN_COMMON || shndx == bss_section->sh_num) {
                /* data symbol keeps precedence over common/bss */
            } else if (s->sh_flags & SHF_DYNSYM) {
                /* we accept that two DLL define the same symbol */
	    } else if (esym->st_other & ST_ASM_SET) {
		/* If the existing symbol came from an asm .set
		   we can override.  */
		goto do_patch;
            } else {
#if 0
                printf("new_bind=%x new_shndx=%x new_vis=%x old_bind=%x old_shndx=%x old_vis=%x\n",
                       sym_bind, shndx, new_vis, esym_bind, esym->st_shndx, esym_vis);
#endif
                tcc_error_noabort("'%s' defined twice", name);
            }
        } else {
            esym->st_other = other;
        do_patch:
            esym->st_info = ELFW(ST_INFO)(sym_bind, sym_type);
            esym->st_shndx = shndx;
            s1->new_undef_sym = 1;
            esym->st_value = value;
            esym->st_size = size;
        }
    } else {
    do_def:
        sym_index = put_elf_sym(s, value, size,
                                ELFW(ST_INFO)(sym_bind, sym_type), other,
                                shndx, name);
    }
    return sym_index;
}

/* put relocation */
ST_FUNC void put_elf_reloca(Section *symtab, Section *s, unsigned long offset,
                            int type, int symbol, addr_t addend)
{
    TCCState *s1 = s->s1;
    char buf[256];
    Section *sr;
    ElfW_Rel *rel;

    sr = s->reloc;
    if (!sr) {
        /* if no relocation section, create it */
        snprintf(buf, sizeof(buf), REL_SECTION_FMT, s->name);
        /* if the symtab is allocated, then we consider the relocation
           are also */
        sr = new_section(s->s1, buf, SHT_RELX, symtab->sh_flags);
        sr->sh_entsize = sizeof(ElfW_Rel);
        sr->link = symtab;
        sr->sh_info = s->sh_num;
        s->reloc = sr;
    }
    rel = section_ptr_add(sr, sizeof(ElfW_Rel));
    rel->r_offset = offset;
    rel->r_info = ELFW(R_INFO)(symbol, type);
#if SHT_RELX == SHT_RELA
    rel->r_addend = addend;
#endif
    if (SHT_RELX != SHT_RELA && addend)
        tcc_error_noabort("non-zero addend on REL architecture");
}

ST_FUNC void put_elf_reloc(Section *symtab, Section *s, unsigned long offset,
                           int type, int symbol)
{
    put_elf_reloca(symtab, s, offset, type, symbol, 0);
}

ST_FUNC struct sym_attr *get_sym_attr(TCCState *s1, int index, int alloc)
{
    int n;
    struct sym_attr *tab;

    if (index >= s1->nb_sym_attrs) {
        if (!alloc)
            return s1->sym_attrs;
        /* find immediately bigger power of 2 and reallocate array */
        n = 1;
        while (index >= n)
            n *= 2;
        tab = tcc_realloc(s1->sym_attrs, n * sizeof(*s1->sym_attrs));
        s1->sym_attrs = tab;
        memset(s1->sym_attrs + s1->nb_sym_attrs, 0,
               (n - s1->nb_sym_attrs) * sizeof(*s1->sym_attrs));
        s1->nb_sym_attrs = n;
    }
    return &s1->sym_attrs[index];
}

static void modify_reloctions_old_to_new(TCCState *s1, Section *s, int *old_to_new_syms)
{
    int i, type, sym_index;
    Section *sr;
    ElfW_Rel *rel;

    for(i = 1; i < s1->nb_sections; i++) {
        sr = s1->sections[i];
        if (sr->sh_type == SHT_RELX && sr->link == s) {
            for_each_elem(sr, 0, rel, ElfW_Rel) {
                sym_index = ELFW(R_SYM)(rel->r_info);
                type = ELFW(R_TYPE)(rel->r_info);
                sym_index = old_to_new_syms[sym_index];
                rel->r_info = ELFW(R_INFO)(sym_index, type);
            }
        }
    }
}

/* In an ELF file symbol table, the local symbols must appear below
   the global and weak ones. Since TCC cannot sort it while generating
   the code, we must do it after. All the relocation tables are also
   modified to take into account the symbol table sorting */
static void sort_syms(TCCState *s1, Section *s)
{
    int *old_to_new_syms;
    ElfW(Sym) *new_syms;
    int nb_syms, i;
    ElfW(Sym) *p, *q;

    nb_syms = s->data_offset / sizeof(ElfW(Sym));
    new_syms = tcc_malloc(nb_syms * sizeof(ElfW(Sym)));
    old_to_new_syms = tcc_malloc(nb_syms * sizeof(int));

    /* first pass for local symbols */
    p = (ElfW(Sym) *)s->data;
    q = new_syms;
    for(i = 0; i < nb_syms; i++) {
        if (ELFW(ST_BIND)(p->st_info) == STB_LOCAL) {
            old_to_new_syms[i] = q - new_syms;
            *q++ = *p;
        }
        p++;
    }
    /* save the number of local symbols in section header */
    if( s->sh_size )    /* this 'if' makes IDA happy */
        s->sh_info = q - new_syms;

    /* then second pass for non local symbols */
    p = (ElfW(Sym) *)s->data;
    for(i = 0; i < nb_syms; i++) {
        if (ELFW(ST_BIND)(p->st_info) != STB_LOCAL) {
            old_to_new_syms[i] = q - new_syms;
            *q++ = *p;
        }
        p++;
    }

    /* we copy the new symbols to the old */
    memcpy(s->data, new_syms, nb_syms * sizeof(ElfW(Sym)));
    tcc_free(new_syms);

    modify_reloctions_old_to_new(s1, s, old_to_new_syms);

    tcc_free(old_to_new_syms);
}

#ifndef ELF_OBJ_ONLY
/* See: https://flapenguin.me/elf-dt-gnu-hash */
#define	ELFCLASS_BITS (PTR_SIZE * 8)

static Section *create_gnu_hash(TCCState *s1)
{
    int nb_syms, i, ndef, nbuckets, symoffset, bloom_size, bloom_shift;
    ElfW(Sym) *p;
    Section *gnu_hash;
    Section *dynsym = s1->dynsym;
    Elf32_Word *ptr;

    gnu_hash = new_section(s1, ".gnu.hash", SHT_GNU_HASH, SHF_ALLOC);
    gnu_hash->link = dynsym->hash->link;

    nb_syms = dynsym->data_offset / sizeof(ElfW(Sym));

    /* count def symbols */
    ndef = 0;
    p = (ElfW(Sym) *)dynsym->data;
    for(i = 0; i < nb_syms; i++, p++)
        ndef += p->st_shndx != SHN_UNDEF;

    /* calculate gnu hash sizes and fill header */
    nbuckets = ndef / 4 + 1;
    symoffset = nb_syms - ndef;
    bloom_shift = PTR_SIZE == 8 ? 6 : 5;
    bloom_size = 1; /* must be power of two */
    while (ndef >= bloom_size * (1 << (bloom_shift - 3)))
	bloom_size *= 2;
    ptr = section_ptr_add(gnu_hash, 4 * 4 +
				    PTR_SIZE * bloom_size +
				    nbuckets * 4 +
				    ndef * 4);
    ptr[0] = nbuckets;
    ptr[1] = symoffset;
    ptr[2] = bloom_size;
    ptr[3] = bloom_shift;
    return gnu_hash;
}

static Elf32_Word elf_gnu_hash (const unsigned char *name)
{
    Elf32_Word h = 5381;
    unsigned char c;

    while ((c = *name++))
        h = h * 33 + c;
    return h;
}

static void update_gnu_hash(TCCState *s1, Section *gnu_hash)
{
    int *old_to_new_syms;
    ElfW(Sym) *new_syms;
    int nb_syms, i, nbuckets, bloom_size, bloom_shift;
    ElfW(Sym) *p, *q;
    Section *vs;
    Section *dynsym = s1->dynsym;
    Elf32_Word *ptr, *buckets, *chain, *hash;
    unsigned int *nextbuck;
    addr_t *bloom;
    unsigned char *strtab;
    struct { int first, last; } *buck;

    strtab = dynsym->link->data;
    nb_syms = dynsym->data_offset / sizeof(ElfW(Sym));
    new_syms = tcc_malloc(nb_syms * sizeof(ElfW(Sym)));
    old_to_new_syms = tcc_malloc(nb_syms * sizeof(int));
    hash = tcc_malloc(nb_syms * sizeof(Elf32_Word));
    nextbuck = tcc_malloc(nb_syms * sizeof(int));

    /* calculate hashes and copy undefs */
    p = (ElfW(Sym) *)dynsym->data;
    q = new_syms;
    for(i = 0; i < nb_syms; i++, p++) {
        if (p->st_shndx == SHN_UNDEF) {
            old_to_new_syms[i] = q - new_syms;
            *q++ = *p;
        }
	else
	    hash[i] = elf_gnu_hash(strtab + p->st_name);
    }

    ptr = (Elf32_Word *) gnu_hash->data;
    nbuckets = ptr[0];
    bloom_size = ptr[2];
    bloom_shift = ptr[3];
    bloom = (addr_t *) (void *) &ptr[4];
    buckets = (Elf32_Word*) (void *) &bloom[bloom_size];
    chain = &buckets[nbuckets];
    buck = tcc_malloc(nbuckets * sizeof(*buck));

    if (gnu_hash->data_offset != 4 * 4 +
				 PTR_SIZE * bloom_size +
				 nbuckets * 4 +
				 (nb_syms - (q - new_syms)) * 4)
	tcc_error_noabort ("gnu_hash size incorrect");

    /* find buckets */
    for(i = 0; i < nbuckets; i++)
	buck[i].first = -1;

    p = (ElfW(Sym) *)dynsym->data;
    for(i = 0; i < nb_syms; i++, p++)
        if (p->st_shndx != SHN_UNDEF) {
	    int bucket = hash[i] % nbuckets;

	    if (buck[bucket].first == -1)
		buck[bucket].first = buck[bucket].last = i;
	    else {
		nextbuck[buck[bucket].last] = i;
		buck[bucket].last = i;
	    }
	}

    /* fill buckets/chains/bloom and sort symbols */
    p = (ElfW(Sym) *)dynsym->data;
    for(i = 0; i < nbuckets; i++) {
	int cur = buck[i].first;

	if (cur != -1) {
	    buckets[i] = q - new_syms;
	    for (;;) {
                old_to_new_syms[cur] = q - new_syms;
                *q++ = p[cur];
	        *chain++ = hash[cur] & ~1;
		bloom[(hash[cur] / ELFCLASS_BITS) % bloom_size] |=
		    (addr_t)1 << (hash[cur] % ELFCLASS_BITS) |
		    (addr_t)1 << ((hash[cur] >> bloom_shift) % ELFCLASS_BITS);
		if (cur == buck[i].last)
		    break;
		cur = nextbuck[cur];
	    }
	    chain[-1] |= 1;
	}
    }

    memcpy(dynsym->data, new_syms, nb_syms * sizeof(ElfW(Sym)));
    tcc_free(new_syms);
    tcc_free(hash);
    tcc_free(buck);
    tcc_free(nextbuck);

    modify_reloctions_old_to_new(s1, dynsym, old_to_new_syms);

    /* modify the versions */
    vs = versym_section;
    if (vs) {
	ElfW(Half) *newver, *versym = (ElfW(Half) *)vs->data;

	if (1/*versym*/) {
            newver = tcc_malloc(nb_syms * sizeof(*newver));
	    for (i = 0; i < nb_syms; i++)
	        newver[old_to_new_syms[i]] = versym[i];
	    memcpy(vs->data, newver, nb_syms * sizeof(*newver));
	    tcc_free(newver);
	}
    }

    tcc_free(old_to_new_syms);

    /* rebuild hash */
    ptr = (Elf32_Word *) dynsym->hash->data;
    rebuild_hash(dynsym, ptr[0]);
}
#endif /* ELF_OBJ_ONLY */

/* relocate symbol table, resolve undefined symbols if do_resolve is
   true and output error if undefined symbol. */
ST_FUNC void relocate_syms(TCCState *s1, Section *symtab, int do_resolve)
{
    ElfW(Sym) *sym;
    int sym_bind, sh_num;
    const char *name;

    for_each_elem(symtab, 1, sym, ElfW(Sym)) {
        sh_num = sym->st_shndx;
        if (sh_num == SHN_UNDEF) {
            if (do_resolve == 2) /* relocating dynsym */
                continue;
            name = (char *) s1->symtab->link->data + sym->st_name;
            /* Use ld.so to resolve symbol for us (for tcc -run) */
            if (do_resolve) {
#if defined TCC_IS_NATIVE && !defined TCC_TARGET_PE
                /* dlsym() needs the undecorated name.  */
                void *addr = dlsym(RTLD_DEFAULT, &name[s1->leading_underscore]);
#if TARGETOS_OpenBSD || TARGETOS_FreeBSD || TARGETOS_NetBSD || TARGETOS_ANDROID
		if (addr == NULL) {
		    int i;
		    for (i = 0; i < s1->nb_loaded_dlls; i++)
                        if ((addr = dlsym(s1->loaded_dlls[i]->handle, name)))
			    break;
		}
#endif
                if (addr) {
                    sym->st_value = (addr_t) addr;
#ifdef DEBUG_RELOC
		    printf ("relocate_sym: %s -> 0x%lx\n", name, sym->st_value);
#endif
                    goto found;
                }
#endif
            /* if dynamic symbol exist, it will be used in relocate_section */
            } else if (s1->dynsym && find_elf_sym(s1->dynsym, name))
                goto found;
            /* XXX: _fp_hw seems to be part of the ABI, so we ignore
               it */
            if (!strcmp(name, "_fp_hw"))
                goto found;
            /* only weak symbols are accepted to be undefined. Their
               value is zero */
            sym_bind = ELFW(ST_BIND)(sym->st_info);
            if (sym_bind == STB_WEAK)
                sym->st_value = 0;
            else
                tcc_error_noabort("undefined symbol '%s'", name);

        } else if (sh_num < SHN_LORESERVE) {
            /* add section base */
            sym->st_value += s1->sections[sym->st_shndx]->sh_addr;
        }
    found: ;
    }
}

/* relocate a given section (CPU dependent) by applying the relocations
   in the associated relocation section */
static void relocate_section(TCCState *s1, Section *s, Section *sr)
{
    ElfW_Rel *rel;
    ElfW(Sym) *sym;
    int type, sym_index;
    unsigned char *ptr;
    addr_t tgt, addr;
    int is_dwarf = s->sh_num >= s1->dwlo && s->sh_num < s1->dwhi;

    qrel = (ElfW_Rel *)sr->data;
    for_each_elem(sr, 0, rel, ElfW_Rel) {
        ptr = s->data + rel->r_offset;
        sym_index = ELFW(R_SYM)(rel->r_info);
        sym = &((ElfW(Sym) *)symtab_section->data)[sym_index];
        type = ELFW(R_TYPE)(rel->r_info);
        tgt = sym->st_value;
#if SHT_RELX == SHT_RELA
        tgt += rel->r_addend;
#endif
        if (is_dwarf && type == R_DATA_32DW
            && sym->st_shndx >= s1->dwlo && sym->st_shndx < s1->dwhi) {
            /* dwarf section relocation to each other */
            add32le(ptr, tgt - s1->sections[sym->st_shndx]->sh_addr);
            continue;
        }
        addr = s->sh_addr + rel->r_offset;
        relocate(s1, rel, type, ptr, addr, tgt);
    }
#ifndef ELF_OBJ_ONLY
    /* if the relocation is allocated, we change its symbol table */
    if (sr->sh_flags & SHF_ALLOC) {
        sr->link = s1->dynsym;
        if (s1->output_type & TCC_OUTPUT_DYN) {
            size_t r = (uint8_t*)qrel - sr->data;
            if (sizeof ((Stab_Sym*)0)->n_value < PTR_SIZE
                && 0 == strcmp(s->name, ".stab"))
                r = 0; /* cannot apply 64bit relocation to 32bit value */
            sr->data_offset = sr->sh_size = r;
#ifdef CONFIG_TCC_PIE
            if (r && 0 == (s->sh_flags & SHF_WRITE))
                tcc_warning("%d relocations to ro-section %s", (unsigned)(r / sizeof *qrel), s->name);
#endif
        }
    }
#endif
}

/* relocate all sections */
ST_FUNC void relocate_sections(TCCState *s1)
{
    int i;
    Section *s, *sr;

    for (i = 1; i < s1->nb_sections; ++i) {
        sr = s1->sections[i];
        if (sr->sh_type != SHT_RELX)
            continue;
        s = s1->sections[sr->sh_info];
#ifndef TCC_TARGET_MACHO
        if (s != s1->got
            || s1->static_link
            || s1->output_type == TCC_OUTPUT_MEMORY)
#endif
        {
            relocate_section(s1, s, sr);
        }
#ifndef ELF_OBJ_ONLY
        if (sr->sh_flags & SHF_ALLOC) {
            ElfW_Rel *rel;
            /* relocate relocation table in 'sr' */
            for_each_elem(sr, 0, rel, ElfW_Rel)
                rel->r_offset += s->sh_addr;
        }
#endif
    }
}

#ifndef ELF_OBJ_ONLY
/* count the number of dynamic relocations so that we can reserve
   their space */
static int prepare_dynamic_rel(TCCState *s1, Section *sr)
{
    int count = 0;
#if defined(TCC_TARGET_I386) || defined(TCC_TARGET_X86_64) || \
    defined(TCC_TARGET_ARM) || defined(TCC_TARGET_ARM64) || \
    defined(TCC_TARGET_RISCV64)
    ElfW_Rel *rel;
    for_each_elem(sr, 0, rel, ElfW_Rel) {
        int sym_index = ELFW(R_SYM)(rel->r_info);
        int type = ELFW(R_TYPE)(rel->r_info);
        switch(type) {
#if defined(TCC_TARGET_I386)
        case R_386_32:
            if (!get_sym_attr(s1, sym_index, 0)->dyn_index
                && ((ElfW(Sym)*)symtab_section->data + sym_index)->st_shndx == SHN_UNDEF) {
                /* don't fixup unresolved (weak) symbols */
                rel->r_info = ELFW(R_INFO)(sym_index, R_386_RELATIVE);
                break;
            }
#elif defined(TCC_TARGET_X86_64)
        case R_X86_64_32:
        case R_X86_64_32S:
        case R_X86_64_64:
#elif defined(TCC_TARGET_ARM)
        case R_ARM_ABS32:
        case R_ARM_TARGET1:
#elif defined(TCC_TARGET_ARM64)
        case R_AARCH64_ABS32:
        case R_AARCH64_ABS64:
#elif defined(TCC_TARGET_RISCV64)
        case R_RISCV_32:
        case R_RISCV_64:
#endif
            count++;
            break;
#if defined(TCC_TARGET_I386)
        case R_386_PC32:
#elif defined(TCC_TARGET_X86_64)
        case R_X86_64_PC32:
	{
	    ElfW(Sym) *sym = &((ElfW(Sym) *)symtab_section->data)[sym_index];
            /* Hidden defined symbols can and must be resolved locally.
               We're misusing a PLT32 reloc for this, as that's always
               resolved to its address even in shared libs.  */
	    if (sym->st_shndx != SHN_UNDEF &&
		ELFW(ST_VISIBILITY)(sym->st_other) == STV_HIDDEN) {
                rel->r_info = ELFW(R_INFO)(sym_index, R_X86_64_PLT32);
	        break;
	    }
	}
#elif defined(TCC_TARGET_ARM64)
        case R_AARCH64_PREL32:
#endif
            if (s1->output_type != TCC_OUTPUT_DLL)
                break;
            if (get_sym_attr(s1, sym_index, 0)->dyn_index)
                count++;
            break;
        default:
            break;
        }
    }
#endif
    return count;
}
#endif

#ifdef NEED_BUILD_GOT
static int build_got(TCCState *s1)
{
    /* if no got, then create it */
    s1->got = new_section(s1, ".got", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE);
    s1->got->sh_entsize = 4;
    /* keep space for _DYNAMIC pointer and two dummy got entries */
    section_ptr_add(s1->got, 3 * PTR_SIZE);
    return set_elf_sym(symtab_section, 0, 0, ELFW(ST_INFO)(STB_GLOBAL, STT_OBJECT),
        0, s1->got->sh_num, "_GLOBAL_OFFSET_TABLE_");
}

/* Create a GOT and (for function call) a PLT entry corresponding to a symbol
   in s1->symtab. When creating the dynamic symbol table entry for the GOT
   relocation, use 'size' and 'info' for the corresponding symbol metadata.
   Returns the offset of the GOT or (if any) PLT entry. */
static struct sym_attr * put_got_entry(TCCState *s1, int dyn_reloc_type,
                                       int sym_index)
{
    int need_plt_entry;
    const char *name;
    ElfW(Sym) *sym;
    struct sym_attr *attr;
    unsigned got_offset;
    char plt_name[200];
    int len;
    Section *s_rel;

    need_plt_entry = (dyn_reloc_type == R_JMP_SLOT);
    attr = get_sym_attr(s1, sym_index, 1);

    /* In case a function is both called and its address taken 2 GOT entries
       are created, one for taking the address (GOT) and the other for the PLT
       entry (PLTGOT).  */
    if (need_plt_entry ? attr->plt_offset : attr->got_offset)
        return attr;

    s_rel = s1->got;
    if (need_plt_entry) {
        if (!s1->plt) {
            s1->plt = new_section(s1, ".plt", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR);
            s1->plt->sh_entsize = 4;
        }
        s_rel = s1->plt;
    }

    /* create the GOT entry */
    got_offset = s1->got->data_offset;
    section_ptr_add(s1->got, PTR_SIZE);

    /* Create the GOT relocation that will insert the address of the object or
       function of interest in the GOT entry. This is a static relocation for
       memory output (dlsym will give us the address of symbols) and dynamic
       relocation otherwise (executable and DLLs). The relocation should be
       done lazily for GOT entry with *_JUMP_SLOT relocation type (the one
       associated to a PLT entry) but is currently done at load time for an
       unknown reason. */

    sym = &((ElfW(Sym) *) symtab_section->data)[sym_index];
    name = (char *) symtab_section->link->data + sym->st_name;
    //printf("sym %d %s\n", need_plt_entry, name);

    if (s1->dynsym) {
	if (ELFW(ST_BIND)(sym->st_info) == STB_LOCAL) {
	    /* Hack alarm.  We don't want to emit dynamic symbols
	       and symbol based relocs for STB_LOCAL symbols, but rather
	       want to resolve them directly.  At this point the symbol
	       values aren't final yet, so we must defer this.  We will later
	       have to create a RELATIVE reloc anyway, so we misuse the
	       relocation slot to smuggle the symbol reference until
	       fill_local_got_entries.  Not that the sym_index is
	       relative to symtab_section, not s1->dynsym!  Nevertheless
	       we use s1->dyn_sym so that if this is the first call
	       that got->reloc is correctly created.  Also note that
	       RELATIVE relocs are not normally created for the .got,
	       so the types serves as a marker for later (and is retained
	       also for the final output, which is okay because then the
	       got is just normal data).  */
	    put_elf_reloc(s1->dynsym, s1->got, got_offset, R_RELATIVE,
			  sym_index);
	} else {
	    if (0 == attr->dyn_index)
                attr->dyn_index = set_elf_sym(s1->dynsym, sym->st_value,
                                              sym->st_size, sym->st_info, 0,
                                              sym->st_shndx, name);
	    put_elf_reloc(s1->dynsym, s_rel, got_offset, dyn_reloc_type,
			  attr->dyn_index);
	}
    } else {
        put_elf_reloc(symtab_section, s1->got, got_offset, dyn_reloc_type,
                      sym_index);
    }

    if (need_plt_entry) {
        attr->plt_offset = create_plt_entry(s1, got_offset, attr);

        /* create a symbol 'sym@plt' for the PLT jump vector */
        len = strlen(name);
        if (len > sizeof plt_name - 5)
            len = sizeof plt_name - 5;
        memcpy(plt_name, name, len);
        strcpy(plt_name + len, "@plt");
        attr->plt_sym = put_elf_sym(s1->symtab, attr->plt_offset, 0,
            ELFW(ST_INFO)(STB_GLOBAL, STT_FUNC), 0, s1->plt->sh_num, plt_name);
    } else {
        attr->got_offset = got_offset;
    }

    return attr;
}

/* build GOT and PLT entries */
/* Two passes because R_JMP_SLOT should become first. Some targets
   (arm, arm64) do not allow mixing R_JMP_SLOT and R_GLOB_DAT. */
ST_FUNC void build_got_entries(TCCState *s1, int got_sym)
{
    Section *s;
    ElfW_Rel *rel;
    ElfW(Sym) *sym;
    int i, type, gotplt_entry, reloc_type, sym_index;
    struct sym_attr *attr;
    int pass = 0;
redo:
    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        if (s->sh_type != SHT_RELX)
            continue;
        /* no need to handle got relocations */
        if (s->link != symtab_section)
            continue;
        for_each_elem(s, 0, rel, ElfW_Rel) {
            type = ELFW(R_TYPE)(rel->r_info);
            gotplt_entry = gotplt_entry_type(type);
            if (gotplt_entry == -1) {
                tcc_error_noabort ("Unknown relocation type for got: %d", type);
                continue;
            }
            sym_index = ELFW(R_SYM)(rel->r_info);
            sym = &((ElfW(Sym) *)symtab_section->data)[sym_index];

            if (gotplt_entry == NO_GOTPLT_ENTRY) {
                continue;
            }

            /* Automatically create PLT/GOT [entry] if it is an undefined
	       reference (resolved at runtime), or the symbol is absolute,
	       probably created by tcc_add_symbol, and thus on 64-bit
	       targets might be too far from application code.  */
            if (gotplt_entry == AUTO_GOTPLT_ENTRY) {
                if (sym->st_shndx == SHN_UNDEF) {
                    ElfW(Sym) *esym;
		    int dynindex;
                    if (!PCRELATIVE_DLLPLT
                        && (s1->output_type & TCC_OUTPUT_DYN))
                        continue;
		    /* Relocations for UNDEF symbols would normally need
		       to be transferred into the executable or shared object.
		       If that were done AUTO_GOTPLT_ENTRY wouldn't exist.
		       But TCC doesn't do that (at least for exes), so we
		       need to resolve all such relocs locally.  And that
		       means PLT slots for functions in DLLs and COPY relocs for
		       data symbols.  COPY relocs were generated in
		       bind_exe_dynsyms (and the symbol adjusted to be defined),
		       and for functions we were generated a dynamic symbol
		       of function type.  */
		    if (s1->dynsym) {
			/* dynsym isn't set for -run :-/  */
			dynindex = get_sym_attr(s1, sym_index, 0)->dyn_index;
			esym = (ElfW(Sym) *)s1->dynsym->data + dynindex;
			if (dynindex
			    && (ELFW(ST_TYPE)(esym->st_info) == STT_FUNC
				|| (ELFW(ST_TYPE)(esym->st_info) == STT_NOTYPE
				    && ELFW(ST_TYPE)(sym->st_info) == STT_FUNC)))
			    goto jmp_slot;
		    }
                } else if (sym->st_shndx == SHN_ABS) {
                    if (sym->st_value == 0) /* from tcc_add_btstub() */
                        continue;
#ifndef TCC_TARGET_ARM
                    if (PTR_SIZE != 8)
                        continue;
#endif
                    /* from tcc_add_symbol(): on 64 bit platforms these
                       need to go through .got */
                } else
                    continue;
            }

#ifdef TCC_TARGET_X86_64
            if ((type == R_X86_64_PLT32 || type == R_X86_64_PC32) &&
		sym->st_shndx != SHN_UNDEF &&
                (ELFW(ST_VISIBILITY)(sym->st_other) != STV_DEFAULT ||
		 ELFW(ST_BIND)(sym->st_info) == STB_LOCAL ||
		 s1->output_type & TCC_OUTPUT_EXE)) {
		if (pass != 0)
		    continue;
                rel->r_info = ELFW(R_INFO)(sym_index, R_X86_64_PC32);
                continue;
            }
#endif
            reloc_type = code_reloc(type);
            if (reloc_type == -1) {
                tcc_error_noabort ("Unknown relocation type: %d", type);
                continue;
            }

            if (reloc_type != 0) {
        jmp_slot:
	        if (pass != 0)
                    continue;
                reloc_type = R_JMP_SLOT;
            } else {
	        if (pass != 1)
                    continue;
                reloc_type = R_GLOB_DAT;
            }

            if (!s1->got)
                got_sym = build_got(s1);

            if (gotplt_entry == BUILD_GOT_ONLY)
                continue;

            attr = put_got_entry(s1, reloc_type, sym_index);

            if (reloc_type == R_JMP_SLOT)
                rel->r_info = ELFW(R_INFO)(attr->plt_sym, type);
        }
    }
    if (++pass < 2)
        goto redo;
    /* .rel.plt refers to .got actually */
    if (s1->plt && s1->plt->reloc)
        s1->plt->reloc->sh_info = s1->got->sh_num;
    if (got_sym) /* set size */
        ((ElfW(Sym)*)symtab_section->data)[got_sym].st_size = s1->got->data_offset;
}
#endif /* def NEED_BUILD_GOT */

ST_FUNC int set_global_sym(TCCState *s1, const char *name, Section *sec, addr_t offs)
{
    int shn = sec ? sec->sh_num : offs || !name ? SHN_ABS : SHN_UNDEF;
    if (sec && offs == -1)
        offs = sec->data_offset;
    return set_elf_sym(symtab_section, offs, 0,
        ELFW(ST_INFO)(name ? STB_GLOBAL : STB_LOCAL, STT_NOTYPE), 0, shn, name);
}

static void add_init_array_defines(TCCState *s1, const char *section_name)
{
    Section *s;
    addr_t end_offset;
    char buf[1024];
    s = have_section(s1, section_name);
    if (!s || !(s->sh_flags & SHF_ALLOC)) {
        end_offset = 0;
        s = data_section;
    } else {
        end_offset = s->data_offset;
    }
    snprintf(buf, sizeof(buf), "__%s_start", section_name + 1);
    set_global_sym(s1, buf, s, 0);
    snprintf(buf, sizeof(buf), "__%s_end", section_name + 1);
    set_global_sym(s1, buf, s, end_offset);
}

ST_FUNC void add_array (TCCState *s1, const char *sec, int c)
{
    Section *s;
    s = find_section(s1, sec);
    s->sh_flags = shf_RELRO;
    s->sh_type = sec[1] == 'i' ? SHT_INIT_ARRAY : SHT_FINI_ARRAY;
    put_elf_reloc (s1->symtab, s, s->data_offset, R_DATA_PTR, c);
    section_ptr_add(s, PTR_SIZE);
}

#ifdef CONFIG_TCC_BCHECK
ST_FUNC void tcc_add_bcheck(TCCState *s1)
{
    if (0 == s1->do_bounds_check)
        return;
    section_ptr_add(bounds_section, sizeof(addr_t));
}
#endif

/* set symbol to STB_LOCAL and resolve. The point is to not export it as
   a dynamic symbol to allow so's to have one each with a different value. */
static void set_local_sym(TCCState *s1, const char *name, Section *s, int offset)
{
    int c = find_elf_sym(s1->symtab, name);
    if (c) {
        ElfW(Sym) *esym = (ElfW(Sym)*)s1->symtab->data + c;
        esym->st_info = ELFW(ST_INFO)(STB_LOCAL, STT_NOTYPE);
        esym->st_value = offset;
        esym->st_shndx = s->sh_num;
    }
}

/* avoid generating debug/test_coverage code for stub functions */
static void tcc_compile_string_no_debug(TCCState *s, const char *str)
{
    int save_do_debug = s->do_debug;
    int save_test_coverage = s->test_coverage;

    s->do_debug = 0;
    s->test_coverage = 0;
    tcc_compile_string(s, str);
    s->do_debug = save_do_debug;
    s->test_coverage = save_test_coverage;
}

#ifdef CONFIG_TCC_BACKTRACE
static void put_ptr(TCCState *s1, Section *s, int offs)
{
    int c;
    c = set_global_sym(s1, NULL, s, offs);
    s = data_section;
    put_elf_reloc (s1->symtab, s, s->data_offset, R_DATA_PTR, c);
    section_ptr_add(s, PTR_SIZE);
}

ST_FUNC void tcc_add_btstub(TCCState *s1)
{
    Section *s;
    int n, o, *p;
    CString cstr;
    const char *__rt_info = &"___rt_info"[!s1->leading_underscore];

    s = data_section;
    /* Align to PTR_SIZE */
    section_ptr_add(s, -s->data_offset & (PTR_SIZE - 1));
    o = s->data_offset;
    /* create a struct rt_context (see tccrun.c) */
    if (s1->dwarf) {
        put_ptr(s1, dwarf_line_section, 0);
        put_ptr(s1, dwarf_line_section, -1);
	if (s1->dwarf >= 5)
            put_ptr(s1, dwarf_line_str_section, 0);
	else
            put_ptr(s1, dwarf_str_section, 0);
    }
    else
    {
        put_ptr(s1, stab_section, 0);
        put_ptr(s1, stab_section, -1);
        put_ptr(s1, stab_section->link, 0);
    }

    /* skip esym_start/esym_end/elf_str (not loaded) */
    section_ptr_add(s, 3 * PTR_SIZE);

    if (s1->output_type == TCC_OUTPUT_MEMORY && 0 == s1->dwarf) {
        put_ptr(s1, text_section, 0);
    } else {
        /* prog_base : local nameless symbol with offset 0 at SHN_ABS */
        put_ptr(s1, NULL, 0);
#if defined TCC_TARGET_MACHO
        /* adjust for __PAGEZERO */
        if (s1->dwarf == 0 && s1->output_type == TCC_OUTPUT_EXE)
            write64le(data_section->data + data_section->data_offset - PTR_SIZE,
	              (uint64_t)1 << 32);
#endif
    }
    n = 3 * PTR_SIZE;
#ifdef CONFIG_TCC_BCHECK
    if (s1->do_bounds_check) {
        put_ptr(s1, bounds_section, 0);
        n -= PTR_SIZE;
    }
#endif
    section_ptr_add(s, n);
    p = section_ptr_add(s, 2 * sizeof (int));
    p[0] = s1->rt_num_callers;
    p[1] = s1->dwarf;
    // if (s->data_offset - o != 10*PTR_SIZE + 2*sizeof (int)) exit(99);

    if (s1->output_type == TCC_OUTPUT_MEMORY) {
        set_global_sym(s1, __rt_info, s, o);
        return;
    }

    cstr_new(&cstr);
    cstr_printf(&cstr,
        "extern void __bt_init(),__bt_exit(),__bt_init_dll();"
        "static void *__rt_info[];"
        "__attribute__((constructor)) static void __bt_init_rt(){");
#ifdef TCC_TARGET_PE
    if (s1->output_type == TCC_OUTPUT_DLL)
#ifdef CONFIG_TCC_BCHECK
        cstr_printf(&cstr, "__bt_init_dll(%d);", s1->do_bounds_check);
#else
        cstr_printf(&cstr, "__bt_init_dll(0);");
#endif
#endif
    cstr_printf(&cstr, "__bt_init(__rt_info,%d);}",
        s1->output_type != TCC_OUTPUT_DLL);
    /* In case dlcose is called by application */
    cstr_printf(&cstr,
        "__attribute__((destructor)) static void __bt_exit_rt(){"
        "__bt_exit(__rt_info);}");
    tcc_compile_string_no_debug(s1, cstr.data);
    cstr_free(&cstr);
    set_local_sym(s1, __rt_info, s, o);
}
#endif /* def CONFIG_TCC_BACKTRACE */

static void tcc_tcov_add_file(TCCState *s1, const char *filename)
{
    CString cstr;
    void *ptr;
    char wd[1024];

    if (tcov_section == NULL)
        return;
    section_ptr_add(tcov_section, 1);
    write32le (tcov_section->data, tcov_section->data_offset);

    cstr_new (&cstr);
    if (filename[0] == '/')
        cstr_printf (&cstr, "%s.tcov", filename);
    else {
        getcwd (wd, sizeof(wd));
        cstr_printf (&cstr, "%s/%s.tcov", wd, filename);
    }
    ptr = section_ptr_add(tcov_section, cstr.size + 1);
    strcpy((char *)ptr, cstr.data);
    unlink((char *)ptr);
#ifdef _WIN32
    normalize_slashes((char *)ptr);
#endif
    cstr_free (&cstr);

    cstr_new(&cstr);
    cstr_printf(&cstr,
        "extern char *__tcov_data[];"
        "extern void __store_test_coverage ();"
        "__attribute__((destructor)) static void __tcov_exit() {"
        "__store_test_coverage(__tcov_data);"
        "}");
    tcc_compile_string_no_debug(s1, cstr.data);
    cstr_free(&cstr);
    set_local_sym(s1, &"___tcov_data"[!s1->leading_underscore], tcov_section, 0);
}

#if !defined TCC_TARGET_PE && !defined TCC_TARGET_MACHO
/* add libc crt1/crti objects */
ST_FUNC void tccelf_add_crtbegin(TCCState *s1)
{
#if TARGETOS_OpenBSD
    if (s1->output_type != TCC_OUTPUT_DLL)
        tcc_add_crt(s1, "crt0.o");
    if (s1->output_type == TCC_OUTPUT_DLL)
        tcc_add_crt(s1, "crtbeginS.o");
    else
        tcc_add_crt(s1, "crtbegin.o");
#elif TARGETOS_FreeBSD || TARGETOS_NetBSD
    if (s1->output_type != TCC_OUTPUT_DLL)
#if TARGETOS_FreeBSD
        tcc_add_crt(s1, "crt1.o");
#else
        tcc_add_crt(s1, "crt0.o");
#endif
    tcc_add_crt(s1, "crti.o");
    if (s1->static_link)
        tcc_add_crt(s1, "crtbeginT.o");
    else if (s1->output_type == TCC_OUTPUT_DLL)
        tcc_add_crt(s1, "crtbeginS.o");
    else
        tcc_add_crt(s1, "crtbegin.o");
#elif TARGETOS_ANDROID
    if (s1->output_type == TCC_OUTPUT_DLL)
        tcc_add_crt(s1, "crtbegin_so.o");
    else
        tcc_add_crt(s1, "crtbegin_dynamic.o");
#else
    if (s1->output_type != TCC_OUTPUT_DLL)
        tcc_add_crt(s1, "crt1.o");
    tcc_add_crt(s1, "crti.o");
#endif
}

ST_FUNC void tccelf_add_crtend(TCCState *s1)
{
#if TARGETOS_OpenBSD
    if (s1->output_type == TCC_OUTPUT_DLL)
        tcc_add_crt(s1, "crtendS.o");
    else
        tcc_add_crt(s1, "crtend.o");
#elif TARGETOS_FreeBSD || TARGETOS_NetBSD
    if (s1->output_type == TCC_OUTPUT_DLL)
        tcc_add_crt(s1, "crtendS.o");
    else
        tcc_add_crt(s1, "crtend.o");
    tcc_add_crt(s1, "crtn.o");
#elif TARGETOS_ANDROID
    if (s1->output_type == TCC_OUTPUT_DLL)
        tcc_add_crt(s1, "crtend_so.o");
    else
        tcc_add_crt(s1, "crtend_android.o");
#else
    tcc_add_crt(s1, "crtn.o");
#endif
}
#endif /* !defined TCC_TARGET_PE && !defined TCC_TARGET_MACHO */

#ifndef TCC_TARGET_PE
/* add tcc runtime libraries */
ST_FUNC void tcc_add_runtime(TCCState *s1)
{
    s1->filetype = 0;

#ifdef CONFIG_TCC_BCHECK
    tcc_add_bcheck(s1);
#endif
    tcc_add_pragma_libs(s1);

    /* add libc */
    if (!s1->nostdlib) {
        int lpthread = s1->option_pthread;

#ifdef CONFIG_TCC_BCHECK
        if (s1->do_bounds_check && s1->output_type != TCC_OUTPUT_DLL) {
            tcc_add_support(s1, "bcheck.o");
# if !(TARGETOS_OpenBSD || TARGETOS_NetBSD)
            tcc_add_library_err(s1, "dl");
# endif
            lpthread = 1;
        }
#endif
#ifdef CONFIG_TCC_BACKTRACE
        if (s1->do_backtrace) {
            if (s1->output_type & TCC_OUTPUT_EXE)
                tcc_add_support(s1, "bt-exe.o");
            if (s1->output_type != TCC_OUTPUT_DLL)
                tcc_add_support(s1, "bt-log.o");
            tcc_add_btstub(s1);
            lpthread = 1;
        }
#endif
        if (lpthread)
            tcc_add_library_err(s1, "pthread");
        tcc_add_library_err(s1, "c");
#ifdef TCC_LIBGCC
        if (!s1->static_link) {
            if (TCC_LIBGCC[0] == '/')
                tcc_add_file(s1, TCC_LIBGCC);
            else
                tcc_add_dll(s1, TCC_LIBGCC, AFF_PRINT_ERROR);
        }
#endif
#if defined TCC_TARGET_ARM && TARGETOS_FreeBSD
        tcc_add_library_err(s1, "gcc_s"); // unwind code
#endif
        if (TCC_LIBTCC1[0])
            tcc_add_support(s1, TCC_LIBTCC1);
#ifndef TCC_TARGET_MACHO
        if (s1->output_type != TCC_OUTPUT_MEMORY)
            tccelf_add_crtend(s1);
#endif
    }
}
#endif /* ndef TCC_TARGET_PE */

/* add various standard linker symbols (must be done after the
   sections are filled (for example after allocating common
   symbols)) */
static void tcc_add_linker_symbols(TCCState *s1)
{
    char buf[1024];
    int i;
    Section *s;

    set_global_sym(s1, "_etext", text_section, -1);
    set_global_sym(s1, "_edata", data_section, -1);
    set_global_sym(s1, "_end", bss_section, -1);
#if TARGETOS_OpenBSD
    set_global_sym(s1, "__executable_start", NULL, ELF_START_ADDR);
#endif
#ifdef TCC_TARGET_RISCV64
    /* XXX should be .sdata+0x800, not .data+0x800 */
    set_global_sym(s1, "__global_pointer$", data_section, 0x800);
#endif
    /* horrible new standard ldscript defines */
    add_init_array_defines(s1, ".preinit_array");
    add_init_array_defines(s1, ".init_array");
    add_init_array_defines(s1, ".fini_array");
    /* add start and stop symbols for sections whose name can be
       expressed in C */
    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        if ((s->sh_flags & SHF_ALLOC)
            && (s->sh_type == SHT_PROGBITS
                || s->sh_type == SHT_STRTAB)) {
            const char *p;
            /* check if section name can be expressed in C */
            p = s->name;
            for(;;) {
                int c = *p;
                if (!c)
                    break;
                if (!isid(c) && !isnum(c))
                    goto next_sec;
                p++;
            }
            snprintf(buf, sizeof(buf), "__start_%s", s->name);
            set_global_sym(s1, buf, s, 0);
            snprintf(buf, sizeof(buf), "__stop_%s", s->name);
            set_global_sym(s1, buf, s, -1);
        }
    next_sec: ;
    }
}

ST_FUNC void resolve_common_syms(TCCState *s1)
{
    ElfW(Sym) *sym;

    /* Allocate common symbols in BSS.  */
    for_each_elem(symtab_section, 1, sym, ElfW(Sym)) {
        if (sym->st_shndx == SHN_COMMON) {
            /* symbol alignment is in st_value for SHN_COMMONs */
	    sym->st_value = section_add(bss_section, sym->st_size,
					sym->st_value);
            sym->st_shndx = bss_section->sh_num;
        }
    }

    /* Now assign linker provided symbols their value.  */
    tcc_add_linker_symbols(s1);
}

#ifndef ELF_OBJ_ONLY
ST_FUNC void fill_got_entry(TCCState *s1, ElfW_Rel *rel)
{
    int sym_index = ELFW(R_SYM) (rel->r_info);
    ElfW(Sym) *sym = &((ElfW(Sym) *) symtab_section->data)[sym_index];
    struct sym_attr *attr = get_sym_attr(s1, sym_index, 0);
    unsigned offset = attr->got_offset;

    if (0 == offset)
        return;
    section_reserve(s1->got, offset + PTR_SIZE);
#if PTR_SIZE == 8
    write64le(s1->got->data + offset, sym->st_value);
#else
    write32le(s1->got->data + offset, sym->st_value);
#endif
}

/* Perform relocation to GOT or PLT entries */
ST_FUNC void fill_got(TCCState *s1)
{
    Section *s;
    ElfW_Rel *rel;
    int i;

    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        if (s->sh_type != SHT_RELX)
            continue;
        /* no need to handle got relocations */
        if (s->link != symtab_section)
            continue;
        for_each_elem(s, 0, rel, ElfW_Rel) {
            switch (ELFW(R_TYPE) (rel->r_info)) {
                case R_X86_64_GOT32:
                case R_X86_64_GOTPCREL:
		case R_X86_64_GOTPCRELX:
		case R_X86_64_REX_GOTPCRELX:
                case R_X86_64_PLT32:
                    fill_got_entry(s1, rel);
                    break;
            }
        }
    }
}

/* See put_got_entry for a description.  This is the second stage
   where GOT references to local defined symbols are rewritten.  */
static void fill_local_got_entries(TCCState *s1)
{
    ElfW_Rel *rel;
    if (!s1->got->reloc)
        return;
    for_each_elem(s1->got->reloc, 0, rel, ElfW_Rel) {
	if (ELFW(R_TYPE)(rel->r_info) == R_RELATIVE) {
	    int sym_index = ELFW(R_SYM) (rel->r_info);
	    ElfW(Sym) *sym = &((ElfW(Sym) *) symtab_section->data)[sym_index];
	    struct sym_attr *attr = get_sym_attr(s1, sym_index, 0);
	    unsigned offset = attr->got_offset;
	    if (offset != rel->r_offset - s1->got->sh_addr)
	        tcc_error_noabort("fill_local_got_entries: huh?");
	    rel->r_info = ELFW(R_INFO)(0, R_RELATIVE);
#if SHT_RELX == SHT_RELA
	    rel->r_addend = sym->st_value;
#else
	    /* All our REL architectures also happen to be 32bit LE.  */
	    write32le(s1->got->data + offset, sym->st_value);
#endif
	}
    }
}

/* Bind symbols of executable: resolve undefined symbols from exported symbols
   in shared libraries */
static void bind_exe_dynsyms(TCCState *s1, int is_PIE)
{
    const char *name;
    int sym_index, index;
    ElfW(Sym) *sym, *esym;
    int type;

    /* Resolve undefined symbols from dynamic symbols. When there is a match:
       - if STT_FUNC or STT_GNU_IFUNC symbol -> add it in PLT
       - if STT_OBJECT symbol -> add it in .bss section with suitable reloc */
    for_each_elem(symtab_section, 1, sym, ElfW(Sym)) {
        if (sym->st_shndx == SHN_UNDEF) {
            name = (char *) symtab_section->link->data + sym->st_name;
            sym_index = find_elf_sym(s1->dynsymtab_section, name);
            if (sym_index) {
                if (is_PIE)
                    continue;
                esym = &((ElfW(Sym) *)s1->dynsymtab_section->data)[sym_index];
                type = ELFW(ST_TYPE)(esym->st_info);
                if ((type == STT_FUNC) || (type == STT_GNU_IFUNC)) {
                    /* Indirect functions shall have STT_FUNC type in executable
                     * dynsym section. Indeed, a dlsym call following a lazy
                     * resolution would pick the symbol value from the
                     * executable dynsym entry which would contain the address
                     * of the function wanted by the caller of dlsym instead of
                     * the address of the function that would return that
                     * address */
                    int dynindex
		      = put_elf_sym(s1->dynsym, 0, esym->st_size,
				    ELFW(ST_INFO)(STB_GLOBAL,STT_FUNC), 0, 0,
				    name);
		    int index = sym - (ElfW(Sym) *) symtab_section->data;
		    get_sym_attr(s1, index, 1)->dyn_index = dynindex;
                } else if (type == STT_OBJECT) {
                    unsigned long offset;
                    ElfW(Sym) *dynsym;
                    offset = bss_section->data_offset;
                    /* XXX: which alignment ? */
                    offset = (offset + 16 - 1) & -16;
                    set_elf_sym (s1->symtab, offset, esym->st_size,
                                 esym->st_info, 0, bss_section->sh_num, name);
                    index = put_elf_sym(s1->dynsym, offset, esym->st_size,
                                        esym->st_info, 0, bss_section->sh_num,
                                        name);

                    /* Ensure R_COPY works for weak symbol aliases */
                    if (ELFW(ST_BIND)(esym->st_info) == STB_WEAK) {
                        for_each_elem(s1->dynsymtab_section, 1, dynsym, ElfW(Sym)) {
                            if ((dynsym->st_value == esym->st_value)
                                && (ELFW(ST_BIND)(dynsym->st_info) == STB_GLOBAL)) {
                                char *dynname = (char *) s1->dynsymtab_section->link->data
                                                + dynsym->st_name;
                                put_elf_sym(s1->dynsym, offset, dynsym->st_size,
                                            dynsym->st_info, 0,
                                            bss_section->sh_num, dynname);
                                break;
                            }
                        }
                    }

                    put_elf_reloc(s1->dynsym, bss_section,
                                  offset, R_COPY, index);
                    offset += esym->st_size;
                    bss_section->data_offset = offset;
                }
            } else {
                /* STB_WEAK undefined symbols are accepted */
                /* XXX: _fp_hw seems to be part of the ABI, so we ignore it */
                if (ELFW(ST_BIND)(sym->st_info) == STB_WEAK ||
                    !strcmp(name, "_fp_hw")) {
                } else {
                    tcc_error_noabort("undefined symbol '%s'", name);
                }
            }
        }
    }
}

/* Bind symbols of libraries: export all non local symbols of executable that
   are referenced by shared libraries. The reason is that the dynamic loader
   search symbol first in executable and then in libraries. Therefore a
   reference to a symbol already defined by a library can still be resolved by
   a symbol in the executable.   With -rdynamic, export all defined symbols */
static void bind_libs_dynsyms(TCCState *s1)
{
    const char *name;
    int dynsym_index;
    ElfW(Sym) *sym, *esym;

    for_each_elem(symtab_section, 1, sym, ElfW(Sym)) {
        name = (char *)symtab_section->link->data + sym->st_name;
        dynsym_index = find_elf_sym(s1->dynsymtab_section, name);
        if (sym->st_shndx != SHN_UNDEF) {
            if (ELFW(ST_BIND)(sym->st_info) != STB_LOCAL
                && (dynsym_index || s1->rdynamic))
                set_elf_sym(s1->dynsym, sym->st_value, sym->st_size,
                            sym->st_info, 0, sym->st_shndx, name);
        } else if (dynsym_index) {
            esym = (ElfW(Sym) *)s1->dynsymtab_section->data + dynsym_index;
            if (esym->st_shndx == SHN_UNDEF) {
                /* weak symbols can stay undefined */
                if (ELFW(ST_BIND)(esym->st_info) != STB_WEAK)
                    tcc_warning("undefined dynamic symbol '%s'", name);
            }
        }
    }
}

/* Export all non local symbols. This is used by shared libraries so that the
   non local symbols they define can resolve a reference in another shared
   library or in the executable. Correspondingly, it allows undefined local
   symbols to be resolved by other shared libraries or by the executable. */
static void export_global_syms(TCCState *s1)
{
    int dynindex, index;
    const char *name;
    ElfW(Sym) *sym;
    for_each_elem(symtab_section, 1, sym, ElfW(Sym)) {
        if (ELFW(ST_BIND)(sym->st_info) != STB_LOCAL) {
	    name = (char *) symtab_section->link->data + sym->st_name;
	    dynindex = set_elf_sym(s1->dynsym, sym->st_value, sym->st_size,
				   sym->st_info, 0, sym->st_shndx, name);
	    index = sym - (ElfW(Sym) *) symtab_section->data;
            get_sym_attr(s1, index, 1)->dyn_index = dynindex;
        }
    }
}

/* decide if an unallocated section should be output. */
static int set_sec_sizes(TCCState *s1)
{
    int i;
    Section *s;
    int textrel = 0;
    int file_type = s1->output_type;

    /* Allocate strings for section names */
    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        if (s->sh_type == SHT_RELX && !(s->sh_flags & SHF_ALLOC)) {
            /* when generating a DLL, we include relocations but
               we may patch them */
            if ((file_type & TCC_OUTPUT_DYN)
                && (s1->sections[s->sh_info]->sh_flags & SHF_ALLOC)) {
                int count = prepare_dynamic_rel(s1, s);
                if (count) {
                    /* allocate the section */
                    s->sh_flags |= SHF_ALLOC;
                    s->sh_size = count * sizeof(ElfW_Rel);
                    if (!(s1->sections[s->sh_info]->sh_flags & SHF_WRITE))
                        textrel += count;
                }
            }
        } else if ((s->sh_flags & SHF_ALLOC)
#ifdef TCC_TARGET_ARM
                   || s->sh_type == SHT_ARM_ATTRIBUTES
#endif
                   || s1->do_debug) {
            s->sh_size = s->data_offset;
        }

#ifdef TCC_TARGET_ARM
        /* XXX: Suppress stack unwinding section. */
        if (s->sh_type == SHT_ARM_EXIDX) {
            s->sh_flags = 0;
            s->sh_size = 0;
        }
#endif

    }
    return textrel;
}

/* various data used under elf_output_file() */
struct dyn_inf {
    Section *dynamic;
    Section *dynstr;
    struct {
        /* Info to be copied in dynamic section */
        unsigned long data_offset;
        addr_t rel_addr;
        addr_t rel_size;
    };

    ElfW(Phdr) *phdr;
    int phnum;
    Section *interp;
    Section *note;
    Section *gnu_hash;

    /* read only segment mapping for GNU_RELRO */
    Section _roinf, *roinf;
};

/* Decide the layout of sections loaded in memory. This must be done before
   program headers are filled since they contain info about the layout.
   We do the following ordering: interp, symbol tables, relocations, progbits,
   nobits */
static int sort_sections(TCCState *s1, int *sec_order, Section *interp)
{
    Section *s;
    int i, j, k, f, f0, n;
    int nb_sections = s1->nb_sections;
    int *sec_cls = sec_order + nb_sections;

    for (i = 1; i < nb_sections; i++) {
        s = s1->sections[i];
        if (s->sh_flags & SHF_ALLOC) {
            j = 0x100;
            if (s->sh_flags & SHF_WRITE)
                j = 0x200;
            if (s->sh_flags & SHF_TLS)
                j += 0x200;
        } else if (s->sh_name) {
            j = 0x700;
        } else {
            j = 0x900; /* no sh_name: won't go to file */
        }
        if (s->sh_type == SHT_SYMTAB || s->sh_type == SHT_DYNSYM) {
            k = 0x10;
        } else if (s->sh_type == SHT_STRTAB && strcmp(s->name, ".stabstr")) {
            k = 0x11;
            if (i == nb_sections - 1) /* ".shstrtab" assumed to remain last */
                k = 0xff;
        } else if (s->sh_type == SHT_HASH || s->sh_type == SHT_GNU_HASH) {
            k = 0x12;
        } else if (s->sh_type == SHT_RELX) {
            k = 0x20;
            if (s1->plt && s == s1->plt->reloc)
                k = 0x21;
        } else if (s->sh_type == SHT_PREINIT_ARRAY) {
            k = 0x41;
        } else if (s->sh_type == SHT_INIT_ARRAY) {
            k = 0x42;
        } else if (s->sh_type == SHT_FINI_ARRAY) {
            k = 0x43;
#ifdef CONFIG_TCC_BCHECK
        } else if (s == bounds_section || s == lbounds_section) {
            k = 0x44;
#endif
        } else if (s == rodata_section || 0 == strcmp(s->name, ".data.rel.ro")) {
            k = 0x45;
        } else if (s->sh_type == SHT_DYNAMIC) {
            k = 0x46;
        } else if (s == s1->got) {
            k = 0x47; /* .got as RELRO needs BIND_NOW in DT_FLAGS */
        } else {
            k = 0x50;
            if (s->sh_type == SHT_NOTE)
                k = 0x60;
            if (s->sh_flags & SHF_EXECINSTR)
                k = 0x70;
            if (s->sh_type == SHT_NOBITS)
                k = 0x80;
            if (s == interp)
                k = 0x00;
        }
        k += j;

        for (n = i; n > 1 && k < (f = sec_cls[n - 1]); --n)
            sec_cls[n] = f, sec_order[n] = sec_order[n - 1];
        sec_cls[n] = k, sec_order[n] = i;
    }
    sec_order[0] = 0;

    /* count PT_LOAD headers needed */
    n = f0 = 0;
    for (i = 1; i < nb_sections; i++) {
        s = s1->sections[sec_order[i]];
        k = sec_cls[i];
        f = 0;
        if (k < 0x700) {
            f = s->sh_flags & (SHF_ALLOC|SHF_WRITE|SHF_EXECINSTR|SHF_TLS);
#if TARGETOS_NetBSD
	    /* NetBSD only supports 2 PT_LOAD sections.
	       See: https://blog.netbsd.org/tnf/entry/the_first_report_on_lld */
	    if ((f & SHF_WRITE) == 0) f |= SHF_EXECINSTR;
#else
            if ((k & 0xfff0) == 0x240) /* RELRO sections */
                f |= 1<<4;
#endif
            if (f != f0) /* start new header when flags changed or relro */
                f0 = f, ++n, f |= 1<<8;
        }
        sec_cls[i] = f;
        //printf("ph %d sec %02d : %3X %3X  %8.2X  %04X  %s\n", !!f * n, i, f, k, s->sh_type, s->sh_size, s->name);
    }
    return n;
}

static ElfW(Phdr) *fill_phdr(ElfW(Phdr) *ph, int type, Section *s)
{
    if (s) {
        ph->p_offset = s->sh_offset;
        ph->p_vaddr = s->sh_addr;
        ph->p_filesz = s->sh_size;
        ph->p_align = s->sh_addralign;
    }
    ph->p_type = type;
    ph->p_flags = PF_R;
    ph->p_paddr = ph->p_vaddr;
    ph->p_memsz = ph->p_filesz;
    return ph;
}

/* Assign sections to segments and decide how are sections laid out when loaded
   in memory. This function also fills corresponding program headers. */
static int layout_sections(TCCState *s1, int *sec_order, struct dyn_inf *d)
{
    Section *s;
    addr_t addr, tmp, align, s_align, base;
    ElfW(Phdr) *ph = NULL;
    int i, f, n, phnum, phfill;
    int file_offset;

    /* compute number of program headers */
    phnum = sort_sections(s1, sec_order, d->interp);
    phfill = 0; /* set to 1 to have dll's with a PT_PHDR */
    if (d->interp)
        phfill = 2;
    phnum += phfill;
    if (d->note)
        ++phnum;
    if (d->dynamic)
        ++phnum;
    if (d->roinf)
        ++phnum;
    d->phnum = phnum;
    d->phdr = tcc_mallocz(phnum * sizeof(ElfW(Phdr)));

    file_offset = 0;
    if (s1->output_format == TCC_OUTPUT_FORMAT_ELF)
        file_offset = sizeof(ElfW(Ehdr)) + phnum * sizeof(ElfW(Phdr));

    s_align = ELF_PAGE_SIZE;
    if (s1->section_align)
        s_align = s1->section_align;

    addr = ELF_START_ADDR;
    if (s1->output_type & TCC_OUTPUT_DYN)
        addr = 0;

    if (s1->has_text_addr) {
        addr = s1->text_addr;
        if (0) {
            int a_offset, p_offset;
            /* we ensure that (addr % ELF_PAGE_SIZE) == file_offset %
               ELF_PAGE_SIZE */
            a_offset = (int) (addr & (s_align - 1));
            p_offset = file_offset & (s_align - 1);
            if (a_offset < p_offset)
                a_offset += s_align;
            file_offset += (a_offset - p_offset);
        }
    }
    base = addr;
    /* compute address after headers */
    addr = addr + (file_offset & (s_align - 1));

    n = 0;
    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[sec_order[i]];
        f = sec_order[i + s1->nb_sections];
        align = s->sh_addralign - 1;

        if (f == 0) { /* no alloc */
            file_offset = (file_offset + align) & ~align;
            s->sh_offset = file_offset;
            if (s->sh_type != SHT_NOBITS)
                file_offset += s->sh_size;
            continue;
        }

        if ((f & 1<<8) && n) {
            /* different rwx section flags */
            if (s1->output_format == TCC_OUTPUT_FORMAT_ELF) {
                /* if in the middle of a page, w e duplicate the page in
                   memory so that one copy is RX and the other is RW */
                if ((addr & (s_align - 1)) != 0)
                    addr += s_align;
            } else {
                align = s_align - 1;
            }
        }

        tmp = addr;
        addr = (addr + align) & ~align;
        file_offset += (int)(addr - tmp);
        s->sh_offset = file_offset;
        s->sh_addr = addr;

        if (f & 1<<8) {
            /* set new program header */
            ph = &d->phdr[phfill + n];
            ph->p_type = PT_LOAD;
            ph->p_align = s_align;
            ph->p_flags = PF_R;
            if (f & SHF_WRITE)
                ph->p_flags |= PF_W;
            if (f & SHF_EXECINSTR)
                ph->p_flags |= PF_X;
            if (f & SHF_TLS) {
                ph->p_type = PT_TLS;
                ph->p_align = align + 1;
            }

            ph->p_offset = file_offset;
            ph->p_vaddr = addr;
            if (n == 0) {
		/* Make the first PT_LOAD segment include the program
		   headers itself (and the ELF header as well), it'll
		   come out with same memory use but will make various
		   tools like binutils strip work better.  */
		ph->p_offset = 0;
		ph->p_vaddr = base;
            }
            ph->p_paddr = ph->p_vaddr;
            ++n;
        }

        if (f & 1<<4) {
            Section *roinf = &d->_roinf;
            if (roinf->sh_size == 0) {
                roinf->sh_offset = s->sh_offset;
                roinf->sh_addr = s->sh_addr;
                roinf->sh_addralign = 1;
	    }
            roinf->sh_size = (addr - roinf->sh_addr) + s->sh_size;
        }

        addr += s->sh_size;
        if (s->sh_type != SHT_NOBITS)
            file_offset += s->sh_size;

        ph->p_filesz = file_offset - ph->p_offset;
        ph->p_memsz = addr - ph->p_vaddr;
    }

    /* Fill other headers */
    if (d->note)
        fill_phdr(++ph, PT_NOTE, d->note);
    if (d->dynamic)
        fill_phdr(++ph, PT_DYNAMIC, d->dynamic)->p_flags |= PF_W;
    if (d->roinf)
        fill_phdr(++ph, PT_GNU_RELRO, d->roinf)->p_flags |= PF_W;
    if (d->interp)
        fill_phdr(&d->phdr[1], PT_INTERP, d->interp);
    if (phfill) {
        ph = &d->phdr[0];
        ph->p_offset = sizeof(ElfW(Ehdr));
        ph->p_vaddr = base + ph->p_offset;
        ph->p_filesz = phnum * sizeof(ElfW(Phdr));
        ph->p_align = 4;
        fill_phdr(ph, PT_PHDR, NULL);
    }
    return file_offset;
}

/* put dynamic tag */
static void put_dt(Section *dynamic, int dt, addr_t val)
{
    ElfW(Dyn) *dyn;
    dyn = section_ptr_add(dynamic, sizeof(ElfW(Dyn)));
    dyn->d_tag = dt;
    dyn->d_un.d_val = val;
}

/* Fill the dynamic section with tags describing the address and size of
   sections */
static void fill_dynamic(TCCState *s1, struct dyn_inf *dyninf)
{
    Section *dynamic = dyninf->dynamic;
    Section *s;

    /* put dynamic section entries */
    put_dt(dynamic, DT_HASH, s1->dynsym->hash->sh_addr);
    put_dt(dynamic, DT_GNU_HASH, dyninf->gnu_hash->sh_addr);
    put_dt(dynamic, DT_STRTAB, dyninf->dynstr->sh_addr);
    put_dt(dynamic, DT_SYMTAB, s1->dynsym->sh_addr);
    put_dt(dynamic, DT_STRSZ, dyninf->dynstr->data_offset);
    put_dt(dynamic, DT_SYMENT, sizeof(ElfW(Sym)));
#if PTR_SIZE == 8
    put_dt(dynamic, DT_RELA, dyninf->rel_addr);
    put_dt(dynamic, DT_RELASZ, dyninf->rel_size);
    put_dt(dynamic, DT_RELAENT, sizeof(ElfW_Rel));
    if (s1->plt && s1->plt->reloc) {
        put_dt(dynamic, DT_PLTGOT, s1->got->sh_addr);
        put_dt(dynamic, DT_PLTRELSZ, s1->plt->reloc->data_offset);
        put_dt(dynamic, DT_JMPREL, s1->plt->reloc->sh_addr);
        put_dt(dynamic, DT_PLTREL, DT_RELA);
    }
    put_dt(dynamic, DT_RELACOUNT, 0);
#else
    put_dt(dynamic, DT_REL, dyninf->rel_addr);
    put_dt(dynamic, DT_RELSZ, dyninf->rel_size);
    put_dt(dynamic, DT_RELENT, sizeof(ElfW_Rel));
    if (s1->plt && s1->plt->reloc) {
        put_dt(dynamic, DT_PLTGOT, s1->got->sh_addr);
        put_dt(dynamic, DT_PLTRELSZ, s1->plt->reloc->data_offset);
        put_dt(dynamic, DT_JMPREL, s1->plt->reloc->sh_addr);
        put_dt(dynamic, DT_PLTREL, DT_REL);
    }
    put_dt(dynamic, DT_RELCOUNT, 0);
#endif
    if (versym_section && verneed_section) {
	/* The dynamic linker can not handle VERSYM without VERNEED */
        put_dt(dynamic, DT_VERSYM, versym_section->sh_addr);
        put_dt(dynamic, DT_VERNEED, verneed_section->sh_addr);
        put_dt(dynamic, DT_VERNEEDNUM, dt_verneednum);
    }
    s = have_section(s1, ".preinit_array");
    if (s && s->data_offset) {
        put_dt(dynamic, DT_PREINIT_ARRAY, s->sh_addr);
        put_dt(dynamic, DT_PREINIT_ARRAYSZ, s->data_offset);
    }
    s = have_section(s1, ".init_array");
    if (s && s->data_offset) {
        put_dt(dynamic, DT_INIT_ARRAY, s->sh_addr);
        put_dt(dynamic, DT_INIT_ARRAYSZ, s->data_offset);
    }
    s = have_section(s1, ".fini_array");
    if (s && s->data_offset) {
        put_dt(dynamic, DT_FINI_ARRAY, s->sh_addr);
        put_dt(dynamic, DT_FINI_ARRAYSZ, s->data_offset);
    }
    s = have_section(s1, ".init");
    if (s && s->data_offset) {
        put_dt(dynamic, DT_INIT, s->sh_addr);
    }
    s = have_section(s1, ".fini");
    if (s && s->data_offset) {
        put_dt(dynamic, DT_FINI, s->sh_addr);
    }
    if (s1->do_debug)
        put_dt(dynamic, DT_DEBUG, 0);
    put_dt(dynamic, DT_NULL, 0);
}

/* Remove gaps between RELX sections.
   These gaps are a result of final_sections_reloc. Here some relocs are removed.
   The gaps are then filled with 0 in tcc_output_elf. The 0 is intepreted as
   R_...NONE reloc. This does work on most targets but on OpenBSD/arm64 this
   is illegal. OpenBSD/arm64 does not support R_...NONE reloc. */
static void update_reloc_sections(TCCState *s1, struct dyn_inf *dyninf)
{
    int i;
    unsigned long file_offset = 0;
    Section *s;
    Section *relocplt = s1->plt ? s1->plt->reloc : NULL;

    /* dynamic relocation table information, for .dynamic section */
    dyninf->rel_addr = dyninf->rel_size = 0;

    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
	if (s->sh_type == SHT_RELX && s != relocplt) {
	    if (dyninf->rel_size == 0) {
		dyninf->rel_addr = s->sh_addr;
		file_offset = s->sh_offset;
	    }
	    else {
		s->sh_addr = dyninf->rel_addr + dyninf->rel_size;
		s->sh_offset = file_offset + dyninf->rel_size;
	    }
	    dyninf->rel_size += s->sh_size;
	}
    }
}

static int tidy_section_headers(TCCState *s1, int *sec_order);
#endif /* ndef ELF_OBJ_ONLY */

/* Create an ELF file on disk.
   This function handle ELF specific layout requirements */
static int tcc_output_elf(TCCState *s1, FILE *f, int phnum, ElfW(Phdr) *phdr,
                           int file_offset, int *sec_order)
{
    int i, shnum, offset, size, file_type;
    Section *s;
    ElfW(Ehdr) ehdr;
    ElfW(Shdr) shdr, *sh;

    file_type = s1->output_type;
    shnum = s1->nb_sections;

    memset(&ehdr, 0, sizeof(ehdr));

    if (phnum > 0) {
        ehdr.e_phentsize = sizeof(ElfW(Phdr));
        ehdr.e_phnum = phnum;
        ehdr.e_phoff = sizeof(ElfW(Ehdr));
#ifndef ELF_OBJ_ONLY
        shnum = tidy_section_headers(s1, sec_order);
#endif
    }

    /* align to 4 */
    file_offset = (file_offset + 3) & -4;

    /* fill header */
    ehdr.e_ident[0] = ELFMAG0;
    ehdr.e_ident[1] = ELFMAG1;
    ehdr.e_ident[2] = ELFMAG2;
    ehdr.e_ident[3] = ELFMAG3;
    ehdr.e_ident[4] = ELFCLASSW;
    ehdr.e_ident[5] = ELFDATA2LSB;
    ehdr.e_ident[6] = EV_CURRENT;

#if TARGETOS_FreeBSD || TARGETOS_FreeBSD_kernel
    ehdr.e_ident[EI_OSABI] = ELFOSABI_FREEBSD;
#elif defined TCC_TARGET_ARM && defined TCC_ARM_EABI
    ehdr.e_flags = EF_ARM_EABI_VER5;
    ehdr.e_flags |= s1->float_abi == ARM_HARD_FLOAT
        ? EF_ARM_VFP_FLOAT : EF_ARM_SOFT_FLOAT;
#elif defined TCC_TARGET_ARM
    ehdr.e_ident[EI_OSABI] = ELFOSABI_ARM;
#elif defined TCC_TARGET_RISCV64
    /* XXX should be configurable */
    ehdr.e_flags = EF_RISCV_FLOAT_ABI_DOUBLE;
#endif

    if (file_type == TCC_OUTPUT_OBJ) {
        ehdr.e_type = ET_REL;
    } else {
        if (file_type & TCC_OUTPUT_DYN)
            ehdr.e_type = ET_DYN;
        else
            ehdr.e_type = ET_EXEC;
        if (s1->elf_entryname)
            ehdr.e_entry = get_sym_addr(s1, s1->elf_entryname, 1, 0);
        else
            ehdr.e_entry = get_sym_addr(s1, "_start", !!(file_type & TCC_OUTPUT_EXE), 0);
        if (ehdr.e_entry == (addr_t)-1)
            ehdr.e_entry = text_section->sh_addr;
        if (s1->nb_errors)
            return -1;
    }

    ehdr.e_machine = EM_TCC_TARGET;
    ehdr.e_version = EV_CURRENT;
    ehdr.e_shoff = file_offset;
    ehdr.e_ehsize = sizeof(ElfW(Ehdr));
    ehdr.e_shentsize = sizeof(ElfW(Shdr));
    ehdr.e_shnum = shnum;
    ehdr.e_shstrndx = shnum - 1;

    fwrite(&ehdr, 1, sizeof(ElfW(Ehdr)), f);
    if (phdr)
        fwrite(phdr, 1, phnum * sizeof(ElfW(Phdr)), f);
    offset = sizeof(ElfW(Ehdr)) + phnum * sizeof(ElfW(Phdr));

    sort_syms(s1, symtab_section);

    for(i = 1; i < shnum; i++) {
        s = s1->sections[sec_order ? sec_order[i] : i];
        if (s->sh_type != SHT_NOBITS) {
            while (offset < s->sh_offset) {
                fputc(0, f);
                offset++;
            }
            size = s->sh_size;
            if (size)
                fwrite(s->data, 1, size, f);
            offset += size;
        }
    }

    /* output section headers */
    while (offset < ehdr.e_shoff) {
        fputc(0, f);
        offset++;
    }

    for(i = 0; i < shnum; i++) {
        sh = &shdr;
        memset(sh, 0, sizeof(ElfW(Shdr)));
        s = s1->sections[i];
        if (s) {
            sh->sh_name = s->sh_name;
            sh->sh_type = s->sh_type;
            sh->sh_flags = s->sh_flags;
            sh->sh_entsize = s->sh_entsize;
            sh->sh_info = s->sh_info;
            if (s->link)
                sh->sh_link = s->link->sh_num;
            sh->sh_addralign = s->sh_addralign;
            sh->sh_addr = s->sh_addr;
            sh->sh_offset = s->sh_offset;
            sh->sh_size = s->sh_size;
        }
        fwrite(sh, 1, sizeof(ElfW(Shdr)), f);
    }
    return 0;
}

static int tcc_output_binary(TCCState *s1, FILE *f,
                              const int *sec_order)
{
    Section *s;
    int i, offset, size;

    offset = 0;
    for(i=1;i<s1->nb_sections;i++) {
        s = s1->sections[sec_order[i]];
        if (s->sh_type != SHT_NOBITS &&
            (s->sh_flags & SHF_ALLOC)) {
            while (offset < s->sh_offset) {
                fputc(0, f);
                offset++;
            }
            size = s->sh_size;
            fwrite(s->data, 1, size, f);
            offset += size;
        }
    }
    return 0;
}

/* Write an elf, coff or "binary" file */
static int tcc_write_elf_file(TCCState *s1, const char *filename, int phnum,
                              ElfW(Phdr) *phdr, int file_offset, int *sec_order)
{
    int fd, mode, file_type, ret;
    FILE *f;

    file_type = s1->output_type;
    if (file_type == TCC_OUTPUT_OBJ)
        mode = 0666;
    else
        mode = 0777;
    unlink(filename);
    fd = open(filename, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, mode);
    if (fd < 0 || (f = fdopen(fd, "wb")) == NULL)
        return tcc_error_noabort("could not write '%s: %s'", filename, strerror(errno));
    if (s1->verbose)
        printf("<- %s\n", filename);
#ifdef TCC_TARGET_COFF
    if (s1->output_format == TCC_OUTPUT_FORMAT_COFF)
        tcc_output_coff(s1, f);
    else
#endif
    if (s1->output_format == TCC_OUTPUT_FORMAT_ELF)
        ret = tcc_output_elf(s1, f, phnum, phdr, file_offset, sec_order);
    else
        ret = tcc_output_binary(s1, f, sec_order);
    fclose(f);

    return ret;
}

#ifndef ELF_OBJ_ONLY
/* Sort section headers by assigned sh_addr, remove sections
   that we aren't going to output.  */
static int tidy_section_headers(TCCState *s1, int *sec_order)
{
    int i, nnew, l, *backmap;
    Section **snew, *s;
    ElfW(Sym) *sym;

    snew = tcc_malloc(s1->nb_sections * sizeof(snew[0]));
    backmap = tcc_malloc(s1->nb_sections * sizeof(backmap[0]));
    for (i = 0, nnew = 0, l = s1->nb_sections; i < s1->nb_sections; i++) {
	s = s1->sections[sec_order[i]];
	if (!i || s->sh_name) {
	    backmap[sec_order[i]] = nnew;
	    snew[nnew] = s;
	    ++nnew;
	} else {
	    backmap[sec_order[i]] = 0;
	    snew[--l] = s;
	}
    }
    for (i = 0; i < nnew; i++) {
	s = snew[i];
	if (s) {
	    s->sh_num = i;
            if (s->sh_type == SHT_RELX)
		s->sh_info = backmap[s->sh_info];
	}
    }

    for_each_elem(symtab_section, 1, sym, ElfW(Sym))
	if (sym->st_shndx != SHN_UNDEF && sym->st_shndx < SHN_LORESERVE)
	    sym->st_shndx = backmap[sym->st_shndx];
    if ( !s1->static_link ) {
        for_each_elem(s1->dynsym, 1, sym, ElfW(Sym))
	    if (sym->st_shndx != SHN_UNDEF && sym->st_shndx < SHN_LORESERVE)
	        sym->st_shndx = backmap[sym->st_shndx];
    }
    for (i = 0; i < s1->nb_sections; i++)
	sec_order[i] = i;
    tcc_free(s1->sections);
    s1->sections = snew;
    tcc_free(backmap);
    return nnew;
}

#ifdef TCC_TARGET_ARM
static void create_arm_attribute_section(TCCState *s1)
{
   // Needed for DLL support.
    static const unsigned char arm_attr[] = {
        0x41,                            // 'A'
        0x2c, 0x00, 0x00, 0x00,          // size 0x2c
        'a', 'e', 'a', 'b', 'i', 0x00,   // "aeabi"
        0x01, 0x22, 0x00, 0x00, 0x00,    // 'File Attributes', size 0x22
        0x05, 0x36, 0x00,                // 'CPU_name', "6"
        0x06, 0x06,                      // 'CPU_arch', 'v6'
        0x08, 0x01,                      // 'ARM_ISA_use', 'Yes'
        0x09, 0x01,                      // 'THUMB_ISA_use', 'Thumb-1'
        0x0a, 0x02,                      // 'FP_arch', 'VFPv2'
        0x12, 0x04,                      // 'ABI_PCS_wchar_t', 4
        0x14, 0x01,                      // 'ABI_FP_denormal', 'Needed'
        0x15, 0x01,                      // 'ABI_FP_exceptions', 'Needed'
        0x17, 0x03,                      // 'ABI_FP_number_model', 'IEEE 754'
        0x18, 0x01,                      // 'ABI_align_needed', '8-byte'
        0x19, 0x01,                      // 'ABI_align_preserved', '8-byte, except leaf SP'
        0x1a, 0x02,                      // 'ABI_enum_size', 'int'
        0x1c, 0x01,                      // 'ABI_VFP_args', 'VFP registers'
        0x22, 0x01                       // 'CPU_unaligned_access', 'v6'
    };
    Section *attr = new_section(s1, ".ARM.attributes", SHT_ARM_ATTRIBUTES, 0);
    unsigned char *ptr = section_ptr_add(attr, sizeof(arm_attr));
    attr->sh_addralign = 1;
    memcpy(ptr, arm_attr, sizeof(arm_attr));
    if (s1->float_abi != ARM_HARD_FLOAT) {
        ptr[26] = 0x00; // 'FP_arch', 'No'
        ptr[41] = 0x1e; // 'ABI_optimization_goals'
        ptr[42] = 0x06; // 'Aggressive Debug'
    }
}
#endif

#if TARGETOS_OpenBSD || TARGETOS_NetBSD
static Section *create_bsd_note_section(TCCState *s1,
					const char *name,
					const char *value)
{
    Section *s = find_section (s1, name);

    if (s->data_offset == 0) {
        char *ptr = section_ptr_add(s, sizeof(ElfW(Nhdr)) + 8 + 4);
        ElfW(Nhdr) *note = (ElfW(Nhdr) *) ptr;

        s->sh_type = SHT_NOTE;
        note->n_namesz = 8;
        note->n_descsz = 4;
        note->n_type = ELF_NOTE_OS_GNU;
	strcpy (ptr + sizeof(ElfW(Nhdr)), value);
    }
    return s;
}
#endif

static void alloc_sec_names(TCCState *s1, int is_obj);

/* Output an elf, coff or binary file */
/* XXX: suppress unneeded sections */
static int elf_output_file(TCCState *s1, const char *filename)
{
    int i, ret, file_type, file_offset, *sec_order;
    struct dyn_inf dyninf = {0};
    Section *interp, *dynstr, *dynamic;
    int textrel, got_sym, dt_flags_1;

    file_type = s1->output_type;
    s1->nb_errors = 0;
    ret = -1;
    interp = dynstr = dynamic = NULL;
    sec_order = NULL;
    dyninf.roinf = &dyninf._roinf;

#ifdef TCC_TARGET_ARM
    create_arm_attribute_section (s1);
#endif

#if TARGETOS_OpenBSD
    dyninf.note = create_bsd_note_section (s1, ".note.openbsd.ident", "OpenBSD");
#endif

#if TARGETOS_NetBSD
    dyninf.note = create_bsd_note_section (s1, ".note.netbsd.ident", "NetBSD");
#endif

#if TARGETOS_FreeBSD || TARGETOS_NetBSD
    dyninf.roinf = NULL;
#endif
        /* if linking, also link in runtime libraries (libc, libgcc, etc.) */
        tcc_add_runtime(s1);
	resolve_common_syms(s1);

        if (!s1->static_link) {
            if (file_type & TCC_OUTPUT_EXE) {
                char *ptr;
                /* allow override the dynamic loader */
                const char *elfint = getenv("LD_SO");
                if (elfint == NULL)
                    elfint = DEFAULT_ELFINTERP(s1);
                /* add interpreter section only if executable */
                interp = new_section(s1, ".interp", SHT_PROGBITS, SHF_ALLOC);
                interp->sh_addralign = 1;
                ptr = section_ptr_add(interp, 1 + strlen(elfint));
                strcpy(ptr, elfint);
                dyninf.interp = interp;
            }

            /* add dynamic symbol table */
            s1->dynsym = new_symtab(s1, ".dynsym", SHT_DYNSYM, SHF_ALLOC,
                                    ".dynstr",
                                    ".hash", SHF_ALLOC);
	    /* Number of local symbols (readelf complains if not set) */
	    s1->dynsym->sh_info = 1;
            dynstr = s1->dynsym->link;
            /* add dynamic section */
            dynamic = new_section(s1, ".dynamic", SHT_DYNAMIC,
                                  SHF_ALLOC | SHF_WRITE);
            dynamic->link = dynstr;
            dynamic->sh_entsize = sizeof(ElfW(Dyn));

            got_sym = build_got(s1);
            if (file_type & TCC_OUTPUT_EXE) {
                bind_exe_dynsyms(s1, file_type & TCC_OUTPUT_DYN);
                if (s1->nb_errors)
                    goto the_end;
            }
            build_got_entries(s1, got_sym);
            if (file_type & TCC_OUTPUT_EXE) {
                bind_libs_dynsyms(s1);
            } else {
                /* shared library case: simply export all global symbols */
                export_global_syms(s1);
            }
	    dyninf.gnu_hash = create_gnu_hash(s1);
        } else {
            build_got_entries(s1, 0);
        }
	version_add (s1);

    textrel = set_sec_sizes(s1);
    alloc_sec_names(s1, 0);

    if (!s1->static_link) {
        /* add a list of needed dlls */
        for(i = 0; i < s1->nb_loaded_dlls; i++) {
            DLLReference *dllref = s1->loaded_dlls[i];
            if (dllref->level == 0)
                put_dt(dynamic, DT_NEEDED, put_elf_str(dynstr, dllref->name));
        }

        if (s1->rpath)
            put_dt(dynamic, s1->enable_new_dtags ? DT_RUNPATH : DT_RPATH,
                   put_elf_str(dynstr, s1->rpath));

        dt_flags_1 = DF_1_NOW;
        if (file_type & TCC_OUTPUT_DYN) {
            if (s1->soname)
                put_dt(dynamic, DT_SONAME, put_elf_str(dynstr, s1->soname));
            /* XXX: currently, since we do not handle PIC code, we
               must relocate the readonly segments */
            if (textrel)
                put_dt(dynamic, DT_TEXTREL, 0);
            if (file_type & TCC_OUTPUT_EXE)
                dt_flags_1 = DF_1_NOW | DF_1_PIE;
        }
        put_dt(dynamic, DT_FLAGS, DF_BIND_NOW);
        put_dt(dynamic, DT_FLAGS_1, dt_flags_1);
        if (s1->symbolic)
            put_dt(dynamic, DT_SYMBOLIC, 0);

        dyninf.dynamic = dynamic;
        dyninf.dynstr = dynstr;
        /* remember offset and reserve space for 2nd call below */
        dyninf.data_offset = dynamic->data_offset;
        fill_dynamic(s1, &dyninf);
        dynamic->sh_size = dynamic->data_offset;
        dynstr->sh_size = dynstr->data_offset;
    }

    /* this array is used to reorder sections in the output file */
    sec_order = tcc_malloc(sizeof(int) * 2 * s1->nb_sections);
    /* compute section to program header mapping */
    file_offset = layout_sections(s1, sec_order, &dyninf);

        if (dynamic) {
            /* put in GOT the dynamic section address and relocate PLT */
            write32le(s1->got->data, dynamic->sh_addr);
            if (file_type == TCC_OUTPUT_EXE
                || (RELOCATE_DLLPLT && (file_type & TCC_OUTPUT_DYN)))
                relocate_plt(s1);
            /* relocate symbols in .dynsym now that final addresses are known */
            relocate_syms(s1, s1->dynsym, 2);
        }

        /* if building executable or DLL, then relocate each section
           except the GOT which is already relocated */
        relocate_syms(s1, s1->symtab, 0);
        if (s1->nb_errors != 0)
            goto the_end;
        relocate_sections(s1);
        if (dynamic) {
	    update_reloc_sections (s1, &dyninf);
            dynamic->data_offset = dyninf.data_offset;
            fill_dynamic(s1, &dyninf);
	}
        /* Perform relocation to GOT or PLT entries */
        if (file_type == TCC_OUTPUT_EXE && s1->static_link)
            fill_got(s1);
        else if (s1->got)
            fill_local_got_entries(s1);

    if (dyninf.gnu_hash)
        update_gnu_hash(s1, dyninf.gnu_hash);

    /* Create the ELF file with name 'filename' */
    ret = tcc_write_elf_file(s1, filename, dyninf.phnum, dyninf.phdr, file_offset, sec_order);
 the_end:
    tcc_free(sec_order);
    tcc_free(dyninf.phdr);
    return ret;
}
#endif /* ndef ELF_OBJ_ONLY */

/* Allocate strings for section names */
static void alloc_sec_names(TCCState *s1, int is_obj)
{
    int i;
    Section *s, *strsec;

    strsec = new_section(s1, ".shstrtab", SHT_STRTAB, 0);
    put_elf_str(strsec, "");
    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        if (is_obj)
            s->sh_size = s->data_offset;
	if (s == strsec || s->sh_size || (s->sh_flags & SHF_ALLOC))
            s->sh_name = put_elf_str(strsec, s->name);
    }
    strsec->sh_size = strsec->data_offset;
}

/* Output an elf .o file */
static int elf_output_obj(TCCState *s1, const char *filename)
{
    Section *s;
    int i, ret, file_offset;
    s1->nb_errors = 0;
    /* Allocate strings for section names */
    alloc_sec_names(s1, 1);
    file_offset = sizeof (ElfW(Ehdr));
    for(i = 1; i < s1->nb_sections; i++) {
        s = s1->sections[i];
        file_offset = (file_offset + 15) & -16;
        s->sh_offset = file_offset;
        if (s->sh_type != SHT_NOBITS)
            file_offset += s->sh_size;
    }
    /* Create the ELF file with name 'filename' */
    ret = tcc_write_elf_file(s1, filename, 0, NULL, file_offset, NULL);
    return ret;
}

LIBTCCAPI int tcc_output_file(TCCState *s, const char *filename)
{
    if (s->test_coverage)
        tcc_tcov_add_file(s, filename);
    if (s->output_type == TCC_OUTPUT_OBJ)
        return elf_output_obj(s, filename);
#ifdef TCC_TARGET_PE
    return  pe_output_file(s, filename);
#elif TCC_TARGET_MACHO
    return macho_output_file(s, filename);
#else
    return elf_output_file(s, filename);
#endif
}

ST_FUNC ssize_t full_read(int fd, void *buf, size_t count) {
    char *cbuf = buf;
    size_t rnum = 0;
    while (1) {
        ssize_t num = read(fd, cbuf, count-rnum);
        if (num < 0) return num;
        if (num == 0) return rnum;
        rnum += num;
        cbuf += num;
    }
}

ST_FUNC void *load_data(int fd, unsigned long file_offset, unsigned long size)
{
    void *data;

    data = tcc_malloc(size);
    lseek(fd, file_offset, SEEK_SET);
    full_read(fd, data, size);
    return data;
}

typedef struct SectionMergeInfo {
    Section *s;            /* corresponding existing section */
    unsigned long offset;  /* offset of the new section in the existing section */
    uint8_t new_section;       /* true if section 's' was added */
    uint8_t link_once;         /* true if link once section */
} SectionMergeInfo;

ST_FUNC int tcc_object_type(int fd, ElfW(Ehdr) *h)
{
    int size = full_read(fd, h, sizeof *h);
    if (size == sizeof *h && 0 == memcmp(h, ELFMAG, 4)) {
        if (h->e_type == ET_REL)
            return AFF_BINTYPE_REL;
        if (h->e_type == ET_DYN)
            return AFF_BINTYPE_DYN;
    } else if (size >= 8) {
        if (0 == memcmp(h, ARMAG, 8))
            return AFF_BINTYPE_AR;
#ifdef TCC_TARGET_COFF
        if (((struct filehdr*)h)->f_magic == COFF_C67_MAGIC)
            return AFF_BINTYPE_C67;
#endif
    }
    return 0;
}

/* load an object file and merge it with current files */
/* XXX: handle correctly stab (debug) info */
ST_FUNC int tcc_load_object_file(TCCState *s1,
                                int fd, unsigned long file_offset)
{
    ElfW(Ehdr) ehdr;
    ElfW(Shdr) *shdr, *sh;
    unsigned long size, offset, offseti;
    int i, j, nb_syms, sym_index, ret, seencompressed;
    char *strsec, *strtab;
    int stab_index, stabstr_index;
    int *old_to_new_syms;
    char *sh_name, *name;
    SectionMergeInfo *sm_table, *sm;
    ElfW(Sym) *sym, *symtab;
    ElfW_Rel *rel;
    Section *s;

    lseek(fd, file_offset, SEEK_SET);
    if (tcc_object_type(fd, &ehdr) != AFF_BINTYPE_REL)
        goto invalid;
    /* test CPU specific stuff */
    if (ehdr.e_ident[5] != ELFDATA2LSB ||
        ehdr.e_machine != EM_TCC_TARGET) {
invalid:
        return tcc_error_noabort("invalid object file");
    }
    /* read sections */
    shdr = load_data(fd, file_offset + ehdr.e_shoff,
                     sizeof(ElfW(Shdr)) * ehdr.e_shnum);
    sm_table = tcc_mallocz(sizeof(SectionMergeInfo) * ehdr.e_shnum);

    /* load section names */
    sh = &shdr[ehdr.e_shstrndx];
    strsec = load_data(fd, file_offset + sh->sh_offset, sh->sh_size);

    /* load symtab and strtab */
    old_to_new_syms = NULL;
    symtab = NULL;
    strtab = NULL;
    nb_syms = 0;
    seencompressed = 0;
    stab_index = stabstr_index = 0;
    ret = -1;

    for(i = 1; i < ehdr.e_shnum; i++) {
        sh = &shdr[i];
        if (sh->sh_type == SHT_SYMTAB) {
            if (symtab) {
                tcc_error_noabort("object must contain only one symtab");
                goto the_end;
            }
            nb_syms = sh->sh_size / sizeof(ElfW(Sym));
            symtab = load_data(fd, file_offset + sh->sh_offset, sh->sh_size);
            sm_table[i].s = symtab_section;

            /* now load strtab */
            sh = &shdr[sh->sh_link];
            strtab = load_data(fd, file_offset + sh->sh_offset, sh->sh_size);
        }
	if (sh->sh_flags & SHF_COMPRESSED)
	    seencompressed = 1;
    }

    /* now examine each section and try to merge its content with the
       ones in memory */
    for(i = 1; i < ehdr.e_shnum; i++) {
        /* no need to examine section name strtab */
        if (i == ehdr.e_shstrndx)
            continue;
        sh = &shdr[i];
	if (sh->sh_type == SHT_RELX)
	  sh = &shdr[sh->sh_info];
        /* ignore sections types we do not handle (plus relocs to those) */
        if (sh->sh_type != SHT_PROGBITS &&
#ifdef TCC_ARM_EABI
            sh->sh_type != SHT_ARM_EXIDX &&
#endif
#if TARGETOS_OpenBSD || TARGETOS_FreeBSD || TARGETOS_NetBSD
            sh->sh_type != SHT_X86_64_UNWIND &&
#endif
            sh->sh_type != SHT_NOTE &&
            sh->sh_type != SHT_NOBITS &&
            sh->sh_type != SHT_PREINIT_ARRAY &&
            sh->sh_type != SHT_INIT_ARRAY &&
            sh->sh_type != SHT_FINI_ARRAY &&
            strcmp(strsec + sh->sh_name, ".stabstr")
            )
            continue;
	if (seencompressed && 0 == strncmp(strsec + sh->sh_name, ".debug_", 7))
	  continue;

	sh = &shdr[i];
        sh_name = strsec + sh->sh_name;
        if (sh->sh_addralign < 1)
            sh->sh_addralign = 1;
        /* find corresponding section, if any */
        for(j = 1; j < s1->nb_sections;j++) {
            s = s1->sections[j];
            if (!strcmp(s->name, sh_name)) {
                if (!strncmp(sh_name, ".gnu.linkonce",
                             sizeof(".gnu.linkonce") - 1)) {
                    /* if a 'linkonce' section is already present, we
                       do not add it again. It is a little tricky as
                       symbols can still be defined in
                       it. */
                    sm_table[i].link_once = 1;
                    goto next;
                }
                if (stab_section) {
                    if (s == stab_section)
                        stab_index = i;
                    if (s == stab_section->link)
                        stabstr_index = i;
                }
                goto found;
            }
        }
        /* not found: create new section */
        s = new_section(s1, sh_name, sh->sh_type, sh->sh_flags & ~SHF_GROUP);
        /* take as much info as possible from the section. sh_link and
           sh_info will be updated later */
        s->sh_addralign = sh->sh_addralign;
        s->sh_entsize = sh->sh_entsize;
        sm_table[i].new_section = 1;
    found:
        if (sh->sh_type != s->sh_type
#if TARGETOS_OpenBSD || TARGETOS_FreeBSD || TARGETOS_NetBSD
            && strcmp (s->name, ".eh_frame")
#endif
            ) {
            tcc_error_noabort("invalid section type");
            goto the_end;
        }
        /* align start of section */
        s->data_offset += -s->data_offset & (sh->sh_addralign - 1);
        if (sh->sh_addralign > s->sh_addralign)
            s->sh_addralign = sh->sh_addralign;
        sm_table[i].offset = s->data_offset;
        sm_table[i].s = s;
        /* concatenate sections */
        size = sh->sh_size;
        if (sh->sh_type != SHT_NOBITS) {
            unsigned char *ptr;
            lseek(fd, file_offset + sh->sh_offset, SEEK_SET);
            ptr = section_ptr_add(s, size);
            full_read(fd, ptr, size);
        } else {
            s->data_offset += size;
        }
    next: ;
    }

    /* gr relocate stab strings */
    if (stab_index && stabstr_index) {
        Stab_Sym *a, *b;
        unsigned o;
        s = sm_table[stab_index].s;
        a = (Stab_Sym *)(s->data + sm_table[stab_index].offset);
        b = (Stab_Sym *)(s->data + s->data_offset);
        o = sm_table[stabstr_index].offset;
        while (a < b) {
            if (a->n_strx)
                a->n_strx += o;
            a++;
        }
    }

    /* second short pass to update sh_link and sh_info fields of new
       sections */
    for(i = 1; i < ehdr.e_shnum; i++) {
        s = sm_table[i].s;
        if (!s || !sm_table[i].new_section)
            continue;
        sh = &shdr[i];
        if (sh->sh_link > 0)
            s->link = sm_table[sh->sh_link].s;
        if (sh->sh_type == SHT_RELX) {
            s->sh_info = sm_table[sh->sh_info].s->sh_num;
            /* update backward link */
            s1->sections[s->sh_info]->reloc = s;
        }
    }

    /* resolve symbols */
    old_to_new_syms = tcc_mallocz(nb_syms * sizeof(int));

    sym = symtab + 1;
    for(i = 1; i < nb_syms; i++, sym++) {
        if (sym->st_shndx != SHN_UNDEF &&
            sym->st_shndx < SHN_LORESERVE) {
            sm = &sm_table[sym->st_shndx];
            if (sm->link_once) {
                /* if a symbol is in a link once section, we use the
                   already defined symbol. It is very important to get
                   correct relocations */
                if (ELFW(ST_BIND)(sym->st_info) != STB_LOCAL) {
                    name = strtab + sym->st_name;
                    sym_index = find_elf_sym(symtab_section, name);
                    if (sym_index)
                        old_to_new_syms[i] = sym_index;
                }
                continue;
            }
            /* if no corresponding section added, no need to add symbol */
            if (!sm->s)
                continue;
            /* convert section number */
            sym->st_shndx = sm->s->sh_num;
            /* offset value */
            sym->st_value += sm->offset;
        }
        /* add symbol */
        name = strtab + sym->st_name;
        sym_index = set_elf_sym(symtab_section, sym->st_value, sym->st_size,
                                sym->st_info, sym->st_other,
                                sym->st_shndx, name);
        old_to_new_syms[i] = sym_index;
    }

    /* third pass to patch relocation entries */
    for(i = 1; i < ehdr.e_shnum; i++) {
        s = sm_table[i].s;
        if (!s)
            continue;
        sh = &shdr[i];
        offset = sm_table[i].offset;
        size = sh->sh_size;
        switch(s->sh_type) {
        case SHT_RELX:
            /* take relocation offset information */
            offseti = sm_table[sh->sh_info].offset;
	    for (rel = (ElfW_Rel *) s->data + (offset / sizeof(*rel));
		 rel < (ElfW_Rel *) s->data + ((offset + size) / sizeof(*rel));
		 rel++) {
                int type;
                unsigned sym_index;
                /* convert symbol index */
                type = ELFW(R_TYPE)(rel->r_info);
                sym_index = ELFW(R_SYM)(rel->r_info);
                /* NOTE: only one symtab assumed */
                if (sym_index >= nb_syms)
                    goto invalid_reloc;
                sym_index = old_to_new_syms[sym_index];
                /* ignore link_once in rel section. */
                if (!sym_index && !sm_table[sh->sh_info].link_once
#ifdef TCC_TARGET_ARM
                    && type != R_ARM_V4BX
#elif defined TCC_TARGET_RISCV64
                    && type != R_RISCV_ALIGN
                    && type != R_RISCV_RELAX
#endif
                   ) {
                invalid_reloc:
                    tcc_error_noabort("Invalid relocation entry [%2d] '%s' @ %.8x",
                        i, strsec + sh->sh_name, (int)rel->r_offset);
                    goto the_end;
                }
                rel->r_info = ELFW(R_INFO)(sym_index, type);
                /* offset the relocation offset */
                rel->r_offset += offseti;
#ifdef TCC_TARGET_ARM
                /* Jumps and branches from a Thumb code to a PLT entry need
                   special handling since PLT entries are ARM code.
                   Unconditional bl instructions referencing PLT entries are
                   handled by converting these instructions into blx
                   instructions. Other case of instructions referencing a PLT
                   entry require to add a Thumb stub before the PLT entry to
                   switch to ARM mode. We set bit plt_thumb_stub of the
                   attribute of a symbol to indicate such a case. */
                if (type == R_ARM_THM_JUMP24)
                    get_sym_attr(s1, sym_index, 1)->plt_thumb_stub = 1;
#endif
            }
            break;
        default:
            break;
        }
    }

    ret = 0;
 the_end:
    tcc_free(symtab);
    tcc_free(strtab);
    tcc_free(old_to_new_syms);
    tcc_free(sm_table);
    tcc_free(strsec);
    tcc_free(shdr);
    return ret;
}

typedef struct ArchiveHeader {
    char ar_name[16];           /* name of this member */
    char ar_date[12];           /* file mtime */
    char ar_uid[6];             /* owner uid; printed as decimal */
    char ar_gid[6];             /* owner gid; printed as decimal */
    char ar_mode[8];            /* file mode, printed as octal   */
    char ar_size[10];           /* file size, printed as decimal */
    char ar_fmag[2];            /* should contain ARFMAG */
} ArchiveHeader;

#define ARFMAG "`\n"

static unsigned long long get_be(const uint8_t *b, int n)
{
    unsigned long long ret = 0;
    while (n)
        ret = (ret << 8) | *b++, --n;
    return ret;
}

static int read_ar_header(int fd, int offset, ArchiveHeader *hdr)
{
    char *p, *e;
    int len;
    lseek(fd, offset, SEEK_SET);
    len = full_read(fd, hdr, sizeof(ArchiveHeader));
    if (len != sizeof(ArchiveHeader))
        return len ? -1 : 0;
    p = hdr->ar_name;
    for (e = p + sizeof hdr->ar_name; e > p && e[-1] == ' ';)
        --e;
    *e = '\0';
    hdr->ar_size[sizeof hdr->ar_size-1] = 0;
    return len;
}

/* load only the objects which resolve undefined symbols */
static int tcc_load_alacarte(TCCState *s1, int fd, int size, int entrysize)
{
    int i, bound, nsyms, sym_index, len, ret = -1;
    unsigned long long off;
    uint8_t *data;
    const char *ar_names, *p;
    const uint8_t *ar_index;
    ElfW(Sym) *sym;
    ArchiveHeader hdr;

    data = tcc_malloc(size);
    if (full_read(fd, data, size) != size)
        goto the_end;
    nsyms = get_be(data, entrysize);
    ar_index = data + entrysize;
    ar_names = (char *) ar_index + nsyms * entrysize;

    do {
        bound = 0;
        for (p = ar_names, i = 0; i < nsyms; i++, p += strlen(p)+1) {
            Section *s = symtab_section;
            sym_index = find_elf_sym(s, p);
            if (!sym_index)
                continue;
            sym = &((ElfW(Sym) *)s->data)[sym_index];
            if(sym->st_shndx != SHN_UNDEF)
                continue;
            off = get_be(ar_index + i * entrysize, entrysize);
            len = read_ar_header(fd, off, &hdr);
            if (len <= 0 || memcmp(hdr.ar_fmag, ARFMAG, 2)) {
                tcc_error_noabort("invalid archive");
                goto the_end;
            }
            off += len;
            if (s1->verbose == 2)
                printf("   -> %s\n", hdr.ar_name);
            if (tcc_load_object_file(s1, fd, off) < 0)
                goto the_end;
            ++bound;
        }
    } while(bound);
    ret = 0;
 the_end:
    tcc_free(data);
    return ret;
}

/* load a '.a' file */
ST_FUNC int tcc_load_archive(TCCState *s1, int fd, int alacarte)
{
    ArchiveHeader hdr;
    /* char magic[8]; */
    int size, len;
    unsigned long file_offset;
    ElfW(Ehdr) ehdr;

    /* skip magic which was already checked */
    /* full_read(fd, magic, sizeof(magic)); */
    file_offset = sizeof ARMAG - 1;

    for(;;) {
        len = read_ar_header(fd, file_offset, &hdr);
        if (len == 0)
            return 0;
        if (len < 0)
            return tcc_error_noabort("invalid archive");
        file_offset += len;
        size = strtol(hdr.ar_size, NULL, 0);
        /* align to even */
        size = (size + 1) & ~1;
        if (alacarte) {
            /* coff symbol table : we handle it */
            if (!strcmp(hdr.ar_name, "/"))
                return tcc_load_alacarte(s1, fd, size, 4);
            if (!strcmp(hdr.ar_name, "/SYM64/"))
                return tcc_load_alacarte(s1, fd, size, 8);
        } else if (tcc_object_type(fd, &ehdr) == AFF_BINTYPE_REL) {
            if (s1->verbose == 2)
                printf("   -> %s\n", hdr.ar_name);
            if (tcc_load_object_file(s1, fd, file_offset) < 0)
                return -1;
        }
        file_offset += size;
    }
}

#ifndef ELF_OBJ_ONLY
/* Set LV[I] to the global index of sym-version (LIB,VERSION).  Maybe resizes
   LV, maybe create a new entry for (LIB,VERSION).  */
static void set_ver_to_ver(TCCState *s1, int *n, int **lv, int i, char *lib, char *version)
{
    while (i >= *n) {
        *lv = tcc_realloc(*lv, (*n + 1) * sizeof(**lv));
        (*lv)[(*n)++] = -1;
    }
    if ((*lv)[i] == -1) {
        int v, prev_same_lib = -1;
        for (v = 0; v < nb_sym_versions; v++) {
            if (strcmp(sym_versions[v].lib, lib))
              continue;
            prev_same_lib = v;
            if (!strcmp(sym_versions[v].version, version))
              break;
        }
        if (v == nb_sym_versions) {
            sym_versions = tcc_realloc (sym_versions,
                                        (v + 1) * sizeof(*sym_versions));
            sym_versions[v].lib = tcc_strdup(lib);
            sym_versions[v].version = tcc_strdup(version);
            sym_versions[v].out_index = 0;
            sym_versions[v].prev_same_lib = prev_same_lib;
            nb_sym_versions++;
        }
        (*lv)[i] = v;
    }
}

/* Associates symbol SYM_INDEX (in dynsymtab) with sym-version index
   VERNDX.  */
static void
set_sym_version(TCCState *s1, int sym_index, int verndx)
{
    if (sym_index >= nb_sym_to_version) {
        int newelems = sym_index ? sym_index * 2 : 1;
        sym_to_version = tcc_realloc(sym_to_version,
                                     newelems * sizeof(*sym_to_version));
        memset(sym_to_version + nb_sym_to_version, -1,
               (newelems - nb_sym_to_version) * sizeof(*sym_to_version));
        nb_sym_to_version = newelems;
    }
    if (sym_to_version[sym_index] < 0)
      sym_to_version[sym_index] = verndx;
}

struct versym_info {
    int nb_versyms;
    ElfW(Verdef) *verdef;
    ElfW(Verneed) *verneed;
    ElfW(Half) *versym;
    int nb_local_ver, *local_ver;
};


static void store_version(TCCState *s1, struct versym_info *v, char *dynstr)
{
    char *lib, *version;
    uint32_t next;
    int i;

#define	DEBUG_VERSION 0

    if (v->versym && v->verdef) {
      ElfW(Verdef) *vdef = v->verdef;
      lib = NULL;
      do {
        ElfW(Verdaux) *verdaux =
	  (ElfW(Verdaux) *) (((char *) vdef) + vdef->vd_aux);

#if DEBUG_VERSION
	printf ("verdef: version:%u flags:%u index:%u, hash:%u\n",
	        vdef->vd_version, vdef->vd_flags, vdef->vd_ndx,
		vdef->vd_hash);
#endif
	if (vdef->vd_cnt) {
          version = dynstr + verdaux->vda_name;

	  if (lib == NULL)
	    lib = version;
	  else
            set_ver_to_ver(s1, &v->nb_local_ver, &v->local_ver, vdef->vd_ndx,
                           lib, version);
#if DEBUG_VERSION
	  printf ("  verdaux(%u): %s\n", vdef->vd_ndx, version);
#endif
	}
        next = vdef->vd_next;
        vdef = (ElfW(Verdef) *) (((char *) vdef) + next);
      } while (next);
    }
    if (v->versym && v->verneed) {
      ElfW(Verneed) *vneed = v->verneed;
      do {
        ElfW(Vernaux) *vernaux =
	  (ElfW(Vernaux) *) (((char *) vneed) + vneed->vn_aux);

        lib = dynstr + vneed->vn_file;
#if DEBUG_VERSION
	printf ("verneed: %u %s\n", vneed->vn_version, lib);
#endif
	for (i = 0; i < vneed->vn_cnt; i++) {
	  if ((vernaux->vna_other & 0x8000) == 0) { /* hidden */
              version = dynstr + vernaux->vna_name;
              set_ver_to_ver(s1, &v->nb_local_ver, &v->local_ver, vernaux->vna_other,
                             lib, version);
#if DEBUG_VERSION
	    printf ("  vernaux(%u): %u %u %s\n",
		    vernaux->vna_other, vernaux->vna_hash,
		    vernaux->vna_flags, version);
#endif
	  }
	  vernaux = (ElfW(Vernaux) *) (((char *) vernaux) + vernaux->vna_next);
	}
        next = vneed->vn_next;
        vneed = (ElfW(Verneed) *) (((char *) vneed) + next);
      } while (next);
    }

#if DEBUG_VERSION
    for (i = 0; i < v->nb_local_ver; i++) {
      if (v->local_ver[i] > 0) {
        printf ("%d: lib: %s, version %s\n",
		i, sym_versions[v->local_ver[i]].lib,
                sym_versions[v->local_ver[i]].version);
      }
    }
#endif
}

/* load a library / DLL
   'level = 0' means that the DLL is referenced by the user
   (so it should be added as DT_NEEDED in the generated ELF file) */
ST_FUNC int tcc_load_dll(TCCState *s1, int fd, const char *filename, int level)
{
    ElfW(Ehdr) ehdr;
    ElfW(Shdr) *shdr, *sh, *sh1;
    int i, nb_syms, nb_dts, sym_bind, ret = -1;
    ElfW(Sym) *sym, *dynsym;
    ElfW(Dyn) *dt, *dynamic;

    char *dynstr;
    int sym_index;
    const char *name, *soname;
    struct versym_info v;

    full_read(fd, &ehdr, sizeof(ehdr));

    /* test CPU specific stuff */
    if (ehdr.e_ident[5] != ELFDATA2LSB ||
        ehdr.e_machine != EM_TCC_TARGET) {
        return tcc_error_noabort("bad architecture");
    }

    /* read sections */
    shdr = load_data(fd, ehdr.e_shoff, sizeof(ElfW(Shdr)) * ehdr.e_shnum);

    /* load dynamic section and dynamic symbols */
    nb_syms = 0;
    nb_dts = 0;
    dynamic = NULL;
    dynsym = NULL; /* avoid warning */
    dynstr = NULL; /* avoid warning */
    memset(&v, 0, sizeof v);

    for(i = 0, sh = shdr; i < ehdr.e_shnum; i++, sh++) {
        switch(sh->sh_type) {
        case SHT_DYNAMIC:
            nb_dts = sh->sh_size / sizeof(ElfW(Dyn));
            dynamic = load_data(fd, sh->sh_offset, sh->sh_size);
            break;
        case SHT_DYNSYM:
            nb_syms = sh->sh_size / sizeof(ElfW(Sym));
            dynsym = load_data(fd, sh->sh_offset, sh->sh_size);
            sh1 = &shdr[sh->sh_link];
            dynstr = load_data(fd, sh1->sh_offset, sh1->sh_size);
            break;
        case SHT_GNU_verdef:
	    v.verdef = load_data(fd, sh->sh_offset, sh->sh_size);
	    break;
        case SHT_GNU_verneed:
	    v.verneed = load_data(fd, sh->sh_offset, sh->sh_size);
	    break;
        case SHT_GNU_versym:
            v.nb_versyms = sh->sh_size / sizeof(ElfW(Half));
	    v.versym = load_data(fd, sh->sh_offset, sh->sh_size);
	    break;
        default:
            break;
        }
    }

    if (!dynamic)
        goto the_end;

    /* compute the real library name */
    soname = tcc_basename(filename);
    for(i = 0, dt = dynamic; i < nb_dts; i++, dt++)
        if (dt->d_tag == DT_SONAME)
            soname = dynstr + dt->d_un.d_val;

    /* if the dll is already loaded, do not load it */
    if (tcc_add_dllref(s1, soname, level)->found)
        goto ret_success;

    if (v.nb_versyms != nb_syms)
        tcc_free (v.versym), v.versym = NULL;
    else
        store_version(s1, &v, dynstr);

    /* add dynamic symbols in dynsym_section */
    for(i = 1, sym = dynsym + 1; i < nb_syms; i++, sym++) {
        sym_bind = ELFW(ST_BIND)(sym->st_info);
        if (sym_bind == STB_LOCAL)
            continue;
        name = dynstr + sym->st_name;
        sym_index = set_elf_sym(s1->dynsymtab_section, sym->st_value, sym->st_size,
                                sym->st_info, sym->st_other, sym->st_shndx, name);
        if (v.versym) {
            ElfW(Half) vsym = v.versym[i];
            if ((vsym & 0x8000) == 0 && vsym > 0 && vsym < v.nb_local_ver)
                set_sym_version(s1, sym_index, v.local_ver[vsym]);
        }
    }

    /* do not load all referenced libraries
       (recursive loading can break linking of libraries) */
    /* following DT_NEEDED is needed for the dynamic loader (libdl.so),
       but it is no longer needed, when linking a library or a program.
       When tcc output mode is OUTPUT_MEM,
       tcc calls dlopen, which handles DT_NEEDED for us */

#if 0
    for(i = 0, dt = dynamic; i < nb_dts; i++, dt++)
        if (dt->d_tag == DT_RPATH)
            tcc_add_library_path(s1, dynstr + dt->d_un.d_val);

    /* load all referenced DLLs */
    for(i = 0, dt = dynamic; i < nb_dts; i++, dt++) {
        switch(dt->d_tag) {
        case DT_NEEDED:
            name = dynstr + dt->d_un.d_val;
            if (tcc_add_dllref(s1, name, -1))
                continue;
            if (tcc_add_dll(s1, name, AFF_REFERENCED_DLL) < 0) {
                ret = tcc_error_noabort("referenced dll '%s' not found", name);
                goto the_end;
            }
        }
    }
#endif

 ret_success:
    ret = 0;
 the_end:
    tcc_free(dynstr);
    tcc_free(dynsym);
    tcc_free(dynamic);
    tcc_free(shdr);
    tcc_free(v.local_ver);
    tcc_free(v.verdef);
    tcc_free(v.verneed);
    tcc_free(v.versym);
    return ret;
}

#define LD_TOK_NAME 256
#define LD_TOK_EOF  (-1)

static int ld_inp(TCCState *s1)
{
    char b;
    if (s1->cc != -1) {
        int c = s1->cc;
        s1->cc = -1;
        return c;
    }
    if (1 == read(s1->fd, &b, 1))
        return b;
    return CH_EOF;
}

/* return next ld script token */
static int ld_next(TCCState *s1, char *name, int name_size)
{
    int c, d, ch;
    char *q;

 redo:
    ch = ld_inp(s1);
    switch(ch) {
    case ' ':
    case '\t':
    case '\f':
    case '\v':
    case '\r':
    case '\n':
        goto redo;
    case '/':
        ch = ld_inp(s1);
        if (ch == '*') { /* comment */
            for (d = 0;; d = ch) {
                ch = ld_inp(s1);
                if (ch == CH_EOF || (ch == '/' && d == '*'))
                    break;
            }
            goto redo;
        } else {
            q = name;
            *q++ = '/';
            goto parse_name;
        }
        break;
    case '\\':
    /* case 'a' ... 'z': */
    case 'a':
       case 'b':
       case 'c':
       case 'd':
       case 'e':
       case 'f':
       case 'g':
       case 'h':
       case 'i':
       case 'j':
       case 'k':
       case 'l':
       case 'm':
       case 'n':
       case 'o':
       case 'p':
       case 'q':
       case 'r':
       case 's':
       case 't':
       case 'u':
       case 'v':
       case 'w':
       case 'x':
       case 'y':
       case 'z':
    /* case 'A' ... 'z': */
    case 'A':
       case 'B':
       case 'C':
       case 'D':
       case 'E':
       case 'F':
       case 'G':
       case 'H':
       case 'I':
       case 'J':
       case 'K':
       case 'L':
       case 'M':
       case 'N':
       case 'O':
       case 'P':
       case 'Q':
       case 'R':
       case 'S':
       case 'T':
       case 'U':
       case 'V':
       case 'W':
       case 'X':
       case 'Y':
       case 'Z':
    case '_':
    case '.':
    case '$':
    case '~':
        q = name;
    parse_name:
        for(;;) {
            if (!((ch >= 'a' && ch <= 'z') ||
                  (ch >= 'A' && ch <= 'Z') ||
                  (ch >= '0' && ch <= '9') ||
                  strchr("/.-_+=$:\\,~", ch)))
                break;
            if ((q - name) < name_size - 1) {
                *q++ = ch;
            }
            ch = ld_inp(s1);
        }
        s1->cc = ch;
        *q = '\0';
        c = LD_TOK_NAME;
        break;
    case CH_EOF:
        c = LD_TOK_EOF;
        break;
    default:
        c = ch;
        break;
    }
    return c;
}

static int ld_add_file(TCCState *s1, const char filename[])
{
    if (filename[0] == '/') {
        if (CONFIG_SYSROOT[0] == '\0'
            && tcc_add_file_internal(s1, filename, AFF_TYPE_BIN) == 0)
            return 0;
        filename = tcc_basename(filename);
    }
    return tcc_add_dll(s1, filename, AFF_PRINT_ERROR);
}

static int ld_add_file_list(TCCState *s1, const char *cmd, int as_needed)
{
    char filename[1024], libname[1024];
    int t, group, nblibs = 0, ret = 0;
    char **libs = NULL;

    group = !strcmp(cmd, "GROUP");
    if (!as_needed)
        s1->new_undef_sym = 0;
    t = ld_next(s1, filename, sizeof(filename));
    if (t != '(') {
        ret = tcc_error_noabort("( expected");
        goto lib_parse_error;
    }
    t = ld_next(s1, filename, sizeof(filename));
    for(;;) {
        libname[0] = '\0';
        if (t == LD_TOK_EOF) {
            ret = tcc_error_noabort("unexpected end of file");
            goto lib_parse_error;
        } else if (t == ')') {
            break;
        } else if (t == '-') {
            t = ld_next(s1, filename, sizeof(filename));
            if ((t != LD_TOK_NAME) || (filename[0] != 'l')) {
                ret = tcc_error_noabort("library name expected");
                goto lib_parse_error;
            }
            pstrcpy(libname, sizeof libname, &filename[1]);
            if (s1->static_link) {
                snprintf(filename, sizeof filename, "lib%s.a", libname);
            } else {
                snprintf(filename, sizeof filename, "lib%s.so", libname);
            }
        } else if (t != LD_TOK_NAME) {
            ret = tcc_error_noabort("filename expected");
            goto lib_parse_error;
        }
        if (!strcmp(filename, "AS_NEEDED")) {
            ret = ld_add_file_list(s1, cmd, 1);
            if (ret)
                goto lib_parse_error;
        } else {
            /* TODO: Implement AS_NEEDED support. */
	    /*       DT_NEEDED is not used any more so ignore as_needed */
            if (1 || !as_needed) {
                ret = ld_add_file(s1, filename);
                if (ret)
                    goto lib_parse_error;
                if (group) {
                    /* Add the filename *and* the libname to avoid future conversions */
                    dynarray_add(&libs, &nblibs, tcc_strdup(filename));
                    if (libname[0] != '\0')
                        dynarray_add(&libs, &nblibs, tcc_strdup(libname));
                }
            }
        }
        t = ld_next(s1, filename, sizeof(filename));
        if (t == ',') {
            t = ld_next(s1, filename, sizeof(filename));
        }
    }
    if (group && !as_needed) {
        while (s1->new_undef_sym) {
            int i;
            s1->new_undef_sym = 0;
            for (i = 0; i < nblibs; i ++)
                ld_add_file(s1, libs[i]);
        }
    }
lib_parse_error:
    dynarray_reset(&libs, &nblibs);
    return ret;
}

/* interpret a subset of GNU ldscripts to handle the dummy libc.so
   files */
ST_FUNC int tcc_load_ldscript(TCCState *s1, int fd)
{
    char cmd[64];
    char filename[1024];
    int t, ret;

    s1->fd = fd;
    s1->cc = -1;
    for(;;) {
        t = ld_next(s1, cmd, sizeof(cmd));
        if (t == LD_TOK_EOF)
            return 0;
        else if (t != LD_TOK_NAME)
            return -1;
        if (!strcmp(cmd, "INPUT") ||
            !strcmp(cmd, "GROUP")) {
            ret = ld_add_file_list(s1, cmd, 0);
            if (ret)
                return ret;
        } else if (!strcmp(cmd, "OUTPUT_FORMAT") ||
                   !strcmp(cmd, "TARGET")) {
            /* ignore some commands */
            t = ld_next(s1, cmd, sizeof(cmd));
            if (t != '(')
                return tcc_error_noabort("( expected");
            for(;;) {
                t = ld_next(s1, filename, sizeof(filename));
                if (t == LD_TOK_EOF) {
                    return tcc_error_noabort("unexpected end of file");
                } else if (t == ')') {
                    break;
                }
            }
        } else {
            return -1;
        }
    }
    return 0;
}
#endif /* !ELF_OBJ_ONLY */

//// tcc: tccgen.c
