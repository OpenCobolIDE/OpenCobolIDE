/*
 * Copyright (C) 2002-2009 Keisuke Nishida
 * Copyright (C) 2007-2009 Roger While
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#ifndef COB_COMMON_H
#define COB_COMMON_H

#ifdef _MSC_VER

#define _CRT_SECURE_NO_DEPRECATE 1
#define inline _inline
#define COB_INLINE _inline
#include <malloc.h>
#include <io.h>
#include <fcntl.h>
#pragma warning(disable: 4996)
#define strncasecmp _strnicmp
#define strcasecmp _stricmp
#define __attribute__(x)
#define __i386__

#ifdef LIBCOB_EXPORTS
#define DLL_EXPIMP __declspec(dllexport)
#else /* LIBCOB_EXPORTS */
#define DLL_EXPIMP __declspec(dllimport)
#endif /* LIBCOB_EXPORTS */

#else /* _MSC_VER */

#define DLL_EXPIMP

#ifdef	__370__
#define inline __inline
#define COB_INLINE __inline
#elif defined(COB_HAS_INLINE)
#define COB_INLINE inline
#else
#define COB_INLINE
#endif

#endif /* _MSC_VER */

#if defined(__GNUC__) && (__GNUC__ >= 3)
#define likely(x)	__builtin_expect(!!(x), 1)
#define unlikely(x)	__builtin_expect(!!(x), 0)
#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1)
#define	COB_NOINLINE	__attribute__((noinline))
#else
#define	COB_NOINLINE
#endif
#else
#define likely(x)	(x)
#define unlikely(x)	(x)
#define	COB_NOINLINE
#endif

#if	' ' == 0x40
#define	COB_EBCDIC_MACHINE
#endif

typedef unsigned char *	ucharptr;

#define	COB_MINI_BUFF		256
#define	COB_SMALL_BUFF		1024
#define	COB_NORMAL_BUFF		2048
#define	COB_MEDIUM_BUFF		8192
#define	COB_LARGE_BUFF		16384
#define	COB_MINI_MAX		(COB_MINI_BUFF - 1)
#define	COB_SMALL_MAX		(COB_SMALL_BUFF - 1)
#define	COB_NORMAL_MAX		(COB_NORMAL_BUFF - 1)
#define	COB_MEDIUM_MAX		(COB_MEDIUM_BUFF - 1)
#define	COB_LARGE_MAX		(COB_LARGE_BUFF - 1)

#define	COB_STACK_SIZE		255

#define	COB_MAX_FIELD_PARAMS	64

/*
 * External
 */

struct cob_external {
	struct cob_external	*next;
	char			*ext_alloc;
	char			*ename;
	int			esize;
};

/*
 * Field
 */

/* field types */

#define COB_TYPE_UNKNOWN		0x00
#define COB_TYPE_GROUP			0x01
#define COB_TYPE_BOOLEAN		0x02

#define COB_TYPE_NUMERIC		0x10
#define COB_TYPE_NUMERIC_DISPLAY	0x10
#define COB_TYPE_NUMERIC_BINARY		0x11
#define COB_TYPE_NUMERIC_PACKED		0x12
#define COB_TYPE_NUMERIC_FLOAT		0x13
#define COB_TYPE_NUMERIC_DOUBLE		0x14
#define COB_TYPE_NUMERIC_EDITED		0x24

#define COB_TYPE_ALPHANUMERIC		0x21
#define COB_TYPE_ALPHANUMERIC_ALL	0x22
#define COB_TYPE_ALPHANUMERIC_EDITED	0x23

#define COB_TYPE_NATIONAL		0x40
#define COB_TYPE_NATIONAL_EDITED	0x41

/* field flags */

#define COB_FLAG_HAVE_SIGN		0x01
#define COB_FLAG_SIGN_SEPARATE		0x02
#define COB_FLAG_SIGN_LEADING		0x04
#define COB_FLAG_BLANK_ZERO		0x08
#define COB_FLAG_JUSTIFIED		0x10
#define COB_FLAG_BINARY_SWAP		0x20
#define COB_FLAG_REAL_BINARY		0x40
#define COB_FLAG_IS_POINTER		0x80

#define COB_FIELD_HAVE_SIGN(f)		((f)->attr->flags & COB_FLAG_HAVE_SIGN)
#define COB_FIELD_SIGN_SEPARATE(f)	((f)->attr->flags & COB_FLAG_SIGN_SEPARATE)
#define COB_FIELD_SIGN_LEADING(f)	((f)->attr->flags & COB_FLAG_SIGN_LEADING)
#define COB_FIELD_BLANK_ZERO(f)		((f)->attr->flags & COB_FLAG_BLANK_ZERO)
#define COB_FIELD_JUSTIFIED(f)		((f)->attr->flags & COB_FLAG_JUSTIFIED)
#define COB_FIELD_BINARY_SWAP(f)	((f)->attr->flags & COB_FLAG_BINARY_SWAP)
#define COB_FIELD_REAL_BINARY(f)	((f)->attr->flags & COB_FLAG_REAL_BINARY)
#define COB_FIELD_IS_POINTER(f)		((f)->attr->flags & COB_FLAG_IS_POINTER)

#define cob_get_sign(f)	(COB_FIELD_HAVE_SIGN (f) ? cob_real_get_sign (f) : 0)
#define cob_put_sign(f,s) if (COB_FIELD_HAVE_SIGN (f)) cob_real_put_sign (f, s)

/* field attributes */

typedef struct {
	unsigned char	type;
	unsigned char	digits;
	signed char	scale;
	unsigned char	flags;
	const char	*pic;
} cob_field_attr;

/* field structure */

typedef struct {
	size_t			size;
	unsigned char		*data;
	const cob_field_attr	*attr;
} cob_field;

#define COB_FIELD_TYPE(f)	((f)->attr->type)
#define COB_FIELD_DIGITS(f)	((f)->attr->digits)
#define COB_FIELD_SCALE(f)	((f)->attr->scale)
#define COB_FIELD_FLAGS(f)	((f)->attr->flags)
#define COB_FIELD_PIC(f)	((f)->attr->pic)
#define COB_FIELD_DATA(f)						  \
  ((f)->data +								  \
   ((COB_FIELD_SIGN_SEPARATE (f) && COB_FIELD_SIGN_LEADING (f)) ? 1 : 0))
#define COB_FIELD_SIZE(f)						\
  ((f)->size - (COB_FIELD_SIGN_SEPARATE (f) ? 1 : 0))

#define COB_FIELD_IS_NUMERIC(f)	(COB_FIELD_TYPE (f) & COB_TYPE_NUMERIC)


/* SIGN */

/*
 * positive: 0123456789
 * negative: pqrstuvwxy
 */
#define GET_SIGN_ASCII(x) x -= 0x40
#define PUT_SIGN_ASCII(x) x += 0x40

#define	COB_DISPLAY_SIGN_ASCII	0
#define	COB_DISPLAY_SIGN_EBCDIC	1

/*
 * Module
 */

struct cob_module {
	struct cob_module		*next;
	const unsigned char		*collating_sequence;
	cob_field			*crt_status;
	cob_field			*cursor_pos;
	cob_field			**cob_procedure_parameters;
	const unsigned char		display_sign;
	const unsigned char		decimal_point;
	const unsigned char		currency_symbol;
	const unsigned char		numeric_separator;
	const unsigned char		flag_filename_mapping;
	const unsigned char		flag_binary_truncate;
	const unsigned char		flag_pretty_display;
	const unsigned char		spare8;
};

/*
 * Exception
 */

/* Exception identifier */
#undef	COB_EXCEPTION
#define	COB_EXCEPTION(code,tag,name,critical)	tag,

enum cob_exception_id {
	COB_EC_ZERO,
#include <libcob/exception.def>
	COB_EC_MAX
};

#undef	COB_EXCEPTION

/*
 * Fatal error
 */

#define COB_FERROR_INITIALIZED	0
#define COB_FERROR_CODEGEN	1
#define COB_FERROR_CHAINING	2
#define COB_FERROR_STACK	3

/*
 * Global variables
 */

DLL_EXPIMP extern int			cob_initialized;
DLL_EXPIMP extern int			cob_exception_code;

DLL_EXPIMP extern struct cob_module	*cob_current_module;

DLL_EXPIMP extern int			cob_call_params;
DLL_EXPIMP extern int			cob_save_call_params;
DLL_EXPIMP extern int			cob_initial_external;

DLL_EXPIMP extern cob_field		cob_zero;		/* ZERO */
DLL_EXPIMP extern cob_field		cob_space;		/* SPACE */
DLL_EXPIMP extern cob_field		cob_high;		/* HIGH-VALUE */
DLL_EXPIMP extern cob_field		cob_low;		/* LOW-VALUE */
DLL_EXPIMP extern cob_field		cob_quote;		/* QUOTE */
DLL_EXPIMP extern cob_field		cob_one;		/* Numeric ONE */

/* convert a digit (e.g., '0') into an integer (e.g., 0) */
#define cob_d2i(x)		((x) - '0')

/* convert an integer (e.g., 0) into a digit (e.g., '0') */
#define cob_i2d(x)		((x) + '0')


/*
 * Function declaration
 */

/* General functions */

extern void cob_init		(int, char **);
extern void cob_module_enter	(struct cob_module *);
extern void cob_module_leave	(struct cob_module *);

#ifdef __GNUC__
extern void cobexit		(int) __attribute__ ((noreturn));
extern void cob_stop_run	(const int) __attribute__ ((noreturn));
extern void cob_fatal_error	(const unsigned int) __attribute__ ((noreturn));
extern void cob_runtime_error	(const char *, ...)
				__attribute__ ((format (printf, 1, 0)));
extern void *cob_malloc		(const size_t) __attribute__ ((malloc));
#else
extern void cobexit		(int);
extern void cob_stop_run	(const int);
extern void cob_fatal_error	(const unsigned int);
extern void cob_runtime_error	(const char *, ...);
extern void *cob_malloc		(const size_t);
#endif

extern const char *cob_get_exception_name	(const int);

extern void cob_set_exception		(const int);
extern void cob_check_version		(const char *, const char *, const int);
extern void cob_accept_date		(cob_field *);
extern void cob_accept_date_yyyymmdd	(cob_field *);
extern void cob_accept_day		(cob_field *);
extern void cob_accept_day_yyyyddd	(cob_field *);
extern void cob_accept_day_of_week	(cob_field *);
extern void cob_accept_time		(cob_field *);
extern void cob_display_command_line	(cob_field *);
extern void cob_accept_command_line	(cob_field *);
extern void cob_set_environment		(cob_field *, cob_field *);
extern void cob_display_environment	(cob_field *);
extern void cob_get_environment		(cob_field *, cob_field *);
extern void cob_accept_environment	(cob_field *);
extern void cob_display_env_value	(cob_field *);
extern void cob_display_arg_number	(cob_field *);
extern void cob_accept_arg_number	(cob_field *);
extern void cob_accept_arg_value	(cob_field *);
extern void cob_chain_setup		(void *, const size_t, const size_t);
extern void cob_allocate		(unsigned char **, cob_field *,
					 cob_field *);
extern void cob_free_alloc		(unsigned char **, unsigned char *);
extern int  cobinit			(void);
extern int  cobtidy			(void);
extern void *cobcommandline		(int, int *, char ***,
					 char ***, char **);
extern char *cobgetenv			(const char *);
extern int  cobputenv			(char *);

/* System routines */
extern int CBL_ERROR_PROC	(unsigned char *, unsigned char *);
extern int CBL_EXIT_PROC	(unsigned char *, unsigned char *);
extern int SYSTEM		(const unsigned char *);
extern int CBL_AND		(unsigned char *, unsigned char *, const int);
extern int CBL_OR		(unsigned char *, unsigned char *, const int);
extern int CBL_NOR		(unsigned char *, unsigned char *, const int);
extern int CBL_XOR		(unsigned char *, unsigned char *, const int);
extern int CBL_IMP		(unsigned char *, unsigned char *, const int);
extern int CBL_NIMP		(unsigned char *, unsigned char *, const int);
extern int CBL_EQ		(unsigned char *, unsigned char *, const int);
extern int CBL_NOT		(unsigned char *, const int);
extern int CBL_XF4		(unsigned char *, unsigned char *);
extern int CBL_XF5		(unsigned char *, unsigned char *);
extern int CBL_X91		(unsigned char *, const unsigned char *,
				 unsigned char *);
extern int CBL_TOUPPER		(unsigned char *, const int);
extern int CBL_TOLOWER		(unsigned char *, const int);
extern int CBL_OC_NANOSLEEP	(unsigned char *);
extern int cob_return_args	(unsigned char *);
extern int cob_parameter_size	(unsigned char *);
extern int cob_acuw_sleep	(unsigned char *);
extern int cob_acuw_justify	(unsigned char *, ...);

/* Utilities */

extern unsigned char	*cob_external_addr	(const char *, const int);
extern unsigned char	*cob_get_pointer	(const unsigned char *);
extern void		*cob_get_prog_pointer	(const unsigned char *);
extern void		cob_set_location	(const char *, const char *,
						 const unsigned int, const char *,
						 const char *, const char *);
extern void		cob_ready_trace		(void);
extern void		cob_reset_trace		(void);

/* Switch */

extern int		cob_get_switch		(const int);
extern void		cob_set_switch		(const int, const int);

/* Comparison */

extern int		cob_cmp			(cob_field *, cob_field *);

/* Class check */

extern int		cob_is_omitted		(const cob_field *);
extern int		cob_is_numeric		(cob_field *);
extern int		cob_is_alpha		(const cob_field *);
extern int		cob_is_upper		(const cob_field *);
extern int		cob_is_lower		(const cob_field *);

/* Table sort */

extern void cob_table_sort_init			(const int, const unsigned char *);
extern void cob_table_sort_init_key		(const int, cob_field *, size_t);
extern void cob_table_sort			(cob_field *, const int);

/* Run-time error checking */

extern void cob_check_numeric			(cob_field *, const char *);
extern void cob_check_based			(const unsigned char *,
						 const char *);
extern void cob_check_odo			(const int, const int,
						 const int, const char *);
extern void cob_check_subscript			(const int, const int,
						 const int, const char *);
extern void cob_check_ref_mod			(const int, const int,
						 const int, const char *);

/* Comparison functions */
extern int cob_numeric_cmp			(cob_field *, cob_field *);

#endif /* COB_COMMON_H */
