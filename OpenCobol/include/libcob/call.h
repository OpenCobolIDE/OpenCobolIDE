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
 * License along with this library; see the file COPYING.LIB.  If not write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#ifndef COB_CALL_H
#define COB_CALL_H

#include <setjmp.h>
#include <libcob/common.h>

struct cobjmp_buf {
	int	cbj_int[4];
	void	*cbj_ptr[4];
	jmp_buf	cbj_jmp_buf;
	void	*cbj_ptr_rest[2];
};

extern void		cob_set_cancel		(const char *, void *, void *);
extern void		*cob_resolve		(const char *);
extern void		*cob_resolve_1		(const char *);
extern const char	*cob_resolve_error	(void);

extern void		*cob_call_resolve	(const cob_field *);
extern void		*cob_call_resolve_1	(const cob_field *);
extern void		cob_field_cancel	(const cob_field *);
extern void		cobcancel		(const char *);
extern int		cobcall			(const char *, const int, void **);
extern int		cobfunc			(const char *, const int, void **);
extern void		*cobsavenv		(struct cobjmp_buf *);
extern void		*cobsavenv2		(struct cobjmp_buf *, const int);
extern void		coblongjmp		(struct cobjmp_buf *);

#define	cobsetjmp(x)	setjmp (cobsavenv (x))

#ifdef __GNUC__
extern void		cob_call_error (void) __attribute__ ((noreturn));
#else
extern void		cob_call_error (void);
#endif

#endif /* COB_CALL_H */
