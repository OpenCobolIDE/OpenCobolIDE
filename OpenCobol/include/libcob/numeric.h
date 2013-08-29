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

#ifndef COB_NUMERIC_H
#define COB_NUMERIC_H

#include <gmp.h>
#include <libcob/common.h>

#define COB_STORE_ROUND			0x01
#define COB_STORE_KEEP_ON_OVERFLOW	0x02
#define COB_STORE_TRUNC_ON_OVERFLOW	0x04

/*
 * Internal representation of decimal numbers.
 *
 *   n = value / 10^scale
 */
typedef struct {
	mpz_t	value;
	int	scale;
} cob_decimal;

extern void	cob_decimal_init	(cob_decimal *);
extern void	cob_decimal_set_field	(cob_decimal *, cob_field *);
extern int	cob_decimal_get_field	(cob_decimal *, cob_field *, const int);
extern void	cob_decimal_add		(cob_decimal *, cob_decimal *);
extern void	cob_decimal_sub		(cob_decimal *, cob_decimal *);
extern void	cob_decimal_mul		(cob_decimal *, cob_decimal *);
extern void	cob_decimal_div		(cob_decimal *, cob_decimal *);
extern void	cob_decimal_pow		(cob_decimal *, cob_decimal *);
extern int	cob_decimal_cmp		(cob_decimal *, cob_decimal *);

extern int	cob_add			(cob_field *, cob_field *, const int);
extern int	cob_sub			(cob_field *, cob_field *, const int);
extern int	cob_add_int		(cob_field *, const int);
extern int	cob_sub_int		(cob_field *, const int);
extern int	cob_div_quotient	(cob_field *, cob_field *,
					 cob_field *, const int);
extern int	cob_div_remainder	(cob_field *, const int);

extern int	cob_cmp_int		(cob_field *, const int);
extern int	cob_cmp_uint		(cob_field *, const unsigned int);
extern int	cob_cmp_packed		(cob_field *, int);
extern int	cob_cmp_numdisp		(const unsigned char *,
					 const size_t, const int);
extern int	cob_cmp_sign_numdisp	(const unsigned char *,
					 const size_t, const int);
extern int	cob_cmp_long_numdisp	(const unsigned char *,
					 const size_t, const int);
extern void	cob_set_packed_zero	(cob_field *);
extern void	cob_set_packed_int	(cob_field *, const int);

extern int	cob_cmp_long_sign_numdisp	(const unsigned char *,
						 const size_t, const int);

#endif /* COB_NUMERIC_H */
