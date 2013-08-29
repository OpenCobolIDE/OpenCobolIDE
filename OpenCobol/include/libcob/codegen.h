/*
 * Copyright (C) 2006-2009 Roger While
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

#ifndef COB_CODEGEN_H
#define COB_CODEGEN_H

#ifdef	COB_LOCAL_INLINE
#define	COB_STATIC	static
#else
#define	COB_STATIC
extern int cob_get_numdisp		(const unsigned char *, const size_t);
extern int cob_cmp_packed_int		(const cob_field *, const int);
extern int cob_get_packed_int		(const cob_field *);
extern int cob_add_packed_int		(cob_field *, const int);

extern int cob_cmp_u8_binary		(const unsigned char *, const int);
extern int cob_cmp_s8_binary		(const unsigned char *, const int);
extern int cob_cmp_u16_binary		(const unsigned char *, const int);
extern int cob_cmp_s16_binary		(const unsigned char *, const int);
extern int cob_cmp_u24_binary		(const unsigned char *, const int);
extern int cob_cmp_s24_binary		(const unsigned char *, const int);
extern int cob_cmp_u32_binary		(const unsigned char *, const int);
extern int cob_cmp_s32_binary		(const unsigned char *, const int);
extern int cob_cmp_u40_binary		(const unsigned char *, const int);
extern int cob_cmp_s40_binary		(const unsigned char *, const int);
extern int cob_cmp_u48_binary		(const unsigned char *, const int);
extern int cob_cmp_s48_binary		(const unsigned char *, const int);
extern int cob_cmp_u56_binary		(const unsigned char *, const int);
extern int cob_cmp_s56_binary		(const unsigned char *, const int);
extern int cob_cmp_u64_binary		(const unsigned char *, const int);
extern int cob_cmp_s64_binary		(const unsigned char *, const int);

extern int cob_cmp_align_u16_binary	(const unsigned char *, const int);
extern int cob_cmp_align_s16_binary	(const unsigned char *, const int);
extern int cob_cmp_align_u32_binary	(const unsigned char *, const int);
extern int cob_cmp_align_s32_binary	(const unsigned char *, const int);
extern int cob_cmp_align_u64_binary	(const unsigned char *, const int);
extern int cob_cmp_align_s64_binary	(const unsigned char *, const int);

extern void cob_add_u8_binary		(unsigned char *, const int);
extern void cob_add_s8_binary		(unsigned char *, const int);
extern void cob_add_u16_binary		(unsigned char *, const int);
extern void cob_add_s16_binary		(unsigned char *, const int);
extern void cob_add_u24_binary		(unsigned char *, const int);
extern void cob_add_s24_binary		(unsigned char *, const int);
extern void cob_add_u32_binary		(unsigned char *, const int);
extern void cob_add_s32_binary		(unsigned char *, const int);
extern void cob_add_u40_binary		(unsigned char *, const int);
extern void cob_add_s40_binary		(unsigned char *, const int);
extern void cob_add_u48_binary		(unsigned char *, const int);
extern void cob_add_s48_binary		(unsigned char *, const int);
extern void cob_add_u56_binary		(unsigned char *, const int);
extern void cob_add_s56_binary		(unsigned char *, const int);
extern void cob_add_u64_binary		(unsigned char *, const int);
extern void cob_add_s64_binary		(unsigned char *, const int);

extern void cob_add_align_u16_binary	(unsigned char *, const int);
extern void cob_add_align_s16_binary	(unsigned char *, const int);
extern void cob_add_align_u32_binary	(unsigned char *, const int);
extern void cob_add_align_s32_binary	(unsigned char *, const int);
extern void cob_add_align_u64_binary	(unsigned char *, const int);
extern void cob_add_align_s64_binary	(unsigned char *, const int);

extern void cob_sub_u8_binary		(unsigned char *, const int);
extern void cob_sub_s8_binary		(unsigned char *, const int);
extern void cob_sub_u16_binary		(unsigned char *, const int);
extern void cob_sub_s16_binary		(unsigned char *, const int);
extern void cob_sub_u24_binary		(unsigned char *, const int);
extern void cob_sub_s24_binary		(unsigned char *, const int);
extern void cob_sub_u32_binary		(unsigned char *, const int);
extern void cob_sub_s32_binary		(unsigned char *, const int);
extern void cob_sub_u40_binary		(unsigned char *, const int);
extern void cob_sub_s40_binary		(unsigned char *, const int);
extern void cob_sub_u48_binary		(unsigned char *, const int);
extern void cob_sub_s48_binary		(unsigned char *, const int);
extern void cob_sub_u56_binary		(unsigned char *, const int);
extern void cob_sub_s56_binary		(unsigned char *, const int);
extern void cob_sub_u64_binary		(unsigned char *, const int);
extern void cob_sub_s64_binary		(unsigned char *, const int);

extern void cob_sub_align_u16_binary	(unsigned char *, const int);
extern void cob_sub_align_s16_binary	(unsigned char *, const int);
extern void cob_sub_align_u32_binary	(unsigned char *, const int);
extern void cob_sub_align_s32_binary	(unsigned char *, const int);
extern void cob_sub_align_u64_binary	(unsigned char *, const int);
extern void cob_sub_align_s64_binary	(unsigned char *, const int);

#ifndef WORDS_BIGENDIAN
extern int cob_cmpswp_u16_binary	(const unsigned char *, const int);
extern int cob_cmpswp_s16_binary	(const unsigned char *, const int);
extern int cob_cmpswp_u24_binary	(const unsigned char *, const int);
extern int cob_cmpswp_s24_binary	(const unsigned char *, const int);
extern int cob_cmpswp_u32_binary	(const unsigned char *, const int);
extern int cob_cmpswp_s32_binary	(const unsigned char *, const int);
extern int cob_cmpswp_u40_binary	(const unsigned char *, const int);
extern int cob_cmpswp_s40_binary	(const unsigned char *, const int);
extern int cob_cmpswp_u48_binary	(const unsigned char *, const int);
extern int cob_cmpswp_s48_binary	(const unsigned char *, const int);
extern int cob_cmpswp_u56_binary	(const unsigned char *, const int);
extern int cob_cmpswp_s56_binary	(const unsigned char *, const int);
extern int cob_cmpswp_u64_binary	(const unsigned char *, const int);
extern int cob_cmpswp_s64_binary	(const unsigned char *, const int);

extern int cob_cmpswp_align_u16_binary	(const unsigned char *, const int);
extern int cob_cmpswp_align_s16_binary	(const unsigned char *, const int);
extern int cob_cmpswp_align_u32_binary	(const unsigned char *, const int);
extern int cob_cmpswp_align_s32_binary	(const unsigned char *, const int);
extern int cob_cmpswp_align_u64_binary	(const unsigned char *, const int);
extern int cob_cmpswp_align_s64_binary	(const unsigned char *, const int);

extern void cob_addswp_u16_binary	(unsigned char *, const int);
extern void cob_addswp_s16_binary	(unsigned char *, const int);
extern void cob_addswp_u24_binary	(unsigned char *, const int);
extern void cob_addswp_s24_binary	(unsigned char *, const int);
extern void cob_addswp_u32_binary	(unsigned char *, const int);
extern void cob_addswp_s32_binary	(unsigned char *, const int);
extern void cob_addswp_u40_binary	(unsigned char *, const int);
extern void cob_addswp_s40_binary	(unsigned char *, const int);
extern void cob_addswp_u48_binary	(unsigned char *, const int);
extern void cob_addswp_s48_binary	(unsigned char *, const int);
extern void cob_addswp_u56_binary	(unsigned char *, const int);
extern void cob_addswp_s56_binary	(unsigned char *, const int);
extern void cob_addswp_u64_binary	(unsigned char *, const int);
extern void cob_addswp_s64_binary	(unsigned char *, const int);

extern void cob_subswp_u16_binary	(unsigned char *, const int);
extern void cob_subswp_s16_binary	(unsigned char *, const int);
extern void cob_subswp_u24_binary	(unsigned char *, const int);
extern void cob_subswp_s24_binary	(unsigned char *, const int);
extern void cob_subswp_u32_binary	(unsigned char *, const int);
extern void cob_subswp_s32_binary	(unsigned char *, const int);
extern void cob_subswp_u40_binary	(unsigned char *, const int);
extern void cob_subswp_s40_binary	(unsigned char *, const int);
extern void cob_subswp_u48_binary	(unsigned char *, const int);
extern void cob_subswp_s48_binary	(unsigned char *, const int);
extern void cob_subswp_u56_binary	(unsigned char *, const int);
extern void cob_subswp_s56_binary	(unsigned char *, const int);
extern void cob_subswp_u64_binary	(unsigned char *, const int);
extern void cob_subswp_s64_binary	(unsigned char *, const int);

extern void cob_setswp_u16_binary	(unsigned char *, const int);
extern void cob_setswp_s16_binary	(unsigned char *, const int);
extern void cob_setswp_u24_binary	(unsigned char *, const int);
extern void cob_setswp_s24_binary	(unsigned char *, const int);
extern void cob_setswp_u32_binary	(unsigned char *, const int);
extern void cob_setswp_s32_binary	(unsigned char *, const int);
extern void cob_setswp_u40_binary	(unsigned char *, const int);
extern void cob_setswp_s40_binary	(unsigned char *, const int);
extern void cob_setswp_u48_binary	(unsigned char *, const int);
extern void cob_setswp_s48_binary	(unsigned char *, const int);
extern void cob_setswp_u56_binary	(unsigned char *, const int);
extern void cob_setswp_s56_binary	(unsigned char *, const int);
extern void cob_setswp_u64_binary	(unsigned char *, const int);
extern void cob_setswp_s64_binary	(unsigned char *, const int);
#endif
#endif

#if	defined(COB_LOCAL_INLINE) || defined(COB_LIB_INCLUDE)

#if !defined(__i386__) && !defined(__x86_64__) && !defined(__powerpc__) && !defined(__powerpc64__) && !defined(__ppc__) && !defined(__amd64__)
	#if defined(_MSC_VER)
		#define ALLOW_MISALIGNED
		#define MISALIGNED __unaligned
	#else
		#define MISALIGNED
	#endif
#else
	#define ALLOW_MISALIGNED
	#define MISALIGNED
#endif

COB_STATIC int
cob_get_numdisp (const unsigned char *data, const size_t size)
{
	int     retval = 0;
	size_t	n;

	for (n = 0; n < size; ++n, ++data) {
		retval *= 10;
		if (*data > '9') {
			retval += 10;
		} else {
			retval += (*data - (unsigned char)'0');
		}
	}
	return retval;
}

COB_STATIC int
cob_cmp_packed_int (const cob_field *f, const int n)
{
	unsigned char	*p;
	size_t		size;
	int		val = 0;

	p = f->data;
	for (size = 0; size < f->size - 1; ++size, ++p) {
		val *= 10;
		val += *p >> 4;
		val *= 10;
		val += *p & 0x0f;
	}
	val *= 10;
	val += *p >> 4;
	if ((*p & 0x0f) == 0x0d) {
		val = -val;
	}
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_get_packed_int (const cob_field *f)
{
	unsigned char	*p;
	size_t		size;
	int		val = 0;

	p = f->data;
	for (size = 0; size < f->size - 1; ++size, ++p) {
		val *= 10;
		val += *p >> 4;
		val *= 10;
		val += *p & 0x0f;
	}
	val *= 10;
	val += *p >> 4;
	if ((*p & 0x0f) == 0x0d) {
		val = -val;
	}
	return val;
}

COB_STATIC int
cob_add_packed_int (cob_field *f, const int val)
{
	unsigned char	*p;
	size_t		size;
	int		carry = 0;
	int		n;
	int		inc;

	if (val == 0) {
		return 0;
	}
	p = f->data + f->size - 1;
	if ((*p & 0x0f) == 0x0d) {
		if (val > 0) {
			return cob_add_int (f, val);
		}
		n = -val;
	} else {
		if (val < 0) {
			return cob_add_int (f, val);
		}
		n = val;
	}
	inc = (*p >> 4) + (n % 10);
	n /= 10;
	carry = inc / 10;
	*p = ((inc % 10) << 4) | (*p & 0x0f);
	p--;

	for (size = 0; size < f->size - 1; ++size, --p) {
		if (!carry && !n) {
			break;
		}
		inc = ((*p >> 4) * 10) + (*p & 0x0f) + carry + (n % 100);
		carry = inc / 100;
		n /= 100;
		inc %= 100;
		*p = ((inc / 10) << 4) | (inc % 10);
	}
	return 0;
}

/* Aligned variants */

#ifndef	ALLOW_MISALIGNED

/* Aligned compares */

COB_STATIC int
cob_cmp_align_u16_binary (const unsigned char *p, const int n)
{
	unsigned short	val;

	if (n < 0) {
		return 1;
	}
	val = *(unsigned short MISALIGNED *)p;
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_align_s16_binary (const unsigned char *p, const int n)
{
	short	val;

	val = *(short MISALIGNED *)p;
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_align_u32_binary (const unsigned char *p, const int n)
{
	unsigned int	val;

	if (n < 0) {
		return 1;
	}
	val = *(unsigned int MISALIGNED *)p;
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_align_s32_binary (const unsigned char *p, const int n)
{
	int	val;

	val = *(int MISALIGNED *)p;
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_align_u64_binary (const unsigned char *p, const int n)
{
	unsigned long long	val;

	if (n < 0) {
		return 1;
	}
	val = *(unsigned long long MISALIGNED *)p;
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_align_s64_binary (const unsigned char *p, const int n)
{
	long long	val;

	val = *(long long MISALIGNED *)p;
	return (val < n) ? -1 : (val > n);
}

/* Aligned adds */

COB_STATIC void
cob_add_align_u16_binary (unsigned char *p, const int val)
{
	*(unsigned short MISALIGNED *)p += val;
}

COB_STATIC void
cob_add_align_s16_binary (unsigned char *p, const int val)
{
	*(short MISALIGNED *)p += val;
}

COB_STATIC void
cob_add_align_u32_binary (unsigned char *p, const int val)
{
	*(unsigned int MISALIGNED *)p += val;
}

COB_STATIC void
cob_add_align_s32_binary (unsigned char *p, const int val)
{
	*(int MISALIGNED *)p += val;
}

COB_STATIC void
cob_add_align_u64_binary (unsigned char *p, const int val)
{
	*(unsigned long long MISALIGNED *)p += val;
}

COB_STATIC void
cob_add_align_s64_binary (unsigned char *p, const int val)
{
	*(long long MISALIGNED *)p += val;
}

/* Aligned subtracts */

COB_STATIC void
cob_sub_align_u16_binary (unsigned char *p, const int val)
{
	*(unsigned short MISALIGNED *)p -= val;
}

COB_STATIC void
cob_sub_align_s16_binary (unsigned char *p, const int val)
{
	*(short MISALIGNED *)p -= val;
}

COB_STATIC void
cob_sub_align_u32_binary (unsigned char *p, const int val)
{
	*(unsigned int MISALIGNED *)p -= val;
}

COB_STATIC void
cob_sub_align_s32_binary (unsigned char *p, const int val)
{
	*(int MISALIGNED *)p -= val;
}

COB_STATIC void
cob_sub_align_u64_binary (unsigned char *p, const int val)
{
	*(unsigned long long MISALIGNED *)p -= val;
}

COB_STATIC void
cob_sub_align_s64_binary (unsigned char *p, const int val)
{
	*(long long MISALIGNED *)p -= val;
}

#ifndef WORDS_BIGENDIAN
COB_STATIC int
cob_cmpswp_align_u16_binary (const unsigned char *p, const int n)
{
	unsigned short	val;

	if (n < 0) {
		return 1;
	}
	val = COB_BSWAP_16 (*(unsigned short MISALIGNED *)p);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_align_s16_binary (const unsigned char *p, const int n)
{
	short	val;

	val = COB_BSWAP_16 (*(short MISALIGNED *)p);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_align_u32_binary (const unsigned char *p, const int n)
{
	unsigned int	val;

	if (n < 0) {
		return 1;
	}
	val = COB_BSWAP_32 (*(unsigned int MISALIGNED *)p);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_align_s32_binary (const unsigned char *p, const int n)
{
	int	val;

	val = COB_BSWAP_32 (*(int MISALIGNED *)p);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_align_u64_binary (const unsigned char *p, const int n)
{
	unsigned long long	val;

	if (n < 0) {
		return 1;
	}
	val = COB_BSWAP_64 (*(unsigned long long MISALIGNED *)p);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_align_s64_binary (const unsigned char *p, const int n)
{
	long long	val;

	val = COB_BSWAP_64 (*(long long MISALIGNED *)p);
	return (val < n) ? -1 : (val > n);
}

#endif	/* WORDS_BIGENDIAN */
#endif	/* ALLOW_MISALIGNED */

/* Binary compare */

COB_STATIC int
cob_cmp_u8_binary (const unsigned char *p, const int n)
{
	if (n < 0) {
		return 1;
	}
	return (*p < n) ? -1 : (*p > n);
}

COB_STATIC int
cob_cmp_s8_binary (const unsigned char *p, const int n)
{
	return (*(const signed char *)p < n) ? -1 : (*(const signed char *)p > n);
}

COB_STATIC int
cob_cmp_u16_binary (const unsigned char *p, const int n)
{
#ifndef ALLOW_MISALIGNED
	unsigned char	*x;
#endif
	unsigned short	val;

	if (n < 0) {
		return 1;
	}
#ifdef ALLOW_MISALIGNED
	val = *(const unsigned short MISALIGNED *)p;
#else
	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
#endif
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_s16_binary (const unsigned char *p, const int n)
{
	short	val;

#ifdef ALLOW_MISALIGNED
	val = *(const short MISALIGNED *)p;
#else
	unsigned char	*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
#endif
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_u24_binary (const unsigned char *p, const int n)
{
	unsigned char	*x;
	unsigned int	val = 0;

	if (n < 0) {
		return 1;
	}
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&val) + 1;
#else
	x = (unsigned char *)&val;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_s24_binary (const unsigned char *p, const int n)
{
	unsigned char	*x;
	int		val = 0;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&val;
#else
	x = ((unsigned char *)&val) + 1;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	val >>= 8;	/* shift with sign */
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_u32_binary (const unsigned char *p, const int n)
{
#ifndef ALLOW_MISALIGNED
	unsigned char	*x;
#endif
	unsigned int	val;

	if (n < 0) {
		return 1;
	}
#ifdef ALLOW_MISALIGNED
	val = *(const unsigned int MISALIGNED *)p;
#else
	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
#endif
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_s32_binary (const unsigned char *p, const int n)
{
	int	val;

#ifdef ALLOW_MISALIGNED
	val = *(const int MISALIGNED *)p;
#else
	unsigned char	*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
#endif
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_u40_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char		*x;

	if (n < 0) {
		return 1;
	}
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&val) + 3;
#else
	x = (unsigned char *)&val;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_s40_binary (const unsigned char *p, const int n)
{
	long long		val = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&val;
#else
	x = ((unsigned char *)&val) + 3;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	val >>= 24;	/* shift with sign */
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_u48_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char		*x;

	if (n < 0) {
		return 1;
	}
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&val) + 2;
#else
	x = (unsigned char *)&val;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_s48_binary (const unsigned char *p, const int n)
{
	long long		val = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&val;
#else
	x = ((unsigned char *)&val) + 2;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	val >>= 16;	/* shift with sign */
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_u56_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char		*x;

	if (n < 0) {
		return 1;
	}
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&val) + 1;
#else
	x = (unsigned char *)&val;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_s56_binary (const unsigned char *p, const int n)
{
	long long		val = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&val;
#else
	x = ((unsigned char *)&val) + 1;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	val >>= 8;	/* shift with sign */
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_u64_binary (const unsigned char *p, const int n)
{
#ifndef ALLOW_MISALIGNED
	unsigned char		*x;
#endif
	unsigned long long	val;

	if (n < 0) {
		return 1;
	}
#ifdef ALLOW_MISALIGNED
	val = *(const unsigned long long MISALIGNED *)p;
#else
	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	*(x + 7) = *(p + 7);
#endif
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmp_s64_binary (const unsigned char *p, const int n)
{
	long long	val;

#ifdef ALLOW_MISALIGNED
	val = *(const long long MISALIGNED *)p;
#else
	unsigned char		*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	*(x + 7) = *(p + 7);
#endif
	return (val < n) ? -1 : (val > n);
}

/* Add/Subtract */

COB_STATIC void
cob_add_u8_binary (unsigned char *p, const int val)
{
	*p += val;
}

COB_STATIC void
cob_add_s8_binary (unsigned char *p, const int val)
{
	*(signed char *)p += val;
}

COB_STATIC void
cob_add_u16_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned short MISALIGNED *)p += val;
#else
	unsigned char	*x;
	unsigned short	n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	n += val;
	*p = *x;
	*(p + 1) = *(x + 1);
#endif
}

COB_STATIC void
cob_add_s16_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(short MISALIGNED *)p += val;
#else
	unsigned char	*x;
	short		n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	n += val;
	*p = *x;
	*(p + 1) = *(x + 1);
#endif
}

COB_STATIC void
cob_add_u24_binary (unsigned char *p, const int val)
{
	unsigned char	*x;
	unsigned int	n = 0;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 1;
#else
	x = (unsigned char *)&n;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	n += val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
}

COB_STATIC void
cob_add_s24_binary (unsigned char *p, const int val)
{
	unsigned char	*x;
	int		n = 0;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&n;
#else
	x = ((unsigned char *)&n) + 1;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	n >>= 8;	/* shift with sign */
	n += val;
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 1;
#else
	x = (unsigned char *)&n;
#endif
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
}

COB_STATIC void
cob_add_u32_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned int MISALIGNED *)p += val;
#else
	unsigned char	*x;
	unsigned int	n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	n += val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
#endif
}

COB_STATIC void
cob_add_s32_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(int MISALIGNED *)p += val;
#else
	unsigned char	*x;
	int		n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	n += val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
#endif
}

COB_STATIC void
cob_add_u40_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 3;
#else
	x = (unsigned char *)&n;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	n += val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
}

COB_STATIC void
cob_add_s40_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&n;
#else
	x = ((unsigned char *)&n) + 3;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	n >>= 24;	/* shift with sign */
	n += val;
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 3;
#else
	x = (unsigned char *)&n;
#endif
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
}

COB_STATIC void
cob_add_u48_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 2;
#else
	x = (unsigned char *)&n;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	n += val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
}

COB_STATIC void
cob_add_s48_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&n;
#else
	x = ((unsigned char *)&n) + 2;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	n >>= 16;	/* shift with sign */
	n += val;
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 2;
#else
	x = (unsigned char *)&n;
#endif
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
}

COB_STATIC void
cob_add_u56_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 1;
#else
	x = (unsigned char *)&n;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	n += val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
	*(p + 6) = *(x + 6);
}

COB_STATIC void
cob_add_s56_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&n;
#else
	x = ((unsigned char *)&n) + 1;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	n >>= 8;	/* shift with sign */
	n += val;
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 1;
#else
	x = (unsigned char *)&n;
#endif
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
	*(p + 6) = *(x + 6);
}

COB_STATIC void
cob_add_u64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned long long MISALIGNED *)p += val;
#else
	unsigned char		*x;
	unsigned long long	n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	*(x + 7) = *(p + 7);
	n += val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
	*(p + 6) = *(x + 6);
	*(p + 7) = *(x + 7);
#endif
}

COB_STATIC void
cob_add_s64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(long long MISALIGNED *)p += val;
#else
	unsigned char		*x;
	long long		n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	*(x + 7) = *(p + 7);
	n += val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
	*(p + 6) = *(x + 6);
	*(p + 7) = *(x + 7);
#endif
}

COB_STATIC void
cob_sub_u8_binary (unsigned char *p, const int val)
{
	*p -= val;
}

COB_STATIC void
cob_sub_s8_binary (unsigned char *p, const int val)
{
	*(signed char *)p -= val;
}

COB_STATIC void
cob_sub_u16_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned short MISALIGNED *)p -= val;
#else
	unsigned char	*x;
	unsigned short	n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	n -= val;
	*p = *x;
	*(p + 1) = *(x + 1);
#endif
}

COB_STATIC void
cob_sub_s16_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(short MISALIGNED *)p -= val;
#else
	unsigned char	*x;
	short		n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	n -= val;
	*p = *x;
	*(p + 1) = *(x + 1);
#endif
}

COB_STATIC void
cob_sub_u24_binary (unsigned char *p, const int val)
{
	unsigned char	*x;
	unsigned int	n = 0;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 1;
#else
	x = (unsigned char *)&n;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	n -= val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
}

COB_STATIC void
cob_sub_s24_binary (unsigned char *p, const int val)
{
	unsigned char	*x;
	int		n = 0;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&n;
#else
	x = ((unsigned char *)&n) + 1;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	n >>= 8;	/* shift with sign */
	n -= val;
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 1;
#else
	x = (unsigned char *)&n;
#endif
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
}

COB_STATIC void
cob_sub_u32_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned int MISALIGNED *)p -= val;
#else
	unsigned char	*x;
	unsigned int	n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	n -= val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
#endif
}

COB_STATIC void
cob_sub_s32_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(int MISALIGNED *)p -= val;
#else
	unsigned char	*x;
	int		n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	n -= val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
#endif
}

COB_STATIC void
cob_sub_u40_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 3;
#else
	x = (unsigned char *)&n;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	n -= val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
}

COB_STATIC void
cob_sub_s40_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&n;
#else
	x = ((unsigned char *)&n) + 3;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	n >>= 24;	/* shift with sign */
	n -= val;
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 3;
#else
	x = (unsigned char *)&n;
#endif
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
}

COB_STATIC void
cob_sub_u48_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 2;
#else
	x = (unsigned char *)&n;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	n -= val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
}

COB_STATIC void
cob_sub_s48_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&n;
#else
	x = ((unsigned char *)&n) + 2;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	n >>= 16;	/* shift with sign */
	n -= val;
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 2;
#else
	x = (unsigned char *)&n;
#endif
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
}

COB_STATIC void
cob_sub_u56_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 1;
#else
	x = (unsigned char *)&n;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	n -= val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
	*(p + 6) = *(x + 6);
}

COB_STATIC void
cob_sub_s56_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

#ifdef	WORDS_BIGENDIAN
	x = (unsigned char *)&n;
#else
	x = ((unsigned char *)&n) + 1;
#endif
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	n >>= 8;	/* shift with sign */
	n -= val;
#ifdef	WORDS_BIGENDIAN
	x = ((unsigned char *)&n) + 1;
#else
	x = (unsigned char *)&n;
#endif
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
	*(p + 6) = *(x + 6);
}

COB_STATIC void
cob_sub_u64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(unsigned long long MISALIGNED *)p -= val;
#else
	unsigned char		*x;
	unsigned long long	n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	*(x + 7) = *(p + 7);
	n -= val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
	*(p + 6) = *(x + 6);
	*(p + 7) = *(x + 7);
#endif
}

COB_STATIC void
cob_sub_s64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	*(long long MISALIGNED *)p -= val;
#else
	unsigned char		*x;
	long long		n;

	x = (unsigned char *)&n;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	*(x + 7) = *(p + 7);
	n -= val;
	*p = *x;
	*(p + 1) = *(x + 1);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 4);
	*(p + 5) = *(x + 5);
	*(p + 6) = *(x + 6);
	*(p + 7) = *(x + 7);
#endif
}

#ifndef WORDS_BIGENDIAN

/* Binary swapped compare */
COB_STATIC int
cob_cmpswp_u16_binary (const unsigned char *p, const int n)
{
#ifndef ALLOW_MISALIGNED
	unsigned char	*x;
#endif
	unsigned short	val;

	if (n < 0) {
		return 1;
	}
#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_16 (*(unsigned short MISALIGNED *)p);
#else
	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	val = COB_BSWAP_16 (val);
#endif
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_s16_binary (const unsigned char *p, const int n)
{
	short	val;

#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_16 (*(short MISALIGNED *)p);
#else
	unsigned char	*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	val = COB_BSWAP_16 (val);
#endif
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_u24_binary (const unsigned char *p, const int n)
{
	unsigned char	*x;
	unsigned int	val = 0;

	if (n < 0) {
		return 1;
	}
	x = ((unsigned char *)&val) + 1;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	val = COB_BSWAP_32 (val);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_s24_binary (const unsigned char *p, const int n)
{
	unsigned char	*x;
	int		val = 0;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	val = COB_BSWAP_32 (val);
	val >>= 8;	/* shift with sign */
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_u32_binary (const unsigned char *p, const int n)
{
#ifndef ALLOW_MISALIGNED
	unsigned char	*x;
#endif
	unsigned int	val;

	if (n < 0) {
		return 1;
	}
#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_32 (*(const unsigned int MISALIGNED *)p);
#else
	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	val = COB_BSWAP_32 (val);
#endif
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_s32_binary (const unsigned char *p, const int n)
{
	int	val;

#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_32 (*(const int MISALIGNED *)p);
#else
	unsigned char	*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	val = COB_BSWAP_32 (val);
#endif
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_u40_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char	*x;

	if (n < 0) {
		return 1;
	}
	x = ((unsigned char *)&val) + 3;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	val = COB_BSWAP_64 (val);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_s40_binary (const unsigned char *p, const int n)
{
	long long	val = 0;
	unsigned char	*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	val = COB_BSWAP_64 (val);
	val >>= 24;	/* shift with sign */
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_u48_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char	*x;

	if (n < 0) {
		return 1;
	}
	x = ((unsigned char *)&val) + 2;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	val = COB_BSWAP_64 (val);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_s48_binary (const unsigned char *p, const int n)
{
	long long	val = 0;
	unsigned char	*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	val = COB_BSWAP_64 (val);
	val >>= 16;	/* shift with sign */
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_u56_binary (const unsigned char *p, const int n)
{
	unsigned long long	val = 0;
	unsigned char	*x;

	if (n < 0) {
		return 1;
	}
	x = ((unsigned char *)&val) + 1;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	val = COB_BSWAP_64 (val);
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_s56_binary (const unsigned char *p, const int n)
{
	long long	val = 0;
	unsigned char	*x;

	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	val = COB_BSWAP_64 (val);
	val >>= 8;	/* shift with sign */
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_u64_binary (const unsigned char *p, const int n)
{
#ifndef ALLOW_MISALIGNED
	unsigned char		*x;
#endif
	unsigned long long	val;

	if (n < 0) {
		return 1;
	}
#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_64 (*(const unsigned long long MISALIGNED *)p);
#else
	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	*(x + 7) = *(p + 7);
	val = COB_BSWAP_64 (val);
#endif
	return (val < n) ? -1 : (val > n);
}

COB_STATIC int
cob_cmpswp_s64_binary (const unsigned char *p, const int n)
{
	long long	val;

#ifdef ALLOW_MISALIGNED
	val = COB_BSWAP_64 (*(const long long MISALIGNED *)p);
#else
	unsigned char	*x;
	x = (unsigned char *)&val;
	*x = *p;
	*(x + 1) = *(p + 1);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 4);
	*(x + 5) = *(p + 5);
	*(x + 6) = *(p + 6);
	*(x + 7) = *(p + 7);
	val = COB_BSWAP_64 (val);
#endif
	return (val < n) ? -1 : (val > n);
}

/* Binary swapped add */
COB_STATIC void
cob_addswp_u16_binary (unsigned char *p, const int val)
{
	unsigned short	n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_16 (*(unsigned short MISALIGNED *)p);
	n += val;
	*(unsigned short MISALIGNED *)p = COB_BSWAP_16(n);
#else
	n = (unsigned short)((p[0] << 8) | p[1]);
	n += val;
	p[0] = (unsigned char)(n >> 8);
	p[1] = (unsigned char)n;
#endif
}

COB_STATIC void
cob_addswp_s16_binary (unsigned char *p, const int val)
{
	short		n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_16 (*(short MISALIGNED *)p);
	n += val;
	*(short MISALIGNED *)p = COB_BSWAP_16(n);
#else
	n = (short)((p[0] << 8) | p[1]);
	n += val;
	p[0] = (unsigned char)(n >> 8);
	p[1] = (unsigned char)n;
#endif
}

COB_STATIC void
cob_addswp_u24_binary (unsigned char *p, const int val)
{
	unsigned char	*x;
	unsigned int	n = 0;

	x = (unsigned char *)&n;
	*x = *(p + 2);
	*(x + 1) = *(p + 1);
	*(x + 2) = *p;
	n += val;
	*p = *(x + 2);
	*(p + 1) = *(x + 1);
	*(p + 2) = *x;
}

COB_STATIC void
cob_addswp_s24_binary (unsigned char *p, const int val)
{
	unsigned char	*x;
	int		n = 0;

	x = ((unsigned char *)&n) + 1;
	*x = *(p + 2);
	*(x + 1) = *(p + 1);
	*(x + 2) = *p;
	n >>= 8;	/* shift with sign */
	n += val;
	x = (unsigned char *)&n;
	*p = *(x + 2);
	*(p + 1) = *(x + 1);
	*(p + 2) = *x;
}

COB_STATIC void
cob_addswp_u32_binary (unsigned char *p, const int val)
{
	unsigned int	n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_32 (*(unsigned int MISALIGNED *)p);
	n += val;
	*(unsigned int MISALIGNED *)p = COB_BSWAP_32(n);
#else
	n = (p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];
	n += val;
	*p++ = (unsigned char)(n >> 24);
	*p++ = (unsigned char)(n >> 16);
	*p++ = (unsigned char)(n >> 8);
	*p = (unsigned char)n;
#endif
}

COB_STATIC void
cob_addswp_s32_binary (unsigned char *p, const int val)
{
	int		n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_32 (*(int MISALIGNED *)p);
	n += val;
	*(int MISALIGNED *)p = COB_BSWAP_32(n);
#else
	n = (int)((p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3]);
	n += val;
	*p++ = (unsigned char)(n >> 24);
	*p++ = (unsigned char)(n >> 16);
	*p++ = (unsigned char)(n >> 8);
	*p = (unsigned char)n;
#endif
}

COB_STATIC void
cob_addswp_u40_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

	x = (unsigned char *)&n;
	*x = *(p + 4);
	*(x + 1) = *(p + 3);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 1);
	*(x + 4) = *p;
	n += val;
	*p = *(x + 4);
	*(p + 1) = *(x + 3);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 1);
	*(p + 4) = *x;
}

COB_STATIC void
cob_addswp_s40_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

	x = ((unsigned char *)&n) + 3;
	*x = *(p + 4);
	*(x + 1) = *(p + 3);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 1);
	*(x + 4) = *p;
	n >>= 24;	/* shift with sign */
	n += val;
	x = (unsigned char *)&n;
	*p = *(x + 4);
	*(p + 1) = *(x + 3);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 1);
	*(p + 4) = *x;
}

COB_STATIC void
cob_addswp_u48_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

	x = (unsigned char *)&n;
	*x = *(p + 5);
	*(x + 1) = *(p + 4);
	*(x + 2) = *(p + 3);
	*(x + 3) = *(p + 2);
	*(x + 4) = *(p + 1);
	*(x + 5) = *p;
	n += val;
	*p = *(x + 5);
	*(p + 1) = *(x + 4);
	*(p + 2) = *(x + 3);
	*(p + 3) = *(x + 2);
	*(p + 4) = *(x + 1);
	*(p + 5) = *x;
}

COB_STATIC void
cob_addswp_s48_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

	x = ((unsigned char *)&n) + 2;
	*x = *(p + 5);
	*(x + 1) = *(p + 4);
	*(x + 2) = *(p + 3);
	*(x + 3) = *(p + 2);
	*(x + 4) = *(p + 1);
	*(x + 5) = *p;
	n >>= 16;	/* shift with sign */
	n += val;
	x = (unsigned char *)&n;
	*p = *(x + 5);
	*(p + 1) = *(x + 4);
	*(p + 2) = *(x + 3);
	*(p + 3) = *(x + 2);
	*(p + 4) = *(x + 1);
	*(p + 5) = *x;
}

COB_STATIC void
cob_addswp_u56_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

	x = (unsigned char *)&n;
	*x = *(p + 6);
	*(x + 1) = *(p + 5);
	*(x + 2) = *(p + 4);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 2);
	*(x + 5) = *(p + 1);
	*(x + 6) = *p;
	n += val;
	*p = *(x + 6);
	*(p + 1) = *(x + 5);
	*(p + 2) = *(x + 4);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 2);
	*(p + 5) = *(x + 1);
	*(p + 6) = *x;
}

COB_STATIC void
cob_addswp_s56_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

	x = ((unsigned char *)&n) + 1;
	*x = *(p + 6);
	*(x + 1) = *(p + 5);
	*(x + 2) = *(p + 4);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 2);
	*(x + 5) = *(p + 1);
	*(x + 6) = *p;
	n >>= 8;	/* shift with sign */
	n += val;
	x = (unsigned char *)&n;
	*p = *(x + 6);
	*(p + 1) = *(x + 5);
	*(p + 2) = *(x + 4);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 2);
	*(p + 5) = *(x + 1);
	*(p + 6) = *x;
}

COB_STATIC void
cob_addswp_u64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	unsigned long long	n;

	n = COB_BSWAP_64 (*(unsigned long long MISALIGNED *)p);
	n += val;
	*(unsigned long long MISALIGNED *)p = COB_BSWAP_64(n);
#else
	size_t	i;
	union {
		unsigned long long	n;
		unsigned char		c[8];
	} u;

	for (i = 0; i < 8; ++i) {
		u.c[7-i] = p[i];
	}
	u.n += val;
	for (i = 0; i < 8; ++i) {
		p[i] = u.c[7-i];
	}
#endif
}

COB_STATIC void
cob_addswp_s64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	long long	n;

	n = COB_BSWAP_64 (*(long long MISALIGNED *)p);
	n += val;
	*(long long MISALIGNED *)p = COB_BSWAP_64(n);
#else
	size_t	i;
	union {
		long long	n;
		unsigned char	c[8];
	} u;

	for (i = 0; i < 8; ++i) {
		u.c[7-i] = p[i];
	}
	u.n += val;
	for (i = 0; i < 8; ++i) {
		p[i] = u.c[7-i];
	}
#endif
}

/* Binary swapped subtract */
COB_STATIC void
cob_subswp_u16_binary (unsigned char *p, const int val)
{
	unsigned short	n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_16 (*(unsigned short MISALIGNED *)p);
	n -= val;
	*(unsigned short MISALIGNED *)p = COB_BSWAP_16(n);
#else
	n = (unsigned short)((p[0] << 8) | p[1]);
	n -= val;
	p[0] = (unsigned char)(n >> 8);
	p[1] = (unsigned char)n;
#endif
}

COB_STATIC void
cob_subswp_s16_binary (unsigned char *p, const int val)
{
	short		n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_16 (*(short MISALIGNED *)p);
	n -= val;
	*(short MISALIGNED *)p = COB_BSWAP_16(n);
#else
	n = (short)((p[0] << 8) | p[1]);
	n -= val;
	p[0] = (unsigned char)(n >> 8);
	p[1] = (unsigned char)n;
#endif
}

COB_STATIC void
cob_subswp_u24_binary (unsigned char *p, const int val)
{
	unsigned char	*x;
	unsigned int	n = 0;

	x = (unsigned char *)&n;
	*x = *(p + 2);
	*(x + 1) = *(p + 1);
	*(x + 2) = *p;
	n -= val;
	*p = *(x + 2);
	*(p + 1) = *(x + 1);
	*(p + 2) = *x;
}

COB_STATIC void
cob_subswp_s24_binary (unsigned char *p, const int val)
{
	unsigned char	*x;
	int		n = 0;

	x = ((unsigned char *)&n) + 1;
	*x = *(p + 2);
	*(x + 1) = *(p + 1);
	*(x + 2) = *p;
	n >>= 8;	/* shift with sign */
	n -= val;
	x = (unsigned char *)&n;
	*p = *(x + 2);
	*(p + 1) = *(x + 1);
	*(p + 2) = *x;
}

COB_STATIC void
cob_subswp_u32_binary (unsigned char *p, const int val)
{
	unsigned int	n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_32 (*(unsigned int MISALIGNED *)p);
	n -= val;
	*(unsigned int MISALIGNED *)p = COB_BSWAP_32(n);
#else
	n = (unsigned int)((p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3]);
	n -= val;
	*p++ = (unsigned char)(n >> 24);
	*p++ = (unsigned char)(n >> 16);
	*p++ = (unsigned char)(n >> 8);
	*p = (unsigned char)n;
#endif
}

COB_STATIC void
cob_subswp_s32_binary (unsigned char *p, const int val)
{
	int		n;

#ifdef ALLOW_MISALIGNED
	n = COB_BSWAP_32 (*(int MISALIGNED *)p);
	n -= val;
	*(int MISALIGNED *)p = COB_BSWAP_32(n);
#else
	n = (int)((p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3]);
	n -= val;
	*p++ = (unsigned char)(n >> 24);
	*p++ = (unsigned char)(n >> 16);
	*p++ = (unsigned char)(n >> 8);
	*p = (unsigned char)n;
#endif
}

COB_STATIC void
cob_subswp_u40_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

	x = (unsigned char *)&n;
	*x = *(p + 4);
	*(x + 1) = *(p + 3);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 1);
	*(x + 4) = *p;
	n -= val;
	*p = *(x + 4);
	*(p + 1) = *(x + 3);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 1);
	*(p + 4) = *x;
}

COB_STATIC void
cob_subswp_s40_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

	x = ((unsigned char *)&n) + 3;
	*x = *(p + 4);
	*(x + 1) = *(p + 3);
	*(x + 2) = *(p + 2);
	*(x + 3) = *(p + 1);
	*(x + 4) = *p;
	n >>= 24;	/* shift with sign */
	n -= val;
	x = (unsigned char *)&n;
	*p = *(x + 4);
	*(p + 1) = *(x + 3);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 1);
	*(p + 4) = *x;
}

COB_STATIC void
cob_subswp_u48_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

	x = (unsigned char *)&n;
	*x = *(p + 5);
	*(x + 1) = *(p + 4);
	*(x + 2) = *(p + 3);
	*(x + 3) = *(p + 2);
	*(x + 4) = *(p + 1);
	*(x + 5) = *p;
	n -= val;
	*p = *(x + 5);
	*(p + 1) = *(x + 4);
	*(p + 2) = *(x + 3);
	*(p + 3) = *(x + 2);
	*(p + 4) = *(x + 1);
	*(p + 5) = *x;
}

COB_STATIC void
cob_subswp_s48_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

	x = ((unsigned char *)&n) + 2;
	*x = *(p + 5);
	*(x + 1) = *(p + 4);
	*(x + 2) = *(p + 3);
	*(x + 3) = *(p + 2);
	*(x + 4) = *(p + 1);
	*(x + 5) = *p;
	n >>= 16;	/* shift with sign */
	n -= val;
	x = (unsigned char *)&n;
	*p = *(x + 5);
	*(p + 1) = *(x + 4);
	*(p + 2) = *(x + 3);
	*(p + 3) = *(x + 2);
	*(p + 4) = *(x + 1);
	*(p + 5) = *x;
}

COB_STATIC void
cob_subswp_u56_binary (unsigned char *p, const int val)
{
	unsigned long long	n = 0;
	unsigned char		*x;

	x = (unsigned char *)&n;
	*x = *(p + 6);
	*(x + 1) = *(p + 5);
	*(x + 2) = *(p + 4);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 2);
	*(x + 5) = *(p + 1);
	*(x + 6) = *p;
	n -= val;
	*p = *(x + 6);
	*(p + 1) = *(x + 5);
	*(p + 2) = *(x + 4);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 2);
	*(p + 5) = *(x + 1);
	*(p + 6) = *x;
}

COB_STATIC void
cob_subswp_s56_binary (unsigned char *p, const int val)
{
	long long		n = 0;
	unsigned char		*x;

	x = ((unsigned char *)&n) + 1;
	*x = *(p + 6);
	*(x + 1) = *(p + 5);
	*(x + 2) = *(p + 4);
	*(x + 3) = *(p + 3);
	*(x + 4) = *(p + 2);
	*(x + 5) = *(p + 1);
	*(x + 6) = *p;
	n >>= 8;	/* shift with sign */
	n -= val;
	x = (unsigned char *)&n;
	*p = *(x + 6);
	*(p + 1) = *(x + 5);
	*(p + 2) = *(x + 4);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 2);
	*(p + 5) = *(x + 1);
	*(p + 6) = *x;
}

COB_STATIC void
cob_subswp_u64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	unsigned long long	n;

	n = COB_BSWAP_64 (*(unsigned long long MISALIGNED *)p);
	n -= val;
	*(unsigned long long MISALIGNED *)p = COB_BSWAP_64(n);
#else
	size_t	i;
	union {
		unsigned long long	n;
		unsigned char		c[8];
	} u;

	for (i = 0; i < 8; ++i) {
		u.c[7-i] = p[i];
	}
	u.n -= val;
	for (i = 0; i < 8; ++i) {
		p[i] = u.c[7-i];
	}
#endif
}

COB_STATIC void
cob_subswp_s64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	long long	n;

	n = COB_BSWAP_64 (*(long long MISALIGNED *)p);
	n -= val;
	*(long long MISALIGNED *)p = COB_BSWAP_64(n);
#else
	size_t	i;
	union {
		long long	n;
		unsigned char	c[8];
	} u;

	for (i = 0; i < 8; ++i) {
		u.c[7-i] = p[i];
	}
	u.n -= val;
	for (i = 0; i < 8; ++i) {
		p[i] = u.c[7-i];
	}
#endif
}

COB_STATIC void
cob_setswp_u16_binary (unsigned char *p, const int val)
{
	unsigned short	n;

#ifdef ALLOW_MISALIGNED
	n = val;
	*(unsigned short MISALIGNED *)p = COB_BSWAP_16(n);
#else
	n = val;
	p[0] = (unsigned char)(n >> 8);
	p[1] = (unsigned char)n;
#endif
}

COB_STATIC void
cob_setswp_s16_binary (unsigned char *p, const int val)
{
	short		n;

#ifdef ALLOW_MISALIGNED
	n = val;
	*(short MISALIGNED *)p = COB_BSWAP_16(n);
#else
	n = val;
	p[0] = (unsigned char)(n >> 8);
	p[1] = (unsigned char)n;
#endif
}

COB_STATIC void
cob_setswp_u24_binary (unsigned char *p, const int val)
{
	unsigned char	*x;
	unsigned int	n;

	n = val;
	x = (unsigned char *)&n;
	*p = *(x + 2);
	*(p + 1) = *(x + 1);
	*(p + 2) = *x;
}

COB_STATIC void
cob_setswp_s24_binary (unsigned char *p, const int val)
{
	unsigned char	*x;
	int		n;

	n = val;
	x = (unsigned char *)&n;
	*p = *(x + 2);
	*(p + 1) = *(x + 1);
	*(p + 2) = *x;
}

COB_STATIC void
cob_setswp_u32_binary (unsigned char *p, const int val)
{
	unsigned int	n;

#ifdef ALLOW_MISALIGNED
	n = val;
	*(unsigned int MISALIGNED *)p = COB_BSWAP_32(n);
#else
	n = val;
	*p++ = (unsigned char)(n >> 24);
	*p++ = (unsigned char)(n >> 16);
	*p++ = (unsigned char)(n >> 8);
	*p = (unsigned char)n;
#endif
}

COB_STATIC void
cob_setswp_s32_binary (unsigned char *p, const int val)
{
	int		n;

#ifdef ALLOW_MISALIGNED
	n = val;
	*(int MISALIGNED *)p = COB_BSWAP_32(n);
#else
	n = val;
	*p++ = (unsigned char)(n >> 24);
	*p++ = (unsigned char)(n >> 16);
	*p++ = (unsigned char)(n >> 8);
	*p = (unsigned char)n;
#endif
}

COB_STATIC void
cob_setswp_u40_binary (unsigned char *p, const int val)
{
	unsigned long long	n;
	unsigned char		*x;

	n = val;
	x = (unsigned char *)&n;
	*p = *(x + 4);
	*(p + 1) = *(x + 3);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 1);
	*(p + 4) = *x;
}

COB_STATIC void
cob_setswp_s40_binary (unsigned char *p, const int val)
{
	long long		n;
	unsigned char		*x;

	n = val;
	x = (unsigned char *)&n;
	*p = *(x + 4);
	*(p + 1) = *(x + 3);
	*(p + 2) = *(x + 2);
	*(p + 3) = *(x + 1);
	*(p + 4) = *x;
}

COB_STATIC void
cob_setswp_u48_binary (unsigned char *p, const int val)
{
	unsigned long long	n;
	unsigned char		*x;

	n = val;
	x = (unsigned char *)&n;
	*p = *(x + 5);
	*(p + 1) = *(x + 4);
	*(p + 2) = *(x + 3);
	*(p + 3) = *(x + 2);
	*(p + 4) = *(x + 1);
	*(p + 5) = *x;
}

COB_STATIC void
cob_setswp_s48_binary (unsigned char *p, const int val)
{
	long long		n;
	unsigned char		*x;

	n = val;
	x = (unsigned char *)&n;
	*p = *(x + 5);
	*(p + 1) = *(x + 4);
	*(p + 2) = *(x + 3);
	*(p + 3) = *(x + 2);
	*(p + 4) = *(x + 1);
	*(p + 5) = *x;
}

COB_STATIC void
cob_setswp_u56_binary (unsigned char *p, const int val)
{
	unsigned long long	n;
	unsigned char		*x;

	n = val;
	x = (unsigned char *)&n;
	*p = *(x + 6);
	*(p + 1) = *(x + 5);
	*(p + 2) = *(x + 4);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 2);
	*(p + 5) = *(x + 1);
	*(p + 6) = *x;
}

COB_STATIC void
cob_setswp_s56_binary (unsigned char *p, const int val)
{
	long long		n;
	unsigned char		*x;

	n = val;
	x = (unsigned char *)&n;
	*p = *(x + 6);
	*(p + 1) = *(x + 5);
	*(p + 2) = *(x + 4);
	*(p + 3) = *(x + 3);
	*(p + 4) = *(x + 2);
	*(p + 5) = *(x + 1);
	*(p + 6) = *x;
}

COB_STATIC void
cob_setswp_u64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	unsigned long long	n;

	n = val;
	*(unsigned long long MISALIGNED *)p = COB_BSWAP_64(n);
#else
	size_t	i;
	union {
		unsigned long long	n;
		unsigned char		c[8];
	} u;

	u.n = val;
	for (i = 0; i < 8; ++i) {
		p[i] = u.c[7-i];
	}
#endif
}

COB_STATIC void
cob_setswp_s64_binary (unsigned char *p, const int val)
{
#ifdef ALLOW_MISALIGNED
	long long	n;

	n = val;
	*(long long MISALIGNED *)p = COB_BSWAP_64(n);
#else
	size_t	i;
	union {
		long long	n;
		unsigned char	c[8];
	} u;

	u.n = val;
	for (i = 0; i < 8; ++i) {
		p[i] = u.c[7-i];
	}
#endif
}

#endif	/* WORDS_BIGENDIAN */

#endif	/* COB_LOCAL_INLINE || COB_LIB_INCLUDE */

#endif	/* COB_CODEGEN_H */
