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

#ifndef COB_FILEIO_H
#define COB_FILEIO_H

#include <libcob/common.h>

/* File version */
#define	COB_FILE_VERSION	0

#define COB_EQ			1 	/* x == y */
#define COB_LT			2 	/* x <  y */
#define COB_LE			3 	/* x <= y */
#define COB_GT			4 	/* x >  y */
#define COB_GE			5 	/* x >= y */
#define COB_NE			6 	/* x != y */

#define COB_ASCENDING		0
#define COB_DESCENDING		1

#define COB_FILE_MODE		0644

/* Organization */

#define COB_ORG_SEQUENTIAL	0
#define COB_ORG_LINE_SEQUENTIAL	1
#define COB_ORG_RELATIVE	2
#define COB_ORG_INDEXED		3
#define COB_ORG_SORT		4
#define COB_ORG_MAX		5

/* Access mode */

#define COB_ACCESS_SEQUENTIAL	1
#define COB_ACCESS_DYNAMIC	2
#define COB_ACCESS_RANDOM	3

/* SELECT features */

#define	COB_SELECT_FILE_STATUS	0x01
#define	COB_SELECT_EXTERNAL	0x02
#define	COB_SELECT_LINAGE	0x04
#define	COB_SELECT_SPLITKEY	0x08

/* Lock mode */

#define COB_LOCK_EXCLUSIVE	1
#define COB_LOCK_MANUAL		2
#define COB_LOCK_AUTOMATIC	4
#define COB_LOCK_MULTIPLE	8
#define COB_LOCK_MASK		0x7

/* Open mode */

#define COB_OPEN_CLOSED		0
#define COB_OPEN_INPUT 		1
#define COB_OPEN_OUTPUT		2
#define COB_OPEN_I_O 		3
#define COB_OPEN_EXTEND		4
#define COB_OPEN_LOCKED		5

/* Close options */

#define COB_CLOSE_NORMAL	0
#define COB_CLOSE_LOCK		1
#define COB_CLOSE_NO_REWIND	2
#define COB_CLOSE_UNIT		3
#define COB_CLOSE_UNIT_REMOVAL	4

/* Write options */

#define COB_WRITE_MASK		0x0000ffff
#define COB_WRITE_LINES		0x00010000
#define COB_WRITE_PAGE		0x00020000
#define COB_WRITE_CHANNEL	0x00040000
#define COB_WRITE_AFTER		0x00100000
#define COB_WRITE_BEFORE	0x00200000
#define COB_WRITE_EOP		0x00400000
#define COB_WRITE_LOCK		0x00800000

/* Read options */
#define COB_READ_NEXT		0x01
#define COB_READ_PREVIOUS	0x02
#define COB_READ_FIRST		0x04
#define COB_READ_LAST		0x08
#define COB_READ_LOCK		0x10
#define COB_READ_NO_LOCK	0x20
#define COB_READ_KEPT_LOCK	0x40
#define COB_READ_WAIT_LOCK	0x80
#define COB_READ_IGNORE_LOCK	0x100

/* I-O status */

#define COB_STATUS_00_SUCCESS			00
#define COB_STATUS_02_SUCCESS_DUPLICATE		02
#define COB_STATUS_04_SUCCESS_INCOMPLETE	04
#define COB_STATUS_05_SUCCESS_OPTIONAL		05
#define COB_STATUS_07_SUCCESS_NO_UNIT		07
#define COB_STATUS_10_END_OF_FILE		10
#define COB_STATUS_14_OUT_OF_KEY_RANGE		14
#define COB_STATUS_21_KEY_INVALID		21
#define COB_STATUS_22_KEY_EXISTS		22
#define COB_STATUS_23_KEY_NOT_EXISTS		23
#define COB_STATUS_30_PERMANENT_ERROR		30
#define COB_STATUS_31_INCONSISTENT_FILENAME	31
#define COB_STATUS_34_BOUNDARY_VIOLATION	34
#define COB_STATUS_35_NOT_EXISTS		35
#define COB_STATUS_37_PERMISSION_DENIED		37
#define COB_STATUS_38_CLOSED_WITH_LOCK		38
#define COB_STATUS_39_CONFLICT_ATTRIBUTE	39
#define COB_STATUS_41_ALREADY_OPEN		41
#define COB_STATUS_42_NOT_OPEN			42
#define COB_STATUS_43_READ_NOT_DONE		43
#define COB_STATUS_44_RECORD_OVERFLOW		44
#define COB_STATUS_46_READ_ERROR		46
#define COB_STATUS_47_INPUT_DENIED		47
#define COB_STATUS_48_OUTPUT_DENIED		48
#define COB_STATUS_49_I_O_DENIED		49
#define COB_STATUS_51_RECORD_LOCKED		51
#define COB_STATUS_52_EOP			52
#define COB_STATUS_57_I_O_LINAGE		57
#define COB_STATUS_61_FILE_SHARING		61
#define COB_STATUS_91_NOT_AVAILABLE		91

/* Special status */

/* Need some value that does not conflict with errno for OPEN/LINAGE */
#define	COB_LINAGE_INVALID	16384
/* Need value that does not conflict with errno 30 (EROFS) for OPEN */
#define	COB_NOT_CONFIGURED	32768

/* File connector */

struct cob_file_key {
	cob_field	*field;	/* key field */
	int		flag;	/* WITH DUPLICATES (for RELATIVE/INDEXED) */
				/* ASCENDING/DESCENDING (for SORT) */
	size_t		offset;	/* Offset of field */
};

struct linage_struct {
	cob_field		*linage;		/* LINAGE */
	cob_field		*linage_ctr;		/* LINAGE-COUNTER */
	cob_field		*latfoot;		/* LINAGE FOOTING */
	cob_field		*lattop;		/* LINAGE AT TOP */
	cob_field		*latbot;		/* LINAGE AT BOTTOM */
	int			lin_lines;		/* Current Linage */
	int			lin_foot;		/* Current Footage */
	int			lin_top;		/* Current Top */
	int			lin_bot;		/* Current Bottom */
};

typedef struct {
	const char		*select_name;		/* Name in SELECT */
	unsigned char		*file_status;		/* FILE STATUS */
	cob_field		*assign;		/* ASSIGN TO */
	cob_field		*record;		/* record area */
	cob_field		*record_size;		/* record size depending on */
	struct cob_file_key	*keys;			/* RELATIVE/RECORD/SORT keys */
	void			*file;			/* file specific data pointer */
	void			*linorkeyptr;		/* LINAGE pointer or SPLIT KEY */
	const unsigned char	*sort_collating;	/* SORT collating */
	void			*extfh_ptr;		/* For EXTFH usage */
	size_t			record_min;		/* record min size */
	size_t			record_max;		/* record max size */
	size_t			nkeys;			/* the number of keys */
	char			organization;		/* ORGANIZATION */
	char			access_mode;		/* ACCESS MODE */
	char			lock_mode;		/* LOCKMODE */
	char			open_mode;		/* OPEN MODE */
	char			flag_optional;		/* OPTIONAL */
	char			last_open_mode;		/* open mode given by OPEN */
	char			special;		/* Special file */
	char			flag_nonexistent;	/* nonexistent file */
	char			flag_end_of_file;	/* reached the end of file */
	char			flag_begin_of_file;	/* reached beginning of file */
	char			flag_first_read;	/* first READ after OPEN/START */
	char			flag_read_done;		/* last READ successfully done */
	char			flag_select_features;	/* SELECT features */
	char			flag_needs_nl;		/* LS file needs NL at close */
	char			flag_needs_top;		/* Linage needs top */
	char			file_version;		/* File I/O version */
} cob_file;

/* File I-O functions */

/* Struct cob_fileio_funcs
	(*open)		(file, filename, mode, sharing);
	(*close)	(file, opt);
	(*start)	(file, cond, key);
	(*read)		(file, key, read_opts);
	(*read next)	(file, read_opts);
	(*write)	(file, opt);
	(*rewrite)	(file, opt);
	(*delete)	(file);
*/

struct cob_fileio_funcs {
	int	(*open)		(cob_file *, char *, const int, const int);
	int	(*close)	(cob_file *, const int);
	int	(*start)	(cob_file *, const int, cob_field *);
	int	(*read)		(cob_file *, cob_field *, int);
	int	(*read_next)	(cob_file *, int);
	int	(*write)	(cob_file *, const int);
	int	(*rewrite)	(cob_file *, const int);
	int	(*fdelete)	(cob_file *);
};

DLL_EXPIMP extern cob_file	*cob_error_file;

extern void cob_default_error_handle	(void);

extern void cob_open		(cob_file *, const int, const int, cob_field *);
extern void cob_close		(cob_file *, const int, cob_field *);
extern void cob_read		(cob_file *, cob_field *, cob_field *, int);
extern void cob_write		(cob_file *, cob_field *, const int, cob_field *);
extern void cob_rewrite		(cob_file *, cob_field *, const int, cob_field *);
extern void cob_delete		(cob_file *, cob_field *);
extern void cob_start		(cob_file *, const int, cob_field *, cob_field *);

extern void cob_unlock_file	(cob_file *, cob_field *);
extern void cob_commit		(void);
extern void cob_rollback	(void);

/* System routines */
extern int CBL_OPEN_FILE	(unsigned char *, unsigned char *,
				 unsigned char *, unsigned char *,
				 unsigned char *);
extern int CBL_CREATE_FILE	(unsigned char *, unsigned char *,
				 unsigned char *, unsigned char *,
				 unsigned char *);
extern int CBL_READ_FILE	(unsigned char *, unsigned char *,
				 unsigned char *, unsigned char *,
				 unsigned char *);
extern int CBL_WRITE_FILE	(unsigned char *, unsigned char *,
				 unsigned char *, unsigned char *,
				 unsigned char *);
extern int CBL_CLOSE_FILE	(unsigned char *);
extern int CBL_FLUSH_FILE	(unsigned char *);
extern int CBL_DELETE_FILE	(unsigned char *);
extern int CBL_COPY_FILE	(unsigned char *, unsigned char *);
extern int CBL_CHECK_FILE_EXIST	(unsigned char *, unsigned char *);
extern int CBL_RENAME_FILE	(unsigned char *, unsigned char *);
extern int CBL_GET_CURRENT_DIR	(const int, const int, unsigned char *);
extern int CBL_CHANGE_DIR	(unsigned char *);
extern int CBL_CREATE_DIR	(unsigned char *);
extern int CBL_DELETE_DIR	(unsigned char *);
extern int cob_acuw_chdir	(unsigned char *, unsigned char *);
extern int cob_acuw_mkdir	(unsigned char *);
extern int cob_acuw_copyfile	(unsigned char *, unsigned char *, unsigned char *);
extern int cob_acuw_file_info	(unsigned char *, unsigned char *);
extern int cob_acuw_file_delete	(unsigned char *, unsigned char *);

/* SORT */
extern void	cob_file_sort_init	(cob_file *, const int,
					 const unsigned char *,
					 void *, cob_field *);
extern void	cob_file_sort_init_key	(cob_file *, const int,
					 cob_field *, size_t);
extern void	cob_file_sort_close	(cob_file *);
extern void	cob_file_sort_using	(cob_file *, cob_file *);
extern void	cob_file_sort_giving	(cob_file *, const size_t, ...);
extern void	cob_file_release	(cob_file *);
extern void	cob_file_return		(cob_file *);

#endif /* COB_FILEIO_H */
