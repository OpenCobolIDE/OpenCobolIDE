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

#ifndef COB_SCREENIO_H
#define COB_SCREENIO_H

#include <libcob/common.h>

#define COB_SCREEN_BLACK	0
#define COB_SCREEN_BLUE		1
#define COB_SCREEN_GREEN	2
#define COB_SCREEN_CYAN		3
#define COB_SCREEN_RED		4
#define COB_SCREEN_MAGENTA	5
#define COB_SCREEN_YELLOW	6
#define COB_SCREEN_WHITE	7

#define COB_SCREEN_LINE_PLUS	0x00000001
#define COB_SCREEN_LINE_MINUS	0x00000002
#define COB_SCREEN_COLUMN_PLUS	0x00000004
#define COB_SCREEN_COLUMN_MINUS	0x00000008

#define COB_SCREEN_AUTO		0x00000010
#define COB_SCREEN_BELL		0x00000020
#define COB_SCREEN_BLANK_LINE	0x00000040
#define COB_SCREEN_BLANK_SCREEN	0x00000080
#define COB_SCREEN_BLINK	0x00000100
#define COB_SCREEN_ERASE_EOL	0x00000200
#define COB_SCREEN_ERASE_EOS	0x00000400
#define COB_SCREEN_FULL		0x00000800
#define COB_SCREEN_HIGHLIGHT	0x00001000
#define COB_SCREEN_LOWLIGHT	0x00002000
#define COB_SCREEN_REQUIRED	0x00004000
#define COB_SCREEN_REVERSE	0x00008000
#define COB_SCREEN_SECURE	0x00010000
#define COB_SCREEN_UNDERLINE	0x00020000
#define COB_SCREEN_OVERLINE	0x00040000
#define COB_SCREEN_PROMPT	0x00080000
#define COB_SCREEN_UPDATE	0x00100000
#define COB_SCREEN_INPUT	0x00200000
#define COB_SCREEN_SCROLL_DOWN	0x00400000

#define COB_SCREEN_TYPE_GROUP		0
#define COB_SCREEN_TYPE_FIELD		1
#define COB_SCREEN_TYPE_VALUE		2
#define COB_SCREEN_TYPE_ATTRIBUTE	3

typedef struct __cob_screen cob_screen;

struct __cob_screen {
	cob_screen		*next;
	cob_screen		*child;		/* for COB_SCREEN_TYPE_GROUP */
	cob_field		*field;		/* for COB_SCREEN_TYPE_FIELD */
	cob_field		*value;		/* for COB_SCREEN_TYPE_VALUE */
	cob_field		*line;
	cob_field		*column;
	cob_field		*foreg;
	cob_field		*backg;
	int			type;
	int			occurs;
	int			attr;		/* for COB_SCREEN_TYPE_ATTRIBUTE */
};

DLL_EXPIMP extern int	cob_screen_mode;

extern void cob_screen_line_col	(cob_field *, const int);
extern void cob_screen_display	(cob_screen *, cob_field *, cob_field *);
extern void cob_screen_accept	(cob_screen *, cob_field *, cob_field *);
extern void cob_field_display	(cob_field *, cob_field *, cob_field *,
				 cob_field *, cob_field *, cob_field *,
				 const int);
extern void cob_field_accept	(cob_field *, cob_field *, cob_field *,
				 cob_field *, cob_field *, cob_field *,
				 const int);

#endif /* COB_SCREENIO_H */
