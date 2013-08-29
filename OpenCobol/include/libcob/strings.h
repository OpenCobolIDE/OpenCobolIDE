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

#ifndef COB_STRINGS_H
#define COB_STRINGS_H

#include <libcob/common.h>

extern void cob_inspect_init		(cob_field *, const int);
extern void cob_inspect_start		(void);
extern void cob_inspect_before		(const cob_field *);
extern void cob_inspect_after		(const cob_field *);
extern void cob_inspect_characters	(cob_field *);
extern void cob_inspect_all		(cob_field *, cob_field *);
extern void cob_inspect_leading		(cob_field *, cob_field *);
extern void cob_inspect_first		(cob_field *, cob_field *);
extern void cob_inspect_trailing	(cob_field *, cob_field *);
extern void cob_inspect_converting	(cob_field *, cob_field *);
extern void cob_inspect_finish		(void);

extern void cob_string_init		(cob_field *, cob_field *);
extern void cob_string_delimited	(cob_field *);
extern void cob_string_append		(cob_field *);
extern void cob_string_finish		(void);

extern void cob_unstring_init		(cob_field *, cob_field *, const size_t);
extern void cob_unstring_delimited	(cob_field *, const int);
extern void cob_unstring_into		(cob_field *, cob_field *, cob_field *);
extern void cob_unstring_tallying	(cob_field *);
extern void cob_unstring_finish		(void);

#endif /* COB_STRINGS_H */
