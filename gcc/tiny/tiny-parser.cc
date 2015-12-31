/* Tiny parser
   Copyright (C) 2016 Free Software Foundation, Inc.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "tiny/tiny-parser.h"

#include <stdio.h>

static void tiny_parse_file (const char *filename);

void
tiny_parse_files (int num_files, const char **files)
{
  for (int i = 0; i < num_files; i++)
    {
      tiny_parse_file (files[i]);
    }
}

static void
tiny_parse_file (const char *filename)
{
  fprintf(stderr, "Going to parse '%s'\n", filename);
}
