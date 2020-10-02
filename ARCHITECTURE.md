Introduction
============

For convenience a fixed size cell is used. The following diagrams
illustrate how the cell layout on 64-bit systems.


Atom
====

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        |               - UNUSED -                |
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                 val_offset              |
        +----------+---------+----------+---------+
   24	|               - UNUSED -                |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_ATOM.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_offset* is into the symbol table.


Var
===

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        |               - UNUSED -                |
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                 val_offset              |
        +----------+---------+----------+---------+
   24	|       slot_nbr     |      - UNUSED -    |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_VAR.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_offset* is into the symbol table.


Integer
=======

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_num                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +                 val_den                 +
   24	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_RATIONAL.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_den* is always 1.


Rational
========

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_num                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +                 val_den                 +
   24	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_RATIONAL.
Where *arity* is always 0.
Where *nbr_cells* is always 1.


Float
=====

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_flt                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +               - UNUSED -                +
   24	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_FLOAT.
Where *arity* is always 0
Where *nbr_cells* is always 1.


Cstring
=======

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                                         +
   12	|                                         |
        +                 val_chr                 +
   16	|                                         |
        +                                         +
   24	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_CSTRING.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_chr* is up to 15 bytes of UTF-8 chars, NULL-terminated.


BLOB
====

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_str                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +        len_str     |    - UNUSED -      +
   24	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_CSTRING.
Where *arity* is always 0.
Where *flags* is FLAG_BLOB.
Where *nbr_cells* is always 1.
Where *val_str* is a pointer to UTF-8 chars.
Where *len_str* is the number of allocated bytes.


String
======

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_str                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +     - UNUSED -     |      rem_str       +
   24	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_CSTRING.
Where *arity* is always 2.
Where *flags* is FLAG_BLOB|FLAG_STRING.
Where *nbr_cells* is always 1.
Where *val_str* is a pointer to UTF-8 chars.
Where *rem_str* is the number of remaining bytes.


String head
===========

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_str                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +        len_str     |    - UNUSED -      +
   24	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_CSTRING.
Where *arity* is always 2.
Where *flags* is FLAG_BLOB|FLAG_STRING|FLAG_HEAD.
Where *nbr_cells* is always 1.
Where *val_str* is a pointer to one UTF-8 char.
Where *len_str* is the number of relevant bytes.


Compound
========

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +               - UNUSED -                +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                 val_offset              |
        +----------+---------+----------+---------+
   24	|               - UNUSED -                |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_ATOM.
Where *arity* is always > 0.
Where *nbr_cells* is always > 1 and includes the args.
Where *val_offset* is into the symbol table.
Where args are the following cells (see *nbr_cells*).


List
====

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +               - UNUSED -                +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                 val_offset              |
        +----------+---------+----------+---------+
   24	|               - UNUSED -                |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_ATOM.
Where *arity* is always 2.
Where *nbr_cells* is always > 1 and includes head & tail args.
Where *val_offset* is into the symbol table to '.'.
Where args are the following cells (see *nbr_cells*).
Where the tail arg is usually a list.
Where the final tail arg is usually the atom '[]'.

