Introduction
============

For convenience a fixed size cell has been implemented. The following
diagrams illustrate the cell layout on 64-bit systems.


Literal
=======

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        |               - UNUSED -                |
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                 val_offset              |
        +----------+---------+----------+---------+
   20	|               - UNUSED -                |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_LITERAL.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_offset* is into the symbol table.

Two atoms will unify if their *val_offset* is the same.
An Atom is always used for functor names.


Var
===

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        |               - UNUSED -                |
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                 val_offset              |
        +----------+---------+----------+---------+
   20	|       slot_nbr     |      - UNUSED -    |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_VAR.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_offset* is into the symbol table.


Integer
=======

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_num                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +                 val_den                 +
   20	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_RATIONAL.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_den* is always 1.


Rational
========

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_num                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +                 val_den                 +
   20	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_RATIONAL.
Where *arity* is always 0.
Where *nbr_cells* is always 1.

A rational with *val_den* of 1 thus becomes an integer.


Float
=====

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_flt                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +               - UNUSED -                +
   20	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_FLOAT.
Where *arity* is always 0
Where *nbr_cells* is always 1.


Cstring
=======

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                                         +
   12	|                                         |
        +                 val_chr[16]             +
   16	|                                         |
        +                                         +
   20	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_CSTRING.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_chr* is up to 15 bytes of UTF-8 chars, NULL-terminated.

A Cstring may be used for atoms that are not functors and need quoting.


BLOB
====

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_str                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                 len_str                 |
        +----------+---------+----------+---------+
   20	|               - UNUSED -                |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_CSTRING.
Where *arity* is always 0.
Where *flags* is FLAG_BLOB.
Where *nbr_cells* is always 1.
Where *val_str* is a char pointer to UTF-8 chars.
Where *len_str* is the number of allocated bytes.

A BLOB is length delimited, not NULL-terminated (though it may be).
A BLOB may be used for atoms that are not functors and > 15 bytes long.


String
======

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_str                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                 len_str                 |
        +----------+---------+----------+---------+
   20	|                 rem_str                 |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_CSTRING.
Where *arity* is always 2.
Where *flags* is FLAG_BLOB|FLAG_STRING.
Where *nbr_cells* is always 1.
Where *val_str* is a pointer to UTF-8 chars.
Where *len_str* is the number of remaining bytes in the string.
Where *rem_str* is the number of remaining bytes in the string.


String head
===========

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_str                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                 len_str                 |
        +----------+---------+----------+---------+
   20	|                 rem_str                 |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_CSTRING.
Where *arity* is always 2.
Where *flags* is FLAG_BLOB|FLAG_STRING|FLAG_HEAD.
Where *nbr_cells* is always 1.
Where *val_str* is a pointer to one UTF-8 char.
Where *len_str* is the number of bytes pointed to.
Where *rem_str* is the number of remaining bytes in the string.


Compound
========

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +               - UNUSED -                +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                 val_offset              |
        +----------+---------+----------+---------+
   20	|               - UNUSED -                |
        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 2

Where *val_type* is TYPE_LITERAL.
Where *arity* is > 0.
Where *nbr_cells* is > 1 and includes the args.
Where *val_offset* is into the symbol table.
Where args are the following cells (see *nbr_cells*).


List
====

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +               - UNUSED -                +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                 val_offset              |
        +----------+---------+----------+---------+
   20	|               - UNUSED -                |
        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 2

Where *val_type* is TYPE_LITERAL.
Where *arity* is always 2.
Where *nbr_cells* is > 1 and includes head & tail args.
Where *val_offset* is into the symbol table to '.'.
Where args are the following cells (see *nbr_cells*).
Where the tail arg is usually a list.
Where the final tail arg is usually the atom '[]'.

