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
   16	|                 val_off                 |
        +----------+---------+----------+---------+
   20	|               - UNUSED -                |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_LITERAL.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_off* is into the symbol table.

Two atoms will unify if their *val_off* is the same.
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
   16	|                 val_off                 |
        +----------+---------+----------+---------+
   20	|       var_nbr      |      - UNUSED -    |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_VAR.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_off* is into the symbol table.
Where *var_nbr* is the index into the frame


Integer
=======

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                 val_int                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +                                         +
   20	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_RATIONAL.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_int* is a signed 64-bit integer.


Bigint
======

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +               val_bigint                +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +                                         +
   20	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_RATIONAL.
Where *arity* is always 0.
Where *nbr_cells* is always 1.
Where *val_bigint* is a pointer.


Float
=====

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +                val_real                 +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|                                         |
        +               - UNUSED -                +
   20	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_FLOAT.
Where *arity* is always 0
Where *nbr_cells* is always 1.
Where *val_real* is a floating-point *double*.


Cstring
=======

A small string < 16 bytes.

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


Static BLOB
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
   16	|                                         |
        +                 len_str                 +
   20	|                                         |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_CSTRING.
Where *arity* is always 0.
Where *flags* is FLAG_BLOB | FLAG2_STATIC.
Where *nbr_cells* is always 1.
Where *val_str* is a pointer to a slice of UTF-8 chars.
Where *len_str* is the length of the slice in bytes.

A static BLOB is length delimited, not NULL-terminated. The
primary use of a static BLOB is to point to memory that is never
freed, such as in a memory-mapped file.


Non-static BLOB
===============

A ref-counted string buffer.

        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 1
		+----------+---------+----------+---------+
    4	|                 nbr_cells               |
        +----------+---------+----------+---------+
    8	|                                         |
        +               val_strbuf                +
   12	|                                         |
        +----------+---------+----------+---------+
   16	|               strbuf_off                |
        +----------+---------+----------+---------+
   20	|               strbuf_len                |
        +----------+---------+----------+---------+

Where *val_type* is TYPE_CSTRING.
Where *arity* is always 0.
Where *flags* is FLAG_BLOB.
Where *nbr_cells* is always 1.
Where *val_strbuf* is a pointer to a strbuf object.
Where *strbuf_off* is the byte offset into a slice of a strbuf.
Where *strbuf_len* is the length of the slice.

A *strbuf* contains a reference count for tracking lifetimes, a length
field and a data payload. The *strbuf_off* field can be used to
define the tail of a BLOB. Or use a *strbuf_len* to get a slice.


String
======

A string is an optimized form of a chars-list and can be a BLOB of
either type. The *arity* is 2 and the *flag* has FLAG_STRING set.


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
   16	|                 val_off                 |
        +----------+---------+----------+---------+
   20	|               - UNUSED -                |
        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 2

Where *val_type* is TYPE_LITERAL.
Where *arity* is > 0.
Where *nbr_cells* is > 1 and includes the args.
Where *val_off* is into the symbol table.
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
   16	|                 val_off                 |
        +----------+---------+----------+---------+
   20	|               - UNUSED -                |
        +----------+---------+----------+---------+
    0	| val_type |  arity  |       flags        |    CELL 2

Where *val_type* is TYPE_LITERAL.
Where *arity* is always 2.
Where *nbr_cells* is > 1 and includes head & tail args.
Where *val_off* is into the symbol table to '.'.
Where args are the following cells (see *nbr_cells*).
Where the tail arg is usually a list.
Where the final tail arg is usually the atom *[]*.

Frames
======

A frame is an element in an array of frames. A frame is of fixed size
and contains an index into the slot table of the base slot for the
frame. It also contains a count of the number of slots that make up the
frame. The frame plus it's slots constitute a working context for a
set of goals. Choices can back-track to a given context.

Since only index numbers are used to refer to frames (a *ctx* number)
the frame table can be easily resized.


Slots
=====

A slot is an element in an array of slots. It is a cell plus a context
index (for vars and compounds). Also attribute info.

Slots are cleared on backtracking via the trailed record.

Since only index numbers are used to refer to slots (a *slot* number)
the slot table can be easily resized. During execution of a builtin
predicate however, slot pointers may need to be refreshed after
creating new variables (eg. length/2, copy_term/2 etc).


Trail
=====

Similar...


Heap
====

A space for dynamically created terms (compounds)...




