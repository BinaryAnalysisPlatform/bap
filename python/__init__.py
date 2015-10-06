r"""Python inteface to BAP.

In a few keystrokes:

    >>> import bap
    >>> print '\n'.join(insn.asm for insn in bap.disasm("\x48\x83\xec\x08"))
        decl    %eax
        subl    $0x8, %esp

A more complex example:

    >>> img = bap.image('coreutils_O0_ls')
    >>> sym = img.get_symbol('main')
    >>> print '\n'.join(insn.asm for insn in bap.disasm(sym))
        push    {r11, lr}
        add     r11, sp, #0x4
        sub     sp, sp, #0xc8
        ... <snip> ...

Bap package exposes two functions:

#. ``disasm`` returns a disassembly of the given object
#. ``image``  loads given file

Disassembling things
====================

``disasm`` is a swiss knife for disassembling things. It takes either a
string object, or something returned by an ``image`` function, e.g.,
images, segments and symbols.

``disasm`` function returns a generator yielding instances of class
``Insn`` defined in module :mod:`asm`. It has the following attributes:

* name - instruction name, as undelying backend names it
* addr - address of the first byte of instruction
* size - overall size of the instruction
* operands - list of instances of class ``Op``
* asm - assembler string, in native assembler
* kinds - instruction meta properties, see :mod:`asm`
* target - instruction lifter to a target platform, e.g., see :mod:`arm`
* bil - a list of BIL statements, describing instruction semantics.

``disasm`` function also accepts a bunch of keyword arguments, to name a few:

* server - either an url to a bap server or a dictionay containing port
  and/or executable name
* arch
* endian  (instance of ``bil.Endian``)
* addr    (should be an instance of type ``bil.Int``)
* backend
* stop_conditions

All attributes are self-describing I hope. ``stop_conditions`` is a list of
``Kind`` instances defined in :mod:`asm`. If disassembler meets instruction
that is instance of one of this kind, it will stop.

Reading files
=============

To read and analyze file one should load it with ``image``
function. This function  returns an instance of class ``Image`` that
allows one to discover information about the file, and perform different
queries. It has function ``get_symbol`` function to lookup symbol in
file by name, and the following set of attributes (self describing):

* arch
* entry_point
* addr_size
* endian
* file (file name)
* segments

Segments is a list of instances of ``Segment`` class, that also has a
``get_symbol`` function and the following attributes:

* name
* perm (a list of ['r', 'w', 'x'])
* addr
* size
* memory
* symbols

Symbols is a list of, you get it, ``Symbol`` class, each having the
following attributes:

* name
* is_function
* is_debug
* addr
* chunks

Where chunks is a list of instances of ``Memory`` class, each having the
following attributes:

* addr
* size
* data

Where data is actual string of bytes.
"""
__all__ = ['disasm', 'image', 'adt', 'asm', 'arm', 'bil']

from .bap import disasm, image
