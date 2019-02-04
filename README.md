# The Meta II Compiler written in Scheme

Adapting my Scheme monadic parsing
[library](https://github.com/siraben/monadic-parsing) to work as a
compiler for the META II compiler-writing language.

## Usage
The bootstrap META II compiler is already in
[meta-II.scm](meta-II.scm), so just load the Scheme file and run

```scheme
(parse-meta-II-file "meta-II.txt")
```

To generate the META II machine assembler code.  I haven't written an
assembler yet to translate this into bytecode, but [this
one](http://www.bayfronttechnologies.com/mc_workshop.html) should work
in the meantime.  Paste the outputted assembler code into the box that
has __Code:__ written above it.  On the left, paste the contents of
[meta-II.txt](meta-II.txt) into it, then press __Compile__.  The
output on the right should match exactly to the assembler code that
was generated by the original Scheme implementation.

## What is META II?
META II is a DSL for writing compilers, created around 1963 by Dewey
Val Schorre.  It has a very simple rule:

`Each syntax equation is translated into a recursive subroutine which
tests the input string for a particular phrase structure, and deletes
it if found.`

There's a little more to it than just that, allowing for parsing of
identifiers, strings and numbers, but it's really just a convenient
abbreviation.

What's even more amazing is that the META II compiler written in
itself racks in at 20 lines _including blank lines_ (see
[meta-II.txt](meta-II.txt)).  This is a full compiler outputting
assembly language for the META II machine, so it's really a bytecode
compiler.  Nevertheless, once you have compiled META II with itself
you can use the compiled code to compile even more compilers.  Note
that META II is _not_ Turing Complete, but that's a good thing,
because you wouldn't want the problem of the compiler compiling
programs to be undecidable, right?

META II is language-agnostic.  You don't need to compile for the META
II machine, instead why not try the VALGOL II machine or real
architectures such as the Z80 (my personal favorite)?  You can rest
assured knowing that your compiler will __never__ crash because of a
parsing error!

## Future Plans
- [ ] An assembler (from META II assembly to bytecode)
- [ ] A META II bytecode interpreter (perhaps written in C)
- [ ] A VALGOL I bytecode interpreter
- [ ] A VALGOL II bytecode interpreter