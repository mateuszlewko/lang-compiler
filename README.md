# *Lang* compiler

*Lang* is a functional programming language based on [Ocaml](https://ocaml.org/) and [F#](http://fsharp.org/). *Langc* is a compiler for this language (written in [Ocaml](https://ocaml.org/))

This is a work in progress.

## Language features

- indent sensitive syntax
- functional first with imperative statements (TODO)

## How to build, test & run
- Get source code: 
```
git clone https://github.com/mateuszlewko/lang-compiler.git && cd lang-compiler
```
- install *opam* and *jbuilder*
- install rest of dependencies: 
```
jbuilder external-lib-deps --missing @@runlangc
```

### Test:
```
make test && ./_build/default/test/test.exe
```

### Ast pretty-printer:
```
make printer && ./_build/default/printer/printer.exe
```

## Project structure
- `compiler/` - compiler library which contains: lexer, parser and codegen 
- `compiler/gammar.mly` - grammar in [Menhir](http://gallium.inria.fr/~fpottier/menhir/) format
- `compiler/lexer.cppo.sedlex.ml` - lexer 
- `printer/` - pretty-printer 
- `langc/` - compiler executable

## Currently done

- Lexer and parser for simple programs (multiple `let` expression with function applications and arithmetic operations)
- Ast pretty printer

## TODO
Check [tasks.todo](https://github.com/mateuszlewko/lang-compiler/blob/master/tasks.todo) for a list of things needed to be done.

## Used libraries:

- [ocaml-parsing](https://github.com/smolkaj/ocaml-parsing) - boilerplate code for parsing in OCaml
- [Menhir](http://gallium.inria.fr/~fpottier/menhir/) - LR(1) parser generator
- [Sedlex](https://github.com/alainfrisch/sedlex) - lexer generator
- Jane Street's [core](https://ocaml.janestreet.com/ocaml-core/latest/doc/), the inofficial standard library for OCaml
- Jane Street's [jbuilder](https://github.com/janestreet/jbuilder), an OCaml build system


<https://stackoverflow.com/questions/12149217/printf-doesnt-print-a-string-immediatly>