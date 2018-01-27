# *Lang* compiler

*Lang* is a functional programming language based on [Ocaml](https://ocaml.org/) and [F#](http://fsharp.org/). *Langc* is a [LLVM](https://llvm.org/) compiler for this language (written in [Ocaml](https://ocaml.org/)).

*This is a work in progress, although usable version is already available.*

## Language features

- indent sensitive syntax
- functional first with imperative features (printing, mutable arrays)
- functions are first class citizen
- simple modules (like in *F#*)
- nested lets, recursion 
- easy and zero-cost C bindings
- currently only supports `int`, `bool` and `unit` as basic types

## How to build, test & run
- Get the source code: 
```
$ git clone https://github.com/mateuszlewko/lang-compiler.git && cd lang-compiler
```
### Install dependencies:
- install [*ocaml*](https://ocaml.org/docs/install.html) and [*opam*](https://opam.ocaml.org/doc/Install.html)
- switch to *ocaml* version `4.05.0`:
```
$ opam switch 4.05.0
```
- configure *opam* in the current shell: 
```
$ eval `opam config env`
```
- install *jbuilder*: 
```
$ opam install jbuilder
```
- install rest of dependencies by following output from this commands (except for `menhirLib`): 
```
$ jbuilder external-lib-deps --missing @runlangc
$ jbuilder external-lib-deps --missing @runtest
```
  They will ask you to install required modules through *opam*, and some external libraries through *depext*.
- install [LLVM](https://llvm.org/) and *gcc* (*gcc* is usually present on linux distributions)
  
 Finally check whether you installed everything correctly:
```
$ llc --version
$ gcc --version
$ jbuilder --version
```

### Test:
Run following command from the root directory of the project as relative paths to input files used for tests are hardcoded into code (source files in `test/compiler-test-srcs/`).
```
$ make test && ./_build/default/test/test.exe
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
