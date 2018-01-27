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
```bash
$ git clone https://github.com/mateuszlewko/lang-compiler.git && cd lang-compiler
```
### Install dependencies
- install [*ocaml*](https://ocaml.org/docs/install.html) and [*opam*](https://opam.ocaml.org/doc/Install.html)
- switch to *ocaml* version `4.05.0`:
```bash
$ opam switch 4.05.0
```
- configure *opam* in the current shell: 
```bash
$ eval `opam config env`
```
- install *jbuilder*: 
```bash
$ opam install jbuilder
```
- install rest of dependencies by following output from this commands (except for `menhirLib`): 
```bash
$ jbuilder external-lib-deps --missing @runlangc
$ jbuilder external-lib-deps --missing @runtest
```
  You will be asked to install required modules through *opam*, and some external libraries through *depext*.
- install [LLVM 5](https://llvm.org/) and *gcc* (*gcc* is usually present on linux)
  
 Finally check whether you installed everything correctly:
```bash
$ llc --version    # Expect something like LLVM version 5.0.1, later versions should also be fine. 
                   # NOTE: Version 3.8 will *not* work.
$ gcc --version
$ jbuilder --version
```

### Test
Run following command from the root directory of the project, because relative paths to input files used for tests are hardcoded into source code (files in `test/compiler-test-srcs/`).
```bash
$ make test && ./_build/default/test/test.exe
```
  You can add your own tests by creating *lang* source files in `test/compiler-test-srcs/` and specifying expected output in file: `test/compiler_tests.ml` (take a look at this file for examples).

### Ast pretty-printer
```bash
make printer && ./_build/default/printer/printer.exe
```

### Compile some code!
- First make sure compiler is built:
```bash
$ make langc -B
```
  It's best if you compile files in project root directory as compiler expects file `external.c` to be present at currenty directory. If you are compiling *lang* source from other directory, make sure to copy `external.c` in there.
- Compile `example.la`:
```bash
$ _build/default/langc/langc.exe example.la
```
  If everythinh went fine, compiler generated binary `a.out`. Check it by running:
 ```bash
 $ ./a.out
 ```
  *Langc* compiler also other options like saving generated *LLVM IR* code in `out.ll`, or changing binary output file with `-o other.out`. For a full list of available options check:
```bash
$ _build/default/langc/langc.exe --help
```

## Project structure
- `compiler/` - compiler library which contains: lexer, parser and codegen 
- `compiler/gammar.mly` - grammar in [Menhir](http://gallium.inria.fr/~fpottier/menhir/) format
- `compiler/lexer.cppo.sedlex.ml` - lexer 
- `compiler/codegen.ml` - main *LLVM IR* code generation 
- `compiler/codegenUtils.ml` - some helper functions for code generation 

- `printer/` - pretty-printer for abstract syntax tree
- `langc/` - compiler executable

- `test/` - tests for compiler and parser
- `test/compiler-test-srcs` - input files for compiler tests

## Used libraries and code:

- [ocaml-parsing](https://github.com/smolkaj/ocaml-parsing) - boilerplate code for parsing in OCaml
- [Menhir](http://gallium.inria.fr/~fpottier/menhir/) - LR(1) parser generator
- [Sedlex](https://github.com/alainfrisch/sedlex) - lexer generator
- Jane Street's [core](https://ocaml.janestreet.com/ocaml-core/latest/doc/), the inofficial standard library for OCaml
- Jane Street's [jbuilder](https://github.com/janestreet/jbuilder), an OCaml build system
- Other specified in jbuild files

<https://stackoverflow.com/questions/12149217/printf-doesnt-print-a-string-immediatly>
