# LustreAST

A compiler both translating AST to Lustre source code and translating Lustre to AST.

The AST syntax is shown in [the AST  document](https://github.com/paulzfm/LustreAST/blob/master/doc/ast.pdf).

The Lustre syntax we supported is shown in [the Lustre document](https://github.com/paulzfm/LustreAST/blob/master/doc/lustre.pdf).

### Compilation

You should first install `ocaml` with `ocamllex` and `ocamlyacc`. In `ast2lustre/src`, type

    $ make

and in `lustre2ast/src`, type

    $ make

### Usage

To translate an AST, in `ast2lustre/src` type

    $ python lexer.py -i <ast_file> | ./ast

To compile a Lustre code into AST, in `lustre2ast/src` type

    $ ./lustre < <lustre_file>
