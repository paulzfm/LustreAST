# LustreAST

A compiler translating AST to Lustre source code. The AST syntax is shown in [the document](https://github.com/paulzfm/LustreAST/blob/master/ast2lustre/doc/ast.pdf).

### Compilation

You should first install `ocaml` with `ocamllex` and `ocamlyacc`. In `ast2lustre/src`, type

    $ make

### Usage

To translate an AST, type

    $ python lexer.py -i <ast_file> | ./ast
