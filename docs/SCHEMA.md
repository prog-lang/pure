# Compilation Schematic

## Definitions

- **Source** - Pure source code (plain text)
- **AST** - Abstract Syntax Tree (internal representation of a `module`)
- **TAST** - _Typed_ AST
- **Target** - Output language (Node.js / Go / LLVM / Bytecode / ...)

## Pipeline

```text

                  *--------*
    [ Source ]--->| Parser |--->[ AST ]---*
                  *--------*              |
                                          |
                      *--------------*    |
    *-----[ TAST ]<---| Type Checker |<---*
    |                 *--------------*
    |
    |    *-----------*                 *----------------*
    *--->| Optimiser |--->[ TAST' ]--->| Code Generator |--->[ Target ]
         *-----------*                 *----------------*

```
