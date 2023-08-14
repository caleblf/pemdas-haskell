# Pemdas (Haskell Version)

A simple calculator using TeX-like syntax, written in Haskell.

## Examples

```console
$ pemdas "3 + 4"
(3.0 + 4.0)
= 7.0

$ pemdas "\\cos(pi)"
\cos(pi)
= -1.0

$ pemdas "\\sqrt^3(125)"
\sqrt^3(125.0)
= 4.999999999999999

$ pemdas "\\Sigma_{x \\in [1..10]}(x ** 2) / 10"
(\Sigma_{x \in [1..10]}(x ** 2) / 10.0)
= 38.5
```

## Status

Though functional, Pemdas is a work in progress.

To do list (internal):

* Improve consistency/conventionality of record spelling throughout
* Add comprehensive tests for the parser
* Improve test coverage in general
* Implement `Pemdas.Types.render` more efficiently, and allow dropping parens

To do list (interface):

* Add a friendly CLI (`--help`, function listing)
* Support switching between floating-point and integer expressions with a CLI flag
* Add comprehensive tests for the parser
* Improve test coverage in general
* Implement `Show` for `Expr` more efficiently, and drop parens at top level
* Rename some badly-named internal structures (`Pemdas.Functions`, `Pemdas.Types.Language`)
* Implement more aggreggation methods (`max`, `argmax`, etc.)

Future extensions:

* Two-argument functions (`atan2`, `gcd`, `lcm`)
* REPL mode
