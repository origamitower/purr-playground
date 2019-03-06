# Purr's Data Types

This playground project explores different aspects of data types in Purr. In particular, we're interested in:

- **Evolution**: It should be possible for people to evolve software without feeling too tied to the initial decisions they've made in the software's architecture. Most of the changes should be backwards-compatible.

- **Security and Privacy**: One of the major focuses in Purr is security and privacy, this should extend to the data structure level. It should be possible to encode these policies directly in the data structures, and have them automatically enforced everywhere with minimal overhead, and little possibility for subversion.

## The calculus

This playground uses a simple object calculus derived from the untyped call-by-value lambda calculus:

```
x in Variables
n in Numbers
s in Strings
b in Booleans
c in Symbols

Label l ::= x | c

Declaration d ::=
  | define <x> = <e>

Expr e ::=
  | let <x> = <e1> in <e2>              -- let
  | if <e1> then <e2> else <e3>         -- if
  | { <e1> with <m1>, ..., <mN> }       -- record
  | <e>.<l>(<e1>, ..., <eN>)            -- send
  | symbol <s>                          -- symbol introduction
  | x                                   -- load
  | nothing                             -- null constant
  | n | b | s                           -- values

Message m ::= <x>.<l>(<x1>, ..., <xN>) -> <e> end
```

Execution is mostly straight-forward. Symbols are unique labels (equality is by reference), records are collection of labelled lambdas, sends are completely up to the receiver, like in Smalltalk.
