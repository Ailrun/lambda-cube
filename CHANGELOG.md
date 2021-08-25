# Unreleased

## Main updates

- Type/Kind level quasiquoter are added.

## Bug fixed

- Substitutions are fixed with valid shifting

## Breaking changes

- Names of quasiquoters are changed.
- The order of arguments for substitution are changed.
- Non-Ast modules now have explicit export lists.

# 0.2.0.0

## Main updates

- New top-level module for each lambda calculus is added.  
  It make easier to use one specific lambda calculus under a qualified name.
- Quasiquoter now supports splicing-in.
- Change the type of elaborator and type checker for composability.  
  Currently they are less type-safe. However, monad to make them type-safe will be added in the future.

## Breaking changes

- Lifters and substitutions into dedicated modules are moved.
- Pretty printers are renamed.
- Elaborators are renamed.
- Quasiquoter are renamed.
- Elaborated quasiquoters are remove.
- The types of elaborators and type checkers are changed.

# 0.1.0.0

The first release including support for following 4 lambda calculi.

- Simply Typed Lambda Calculus
- System F
- System F omega underbar
- System F omega
