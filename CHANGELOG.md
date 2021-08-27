# Unreleased

## Main updates

- Now systems support top-level definitions.

## Breaking changes

- Names of elaborators are changed.
- Names of evaluators are changed.
- Names of type checkers are changed.
- Names of normalizer are changed.
- `dollarsign` common parser is removed.

# 0.3.0.0

## Main updates

- Type/Kind level quasiquoter are added.
- Evaluators are added to the top-level modules.
- STLC and System F omega are tested with various examples.
- Identifiers now accepts `_`.

## Bug fixed

- Substitutions are fixed with valid shifting.
- STLC normalizer is fixed with valid beta-reduction.
- Fix type inference for System F and System F omega.

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
