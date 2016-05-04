# Revision history for monad-ste

## 0.1.0.0  -- 2016-05-03

* First hackage release

* Added support for GHC 7.4 through 7.8 along with preexisting support for
7.10 and 8.0

* Added MonadFix instance

* Moved the definitions and unsafe code to .STE.Internal and marked the
  .STE module which only provides safe exports as Trustworthy

* Added a number of unsafe helper functions such as `unsafeInterleaveSTE`
to the Internal module, as well as exposing

## pre 0.1.0.0

* Thanks to Duncan Coutts for design feedback that using the
raw `raiseIO#` and `catch#` primitives would simplify the core code

* Thanks to Brian Schroeder and Joel Burget for helping debug and improve
the code.

* Originally a module in the Hopper project.
