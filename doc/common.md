# PROTEST/COMMON

## Summary

The package `PROTEST/COMMON` is the package for common PROTEST protocols. These
protocols may serve as examples of `PROTEST/PROTOCOL` syntax, but are also
functional and usable as imports by other systems.

Currently available protocols:
  * [`ADDRESSED`](../src/common/mixin/addressed.lisp) -
    a protocol for objects which have a network address,
  * [`DATE`](../src/common/date.lisp) -
    a protocol for microsecond-precision dates,
  * [`HANDLING`](../src/common/mixin/handling.lisp) -
    a protocol for objects with a handler function,
  * [`KILLABLE`](../src/common/mixin/killable.lisp) -
    a protocol for objects that need to be finalized,
  * [`NAMED`](../src/common/mixin/named.lisp) -
    a protocol for objects having a string name,
  * [`SERIALIZABLE`](../src/common/mixin/serializable.lisp) -
    a protocol for objects that are serializable to human-readable text.

All of the common protocols may be loaded by loading the `PROTEST/COMMON` ASDF
system. Individual protocols may be loaded by loading a system named
`PROTEST/COMMON/FOO`, replacing `FOO` with the system name

Each common protocol exports all symbols mentioned in its definition. See each
protocol's code file for the description and details of that protocol.

## Internal dependencies

  * [`PROTEST/PROTOCOL`](protocol.md)

## TODO

* Export protocol files into Markdown.
