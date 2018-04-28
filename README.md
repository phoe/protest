<p align="center">
  <img src="doc/logo.png">
</p>

# Common Lisp PROtocol and TESTcase manager

PROTEST is a tool for defining protocols and test cases written in and for
Common Lisp.

## tl;dr

```common-lisp
;; clone PROTEST into your local-projects directory, we are not in Quicklisp yet
(ql:quickload :protest)
(use-package :protest)

;; depending on your choice of test library, load one of the following
(ql:quickload :protest/parachute)
(use-package :protest/parachute)

;; (ql:quickload :protest/5am) ;; TODO
;; (use-package :protest/5am) ;; TODO

;; (ql:quickload :protest/prove) ;; TODO
;; (use-package :protest/prove) ;; TODO

;; (ql:quickload :protest/1am) ;; TODO
;; (use-package :protest/1am) ;; TODO
```

## Modules

The currently implemented modules are:

  * [`PROTEST/BASE`](doc/base.md) -
    for defining protocol classes and protocol condition types
  * [`PROTEST/PROTOCOL`](doc/protocol.md) -
    for defining and executing protocols
  * [`PROTEST/TEST-CASE`](doc/test-case.md) -
    for defining test cases
  * [`PROTEST/PARACHUTE`](doc/parachute.md) -
    for integrating test cases with
    [Parachute](https://github.com/Shinmera/parachute/) testing library

The modules planned for development are:

  * [`PROTEST/WEB`](doc/web.md) -
    output test cases and protocols to HTML
  * [`PROTEST/5AM`](doc/5am.md) -
    for integratingtest cases with [5AM](https://github.com/sionescu/fiveam)
    testing library
  * [`PROTEST/PROVE`](doc/prove.md) -
    for integrating test cases with [Prove](https://github.com/fukamachi/prove)
    testing library
  * [`PROTEST/1AM`](doc/1am.md) -
    for integrating test cases with [1AM](https://github.com/lmj/1am/) testing
    library

### License

All of PROTEST is licensed under GNU Lisp Lesser General Public License,
**except** the `PROTEST/PARACHUTE` system which is an extension to the Parachute
library and therefore licensed under the Artistic license.

-----------

PROTEST © 2018 Michał "phoe" Herda

This library is free software; you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation; either version 2.1 of the License, or (at your option) any
later version.

This library is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with this library; if not, write to the Free Software Foundation, Inc., 51
Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
