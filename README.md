[![SchemeSpheres](http://www.schemespheres.org/assets/scheme-spheres-c309430cd6d751febdd151c2b30de701.png)](http://www.schemespheres.org)

_A set of tools and libraries for real-life Scheme. Multiplatform and mobile._

[![Join the chat at https://gitter.im/fourthbit/spheres](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/fourthbit/spheres?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


## Installation

1. Compile the syntax expander: go to /spheres/gambit and run ./compile-scsc.sh
2. Copy the .gambcini file to your HOME directory. This is Gambit's initialization file.
3. (optional) Copy the _spheres/_ directory into your Gambit's installation library directory (i.e. /usr/local/Gambit/lib)
4. Update all .gambcini paths to where your Spheres files are located (either Gambit's library directory, if you copied the files there, or any other path where you choose to leave Spheres' files).

### Scheme implementation requirements

[![Gambit Version](http://img.shields.io/badge/Gambit version -4.7.5-blue.svg)](http://gambitscheme.org)
[![Build Status](https://travis-ci.org/fourthbit/spheres.svg?branch=master)](https://travis-ci.org/fourthbit/spheres)

Some libraries in Spheres are already compatible with r7rs implementations such as Chibi, but only Gambit is actively supported.


## Basic usage

In order to make a library available within the REPL, the extended _load_ form is used:

    (load (spheres/core base))

Spheres also brings automated compilation and __multiplatform/mobile__ build scripts. In order to make this functionality available, you need to install [Ssrun](https://github.com/fourthbit/ssrun). Once you do this, you can download the [project template](https://github.com/fourthbit/sphere-project) if you want to create a project based on Spheres. Additionally, with _Ssrun_, you can call the compilation tasks within the REPL:

    (load (spheres/core base) compile: #t)


## Notes

Spheres supports _syntax-rules_ and _define-macro_ syntax. The implementation is an R7RS Library Syntax subset. Current support is practical:

* Macro systems supported: _syntax-rules_ and _define-macro_ (_syntax-case_ is also supported, but not used).
* Procedures in each library are within their own namespace (imports/exports are handled).
* Not supported from R7RS: _except_, _only_, _rename_, _include-ci_, _include-library-declarations_. Syntax is always in the global namespace (imports/exports and not handled).


## Contribute

Please, write to the project contributors if you are interested. Any help is welcome!
