[![Spheres](http://www.schemespheres.org/assets/scheme-spheres-c309430cd6d751febdd151c2b30de701.png)](http://www.schemespheres.org)

_A set of tools and libraries for real-life Scheme. Multiplatform and mobile._

[![Join the chat at https://gitter.im/fourthbit/spheres](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/fourthbit/spheres?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


## Installation

The libraries are implemented in R7RS style. Support and installation depends on the Scheme implementation used. Spheres is mainly developed for Gambit, but nothing stops other implementations to use and adapt these libraries.

### Installation for Gambit Scheme

[![Gambit Version](http://img.shields.io/badge/Gambit version -4.8.8-blue.svg)](http://gambitscheme.org)
[![Build Status](https://travis-ci.org/fourthbit/spheres.svg?branch=master)](https://travis-ci.org/fourthbit/spheres)

In order to use these libraries, we need to provide Gambit with _R7RS libraries_ and _syntax-rules_ support. See below for current limitations. The following process will set up Gambit for use with Spheres in Linux, OSX or Windows:

1. Compile the syntax expander: go to /spheres/gambit and run ./compile-scsc.sh. This file provides improved syntax-rules support for Gambit.
2. Copy the .gambini file to your _HOME_ directory. This is Gambit's initialization file, and it is configured to load the syntax expander and a minimal R7RS layer.
3. (optional) Copy the _spheres/_ directory into your Gambit's installation library directory (i.e. /usr/local/Gambit/lib).
4. Update all .gambini paths to where your Spheres files are located (either Gambit's library directory, if you copied the files there, or any other path where you choose to leave Spheres' files).

## Basic usage

In order to make a library available within the REPL, the extended _load_ form is used:

    (load (spheres/core base))

Spheres also brings automated compilation and __multiplatform/mobile__ build scripts. In order to make this functionality available, you need to install [Ssrun](https://github.com/fourthbit/ssrun). Once you do this, you can download the [project template](https://github.com/fourthbit/sphere-project) if you want to create a project based on Spheres. Additionally, with _Ssrun_, you can call the compilation tasks within the REPL:

    (load (spheres/core base) compile: #t)


## Notes

Spheres supports _syntax-rules_ and _define-macro_ syntax. While Gambit will natively support R7RS in the future, at the moment we provide asubset of the functionality for practical usage:

* Macro systems supported: _syntax-rules_ and _define-macro_ (_syntax-case_ is also supported, but not used).
* Procedures in each library are within their own namespace (imports/exports are handled).
* Not supported from R7RS: _except_, _only_, _rename_, _include-ci_, _include-library-declarations_. Syntax is always in the global namespace (imports/exports and not handled).


## Contribute

Please, write to the project contributors if you are interested. Any help is welcome!
