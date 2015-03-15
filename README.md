# [Spheres](http://www.schemespheres.org)

_A set of tools and libraries for real-life Scheme. Multiplatform and mobile._

[Visit the project web page](http://www.schemespheres.org).


## Installation

1. Copy the .gambcini file to your HOME directory. This is Gambit's initialization file.
2. (optional) Copy the _spheres/_ directory into your Gambit's installation library directory (i.e. /usr/local/Gambit/lib)
3. Update all .gambcini paths to where your Spheres files are located (either Gambit's library directory, if you copied the files there, or any other path where you choose to leave Spheres' files).

Spheres requires the [Gambit Scheme compiler](http://gambitscheme.org). However, some libraries in Spheres are already compatible with r7rs implementations such as Chibi.


## Basic usage

In order to make a library available within the REPL, the extended _load_ form is used:

    (load (spheres/core base))

Spheres also brings automated compilation and __multiplatform/mobile__ build scripts. In order to make this functionality available, you need to install [Ssrun](https://github.com/fourthbit/ssrun). Once you do this, you can download the [project template](https://github.com/fourthbit/sphere-project) if you want to create a project based on Spheres. Additionally, with _Ssrun_, you can call the compilation tasks within the REPL using the lower-level form _%load-library_:

    (load (spheres/core base) compile: #t)


## Notes

Spheres supports _syntax-rules_ and _define-macro_ syntax. The implementation is an R7RS Library Syntax subset. Current support is practical:

* Macro systems supported: _syntax-rules_ and _define-macro_ (_syntax-case_ is also supported, but not used).
* Procedures in each library are within their own namespace (imports/exports are handled).
* Not supported from R7RS: _except_, _only_, _rename_, _include-ci_, _include-library-declarations_. Syntax is always in the global namespace (imports/exports and not handled).


## Contribute

Please, write to the project contributors if you are interested. Any help is welcome!
