# [SchemeSpheres](http://www.schemespheres.org)

_A set of tools and libraries for real-life Scheme. Multiplatform and mobile._

[Visit the project web page](http://www.schemespheres.org).


## Installation

1. Copy the .gambcini file to your HOME directory. This is Gambit's initialization file.
2. (optional) Copy the _spheres/_ directory into your Gambit's installation library directory (i.e. /usr/local/Gambit/lib)
3. Update all .gambcini paths to where your SchemeSpheres files are located (either Gambit's library directory, if you copied the files there, or any other path where you choose to leave SchemeSpheres' files).

Currently, SchemeSpheres requires the [Gambit Scheme compiler](http://schemespheres.org). Some libraries in SchemeSpheres are already compatible with r7rs implementations such as Chibi.


## Basic usage

To load and include all necessary files for a library within the REPL, use:

    (load '(spheres/core base))

SchemeSpheres also supports automated compilation and multiplatform build scripts. In order to make this functionality available, you need to install [Ssrun](https://github.com/fourthbit/ssrun). Once you do this, you can download the [project template](https://github.com/fourthbit/sphere-project) if you want to create a project based on SchemeSpheres.


## Notes

SchemeSpheres supports _syntax-rules_ and _define-macro_ syntax, and it loads/evaluates all necessary code for library availability. However, it doesn't handle the name hiding of the export/import syntax. This will be implemented by Gambit any time soon.


## Contribute

Please, write to the project contributors if you are interested. Any help is welcome!
