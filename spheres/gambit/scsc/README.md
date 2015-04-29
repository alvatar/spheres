scsc
====

An integration of gambit scheme and psyntax, with module system that
unifies psyntax modules with git repository content.

The scsc extends the Gambit included version of syntax-case to include:

* A bootstrapper implemented in Gambit Scheme.
* Nominal support for visit/revisit primitives.
* Nominal support for module loading.

The scsc modifies the Gambit included version of syntax-case to:

* Provide native support for Gambit ##begin syntax.
* Remove gambit namespace support.
* Remove gambit core syntaxes beginning with ##.
* Revert the build-data implementation to unconditional quoting.

Installation
------------

To install, clone this repsitory and run `boot.scm` to bootstrap
`syntax-case.scm` from its `psyntax.ss` source. Once bootstrapping is
complete, use `scsc.scm` to load the bootstrapped syntax expander for
general use.

```bash
bash-3.2$ git clone http://github.com/matthastie/scsc
Cloning into 'scsc'...
remote: Counting objects: 27, done.        
remote: Compressing objects: 100% (13/13), done.        
remote: Total 27 (delta 13), reused 27 (delta 13)        
Unpacking objects: 100% (27/27), done.
Checking connectivity... done
bash-3.2$ cd scsc
bash-3.2$ gsi boot
```

The `gsi boot` command creates a `syntax-case.scm` file that contains
the compiled psyntax implementation.

To use the psyntax expander, use `scsc.scm`, which configures the
Gambit reader before loading `syntax-case.scm`.

```scheme
bash-3.2$ gsc
Gambit v4.7.1
> (load "scsc")
"/Volumes/Data/scratch/scheme/control/system/scsc/scsc.scm"
> (module M (alpha (beta b))
    (module ((alpha a) b)
      (define-syntax alpha (identifier-syntax a))
      (define a 'a)
      (define b 'b))
      (define-syntax beta (identifier-syntax b)))
> (import M)
> (values alpha beta)
a
b
> 
```

To use the git module loader, load `module.scm` into your Gambit
interaction environment after loading `scsc.scm`.

Modules
-------

The provided module system extends psyntax modules with a rudimentary
module loading system.

When `(import <module>)` is evaluated, a module is loaded if:

 * `<module>` is not defined in the psyntax compile-time environment,
   or

 * either the file that defines `<module>`, or one or more `(include
  <file>)` within the module definition are newer than when previously
  loaded.

The search path for modules is defined by the value of
`current-module-paths`; it is a parameter containing a thunk, which
when applied returns a list of paths. i.e.

```scheme
(import scsc/modules)
(current-module-paths (lambda () (current-directory)))
```

The default value of `current-module-paths` searches for modules in
the git repository root relative to `current-directory`, and root
directories of the respective git submodule paths.

Two helper functions are exposed at the module interface to provide
for bespoke customization of `current-module-path`:

 * `system-root` - returns the git repository root for
current-directory, or #f if current-directory is not under git source
control.

 * `submodule-paths` - returns a list of paths that denote git submodule
root directories for a git repository defined by current-directory,
or () if current-directory is not under git source control.

Note that the value of current-directory is itself parameterized
during loading of module definitions. This behavior facilitates the
inclusion of scheme using relative path-names.

An unexpected side-effect or gotcha of this behavior is that the
current git repository root becomes that of the submodule being
loaded. Thus, users should note: in the current implementation, it is
necessary to customize `current-module-paths` if support for peer
submodule dependency is required.

Future versions of this module system may provide a better default
implementation of current-module-paths, probably in combination with
an extended import syntax to permit specification of git repository
versions.

Defining Modules
----------------

Module definitions may be located in the path list emitted by
current-module-paths, and may have either the literal name `module.scm`
name, or `<arbitrary>.module.scm`. The following example shows the
venerable hello world module:

```bash
bash-3.2$ git submodule
 8b4d8c00c6d4dd037189a0e2146e4ad417563127 home/hello (heads/master)
bash-3.2$ ls home/hello
hello.module.scm	hello.scm
bash-3.2$ cat home/hello/foo.module.scm 
(module hello
  (hello-world)
  (include "hello.scm"))
bash-3.2$ cat home/hello/hello.scm 
(define hello-world
    (lambda ()
      "hello, world."))
bash-3.2$ 
```

When a module import is requested, each module definition file,
`hello.module.scm` in the above example, is visited to determine
whether it contains a toplevel module definition that matches the
requsted `<module>`. if a match is found, the module definition is
loaded into the interaction environment.

Thus, module files may include more than one module definition, and
file naming is arbitrary, other than the name format specified above.

The following code imports the `hello` module and
executes exported function `hello-world`.

```scheme
Gambit v4.7.1

> (load "syntax-case")
"/Volumes/Data/scratch/scheme/control/system/scsc/syntax-case.scm"
> (load "module")
"/Volumes/Data/scratch/scheme/control/system/scsc/module.scm"
> (import hello)
> (hello-world)
"hello, world."
> 
```


DSSSL
-----

The scsc provides complete support for DSSSL lambda formals;
i.e. those that include one or more of `#!optional`, `#!keyword`, or
`#!rest` variables.

Roadmap
-------

* support compiled modules / compiled syntax
* support Gambit FFI & features
* <your feature request here>!

Support
-------

Issues / feature requests welcome - please get in contact!
