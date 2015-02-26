# Files in this directory

* debug-server.scm: Scheme code that runs a server that will spawn a REPL once the debuggee sends a signal
* debuggee.scm: Scheme code able to signal the server to spawn a REPL
* pump.scm: Scheme code needed by the spawned REPLs
* rdi.scm: common client/server Scheme code
* sense: command-line utility to create the server
* sense-emacs: command-line utility to create the server within Emacs (called from sense-emacs.el)
* sense-emacs.el: Emacs module for creating a Sense server and REPLs within the Emacs environment
* sense-pump: command-line utility to spawn a REPL (used by the server)

The process works as follows:

In one terminal start the debug server:

    gsi -:dar,h10000 debug-server.scm 20000

This can be automated with the _sense_ script, or within Emacs using the _sense-emacs.el_ module, which in turn runs _sense-emacs_ script.
In another terminal start the debuggee:

    gsi -:dar,h10000 debuggee.scm

This will pop up a new terminal which has the main Gambit REPL. In this terminal you can evaluate expressions. Errors in different threads will pop up in a new terminal.
