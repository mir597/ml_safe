ML-SAFE
====

This is a subset of SAFE(http://plrg.kaist.ac.kr) for parsing and traversing ECMAScript 5 code.

### Requirements

We assume you are using an operating system with a Unix-style shell (for example, Mac OS X, Linux, or Cygwin on Windows).  Assuming **JS_HOME** points to the SAFE directory, you will need to have access to the following:

  * J2SDK 1.7.  See http://java.sun.com/javase/downloads/index.jsp
  * sbt version 0.13 or later.  See http://www.scala-sbt.org
  * Bash version 2.5 or later, installed at /bin/bash.  See http://www.gnu.org/software/bash/
  * xtc, copied as $JS_HOME/bin/xtc.jar.  See http://cs.nyu.edu/rgrimm/xtc/

In your shell startup script, add $JS_HOME/bin to your path.  The shell scripts in this directory are Bash scripts.  To run them, you must have Bash accessible in /bin/bash.

### Installation

After launching sbt, type **antRun clean compile** and then **compile**.

Once you have built the framework, you can call it from any directory, on any JavaScript file, simply by typing one of available commands at a command line.  You can see the available commands by typing:

    bin/jsaf
    bin/jsaf help

### Run Tests

The following test script will generate a vector file 'vectors.txt':

	./test.sh


