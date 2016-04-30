# sgCFG

## description

sgCFG is a command line tool for processing context-free grammars, in particular applying transformations to a grammar.
A program can read and write grammars in various formats.
Also the details of the format can be modified via command line arguments.

## notable features

- read/write a grammar in backus-naur format
- read/write a grammar using grouped format(A -> X1 | ... | Xn) or ungrouped format (A -> X1, ... , A -> Xn)
- apply transformations to a grammar, notably:
	
	- left factoring
	- left recursion elimination
	- elimination of epsilon rules
	- isolation of the grammar for a specific nonterminal
	- finding unused rules
	- calculation of "FIRST" sets as described in the famous [Dragonbook](#bibliography)

## installation

### preriquisits

- ghc 7.10
- cabal-install

### how to install sgCFG

1. clone the git repository
1. open a command line window
1. excute these commands:

	$ git checkout release
	$ cabal sandbox init
	$ cabal install --only-dependencies
	$ cabal configure
	$ cabal build

you will find the executable in the cabal output directory (usually "dist")

To be able to invoke sgCFG from the command line without having to type the whole path to the executable, add it to your PATH.

## usage

(this passage assumes that the sgCFG executable is in your PATH)

To print a list of OPTIONS, run:

	$ sgCFG -h

The program is implemented after the well known "filter" paradigm used for many linux/unix utilites - it reads a grammar from stdin, and outputs a grammar to stdout (potentially after applying some transformations to it).
Also, it logs what is being done to stderr.

If you have a text file "file" containing your grammar, you can use sgCFG like this:

	$ cat "file" | sgCFG --input-format=default --output-grouped=default [OPTIONS] 2>"logfile.log" > "outputfile"

*Please notice*: the options "--input-format" and "--output" (or "--output-grouped") are always needed!

## doc

Until now, calling the program with the "--help" command line argument is the only documentation available...

## Bibliography

[Dragonbook]
	Alfred V. Aho, Monica S. Lam, Ravi Sethi, and Jeffrey D. Ullman:
		Compilers: Principles, Techniques, and Tools
