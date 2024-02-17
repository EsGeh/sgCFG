# sgCFG

## Description

sgCFG is a command line tool for processing context-free grammars, in particular applying transformations to a grammar.
A program can read and write grammars in various formats.
Also the details of the format can be modified via command line arguments.

## Example Use Case

There are many strategies for writing parsers based on a given context-free grammar.
In functional languages *parser combinators* such as [parsec](https://hackage.haskell.org/package/parsec) are a common and convenient approach, since the code for a parser may be (more or less) directly derived from a given grammar, maybe even automatically generated.
In such scenarios, the shape of the input grammar has an important influence on the effiency and even on termination properties of a parser.
One might find the need to transform or "optimise" a grammar beforehand (usually without changing the language).
This tool may be used to apply such transformations.

## Notable Features

- read/write a grammar in backus-naur format
- read/write a grammar using grouped format(A -> X1 | ... | Xn) or ungrouped format (A -> X1, ... , A -> Xn)
- apply transformations to a grammar, notably:
	
	- left factoring
	- left recursion elimination
	- elimination of epsilon rules
	- isolation of the grammar for a specific nonterminal
	- finding unused rules
	- calculation of "FIRST" sets as described in the famous [Dragonbook](#bibliography)

## Remark

This software is work in progress. Bug reports and pull requests are welcome.

## Prerequisits

- [stack](https://haskellstack.org)

## Build

    $ stack build

## Installation

The executable can be run from a local directory:

    $ stack exec -- sgCFG [ARGS...]

To be able to invoke sgCFG from the command line without having to type the whole path to the executable, issue

    $ stack install

and make sure that `stack path --local-bin` is in your `PATH`. Now, the program may be run from the command line:

    $ sgCFG [ARGS...]

## Usage

(this passage assumes that the sgCFG executable is in your PATH)

To print a list of OPTIONS, run:

	$ sgCFG -h

The program is implemented after the well known "filter" paradigm used for many linux/unix utilites - it reads a grammar from stdin, and outputs a grammar to stdout (potentially after applying some transformations to it).
Also, it logs what is being done to stderr.

If you have a text file "file" containing your grammar, you can use sgCFG like this:

	$ cat "file" | sgCFG --input-format=default --output-grouped=default [OPTIONS] 2>"logfile.log" > "outputfile"

*Please notice*: the options "--input-format" and "--output" (or "--output-grouped") are always needed!

## Examples

- Identity (no transformations), *default* output format

        $ echo -e 'A -> A "a" | "a"' | stack exec -- sgCFG -i default -d default
        A->
          A "a"
        A->
          "a" 

- Identity (no transformations), *grouped* output format

        $ echo -e 'A -> A "a" | "a"' | stack exec -- sgCFG -i default -g default
        A ->
           A "a"
          |"a"

- Eliminate Left Recursion

        $ echo -e 'A -> A "a" | "a"' | stack exec -- sgCFG -i default -g default -t 'elimLeftRec(%v%n)' 2>/dev/null
        A ->
           "a" A0
        A0 ->
           "a" A0
          |""

- Chaining multiple transformations: Eliminate Left Recursion > Eliminate Epsilon Rules:

        $ echo -e 'A -> A "a" | "a"' | stack exec -- sgCFG -i default -g default -t 'elimLeftRec(%v%n)' -t 'elimEpsilon()' 2>/dev/null
        A ->
           "a" A0
          |"a"
        A0 ->
           "a" A0
          |"a 

## Bibliography

[Dragonbook]
	Alfred V. Aho, Monica S. Lam, Ravi Sethi, and Jeffrey D. Ullman:
		Compilers: Principles, Techniques, and Tools
