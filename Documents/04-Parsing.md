# Parsing strategy

This document describes the approach we take for parsing.
It will be fleshed out as development progresses.

Due to the challenge of dealing with `unknown-statements` (see [Note-03](03-UnknownStatements.md)),
we take a flexible approach to parsing, and separate statement parsing with statement checking.
In the first step, we parse all statements in a list, and then we need to verify that the order
of statements in the list, and the cardinality of statements is correct.

Parsing is done with the help of [FParsec](http://www.quanttec.com/fparsec/) library.
We may consider using the [FParsec-Pipes](http://rspeele.github.io/FParsec-Pipes/Intro.html) library,
which builds on top of FParsec and provides a set of handy extensions. Those extensions make
it easier to translate from EBNF (the notation used in the YANG description in
[RFC 7950](https://tools.ietf.org/html/rfc7950#section-14)). However, the `FParsec-Pipes`
project is still in pre-release mode.

// TODO: More details.