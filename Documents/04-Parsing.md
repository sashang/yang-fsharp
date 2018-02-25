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

## Parsing of the module statement

The structure of the module is roughly as follows:

- Module header statements (`module-header-stmts`)

- Linkage statements (`linkage-stmts`)

- Meta statements (`meta-stmts`)

- Revision statements (`revision-stmts`)

- Body (`body-stmts`)

The Module, Meta, and Revision statements carry basic information about the module.
The order of statements in each of those sections is not important. Hence, the parse
accumulates them in records (in files `Header.fs`, `Meta.fs`, and `Revisions.fs`).

We also keep the unknown statements in each of these modules. We keep them in order.
However, we do not keep information about their order in relation with the expected
statements.

TODO: Comments on linkage section.

The assignment of unknown statements to sections is a bit tricky. When parsing a section
(e.g. header, linkage, meta, and revision), we associate all unknown statements with
the section being parsed. Hence, unknown statements that appear in the beginning of a section
will be associated with the preceding section.
