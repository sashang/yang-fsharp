# Parsing strategy

This document describes the approach we take for parsing.
It will be fleshed out as development progresses.

Due to the challenge of dealing with `unknown-statements` (see [Note-03](03-UnknownStatements.md)),
we take a flexible approach to parsing, and separate statement parsing with statement checking.
In the first step, we parse all statements in a list, and then we need to verify that the order
of statements in the list, and the cardinality of statements is correct.

// TODO: More details.