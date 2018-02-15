# Parsing unknown-statement declarations

The `unknown-statement` is described in [RFC 7950, Section 6.3.1] and defined in [RFC 7950, page 202, 208] as:

```ABNF
unknown-statement   = prefix ":" identifier [sep string] optsep
                      (";" /
                       "{" optsep
                           *((yang-stmt / unknown-statement) optsep)
                        "}") stmtsep

prefix              = identifier
stmtsep             = *(WSP / line-break / unknown-statement)
```

Observe that `unknown-statement` declarations exist in the very beginning of the
module definition, and they can be identified by their name, which has a prefix
followed by a `colon`. They can also exist wherever there is a stmtsep token,
which is in very many places. In most places, the parser needs to be on the lookout for unknown statements.

The standard basically encourages to treat them as comments, and ignore them.
However, we would like to keep them. One approach for parsing would be to just collect
them during parsing (e.g. to a list). However, in doing so, we loose information about their
order, e.g. which other (valid) statements preceded them, and which follow.
An alternative is to make the parser agnostic to the order of statements, and parse them
in a list (which preserves order). This is also convenient from a parser point of view.
However, then it would be difficult to have the parser check the order to statements,
and verify that they are in the order of the standard.
A third approach is to respect the order and bloat the structures to allow `unknown-statement`
information practically everywhere. This will complicate the parsing and the data structures
for an uncommon feature.

The most practical approach seems to be to parse statements in ordered list,
and then verify that the ordering is correct, and that other conditions are met
(e.g. some statements can appear only once). This will also simplify the development
of the parser (it will be easy to break the parser into sub-parsers). The main
disadvantage is that the grammar of the parser will not follow the standard.

## Parsing string in unknown-statement

The standard says that the `string` in the definition above should be unquoted string.
See the definition of Quoting in [RFC 7950, Section 6.1.3, page 45].
