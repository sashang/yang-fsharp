# Mapping YANG constructs to .NET types

This document describes that mapping of common YANG statements to
code constructs during code generation.

More advanced features (such as refinements, augmentations, conditionals, etc)
will be covered in a separate document.

TODO: Create the other document(s) and add references.

## Common sub-statements

### `description` [RFC 7950, Sec. 7.21.4, p. 136]

The `description` sub-statement is used to generate documentation strings.

### `reference` [RFC 7950, Sec. 7.21.4, p. 136]

The `reference` statement is also used in the documentation strings.
It points to an external document that defines or provides extra
information.
It seems reasonable to assume that it should follow the `description` string
(when that exists).

## Standard elements

### `typedef` [RFC 7950, Sec. 7.3, pp. 65-66]

A direct approach would be to translate `typedef` statements into classes.
However, `typedef`'s are a light way to amend an existing type,
e.g. add default values, units, change description.
In many cases, they are just used as type aliases (without adding any
extra information, apart from the new name).

For the common case of type aliases, a plausible approach is with the
`using` statement, e.g.

```C#
using ipv6addr = string;
```

However, such type aliases are per-file, and hence not appropriate for
exposing library functionality.
An alternative could be to define derived classes, e.g.:

```C#
class NewType : OldType {}
```

This works unless the `OldType` is sealed, which is the case
for many common types (e.g. `System.String`).

Juniper configuration files contain many simple type aliases (often, to string).
These are nice from the point of view of providing isolated types,
but, on the other hand, they hide the actual implementation type.

It seems that it will be sufficient to keep track of the changes introduced
by a `typedef`, and apply them at the point that the `typedef` is used.

In particular, `description` and `reference` should be used to
amend the generated documentation.

The `status` can be used to add an attribute to the generated member
(at the point of use of `typedef`).

The `unit` can be used both in the documentation, and also in the pretty
printing overloads (i.e. `ToString()`).
In F\#, it can also be used to add units-of-measure.

The `default` value can be used to drive code generation in the default constructor,
and also to define methods such as `IsDefault`.
Observe that we will also need to provide initializations from string values.

### `type` [RFC 7950, Sec. 7.4, pp. 66-67]

The `type` statement defines the type of an internal node.
If it does not add restrictions, it just defines the name of the type of the node.

Most of the restrictions that it can define should be used during the construction
of the enclosing class. This appears to be the case for:

- Type restrictions (e.g. `fraction-digits`, `length`, `pattern`, `range`).

- Custom type restrictions (e.g. `junos:posix-pattern` and the associated error message
  `junos:pattern-message`).

TODO: Figure out what to do with the following sub-statements of `type`:

- `base`

- `bit`

- `enum`

- `path`

- `require-instance`

- `type` (of which there can be many, and obviously they can recurse).

### `leaf` [RFC 7950, Sec. 7.6, pp. 73-77]

Example of a `leaf` definition:

```YANG
leaf priority-cost {
    description "Value subtracted from priority when bandwidth is below threshold";
    type uint32 {
        range "1 .. 254";
    }
}
```

`leaf` definitions correspond to standard class member definitions.
They define the type of the member, and often provide some description of its
use, and constraints.

Observe that the leaf may not appear in the definition. The `leaf`'s must appear
only then the `mandatory` tag is used, e.g.:

```YANG
leaf interface-name {
    description "Interface name";
    type interface-name;
    mandatory true;
}
```

TODO: Specify what to do with `leaf`'s sub-statements.

### `leaf-list` [RFC 7950, Sec. 7.7, pp. 77-84]

Example of a `leaf-list` definition:

```YANG
leaf-list source-address {
    description "Prefix to match";
    type ipv4prefix;
    max-elements 32;
}
```

`leaf-list`'s are similar to `leaf`'s, but instead of a simple member, they
define an array of values. The safest is probably to map them to an
`IEnumerable<T>`, for the right type `T`.
This will work only for consuming configuration data, but not necessarily
for changing configuration data (see below).
There are a couple of challenges with `leaf-list`'s:

1. The order of the items may or may not be important (see [RFC9970, Sec. 7.7.1]).
   If the order does not matter, the server is free to order them in any order.
   This corresponds to the `ordered-by system;` statement, and is the default.
   In many cases the order matters, e.g. packet filters. Those cases are
   specified with the `ordered-by user;` statement.

   For the purpose of updating the configuration, `YANG` gives a number of
   options for inserting new entries at the appropriate location in the list.
   How do we expose them programmatically?

1. Default values: the rules for deciding when to apply default values are
   rather complicated. See [RFC 7950, Sec. 7.7.2].

   TODO: Decide what to do with default values

TODO: Specify what to do with `leaf-list`'s sub-statements.

### `list` [RFC 7950, Sec. 7.8, pp. 84-93]

Example of a `list` statement:

```YANG
list server {
    key "name";
    unique "ip port";
    leaf name {
        type string;
    }
    leaf ip {
        type inet:ip-address;
    }
    leaf port {
        type inet:port-number;
    }
}
```

For the point of view of generating types, `list` definitions combine
type definition and type use (i.e. definition of a member element).
It may be convenient to separate the definition of the list item from
the definition of the collection. The latter can capture other properties
such as constraints, keys, etc.

The definition of the list item is reasonable to stay inside the definition
of the definition of the collection (say using the standard name `Item`).
Does it make sense to separate the definition of the collection with its use?
In the model, they are always defined together, but this does not need
to constraint the generated types.

Lifting the definition (of the collection type) higher in the hierarchy
reduces nesting and simplifies the source code (visually).
However, often those definitions have names that are not descriptive
outside of the context of which they are defined.
If we lift them, it will be more difficult to identify their purpose
from their name, unless we extend the name to contain more information
about the context (and, thus, complicate the name).

TODO: Specify what to do with `list`'s sub-statements.

### `choice` [RFC 7950, Sec. 7.9, pp. 93-99]

TODO: How to translate choice?

### `container` [RFC 7950, Sec 7.5, pp. 67-73]

Containers organize data into logical groups. There are two types
of containers, those that are purely used for organizing data, and can be skipped
(if no information is present in their internal nodes),
and those that must be there.

In the configuration settings (the XML retrieved from the device),
the container name is important for parsing and figuring out the types
of the objects to be generated.
However, it is possible to lift the type definitions that descent from
the container higher in the hierarchy (to minimize nesting).
However, there are some problems with this approach:

1. It is not obvious which containers to collapse. In the example below
   there are two container definitions:

   ```YANG
    container services {
        description "Service PIC applications settings";
        uses apply-advanced;

        container radius-flow-tap {
            description "Configure radius triggered flow-tap parameters";
            uses apply-advanced;

            leaf forwarding-class {
                ...
            }
        }
    }
    ```

    We could collapse the `services` container, since it does not contain
    extra information; indeed, the inner `radius-flow-tap` resembles a service.
    However, it would not be desirable to collapse `radius-flow-tap` since,
    the inner definition of `forwarding-class` has a meaning only inside a
    `radius-flow-tap`.

    One heuristic could be to collapse all containers that contain only
    other containers as immediate children. This will solve the common case
    of a container that just brings together a number of child containers that
    actually implement the functionality.

    However, even this simple case wouldn't work well with the Juniper
    configurations which practically contain a `uses apply-advanced` definition;
    The grouping `apply-advanced` defines a `leaf-list`. This `leaf-list`
    is a vendor-specific mechanism for defining default values
    (see also [Juniper specifics](08-Juniper-md)).
    The simple heuristic above would be tricked into thinking that the
    `services` container indeed holds configuration information,
    where in reality this is not the case (but that fact is difficult
    to put so early in the processing pipeline).

1. The second problem also relates to vendor-specific extensions,
   such as the `uses apply-advanced` construct above.
   The problem is that at the end, in a vendor specific step, we would
   need to merge information from various parts of the tree.
   The custom step that performs the merging will need detailed information
   of all those changes to guarantee correctness.

   TODO: Think this case in more detail. Is is really a problem?

For now, it may be safer to avoid flattening containers.

An alternative would be to map containers to namespaces.
This seems to be a more natural mapping.
Doing so can avoid nesting.
It may even be possible to generate multiple files,
which will make it a bit easier to navigate the generated types.
However, this approach cannot work for containers defined
inside non-container constructs (such as `list`'s).
For those, we will need to create inner classes.

### `anydata` [RFC 7950, Sec 7.10, pp. 100-102]

Example in definition:

```YANG

list logged-notification {
    key time;
    leaf time {
        type yang:date-and-time;
    }
    anydata data;
}

```

It specifies opaque data. The data can be represented as a string.
It seems typical to be in XML format. However, the mapping of the XML
into data structures is beyond our goals.

TODO: How to deal with internal nodes of `anydata`?

### `anyxml` [RFC 7950, Sec 7.10, pp. 100-102]

This is very similar to `anydata`, but we know that the data as in XML format.

