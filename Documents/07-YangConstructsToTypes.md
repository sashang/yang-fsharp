# Mapping YANG constructs to .NET types

## Standard elements

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

   _TODO:_ Decide what to do with default values

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

   _TODO:_ Think this case in more detail. Is is really a problem?

For now, it may be safer to avoid flattening containers.

An alternative would be to map containers to namespaces.
This seems to be a more natural mapping.
Doing so can avoid nesting.
It may even be possible to generate multiple files,
which will make it a bit easier to navigate the generated types.
