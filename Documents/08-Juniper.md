# Specifics of Juniper's models

## Extension: `apply-groups`

Juniper uses the `apply-groups` construct to provide default values for various configuration options (e.g. `isis`, `bgp`, `tacacs`).
It is used as follows:

1. Named groups represent arbitrary configuration data. These typically appear in the
   beginning of the settings retrieved from the router. E.g.:

   ```xml
    <groups>
        <name>internal-bgp-md5-key</name>
        <protocols>
            <bgp>
                <group>
                    <name>&lt;*&gt;</name>
                    <authentication-key>AN AUTHENTICATION KEY HERE</authentication-key>
                </group>
            </bgp>
        </protocols>
    </groups>
    ```

1. In the main body of the configuration, they use the custom `apply-group` to reference that data:

   ```xml
   <system>
     <protocols>
       <bgp>
         <group>
           <apply-groups>internal-bgp-md5-key</name>
         </group>
       </bgp>
     </protocols>
   </system>
   ```

The `apply-group` construct is defined in the `apply-advanced` grouping as follows:

```yang
grouping apply-advanced {
    description "Apply advanced configuration logic";
    leaf-list apply-groups {
        description "Groups from which to inherit configuration data";
        type string;
    }
    leaf-list apply-groups-except {
        description "Don't inherit configuration data from these groups";
        type string;
    }
}
```

This construct is non-standard, i.e. the application of the extra configuration parameters is not part of the `YANG` standard.
Moreover, the connection of group names to keys is also vendor specific.
The mapping of elements to items also seem to be arbitrary;
why, for example, the group definition does not include in its hierarchy the `system` container?

This vendor extension should not affect type generation or configuration parsing. However, we will
need an extra custom step to merge the various configuration parameters.

_TODO:_ Unclear how to present the merged information to the user.

_TODO:_ Should we assume that if the same configuration appears both in the group
        and in the proper configuration, the values in the latter win?

## Extension: `junos:must`

The `junos:must` extension define conditions on the configuration.

```YANG
container interfaces {

}
container mobile-flow-tap {
    container source-interface {
        leaf interface-name {
            junos:must "(\"interfaces $$\")";
            junos:must-message "Interface must be defined";
            type interface-name;
            mandatory true;
        }
    }
}
```

_TODO:_ Better example with configuration data as well.

In the must statement the `$$` probably expands to the name (value)
associated with `interface-name` (which is of type string in this example).
Then rule then is roughly `("interfaces <interface_name>")`.
The `interface_name` should be matched against the keys defined
in the list of interfaces.

Checking the condition seems to be rather ad-hoc:
Probably, the `interfaces` container is resolved by navigating up the
configuration tree. However, the `inteface_name` is a bit more tricky,
since the `interfaces` container stores more things than just interfaces.

_TODO:_ To enforce that rule, we probably need a post-processing rule.
There is no guarantee (I believe) that, e.g., the definition of
`mobile-flow-tap` (above) will appear after the definition of the interfaces.

_TODO:_ It would also be nice to be able to point from the `interface-name` leaf
to the object that corresponds to the interface. That pointer may need
to be patched later. However, we will need to create a member that points
to the interface. Moreover, that member should be of the appropriate type.
Observe that finding the correct type for the interface in the example,
may be tricky, because the algorithm to identify definition-use statements
does not consider such locations.
