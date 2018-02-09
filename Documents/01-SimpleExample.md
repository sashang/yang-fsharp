# Examples of processing pipeline

## Example 1

As a simple example, let's consider the parsing of the global options for the
Cisco Discovery Protocol (CDP). The configuration file may have a statement
as the following

> cdp enable

The documentation for the option is
[here](https://www.cisco.com/c/en/us/td/docs/ios-xml/ios/cdp/configuration/15-mt/cdp-15-mt-book/nm-cdp-discover.html).

The YANG description is given many places, e.g. in
[here](https://github.com/YangModels/yang/blob/master/vendor/cisco/xr/631/Cisco-IOS-XR-cdp-cfg.yang).

It boils down to:

```YANG
  container cdp {
    description "Global CDP configuration data";
    leaf timer {
      type uint32 {
        range "5..255";
      }
      default "60";
      description "Specify the rate at which CDP packets are sent";
    }
    leaf advertise-v1-only {
      type empty;
      description "Enable CDPv1 only advertisements";
    }
    leaf enable {
      type boolean;
      default "true";
      description "Enable or disable CDP globally";
    }
    leaf hold-time {
      type uint32 {
        range "10..255";
      }
      default "180";
      description
        "Length of time (in sec) that the receiver must
        keep a CDP packet";
    }
    leaf log-adjacency {
      type empty;
      description "Enable logging of adjacency changes";
    }
  }
```

Based on the YANG model above, we can generate a .NET structure that looks similar to the following:

```C#
/// <summary>Global CDP configuration data</summary>
class cdp {
    /// <summary>Specify the rate at which CDP packets are sent</summary>
    int timer;
    /// <summary>Enable CDPv1 only advertisements</summary>
    Empty advertise_v1_only;
    /// <summary>Enable or disable CDP globally</summary>
    bool enable;
    /// <summary>Length of time (in sec) that the receiver must keep a CDP packet</summary>
    uint hold_time;
    /// <summary>Enable logging of adjacency change</summary>
    Empty log_adjacency;

    cdp(XmlReader reader) { ... }
}
```

An example of requesting the configuration of the CDP configuration from the router is
[here](https://www.cisco.com/c/en/us/td/docs/optical/ncs1000/60x/b_Datamodels_cg_ncs1000/b_Datamodels_cg_ncs1000_chapter_00.html).

The router will return:

```xml
<cdp xmlns="http://cisco.com/ns/yang/Cisco-IOS-XR-cdp-cfg">
    <timer>10</timer>
    <enable>true</enable>
    <log-adjacency></log-adjacency>
    <hold-time>200</hold-time>
    <advertise-v1-only></advertise-v1-only>
</cdp>
```

which should be easy to translate to objects of type `cdp` above.
