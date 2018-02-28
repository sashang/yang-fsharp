(* Unit tests for the simple model from RFC7950 *)

#if INTERACTIVE
#else
namespace Yang.Examples.RFC7950.SimpleModel
#endif

// Compile with:
// fsc --target:library rfc7950-example-system.fsx
//
// Run tests with:
// ..\..\packages\xunit.runner.console\tools\net452\xunit.console.exe .\rfc7950-example-system.dll
// Make sure that: xunit.assert.dll, xunit.core.dll, and xunit.execution.desktop.dll are in same directory.
//
// Stop provider with: pskill fsautocomplete

// TODO: The following does not seem to work
// #load @"..\..\.paket\load\net471\xunit.fsx"
//       and instead we do the following:
#I @"..\..\packages\"
#r @"xunit.abstractions\lib\net35\xunit.abstractions.dll"
#r @"xunit.assert\lib\netstandard1.1\xunit.assert.dll"
#r @"xunit.extensibility.core\lib\netstandard1.1\xunit.core.dll"
#r @"xunit.core\build\xunit.execution.desktop.dll"

#r @"..\..\build\Yang.Generator.dll"

module Model =
    open Yang.YangProvider

    let [<Literal>] model = """
    // This is the example from Sec. 4.2.2.5 of RFC 7950 (p.22-23)
    // Contents of "example-system.yang"
    module example-system {
        yang-version 1.1;
        namespace "urn:example:system";
        prefix "sys";

        organization "Example Inc.";
        contact "joe@example.com";
        description
            "The module for entities implementing the Example system.";

        revision 2007-06-09 {
            description "Initial revision.";
        }

        container system {
            leaf host-name {
                type string;
                description
                    "Hostname for this system.";
            }

            leaf-list domain-search {
                type string;
                description
                    "List of domain names to search.";
            }

            container login {
                leaf message {
                    type string;
                    description
                    "Message given at start of login session.";
                }

                list user {
                    key "name";
                    leaf name {
                        type string;
                    }
                    leaf full-name {
                        type string;
                    }
                    leaf class {
                        type string;
                    }
                }
            }
        }
    }
    """

    type T = YangFromStringProvider<model>
    // type X =
    //     class
    //         inherit T

    //         new () = { inherit T() }
    //     end

module Tests =
    open Xunit

    [<Fact>]
    let ``prefix should match`` () = Assert.Equal("sys", Model.T.Prefix)
    [<Fact>]
    let ``organization should match`` () = Assert.Equal("Example Inc.", Model.T.Organization)
    [<Fact>]
    let ``contact should match`` () = Assert.Equal("joe@example.com", Model.T.Contact)
    [<Fact>]
    let ``description should match`` () = Assert.Equal("The module for entities implementing the Example system.", Model.T.Description)

// printfn "Version        : %A" T.YangVersion
// printfn "Namespace      : %A" T.Namespace
#if CONSOLE
printfn "Prefix         : %s" Model.T.Prefix
printfn "Organization   : %s" Model.T.Organization
printfn "Contact        : %s" Model.T.Contact
printfn "Description    : %s" Model.T.Description
#endif
