namespace Yang.Model.Tests

module ArgumentsTests =
    open Xunit
    open Yang.Model.Arguments

    [<Fact>]
    let ``parse key that spans multiple lines`` () =
        let input = "source-port destination-port
       source-address destination-address"
        let (Key keys) = KeyFromString input
        Assert.Equal(4, keys.Length)
        Assert.Equal("source-port",         (List.item 0 keys).Value)
        Assert.Equal("destination-port",    (List.item 1 keys).Value)
        Assert.Equal("source-address",      (List.item 2 keys).Value)
        Assert.Equal("destination-address",      (List.item 3 keys).Value)
