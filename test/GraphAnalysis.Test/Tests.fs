module Tests

open System
open Xunit

type 'a NodeStub = {
    value: 'a
    nodes: 'a NodeStub seq
}



let ``generate tree with one node`` =
    {value=0;nodes=[]}


let ``tree with root and leaf`` =
    {value=0;nodes=[{value=0;nodes=[]}]}

[<Fact>]
let ``My test`` () =
    // Arrange
    let tree = ``generate tree with one node``


    // Act
    let act = GraphAnalysis.GraphQuery.roots (fun (x:int NodeStub)->x.nodes |> Seq.toList)

    // Assert
    Assert.True(act [tree] = [tree])

[<Fact>]
let ``test 2``() =
    // Act
    let act = GraphAnalysis.GraphQuery.roots (fun (x:int NodeStub)->x.nodes |> Seq.toList)

    // Assert
    Assert.True(act [] = [])

[<Fact>]
let ``test 3``() =
    // Act
    let act = GraphAnalysis.GraphQuery.roots (fun (x:int NodeStub)->x.nodes |> Seq.toList)

    // Assert
    Assert.True(act [] = [])
