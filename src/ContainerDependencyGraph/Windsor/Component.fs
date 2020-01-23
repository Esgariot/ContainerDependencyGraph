namespace WindsorAnalysis

open System.Linq
open Castle.Core
open Castle.Core.Internal


module Component =
    let dependents (node: ComponentModel) =
        node.Dependents |> seq

    let dependencies (node: ComponentModel) =
        node.Dependencies |> seq

    let componentModelNodes (graphNodes: GraphNode seq) =
        graphNodes |> Enumerable.OfType<ComponentModel> |> seq

    let implementation (node: ComponentModel) =
        node.Implementation

    let services (node: ComponentModel) =
        node.Services |> seq
