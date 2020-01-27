namespace WindsorAnalysis

open Castle.Core
open Castle.Core.Internal
open ContainerDependencyGraph.Graph
open ContainerDependencyGraph.Utility
open FSharpPlus
open Utility
open System.Collections.Generic

module WindsorDependency =
    let componentsImplementingType all givenType = all |> filter (fun x -> (Component.implementation x) = givenType)

    let componentsServingType all givenType =
        all |> filter (fun x -> (Component.services x) |> exists ((=) givenType))

    let typeToComponents all givenType =
        seq {
            yield! componentsImplementingType all givenType
            yield! componentsServingType all givenType
        }
        |> distinct
        |> filter (fun x -> Component.implementation x <> typeof<LateBoundComponent>)

    let sequenceDependencies all dependencyModel =
        dependencyModel
        |> Dependency.targetType
        |> filter (TypeAnalysis.isAssignableToGenericType typedefof<IEnumerable<_>>)
        |>> (fun x -> x.GetGenericArguments() |> seq)
        |> function
        | Some x -> x
        | None -> Seq.empty
        >>= typeToComponents all

    let componentDependencies all node =
        seq {
            yield! node |> (Component.dependents >> Component.componentModelNodes)
            yield! node
                   |> Component.dependencies
                   >>= sequenceDependencies all
        }
        |> distinct

    let typesExposedByComponent node =
        seq {
            yield Component.implementation node
            yield! Component.services node
        }
        |> distinct
        |> filter ((<>) typeof<LateBoundComponent>)

module ContainerGraphComposition =
    open GraphAnalysis.GraphQuery
    open WindsorDependency

    let nodesWithDescendants all childrenSelector givenType =
        let nodes = typeToComponents all givenType
        nodes >>= descendants childrenSelector

    let nodesWithAncestors all childrenSelector givenType =
        let nodes = typeToComponents all givenType |> toList
        nodes >>= fun x -> ancestors childrenSelector all ((=) x)


    let nodeName transformator node =
        node
        |> typesExposedByComponent
        |>> transformator
        |> String.concat " | "

    let core types allComponents childrenSelector colorizer nameTransformer =
        types
        >>= fun x ->
            seq {
                yield! nodesWithDescendants allComponents childrenSelector x
                yield! nodesWithAncestors allComponents childrenSelector x
            }
            >>= fun x ->
                seq {
                    yield x.source
                          |> nodeName nameTransformer
                          |> Dot.node (colorizer x.source.LifestyleType)
                    yield x.target
                          |> nodeName nameTransformer
                          |> Dot.node (colorizer x.target.LifestyleType)
                    yield Dot.edge (colorizer x.target.LifestyleType) (x.target |> nodeName nameTransformer)
                              (x.source |> nodeName nameTransformer)
                }

    let legend colorizer = Enum.Enumerate<LifestyleType> |>> (fun x -> Dot.node (colorizer x) (string x))

    let graph types prune allComponents colorizer typeName =
        let childrenSelector node =
            componentDependencies allComponents node
            |> filter (fun x ->
                prune
                |> Seq.exists ((=) x.Implementation)
                |> not)
            |> toList
        seq {
            yield "digraph {"
            yield! legend colorizer
            yield "rankdir=RL"
            yield "splines=ortho"
            yield! core types allComponents childrenSelector colorizer typeName
            yield "}"
        }
        |> String.concat "\n"
