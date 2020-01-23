namespace WindsorAnalysis

open Castle.Core.Internal
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
        |> choice
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



module Dot =
    let node color id = sprintf "\"%s\" [color=\"%s\"];" id color

    let edge color source destination = sprintf "\"%s\" -> \"%s\" [color=\"%s\"];" source destination color

module ContainerGraphComposition =
    open GraphAnalysis.GraphQuery
    open WindsorDependency

    // TODO types to prune
    let nodesWithDescendants all givenType =
        let childrenSelector node = componentDependencies all node |> toList
        let nodes = typeToComponents all givenType
        nodes >>= descendants childrenSelector

    // TODO types to prune
    let nodesWithAncestors all givenType =
        let childrenSelector node = componentDependencies all node |> toList
        let nodes = typeToComponents all givenType |> toList
        nodes >>= fun x -> ancestors childrenSelector all ((=) x)


    let nodeName transformator node =
        node
        |> typesExposedByComponent
        |>> transformator
        |> String.concat "|"

    let core types allComponents colorizer nameTransformer =
        types
        >>= fun x ->
            seq {
                yield! nodesWithDescendants allComponents x
                yield! nodesWithAncestors allComponents x
            }
            >>= fun x ->
                seq {
                    yield x.source
                          |> nodeName nameTransformer
                          |> Dot.node (colorizer x.source)
                    yield x.target
                          |> nodeName nameTransformer
                          |> Dot.node (colorizer x.target)
                    yield Dot.edge (colorizer x.target) (x.source |> nodeName nameTransformer)
                              (x.target |> nodeName nameTransformer)
                }
