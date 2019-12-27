namespace WindsorAnalysis

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

    let sequenceDependencies all dependencyModel =
        dependencyModel
        |> Dependency.targetType
        |> filter (TypeAnalysis.isAssignableToGenericType typedefof<IEnumerable<_>>)
        |>> (fun x -> x.GetGenericTypeDefinition())
        |>> typeToComponents all
        |> choice

    let componentDependencies all node =
        seq {
            yield! node |> (Component.dependents >> Component.componentModelNodes)
            yield! node |> Component.dependencies >>=sequenceDependencies all
        } |> distinct


//        |> function
//            | Some (_,false) | None -> zero
//            | Some (x',true) -> x'.GetGenericArguments()
//            >>= WindsorServicing.componentsTiedToType all

//    let getSequenceDependents all node =
//
//        specialDependents node
//            |> filter (isAssignableToGenericType (typedefof<_ IEnumerable>))
//            >>= genericParameterTypes |> toList
//            >>= (componentsServingType all)
//
//    let componentDependents all node =
//        seq {
//        yield! node |> (Component.dependents >> Component.componentModelNodes)
//        yield! node |> getSequenceDependents all
//        } |> distinct
//
//
//
//module WindsorQuery =
//    let rootComponents =
