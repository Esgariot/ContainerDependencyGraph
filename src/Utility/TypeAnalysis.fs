namespace Utility

module TypeAnalysis =
    open System
    open System.Collections
    open System.Linq
    open FSharpPlus
    open System.Reflection

    let isEnumerableType (givenType: Type) =
        givenType.IsAssignableFrom(typeof<IEnumerable>)

    let rec isAssignableToGenericType (genericType: Type) (givenType: Type) =
        let isMappableTo (generic: Type) (given: Type) =
            given.IsGenericType && given.GetGenericTypeDefinition() = generic
        let isAssignable (generic: Type) (given: Type) =
            given.GetInterfaces().Append(given) |>> (isMappableTo generic) |> exists id
        let rec isThisOrBaseAssignable (generic: Type) (given: Type) =
            match generic, given with
            | null, null -> true
            | _, null
            | null, _ -> false
            | _ -> (isAssignable generic given) || (isThisOrBaseAssignable generic given.BaseType)
        isThisOrBaseAssignable genericType givenType

    let genericParameterTypes (givenType: Type) =
        givenType.GetTypeInfo().GenericTypeArguments |> seq
