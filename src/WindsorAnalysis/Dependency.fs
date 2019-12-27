namespace WindsorAnalysis

open Castle.Core

module Dependency =
    let targetType (dependency: DependencyModel) =
        match dependency.TargetType with
        | null -> None
        | x -> Some x
