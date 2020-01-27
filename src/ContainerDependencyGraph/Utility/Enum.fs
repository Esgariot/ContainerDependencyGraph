namespace ContainerDependencyGraph.Utility

open System.Linq
open System
module Enum =
    let Enumerate<'t> =
        Enum.GetValues(typeof<'t>).Cast<'t>() |> seq

