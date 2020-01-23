namespace Utility

open System.Collections.Generic
open FSharpPlus

module Seq =
    let intersection (left: 'a seq) (right: 'a seq) =
        let cache = HashSet<'a>(right, HashIdentity.Structural)
        left |> filter cache.Contains

    let difference (left: 'a seq) (right: 'a seq) =
        let cache = HashSet<'a>(right, HashIdentity.Structural)
        left |> filter (cache.Contains >> not)
