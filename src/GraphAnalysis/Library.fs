namespace GraphAnalysis

open FSharpPlus

/// Works on directed acyclic tree
module GraphQueries =
    let roots children_selector all =
        all |> List.filter (fun x -> all >>= children_selector |> exists ((=) x) |> not)

    let descendants children_selector node =
        let rec childrenRecursively node' =
            match node' |> children_selector with
            | [] -> []
            | xs -> node' :: (xs >>= childrenRecursively)
        node |> childrenRecursively |> distinct


    let ancestors children_selector all node =
        let rec searchUntilNode searchedSoFar source =
            match source|> children_selector |> toList with
            | [] -> []
            | xs when xs |> exists ((=) node) -> searchedSoFar
            | xs -> xs >>= searchUntilNode (source :: searchedSoFar)
        all |> roots children_selector |> toList >>= (searchUntilNode [])
