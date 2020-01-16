namespace GraphAnalysis

open FSharpPlus

module Connections =
    type 'a Parents =
        { parents: 'a Parents seq
          node: 'a }

    type 'a Children =
        { node: 'a
          children: 'a Children seq }

module TreeQuery =
    open Connections

    let roots children_selector all =
        all
        |> List.filter (fun x ->
            all
            >>= children_selector
            |> exists ((=) x)
            |> not)

    let leaves children_selector all = all |> Seq.filter (children_selector >> Seq.isEmpty)

    let ancestors children_selector all node =
        let rec searchUntilNode searchedSoFar source =
            match source
                  |> children_selector
                  |> toList with
            | [] -> []
            | xs when xs |> exists ((=) node) -> searchedSoFar
            | xs -> xs >>= searchUntilNode (source :: searchedSoFar)
        all
        |> roots children_selector
        >>= (searchUntilNode [])


    let descendants children_selector node =
        let rec childrenRecursively node' =
            match node' |> children_selector with
            | [] -> []
            | xs -> node' :: (xs >>= childrenRecursively)
        node
        |> childrenRecursively
        |> distinct


    let ancestors_hierarchy children_selector all search: 'a -> bool =
        let rec search_all results source =
            match source.node
                  |> children_selector
                  |>> (fun x ->
                  { parents = [ source ]
                    node = x })
                  |> toList with
            | [] -> results
            | xs -> xs >>= search_all ((xs |> filter (fun x -> x.node |> search)) @ results)
        all
        |> roots children_selector
        >>= (fun x->{parents=[];node=x})
        >>= (search_all [])
