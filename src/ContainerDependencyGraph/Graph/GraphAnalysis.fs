namespace GraphAnalysis

open FSharpPlus

module Connections =
    type 'a Chain =
        { source: 'a
          targets: 'a Chain seq }

    let link source targets =
        { source = source
          targets = targets }

    type 'a Link =
        { source: 'a
          target: 'a }

    let connection source target =
        { source = source
          target = target }

    let reverse (connection: 'a Link) =
        { source = connection.target
          target = connection.source }

    let rec traverse (link: 'a Chain): 'a Link seq =
        seq {
            yield! link.targets |>> (fun x -> connection link.source x.source)
            yield! link.targets >>= traverse
        }

module GraphQuery =
    open Connections

    let roots children_selector all =
        all
        |> List.filter (fun x ->
            all
            >>= children_selector
            |> exists ((=) x)
            |> not)

    let leaves children_selector all = all |> Seq.filter (children_selector >> Seq.isEmpty)

    let descendants_hierarchy children_selector node =
        let rec children_recursively node' =
            node'
            |> children_selector
            |> Seq.map children_recursively
            |> Connections.link node'
        children_recursively node


    let descendants children_selector node =
        node
        |> descendants_hierarchy children_selector
        |> traverse



    let ancestors_hierarchy children_selector all search =
        let rec search_all results (source: _ Chain) =
            match source.source
                  |> children_selector
                  |>> (fun x -> link x [source])
                  |> toList with
            | [] -> results
            | xs -> xs >>= search_all ((xs |> filter (fun x -> x.source |> search)) @ results)
        all
        |> roots children_selector
        |>> (fun x ->
        { targets = []
          source = x })
        >>= (search_all [])

    let ancestors children_selector all search =
        search
        |> ancestors_hierarchy children_selector all
        >>= traverse
        |>> reverse
