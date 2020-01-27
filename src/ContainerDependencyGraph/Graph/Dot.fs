namespace ContainerDependencyGraph.Graph

open System.Drawing

module Dot =
    let node (color: Color) id = sprintf "\"%s\" [color=\"%s\"];" id color.Name

    let edge (color: Color) source destination =
        sprintf "\"%s\" -> \"%s\" [color=\"%s\"];" source destination color.Name
