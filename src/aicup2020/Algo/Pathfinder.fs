namespace Jaina.Algo

open Aicup2020.Model
open Jaina.Collections

module Pathfinder =
    
    let neighboursOf field curr = 
        seq {
            if curr.X > 0 then yield {X = curr.X - 1; Y = curr.Y}
            if curr.X < field.X then yield {X = curr.X + 1; Y = curr.Y}
            if curr.Y > 0 then yield {X = curr.X; Y = curr.Y - 1}
            if curr.Y < field.Y then yield {X = curr.X; Y = curr.Y + 1}
        }

    let aStarField field start calcWeight =
        let mutable frontier = [|(0u, start)|] |> PriorityQ.fromSeq
        let mutable costSoFar = [|(start, 0u)|] |> Map.ofSeq
        let mutable cameFrom = [|(start, None)|] |> Map.ofSeq

        while not (PriorityQ.isEmpty frontier) do
            let ((_, curr), queue) = (PriorityQ.popMin frontier).Value
            frontier <- queue

            neighboursOf field curr 
                |> Seq.iter (fun next -> 
                    let newCost = costSoFar.[curr] + calcWeight (curr,next)
                    if not(cameFrom.ContainsKey next) || newCost < costSoFar.[next] then
                        costSoFar <- costSoFar.Add(next, newCost)
                        frontier <- PriorityQ.push newCost next frontier
                        cameFrom <- cameFrom.Add(next, Some(curr))
                )
        costSoFar
