namespace Jaina.Algo

open Aicup2020.Model
open Jaina.Collections

module Pathfinder =
    
    let neighboursOf field curr = 
        seq {
            if curr.X > 0 then yield {X = curr.X - 1; Y = curr.Y}
            if curr.X < field.X-1 then yield {X = curr.X + 1; Y = curr.Y}
            if curr.Y > 0 then yield {X = curr.X; Y = curr.Y - 1}
            if curr.Y < field.Y-1 then yield {X = curr.X; Y = curr.Y + 1}
        }

    let aStarField field starts calcWeight =
        let mutable frontier = starts |> Seq.map(fun (pos, weight) -> (weight, pos)) 
                                      |> PriorityQ.fromSeq
        let mutable costSoFar = starts |> Seq.map(fun (pos, weight) -> (pos, weight)) 
                                       |> Map.ofSeq
        let mutable cameFrom = starts |> Seq.map(fun (pos, _) -> (pos, None)) 
                                      |> Map.ofSeq

        while not (PriorityQ.isEmpty frontier) do
            let ((queuedCost, curr), queue) = (PriorityQ.popMin frontier).Value
            frontier <- queue
            let currCost = costSoFar.[curr]
            
            if currCost = queuedCost then
                neighboursOf field curr 
                    |> Seq.iter (fun next -> 
                        let newCost = currCost + calcWeight (curr,next)
                        if not(cameFrom.ContainsKey next) || newCost < costSoFar.[next] then
                            costSoFar <- costSoFar.Add(next, newCost)
                            frontier <- PriorityQ.push newCost next frontier
                            cameFrom <- cameFrom.Add(next, Some(curr))
                    )
        costSoFar
