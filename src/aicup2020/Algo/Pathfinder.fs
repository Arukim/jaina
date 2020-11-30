namespace Jaina.Algo

open Aicup2020.Model
open Jaina.Collections

module Pathfinder =
    
    let dist a b =
        abs(a.X - b.X) + abs(a.Y - b.Y)

    let outerBorders mapSize size pos =
        seq {
            for i in 0..size-1 do
                // left border
                if pos.X > 0 then yield {X = pos.X - 1; Y = pos.Y + i}
                // right border
                if pos.X < mapSize - size - 1 then yield {X = pos.X + size; Y = pos.Y + i}
                // bottom border
                if pos.Y > 0 then yield {X = pos.X + i; Y = pos.Y - 1}
                // top border
                if pos.Y < mapSize - 1 - size then yield {X = pos.X + i; Y = pos.Y + size}
        }

    let neighboursOf mapSize curr = 
        seq {
            if curr.X > 0 then yield {X = curr.X - 1; Y = curr.Y}
            if curr.X < mapSize-1 then yield {X = curr.X + 1; Y = curr.Y}
            if curr.Y > 0 then yield {X = curr.X; Y = curr.Y - 1}
            if curr.Y < mapSize-1 then yield {X = curr.X; Y = curr.Y + 1}
        }

    let aStarField mapSize starts calcWeight =
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
                neighboursOf mapSize curr 
                    |> Seq.iter (fun next -> 
                        let newCost = currCost + calcWeight (curr,next)
                        if not(cameFrom.ContainsKey next) || newCost < costSoFar.[next] then
                            costSoFar <- costSoFar.Add(next, newCost)
                            frontier <- PriorityQ.push newCost next frontier
                            cameFrom <- cameFrom.Add(next, Some(curr))
                    )
        costSoFar
