namespace Jaina.Algo

open Jaina.Collections
open Aicup2020.Model
open System.Collections.Generic
open System.Linq

module Pathfinder =
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
                Cells.neighboursOf mapSize curr 
                    |> Seq.iter (fun next -> 
                        let newCost = currCost + calcWeight (curr,next)
                        if not(cameFrom.ContainsKey next) || newCost < costSoFar.[next] then
                            costSoFar <- costSoFar.Add(next, newCost)
                            frontier <- PriorityQ.push newCost next frontier
                            cameFrom <- cameFrom.Add(next, Some(curr))
                    )
        costSoFar


    let pathMap (moveCost: uint[,]) range start  =
        let baseSize = moveCost.GetLength(0)
        let patch = InfluencePatch<uint>.Create baseSize start range 0u
        let resMap = patch.Map

        let frontier = new Queue<uint*Vec2Int>()
        frontier.Enqueue(1u, start)
        let trStart = patch.ToPatchCoord start
        resMap.[trStart.X, trStart.Y] <- 1u

        while frontier.Any() do
            let queuedCost, curr = frontier.Dequeue()

            curr |> Cells.neighboursOf baseSize
                 |> Seq.iter (fun next ->
                    let trNext = patch.ToPatchCoord next
                    let newCost = queuedCost + moveCost.[next.X, next.Y]
                    if newCost <= uint (range + 1) &&
                       resMap.[trNext.X, trNext.Y] = 0u then
                            resMap.[trNext.X, trNext.Y] <- newCost
                            frontier.Enqueue(newCost, next))
        patch

    let extendPathMap range (pathMap: InfluencePatch<uint>) =
        let patch = pathMap.GetExpanded range 0u
        let mapSize = {X = patch.Map.GetLength(0); Y = patch.Map.GetLength(1)}

        let threat = uint pathMap.Range
        
        let frontier = new Queue<Vec2Int>()
        pathMap.Map |> Array2D.iteri(fun x y v ->
                            match v with
                                | threat -> frontier.Enqueue {X=x; Y=y}
                                | _ -> ())

        while frontier.Any() do
            frontier.Dequeue() |> Cells.neighboursOf2 mapSize
                               |> Seq.iter(fun n ->
                                if patch.Map.[n.X, n.Y] = 0u then
                                    patch.Map.[n.X, n.Y] <- threat + 1u
                                    frontier.Enqueue n)
        patch