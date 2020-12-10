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

    let pathMapEx (moveCost: uint[,]) range start startSize =
        let baseSize = moveCost.GetLength(0)
        let patch = InfluencePatch<uint>.Create baseSize start startSize range 0u
        let resMap = patch.Map

        let frontier = new Queue<uint*Vec2Int>()
        for x in 0..startSize - 1 do
            for y in 0..startSize - 1 do
                let p = {X=start.X + x; Y=start.Y + y}
                frontier.Enqueue(1u, p)
                let trStart = patch.ToPatchCoord p
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
    
    let pathMap (moveCost: uint[,]) range start = 
        pathMapEx moveCost range start 1

    let createPatch baseSize start startSize =
        let patch = InfluencePatch<uint>.Create baseSize start startSize 0 0u
        let resMap = patch.Map
        for x in 0..startSize - 1 do
            for y in 0..startSize - 1 do
                  let point = {X=start.X + x; Y=start.Y + y}
                  let patchPoint = patch.ToPatchCoord point          
                  resMap.[patchPoint.X, patchPoint.Y] <- 1u
        patch

    let extendPathMap range (pathMap: InfluencePatch<uint>) =
        let patch = pathMap.GetExpanded range 0u
        let mapSize = {X = patch.Map.GetLength(0); Y = patch.Map.GetLength(1)}

        let threat = uint pathMap.Range + 1u
        let limit = threat + uint range
        
        let frontier = new Queue<Vec2Int*uint>()
        pathMap.Map |> Array2D.iteri(fun x y v ->
                            match v with
                                | threat -> frontier.Enqueue (patch.ToPatchCoord(pathMap.ToGlobalCoord {X=x; Y=y}), threat)
                                | _ -> ())

        while frontier.Any() do
            let (pos, t) = frontier.Dequeue()
            if t < limit then
                pos |> Cells.neighboursOf2 mapSize
                    |> Seq.iter(fun n ->
                        if patch.Map.[n.X, n.Y] = 0u then
                            patch.Map.[n.X, n.Y] <- t + 1u
                            frontier.Enqueue (n, t + 1u))
        patch