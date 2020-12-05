namespace Jaina.Algo

open Aicup2020.Model

module PotentialField =
    
    let create(size, tileSize, translate: Vec2Int -> int) = 
        seq {0 .. size*size - 1} 
            |> Seq.map(fun x ->  {X = x % size; Y = x / size})
            |> Seq.groupBy(fun pos -> {X = pos.X / tileSize; Y = pos.Y / tileSize})
            |> Seq.map(fun (pos, tiles) -> (pos, tiles |> Seq.sumBy translate))
            |> Map.ofSeq
