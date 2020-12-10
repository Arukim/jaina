namespace Jaina.Algo

open Aicup2020.Model
open Jaina.Debug

type InfluenceMap(size) =    
    let map = Array2D.create size size 0.0

    member this.Map with get() = map
    member this.MaxPos with get() = fst (map |> Array2D.mapi(fun x y v -> ({X=x; Y=y},v))
                                             |> Array2D.toSeq
                                             |> Seq.maxBy snd)

    member this.AddLayer (layer:InfluencePatch<float>) =
        let mergeLayer x y v =
            let pos = layer.ToGlobalCoord {X=x;Y=y}
            map.[pos.X, pos.Y] <- map.[pos.X, pos.Y] + v
        layer.Map |> Array2D.iteri mergeLayer
        ()

    member this.SubLayer (layer:InfluencePatch<float>) =
        let mergeLayer x y v =
            let pos = layer.ToGlobalCoord {X=x;Y=y}
            map.[pos.X, pos.Y] <- map.[pos.X, pos.Y] - v
        layer.Map |> Array2D.iteri mergeLayer
        ()

    member this.Mult (other: InfluenceMap) =
        for x in 0..size-1 do
            for y in 0..size-1 do
                map.[x,y] <- other.Map.[x,y] * map.[x,y]

    member this.Normalize() =
        let max = map |> Array2D.toSeq
                      |> Seq.map abs
                      |> Seq.max
        
        if max <> 0.0 then
            map |> Array2D.iteri(fun x y v -> map.[x,y] <- v/max)
        ()

    member this.Render (color,baseA) (debug: IDebugger) =
        let renderTile x y v = 
            let pos = {X=x;Y=y}
            match v with
                | 0.0 -> ()
                | _ -> debug.FillCellA color (single v * baseA) pos
        
        map |> Array2D.iteri renderTile
        ()

    member this.Render2 (colorA,alphaA) (colorB, alphaB) (debug: IDebugger) =
        let renderTile x y v = 
            let pos = {X=x;Y=y}
            match v with
                | f when f < 0.05 && f > 0.05 -> ()
                | f when f >= 0.05 -> debug.FillCellA colorA (single v * alphaA) pos
                | _ -> debug.FillCellA colorB (single v * -alphaB) pos

        
        map |> Array2D.iteri renderTile
        ()


