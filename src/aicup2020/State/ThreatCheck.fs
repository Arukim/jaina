namespace Jaina.State 

open Jaina.Core
open Jaina.Algo
open Aicup2020.Model

module ThreatCheck = 
    let buildInfluenceThreat (ownTerritoryField: Map<Vec2Int, int>) (foeUnitsField: Map<Vec2Int, int>) =
        let size = Config.PotentialFieldSize
        let influenceMap = 0 |> Seq.unfold(fun num ->
                            let pos = Cells.toVec size num
                            let item = ownTerritoryField.TryFind pos
                            match item with
                                | Some x -> Some(x, num + 1)
                                | _ -> None)
                         |> Array.ofSeq

        for _ in 1..Config.ThreatCheckInfluenceSpread do
            let newInfluence : int array = Array.zeroCreate influenceMap.Length
            for i in 0..influenceMap.Length-1 do
                let influence = influenceMap.[i]
                Array2d.neighboursOf size i 
                    |> Seq.iter(fun n -> 
                        if influence > influenceMap.[n] then
                            newInfluence.[n] <- max newInfluence.[n] influence / Config.ThreatCheckInfluenceDiminutionRate)
            newInfluence |> Array.iteri(fun idx value -> influenceMap.[idx] <- influenceMap.[idx] + value)

        influenceMap 
            |> Array.mapi(fun x value ->
                let pos = Cells.toVec size x
                (pos, value))
            |> Array.filter(fun (_, value) -> value > 0)
            |> Array.map(fun (pos, value) -> 
                let threat = foeUnitsField.[pos]
                (pos, value, threat))
            |> List.ofSeq

