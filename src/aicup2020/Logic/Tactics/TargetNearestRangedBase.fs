namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Algo
open Jaina.Core

type TargetNearestRangedBase() =
    member _.buildWalkMap entities =
        entities |> Seq.map(fun x -> 
                            seq {
                                match x.EntityType with 
                                    | EntityType.Resource -> yield (x.Position, Config.Resource_Tile_Walk_Price)
                                    | _ -> ()
                            })
                 |> Seq.collect(fun x -> x)
                 |> Map.ofSeq

    member this.Run (playerView: PlayerView) =
        let field = {X = playerView.MapSize; Y = playerView.MapSize}

        let walkMap = this.buildWalkMap playerView.Entities

        let targetBase = playerView.Entities |> Seq.filter(fun x -> x.PlayerId <> Some(playerView.MyId))
                                             |> Seq.filter(fun x -> x.EntityType = EntityType.RangedBase)
                                             |> Seq.sortBy(fun x -> x.Position.X + x.Position.Y)
                                             |> Seq.tryHead
        match targetBase with
                   | Some x -> Pathfinder.aStarField field x.Position (fun (_, next) ->
                                            match walkMap.TryFind next with
                                                | Some q -> q
                                                | _ -> 1u)
                   | _ -> Map.empty      
                            

