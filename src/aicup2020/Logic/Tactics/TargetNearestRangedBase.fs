﻿namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Algo
open Jaina.Core
open System

type TargetNearestRangedBase() =
    member _.buildWalkMap (playerView: PlayerView) =
        let entities = playerView.Entities
        let matchSomeBuilding = fun x -> match x.EntityType with
                                            | EntityType.BuilderBase
                                            | EntityType.MeleeBase
                                            | EntityType.RangedBase
                                            | EntityType.Turret -> Some(x)
                                            | _ -> None
        
        let getObjectTiles = fun x -> 
            seq {
                let size = playerView.EntityProperties.[x.EntityType].Size - 1
                for r in 0..size do
                    for c in 0..size do
                        yield ({X= x.Position.X + r; Y=x.Position.Y + c}, 10000u)
            }


        let resources = entities |> Seq.choose(fun x -> 
                                match x.EntityType with 
                                    | EntityType.Resource -> Some(x.Position, Config.Resource_Tile_Walk_Price)
                                    | _ -> None)
        let ownBuildings = entities |> Seq.filter(fun x -> x.PlayerId = Some(playerView.MyId))
                                    |> Seq.choose matchSomeBuilding
                                    |> Seq.map getObjectTiles
                                    |> Seq.collect(fun x -> x)
        
        resources |> Seq.append ownBuildings |> Map.ofSeq

    member this.Run (playerView: PlayerView) =
        let field = {X = playerView.MapSize; Y = playerView.MapSize}

        let walkMap = this.buildWalkMap playerView

        let enemySelector = fun x -> match x.EntityType with
                                     | EntityType.RangedBase -> Some((x.Position, 0u))
                                     | EntityType.BuilderBase -> Some((x.Position, 10u))
                                     | EntityType.MeleeBase -> Some((x.Position, 5u))
                                     | EntityType.Turret -> Some((x.Position, 250u))
                                     | _ -> None

        let targets = playerView.Entities |> Seq.filter(fun x -> x.PlayerId <> Some(playerView.MyId))
                                          |> Seq.choose enemySelector
        match Seq.isEmpty targets with
                   | _ -> Pathfinder.aStarField field targets (fun (_, next) ->
                                            match walkMap.TryFind next with
                                                | Some q -> q
                                                | _ -> 1u)
                   | _ -> Map.empty      
                            

