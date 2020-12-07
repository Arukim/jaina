namespace Jaina.Logic

open Aicup2020.Model
open Jaina.Algo
open Jaina.Core

[<AbstractClass>]
type HeatAttack() =
    static let mutable attackMap = Map.empty
    
    static member private buildWalkMap playerView =
        let entities = playerView.Entities
        let matchSomeBuilding = fun x -> match x.EntityType with
                                            | EntityType.BuilderBase
                                            | EntityType.MeleeBase
                                            | EntityType.Wall
                                            | EntityType.RangedBase
                                            | EntityType.House
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

        let ownUnits = View.ownEntities playerView 
                                    |> Seq.choose(fun x ->
                                match x.EntityType with
                                    | EntityType.BuilderUnit -> Some(x.Position, Config.BuilderUnit_Tile_Walk_Price)
                                    | EntityType.MeleeUnit
                                    | EntityType.RangedUnit -> Some(x.Position, Config.CombatUnit_Tile_Walk_Price)
                                    | _ -> None)
                                    
        let ownBuildings = entities |> Seq.filter(fun x -> x.PlayerId = Some(playerView.MyId))
                                    |> Seq.choose matchSomeBuilding
                                    |> Seq.map getObjectTiles
                                    |> Seq.collect(fun x -> x)
        
        resources |> Seq.append ownUnits 
                  |> Seq.append ownBuildings 
                  |> Map.ofSeq

    static member Update (playerView: PlayerView) =
        let walkMap = playerView |> HeatAttack.buildWalkMap

        let enemySelector = fun x -> match x.EntityType with
                                     | EntityType.RangedBase -> Some((x.Position, 0u))
                                     | EntityType.BuilderBase -> Some((x.Position, 10u))
                                     | EntityType.MeleeBase -> Some((x.Position, 5u))
                                     | EntityType.House -> Some((x.Position, 0u))
                                     | EntityType.BuilderUnit -> Some((x.Position, 200u))
                                     | EntityType.Turret -> Some((x.Position, 250u))
                                     | _ -> None

        let enemyTargets = playerView.Entities |> Seq.filter(fun x -> x.PlayerId <> Some(playerView.MyId))
                                               |> Seq.choose enemySelector
        
        let targets = match enemyTargets |> Seq.exists(fun _ -> true) with
                        | true -> enemyTargets
                        | _ -> Config.Global_Attack_Targets |> Seq.map(fun x -> (x, 0u))
        
        let fieldWeightFunc (_, next) = match walkMap.TryFind next with
                                                | Some q -> q
                                                | _ -> 1u
        attackMap <- Pathfinder.aStarField playerView.MapSize targets fieldWeightFunc

    static member AttackMap with get() = attackMap
