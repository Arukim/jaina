namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Collections
open Jaina.Algo
open Jaina.Core
open Jaina.State

type Fabric(playerView: PlayerView, turnState: TurnState, unitType: EntityType, count) =
    inherit Tactics(playerView)

    override this.Execute entities = 
        let buildingType = enum(LanguagePrimitives.EnumToValue unitType - 1)
        let fabrics = entities |> Seq.filter(fun x -> x.EntityType = buildingType)
        let mapSize = playerView.MapSize

        let getDefaultPos entity = {
            X = entity.Position.X + (this.EntityProps entity).Size
            Y = entity.Position.Y + (this.EntityProps entity).Size - 1
        }

        let getBuildPos entity = 
            match entity.EntityType with
                | EntityType.BuilderBase -> 
                    let bestNeighbour = Cells.outerBorders (mapSize / 5) 1 {X = entity.Position.X / 5; Y = entity.Position.Y / 5}
                                            |> Seq.choose(fun x -> match turnState.ResourcesField.TryFind(x) with
                                                                        | Some v -> Some(x,v)
                                                                        | _ -> None)
                                            |> Seq.sortByDescending(fun (_, v) -> v)
                                            |> Seq.tryHead
                    match bestNeighbour with
                        | Some (x,_) ->
                            let pos = {X=x.X * 5 + 5/2; Y=x.Y*5 + 5/2};
                            this.SelectBuilderPos pos entity.Position entity.EntityType
                        | _ -> getDefaultPos entity
                | _  -> getDefaultPos entity

        let createUnit = fun fabric -> 
            let buildAction = Some({
                EntityType = enum(LanguagePrimitives.EnumToValue fabric.EntityType + 1)
                Position = getBuildPos fabric                              
            })
            (fabric.Id, {
                MoveAction = None
                BuildAction = buildAction
                AttackAction = None
                RepairAction = None
            })

        let actions = fabrics |> Seq.truncate count
                              |> Seq.map createUnit
                              |> List.ofSeq
                             
        let others = this.FilterInactive entities actions
        
        (others, actions)

