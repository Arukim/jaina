namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Collections

type Fabric(playerView: PlayerView, unitType: EntityType, count) =
    inherit Tactics(playerView)

    override this.Execute entities = 
        let buildingType = enum(LanguagePrimitives.EnumToValue unitType - 1)
        let fabrics = entities |> Seq.filter(fun x -> x.EntityType = buildingType)

        let getBuildPos entity = {
            X = entity.Position.X + (this.EntityProps entity).Size
            Y = entity.Position.Y + (this.EntityProps entity).Size - 1
        }

        let createBuilder = fun fabric -> 
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
                              |> Seq.map createBuilder
                              |> List.ofSeq
                             
        let others = this.FilterInactive entities actions
        
        (others, actions)

