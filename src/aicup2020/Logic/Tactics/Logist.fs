namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Core

type Logist(playerView: PlayerView, recruits) =
    inherit Tactics(playerView)

    override this.Execute entities =

        let createMoveTo = fun entity ->
        
            let attackAction = Some({
                Target = None 
                AutoAttack = Some({
                    PathfindRange = (this.EntityProps entity).SightRange
                    ValidTargets = match entity.EntityType with | _ -> [||]})
            })
            let moveAction = Some({
                Target = Config.RallyPoint
                FindClosestPosition = true
                BreakThrough = true
            })
            (entity.Id, {
                MoveAction = moveAction
                BuildAction = None
                AttackAction = attackAction
                RepairAction = None
            })

        let actions = entities 
                        |> List.filter(fun e -> recruits |> List.contains e)
                        |> List.map createMoveTo
                        
        let others = this.FilterInactive entities actions
        (others, actions)