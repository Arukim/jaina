namespace Jaina.Logic.Tactics

open Aicup2020.Model

type Idler(playerView: PlayerView) =
    inherit Tactics(playerView)

    override this.Execute entities =

        let createIdler = fun entity ->
        
            let attackAction = Some({
                Target = None 
                AutoAttack = Some({
                    PathfindRange = (this.EntityProps entity).SightRange
                    ValidTargets = match entity.EntityType with
                                            | EntityType.BuilderUnit -> [|EntityType.Resource|]
                                            | _ -> [||]})
            })

            (entity.Id, {
                MoveAction = None
                BuildAction = None
                AttackAction = attackAction
                RepairAction = None
            })

        let idlers = entities |> List.map createIdler
        ([], idlers)
    