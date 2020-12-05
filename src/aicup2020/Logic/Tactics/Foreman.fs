namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Collections
open Jaina.Algo

type Foreman(playerView: PlayerView, entityType: EntityType, position: Vec2Int) =
    inherit Tactics(playerView)

    override this.Execute entities = 
        let workers = entities |> Seq.filter (fun x -> x.EntityType = EntityType.BuilderUnit)

        let createAction worker =
            let builderPos = this.SelectBuilderPos worker position entityType

            let moveAction = Some({
                Target = builderPos
                FindClosestPosition = true
                BreakThrough = false
            })
            let buildAction = Some({
                Position = position
                EntityType = entityType
            })

            (worker.Id, {
                MoveAction = moveAction
                BuildAction = buildAction
                AttackAction = None
                RepairAction = None
            })    


        let worker = workers |> Seq.sortBy(fun x -> Cells.dist x.Position position)
                             |> Seq.tryHead
        match worker with
            | Some w -> 
                (entities |> List.filter(fun x -> x.Id <> w.Id), [createAction w])
            | _ -> (entities, [])

