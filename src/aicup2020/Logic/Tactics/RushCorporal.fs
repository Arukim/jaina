namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Collections
open Jaina.Logic
open Jaina.Algo
open Jaina.Core

type RushCorporal(playerView: PlayerView) =
    inherit Tactics(playerView)

    member this.findAttackTarget(entity: Entity) =
        let myPos = HeatAttack.AttackMap.TryFind(entity.Position)
        match myPos with
            | Some p -> 
                    let (pos, _) = Cells.neighboursOf playerView.MapSize entity.Position 
                                |> Seq.map(fun x -> (x, HeatAttack.AttackMap.[x]))
                                // psedo-randomize movement. Instead of always following NNNWWW pattern
                                // use something like NWNWWN. GetHasCode makes it stable for all units,
                                // so they continue to keep together.
                                |> Seq.sortBy(fun x -> snd x, (fst x).GetHashCode())
                                |> Seq.head
                    pos
                  // don't let all army collect in a sinlge edge in the end
            | _ -> Config.Global_Attack_Targets.[entity.Id % 3]

    override this.Execute entities =

        let createPrivate = fun entity ->

            let breakThrough = entity.Position.X > 20 || entity.Position.Y > 20
        
            let attackAction = Some({
                Target = None 
                AutoAttack = Some({
                    PathfindRange = (this.EntityProps entity).SightRange
                    ValidTargets = [||]})
            })

            let moveAction = match entity.EntityType with
                                | EntityType.RangedUnit
                                | EntityType.MeleeUnit -> Some({
                                                                Target = this.findAttackTarget entity
                                                                FindClosestPosition = true
                                                                BreakThrough = breakThrough         
                                                            })
                                | _ -> None

            (entity.Id, {
                MoveAction = moveAction
                BuildAction = None
                AttackAction = attackAction
                RepairAction = None
            })

        let idlers = entities |> List.map createPrivate
        ([], idlers)
    