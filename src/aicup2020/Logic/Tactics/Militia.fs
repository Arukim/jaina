namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Collections
open Jaina.Logic
open Jaina.Algo
open Jaina.Core
open Jaina.State

type Militia(playerView: PlayerView, turnState: TurnState) =
    inherit Tactics(playerView)

    override this.Execute entities =
        let militia = entities |> View.filterMilitaryUnits
                               |> List.ofSeq

        let castPos pos = Cells.upcastPos Config.PotentialFieldTileSize pos

        let mutable defenseMap = turnState.InfluenceAndThreat 
                                |> List.map(fun (pos, _, threat) -> (castPos pos, threat))
                                |> Map.ofList

        let militiaSort unit pos value =
            ((Cells.dist pos unit.Position) /  Config.PotentialFieldTileSize, -value)

        let actions = militia |> List.map(fun unit ->
                // dynamic programming!
                let keyValue = defenseMap |> Seq.sortBy(fun t -> militiaSort unit t.Key t.Value)                            
                                          |> Seq.head


                let moveAction = Some({
                    Target = keyValue.Key
                    FindClosestPosition = true
                    BreakThrough = true
                })

                let attackAction = Some({
                    Target = None 
                    AutoAttack = Some({
                        PathfindRange = (this.EntityProps unit).SightRange
                        ValidTargets = match unit.EntityType with | _ -> [||]})
                })

                let decreaseValue v =
                    match v with 
                        | Some x -> Some(x - unit.Health)
                        | _ -> None

                defenseMap <- defenseMap.Change(keyValue.Key, decreaseValue)

                (unit.Id, {
                    MoveAction = moveAction
                    BuildAction = None
                    AttackAction = attackAction
                    RepairAction = None
                })
            )
                          
        let others = this.FilterInactive entities actions
        (others, actions)