namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Core
open Jaina.Collections
open Jaina.Algo

type Mechanic(playerView: PlayerView) =
    inherit Tactics(playerView)

    override this.Execute entities = 
        let workers = entities |> List.filter(fun x -> x.EntityType = EntityType.BuilderUnit)

        let createAction worker building =
            let targetPos = this.SelectBuilderPos worker building.Position building.EntityType
            let moveAction = Some({
                Target = targetPos
                FindClosestPosition = true
                BreakThrough = false
            })
            (worker.Id, {
                MoveAction = moveAction
                BuildAction = None 
                AttackAction = None
                RepairAction = Some({Target = building.Id})
            })          

        let actions = this.SelectRepairs workers 
                        |> Map.map createAction
                        |> Seq.map(fun (KeyValue(_, v)) -> v)
                        |> List.ofSeq
        
       
        let others = this.FilterInactive entities actions
        (others, actions)

    member private this.SelectRepairs availableWorkers =
        let damagedBuildings = playerView  |> View.ownEntities
                                           |> View.filterBuildings
                                           |> Seq.filter(fun x -> x.Health < (this.EntityProps x).MaxHealth)

        
        let totalWorkers = availableWorkers |> List.length

        let repairWorkers = min (damagedBuildings |> Seq.length) totalWorkers
        let queue = availableWorkers |> Seq.collect(fun a -> damagedBuildings |> Seq.map (fun b -> a,b))
                                     |> Seq.sortBy(fun (a,b) -> Cells.dist a.Position b.Position)
                                     |> List.ofSeq
               
        let mutable matches = Map.empty
        let mutable toRepair = damagedBuildings |> Set.ofSeq

        for i in 1..repairWorkers do
            for (worker, building) in queue do
                if not(matches.ContainsKey(worker)) && toRepair.Contains(building) then
                    matches <- matches.Add (worker, building)
                    toRepair <- toRepair.Remove(building)                 
        matches