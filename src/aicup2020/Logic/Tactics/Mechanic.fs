namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Core
open Jaina.Algo
open Jaina.Collections

type Mechanic(playerView: PlayerView) =
    inherit Tactics(playerView)

    override this.Execute entities = 
        let workers = entities |> List.filter(fun x -> x.EntityType = EntityType.BuilderUnit)

        let createAction worker building =
            let targetPos = this.SelectBuilderPos worker.Position building.Position building.EntityType
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
        
        let queue = availableWorkers 
                    |> Seq.collect(fun a -> damagedBuildings 
                                            |> Seq.map (fun b -> 
                                                let dist = Cells.distToBuilding a.Position b.Position (this.EntityProps b).Size
                                                dist, (a,b)))
                                     |> Seq.sortBy(fun (d,_) -> d)
                                     |> List.ofSeq
               
        let mutable matches = Map.empty


        // find initial repairman
        damagedBuildings |> Seq.iter (fun building ->
            let repairMatch = queue |> List.filter(fun (_, (w,b)) -> b = building && not(matches.ContainsKey(w)))
                                    |> List.tryHead
            match repairMatch with
                | Some (_, (w,b)) -> 
                    matches <- matches.Add (w, b)
                | _ -> ())
        
        // assign additional repairmans
        damagedBuildings |> Seq.iter (fun building ->
             let repairMatches = queue |> List.filter(fun (d, _) -> d < Config.RepairAssistDistance)
                                       |> List.filter(fun (_, (w,b)) -> b = building && not(matches.ContainsKey(w)))
                                       |> Seq.tryTake Config.RepairAssistMaxCount

             repairMatches |> Seq.iter(fun (_, (w, b)) -> 
                                          match not(matches.ContainsKey(w)) with
                                            | true -> 
                                                matches <- matches.Add (w, b)
                                            | false -> ()))

        matches