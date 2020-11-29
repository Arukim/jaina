namespace Jaina.Logic

open Aicup2020.Model
open Jaina.Core
open Jaina.Collections

type WorkerRole =
    | Miner
    | Repairman
    | Builder

type Worker = {Role: WorkerRole; Target: Option<Vec2Int>}

type Foreman(playerView: PlayerView, architect: Architect) =
    let myId = playerView.MyId
    let totalWorkers = ViewHelper.countOwnUnits playerView EntityType.BuilderUnit
    let workers = Map.empty
    let calcDist a b = (uint)((a.X-b.X)*(a.X-b.X) + (a.Y-b.Y)*(a.Y-b.Y))
    let mutable workers = Map.empty
    let mutable repairTargets = Map.empty
    let mutable buildTargets = Map.empty

    let sightRange = (playerView.EntityProperties.TryFind EntityType.BuilderUnit).Value.SightRange

    member private this.selectRepairs availableWorkers =
        let damagedBuildings = playerView.Entities |> Seq.filter(fun x -> x.PlayerId = Some(myId))
                                                   |> ViewHelper.filterBuildings
                                                   |> Seq.filter(fun x -> x.Health < playerView.EntityProperties.[x.EntityType].MaxHealth)
        let availableWorkers = ViewHelper.ownEntitiesOf playerView EntityType.BuilderUnit
        let repairWorkers = min (damagedBuildings |> Seq.length) totalWorkers
        let mutable queue = availableWorkers |> Seq.collect(fun a -> damagedBuildings |> Seq.map (fun b -> a,b))
                                                    |> Seq.map(fun (a,b) -> (calcDist a.Position b.Position, (a,b)))
                                                    |> PriorityQ.fromSeq
               
        let mutable selectedRepairmans = Map.empty
        let mutable toRepair = damagedBuildings |> Set.ofSeq

        for i in 1..repairWorkers do
            while not(PriorityQ.isEmpty queue) && selectedRepairmans.Count <> i do
                let ((_, pair), newQueue) = (PriorityQ.popMin queue).Value
                queue <- newQueue
                if not(selectedRepairmans.ContainsKey(fst pair)) && toRepair.Contains(snd pair) then
                    selectedRepairmans <- selectedRepairmans.Add pair
                    toRepair <- toRepair.Remove(snd pair)
                 
        repairTargets <- selectedRepairmans

    member this.Init ()=
    
        let availableWorkers = ViewHelper.ownEntitiesOf playerView EntityType.BuilderUnit

        this.selectRepairs availableWorkers

        let minersOrBuilders = totalWorkers - (repairTargets |> Seq.length)

        let buildings = architect.GetBuildings() |> Seq.toArray

        let builders = if buildings.Length > 0 && minersOrBuilders > 0 then 1 else 0
        let miners = minersOrBuilders - builders

        if builders > 0 then 
            let buildingInfo = buildings.[0]
            let builder = availableWorkers |> Seq.filter(fun x -> not(repairTargets.ContainsKey(x)))
                                           |> Seq.sortBy(fun x -> calcDist x.Position (snd buildingInfo))
                                           |> Seq.head
            buildTargets <- buildTargets.Add (builder, buildingInfo)
            

        workers <- availableWorkers |> Seq.map(fun x -> (x, if repairTargets.ContainsKey(x) then
                                                               {Role = WorkerRole.Repairman; Target = Some(repairTargets.[x].Position)}
                                                            else if buildTargets.ContainsKey(x) then
                                                               {Role = WorkerRole.Builder; Target = Some(snd buildTargets.[x])}
                                                            else 
                                                                {Role = WorkerRole.Miner; Target = None}
                                                            ))
                                    |> Map.ofSeq

        ignore()

    member this.GetMove entity =
        let worker = workers.[entity]
        match worker.Role with
            | WorkerRole.Repairman ->          
                let target = worker.Target.Value 
                Some({
                    Target = {X = target.X + 1; Y = target.Y - 1}
                    FindClosestPosition = true
                    BreakThrough = false
                })
            | WorkerRole.Builder ->
                let target = worker.Target.Value 
                let building = fst buildTargets.[entity]
                let size = playerView.EntityProperties.[building].Size
                // it's not so easy to build near borders
                let xShift = if target.X = 0 then size-1 else 0
                let yShift = if target.Y = 0 then size-1 else -1

                Some({
                    Target = {X = target.X + xShift; Y = target.Y + yShift}
                    FindClosestPosition = true
                    BreakThrough = false
                })
            | WorkerRole.Miner ->
                Some({
                    Target = Config.Global_Attack_Target
                    FindClosestPosition = true
                    BreakThrough = true
                })

    member this.GetRepair entity =
        match repairTargets.TryFind entity with
            | Some x -> Some({Target = x.Id})
            | _ -> None

    member this.GetBuilding entity =
        match buildTargets.TryFind entity with
            | Some (t, pos) -> Some({Position = pos; EntityType = t})
            | _ -> None

    member this.GetAttack entity =
        let worker = workers.[entity]
        match worker.Role with
            | WorkerRole.Repairman
            | WorkerRole.Builder ->
                Some({
                    Target = None
                    AutoAttack = None
                })
            | WorkerRole.Miner ->
                Some({
                    Target = None
                    AutoAttack = Some({
                        PathfindRange = sightRange
                        ValidTargets = match entity.EntityType with
                                               | EntityType.BuilderUnit -> [|EntityType.Resource|]
                                               | _ -> [||]
                    })
                })
                
