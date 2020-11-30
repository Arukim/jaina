namespace Jaina.Logic

open Aicup2020.Model
open Jaina.Core
open Jaina.Collections
open Jaina.Algo

type WorkerRole =
    | Miner
    | Repairman
    | Builder

type Worker = {Role: WorkerRole; Target: Option<Vec2Int>}

type Foreman(playerView: PlayerView, architect: Architect) =
    let myId = playerView.MyId
    let mapSize = playerView.MapSize
    let totalWorkers = ViewHelper.countOwnUnits playerView EntityType.BuilderUnit
    let workers = Map.empty
    let calcDist a b = (uint)((a.X-b.X)*(a.X-b.X) + (a.Y-b.Y)*(a.Y-b.Y))
    let mutable workers = Map.empty
    let mutable repairTargets = Map.empty
    let mutable buildTargets = Map.empty

    let sightRange = (playerView.EntityProperties.TryFind EntityType.BuilderUnit).Value.SightRange

    
    member private this.TargetPosBuilding unit entity = 
        this.TargetPos unit entity.Position entity.EntityType

    member private this.TargetPos unit pos entityType  =
        let size = playerView.EntityProperties.[entityType].Size
        Pathfinder.outerBorders mapSize size pos
            |> Seq.sortBy(fun x -> Pathfinder.dist x unit.Position)
            |> Seq.head

    member private this.SelectRepairs availableWorkers =
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

        this.SelectRepairs availableWorkers

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
                let building = repairTargets.[entity]
                Some({
                    Target = this.TargetPosBuilding entity building
                    FindClosestPosition = true
                    BreakThrough = false
                })
            | WorkerRole.Builder ->
                let target = worker.Target.Value 
                let buildingType = fst buildTargets.[entity]         
                let pos = this.TargetPos entity target buildingType

                Some({
                    Target = pos
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
                
