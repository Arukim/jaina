namespace Jaina.Logic

open Aicup2020.Model
open Jaina.Core
open Jaina.Collections

type WorkerRole =
    | Miner
    | Repairman

type Worker = {Role: WorkerRole; Target: Option<Vec2Int>}

type BuilderManager(playerView: PlayerView) =
    let myId = playerView.MyId
    let totalWorkers = ViewHelper.countOwnUnits playerView EntityType.BuilderUnit
    let workers = Map.empty
    let calcDist a b = (uint)((a.X-b.X)*(a.X-b.X) + (a.Y-b.Y)*(a.Y-b.Y))
    let mutable workers = Map.empty
    let mutable repairTargets = Map.empty
    let sightRange = (playerView.EntityProperties.TryFind EntityType.BuilderUnit).Value.SightRange

    member this.Init ()=
        let damagedBuildings = playerView.Entities |> Seq.filter(fun x -> x.PlayerId = Some(myId))
                                                   |> ViewHelper.filterBuildings
                                                   |> Seq.filter(fun x -> x.Health < playerView.EntityProperties.[x.EntityType].MaxHealth)
        let repairWorkers = min (damagedBuildings |> Seq.length) totalWorkers
        let availableWorkers = ViewHelper.ownEntitiesOf playerView EntityType.BuilderUnit

        let mutable queue = availableWorkers |> Seq.collect(fun a -> damagedBuildings |> Seq.map (fun b -> a,b))
                                             |> Seq.map(fun (a,b) -> (calcDist a.Position b.Position, (a,b)))
                                             |> PriorityQ.fromSeq
        
        let mutable selected = Map.empty
        let mutable toRepair = damagedBuildings |> Set.ofSeq

        for i in 1..repairWorkers do
            while not(PriorityQ.isEmpty queue) && selected.Count <> i do
                let ((_, pair), newQueue) = (PriorityQ.popMin queue).Value
                queue <- newQueue
                if not(selected.ContainsKey(fst pair)) && toRepair.Contains(snd pair) then
                    selected <- selected.Add pair
                    toRepair <- toRepair.Remove(snd pair)
          
        repairTargets <- selected

        workers <- availableWorkers |> Seq.map(fun x -> (x, match selected.ContainsKey(x) with
                                                                | true -> {Role = WorkerRole.Repairman; Target = Some(selected.[x].Position)}
                                                                | _ -> {Role = WorkerRole.Miner; Target = None}))
                                    |> Map.ofSeq

        //let repairmanId = available |> Seq.sortBy(fun x )
        // assign repair workers
        //
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

    member this.GetAttack entity =
        let worker = workers.[entity]
        match worker.Role with
            | WorkerRole.Repairman -> 
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
                
