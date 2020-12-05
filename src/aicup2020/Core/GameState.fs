namespace Jaina.Core

open Aicup2020.Model

type UnitFactory = {
    Current: int
    mutable Capacity: int
    mutable Enlisted: int
} with 
    member this.Inflation = this.Current + this.Enlisted + 1
    member this.HasCapacity = this.Enlisted < this.Capacity
    member this.EnlistSingle() = 
        if this.Enlisted < this.Capacity then
            this.Enlisted <- this.Enlisted + 1
        else invalidOp "Capacity reached"


type GameState(playerView: PlayerView) = 
    
    let builders = playerView |> View.countOwnUnits  EntityType.BuilderUnit
    let melees = playerView |> View.countOwnUnits  EntityType.MeleeUnit
    let rangeds = playerView |> View.countOwnUnits  EntityType.RangedUnit
    let builderBases = playerView |> View.countOwnUnits  EntityType.BuilderBase
    let meleeBases = playerView |> View.countOwnUnits  EntityType.MeleeBase
    let rangedBases = playerView |> View.countOwnUnits  EntityType.RangedBase
    let totalUnits = builders + melees + rangeds
    let maxUnits = playerView |> View.ownEntities
                              |> View.filterHousing
                              |> Seq.map(fun x -> playerView.EntityProperties.[x.EntityType].PopulationProvide)
                              |> Seq.sum

    let me = playerView.Players |> Array.find(fun x -> x.Id = playerView.MyId)
    let mutable resources = me.Resource

    let factories = Map.empty
                       .Add(EntityType.BuilderUnit, {Current = builders; Capacity = builderBases; Enlisted = 0})
                       .Add(EntityType.RangedUnit, {Current = rangeds; Capacity = rangedBases; Enlisted = 0})
                       .Add(EntityType.MeleeUnit, {Current = melees; Capacity = meleeBases; Enlisted = 0})

    let buildableTiles = BuildableTiles.construct playerView       

    member _.Resources
        with get() = resources

    member _.BuildableTiles
        with get() = buildableTiles

    member _.MeleeBases with get() = meleeBases
    member _.RangedBases with get() = rangedBases

    member this.PlanBuild entityType =
        resources <- this.Resources - this.GetPrice entityType
        match View.isUnit entityType with 
            | true -> (factories.[entityType]).EnlistSingle()
            | _ -> ()

    member this.GetPrice entityType =         
        let basePrice = playerView.EntityProperties.[entityType].InitialCost
        let inflation = match View.isUnit entityType with
                            | true -> factories.[entityType].Inflation 
                            | _ -> 0
        basePrice + inflation

    member this.CanAfford entityType =
        resources >= this.GetPrice entityType &&
            match View.isUnit entityType with
                | true -> (factories.[entityType]).HasCapacity
                | _ -> true
