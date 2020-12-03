namespace Jaina.Core

open Aicup2020.Model
open Jaina.Algo

type GameState(playerView: PlayerView) = 
    
    let builders = playerView |> View.countOwnUnits  EntityType.BuilderUnit
    let melees = playerView |> View.countOwnUnits  EntityType.MeleeUnit
    let ranges = playerView |> View.countOwnUnits  EntityType.RangedUnit
    let meleeBases = playerView |> View.countOwnUnits  EntityType.MeleeBase
    let rangesBases = playerView |> View.countOwnUnits  EntityType.RangedBase
    let totalUnits = builders + melees + ranges
    let maxUnits = playerView |> View.ownEntities
                              |> View.filterHousing
                              |> Seq.map(fun x -> playerView.EntityProperties.[x.EntityType].PopulationProvide)
                              |> Seq.sum
    let me = playerView.Players |> Array.find(fun x -> x.Id = playerView.MyId)
    let mutable resources = me.Resource
    let mutable enlistedMelees = 0
    let mutable enlistedRanges = 0
    let mutable enlistedBuilders = 0
    let buildableTiles = BuildableTiles.construct playerView       

    member _.Resources
        with get() = resources
        and  set(r) = resources <- r

    member _.BuildableTiles
        with get() = buildableTiles

    member _.MeleeBases with get() = meleeBases
    member _.RangedBases with get() = rangesBases

    member this.PlanBuild entityType =
        match entityType with
            | EntityType.BuilderUnit -> enlistedBuilders <- enlistedBuilders + 1
            | EntityType.MeleeUnit -> enlistedMelees <- enlistedMelees + 1
            | EntityType.RangedUnit -> enlistedRanges <- enlistedRanges + 1
            | _ -> ()
        ()

    member this.CanAfford entityType =
        // MAX UNITS!!!
        let basePrice = playerView.EntityProperties.[entityType].InitialCost
        let inflation = match entityType with
                            | EntityType.BuilderUnit -> builders + enlistedBuilders
                            | EntityType.MeleeUnit -> melees + enlistedMelees
                            | EntityType.RangedUnit -> ranges + enlistedRanges
                            | _ -> 0
        resources >= basePrice + inflation

