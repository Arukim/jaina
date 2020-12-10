namespace Jaina.State

open Aicup2020.Model
open Jaina.State.Fields
open Jaina.Algo
open Jaina.Core

type TurnState(playerView: PlayerView) = 
    
    let builders = playerView |> View.countOwnUnits  EntityType.BuilderUnit
    let melees = playerView |> View.countOwnUnits  EntityType.MeleeUnit
    let rangeds = playerView |> View.countOwnUnits  EntityType.RangedUnit
    let builderBases = playerView |> View.countOwnUnits  EntityType.BuilderBase
    let meleeBases = playerView |> View.countOwnUnits  EntityType.MeleeBase
    let rangedBases = playerView |> View.countOwnUnits  EntityType.RangedBase

    let fieldsManager = new FieldsManager(playerView)

    let minerals = playerView 
                    |> View.entitiesOf EntityType.Resource
                    |> Seq.map(fun entity -> (entity.Position, entity.Health))
                    |> Map.ofSeq

    let foeUnits = playerView
                    |> View.foeEntities
                    |> View.filterMilitaryUnits
                    |> Seq.map(fun entity -> (entity.Position, entity.Health))
                    |> Map.ofSeq
    
    let ownTerritory = playerView
                    |> View.ownEntities
                    |> Seq.choose(fun x -> 
                        match x.EntityType with 
                            | EntityType.BuilderUnit
                            | EntityType.BuilderBase
                            | EntityType.MeleeBase
                            | EntityType.RangedBase -> Some((x.Position, x.Health))
                            | _ -> None)
                    |> Map.ofSeq

    let me = playerView.Players |> Array.find(fun x -> x.Id = playerView.MyId)
    let mutable resources = me.Resource

    let factories = Map.empty
                       .Add(EntityType.BuilderUnit, new UnitFactory(builders, builderBases))
                       .Add(EntityType.RangedUnit, new UnitFactory(rangeds, rangedBases))
                       .Add(EntityType.MeleeUnit, new UnitFactory(melees, meleeBases))

    let buildableTiles = BuildableTiles.construct playerView       

    let translate (data:Map<Vec2Int, int>) pos =
        match data.TryFind pos with
            | Some x -> x
            | _ -> 0
     
    let resourcesField = PotentialField.create(playerView.MapSize, Config.PotentialFieldTileSize, minerals |> translate)
    let foeUnitsField = PotentialField.create(playerView.MapSize,  Config.PotentialFieldTileSize, foeUnits |> translate)
    let ownTerritoryField = PotentialField.create(playerView.MapSize,  Config.PotentialFieldTileSize, ownTerritory |> translate)
    let influenceAndThreat = ThreatCheck.buildInfluenceThreat ownTerritoryField foeUnitsField


    member _.Resources
        with get() = resources

    member _.BuildableTiles
        with get() = buildableTiles

    member _.MeleeBases with get() = meleeBases
    member _.RangedBases with get() = rangedBases

    member _.ResourcesField with get() = resourcesField
    member _.FoeUnitsField with get() = foeUnitsField
    member _.OwnTerritoryField with get() = ownTerritoryField
    member _.InfluenceAndThreat with get() = influenceAndThreat

    member _.FieldsManager with get() = fieldsManager

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

    member this.BuildMoveCost with get() =
        let size = playerView.MapSize
        let moveCost = Array2D.create size size 1u

        
        let fillBuildings building = 
            let pos = building.Position
            let buildingSize = playerView.EntityProperties.[building.EntityType].Size

            for x in pos.X .. pos.X + buildingSize - 1 do
                for y in pos.Y .. pos.Y + buildingSize - 1 do
                    moveCost.[x,y] <- System.UInt32.MaxValue / 2u

        minerals |> Map.iter(fun pos v -> moveCost.[pos.X, pos.Y] <- uint (v / Config.TypicalAttack) + 1u)

        playerView.Entities |> View.filterBuildings
                            |> Seq.iter fillBuildings

        moveCost
