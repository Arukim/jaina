namespace Jaina.State.Fields

open Aicup2020.Model
open Jaina.Algo

type FieldsManager(playerView:PlayerView) =
    let MaxThreatIterations = 5
    let HouseInfluenceIterations = 25

    member _.GetBuildingInfluence house =
        let pos = house.Position 
        let size = (playerView.EntityProperties.[house.EntityType]).Size
        let influenceMap = Pathfinder.createPatch playerView.MapSize pos size 
        let extPathMap = influenceMap |> Pathfinder.extendPathMap HouseInfluenceIterations
        
        let rank = float extPathMap.Rank
        extPathMap.ToNormalized (fun x -> match float x with
                                                | 0.0 -> 0.0
                                                | f ->  (rank - f) / (rank - 1.0))

    member _.GetUnitThreat unit = 
        let pos = unit.Position 
        let size = (playerView.EntityProperties.[unit.EntityType]).Size
        let pathMap = Pathfinder.createPatch playerView.MapSize pos size 

        let props = (playerView.EntityProperties.[unit.EntityType])
        let attackRange = match props.Attack with
                            | Some x -> x.AttackRange
                            | None _ -> 0
        let walkRange = match props.CanMove with
                            | true -> MaxThreatIterations
                            | _ -> 0
        
        let range = attackRange + walkRange

        let extPathMap = pathMap |> Pathfinder.extendPathMap range
        let rank = float extPathMap.Rank
        extPathMap.ToNormalized (fun x -> match x with
                                            | 0u -> 0.0
                                            | f when f <= uint attackRange -> 1.0
                                            | f ->  (rank - float f) / (rank - 1.0))