namespace Jaina.Logic.Strategy

open Jaina.Logic.Tactics
open Aicup2020.Model
open Jaina.Logic
open Jaina.State
open Jaina.Core
open Jaina.Logic.Management

[<AbstractClass>]
type Strategy(playerView: PlayerView, turnState: TurnState) = 
    inherit LogicUnit(playerView)
    let rec apply (managers:list<Manager>) acc =
        match managers with
            | head :: tail ->
                apply tail (acc @ head.Execute())
            | _ -> acc

    abstract member Execute : unit -> list<Tactics>
    

    member _.Apply managers = apply managers []

    member _.IsStageComplete stage =
        let checkEntity (entity, count) = (playerView |> View.countOwnUnits entity) >= count
        stage |> List.forall checkEntity

    member this.CurrStage stages = stages |> List.skipWhile this.IsStageComplete
                                          |> List.tryHead  

    member this.GetRecruitment recruitPlan =
        recruitPlan |> List.choose(fun recruit ->
            match turnState.CanAfford recruit with
                | true -> 
                    turnState.PlanBuild recruit
                    Some(new Fabricant(playerView, turnState, (recruit, 1)) :> Manager)
                | _ -> None)

    member this.GetEconomics stage =
        let needed = stage |> List.choose(fun (entity,required) ->
                                let needed = required - (playerView |> View.countOwnUnits entity)
                                match needed > 0 with
                                    | true -> Some(entity, needed)
                                    | _ -> None)
    
        needed |> List.takeWhile(fun (entityType, count) -> 
                    match turnState.CanAfford entityType with
                        | true -> turnState.PlanBuild entityType; true
                        | false -> false)
                |> List.groupBy(fun (entityType, _) -> 
                    match entityType with
                        | EntityType.House 
                        | EntityType.BuilderBase
                        | EntityType.MeleeBase
                        | EntityType.RangedBase -> 0
                        | EntityType.BuilderUnit -> 1
                        | _ -> -1)
                |> List.choose(fun (key, values) ->
                    match key with
                    | 0 -> Some(new Architect(playerView, turnState, values) :> Manager)
                    | 1 -> Some(new Fabricant(playerView, turnState, values |> List.head) :> Manager)
                    | _ -> None)  

