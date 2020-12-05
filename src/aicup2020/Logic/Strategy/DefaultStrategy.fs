namespace Jaina.Logic.Strategy

open Aicup2020.Model
open Jaina.Logic.Management
open Jaina.Core


type DefaultStrategy(playerView: PlayerView) =
    inherit Strategy(playerView)
    
    let gameState = new GameState(playerView)
    let stages = [
        // inital state
        [EntityType.BuilderBase, 1;
         EntityType.RangedBase, 1;
         EntityType.MeleeBase, 1;]
        // power-up builders
        [EntityType.BuilderUnit, 10];
        // get some houses
        [EntityType.House, 2;
          EntityType.BuilderUnit, 11];
        [EntityType.House, 4;
            EntityType.BuilderUnit, 15];
        [EntityType.House, 6;
          EntityType.BuilderUnit, 20;];
        [EntityType.House, 9;
          EntityType.BuilderUnit, 40;];
        [EntityType.House, 14;
          EntityType.BuilderUnit, 50;];
        [EntityType.House, 19;
          EntityType.BuilderUnit, 60;];
        [EntityType.House, 24;
          EntityType.BuilderUnit, 70;];
        [EntityType.House, 27;
          EntityType.BuilderUnit, 75;];
        [EntityType.RangedBase, 2];
        [EntityType.MeleeBase, 2];
    ]

    let recruitPlan = [
        EntityType.RangedUnit;
        EntityType.MeleeUnit;
        EntityType.RangedUnit;
        EntityType.MeleeUnit;
    ]

    let stageIsComplete stage =
        let checkEntity (entity, count) = (playerView |> View.countOwnUnits entity) >= count
        stage |> List.forall checkEntity

    override this.Execute() =
        let currStage = stages |> List.skipWhile stageIsComplete
                                |> List.tryHead
                                
          
        let economics = [new Upkeep(playerView)] @
                        match currStage with
                            | Some stage -> this.GetEconomics stage
                            | _ -> []
                        @ [ RawManager(playerView)]:list<Manager>

        let recruiment = this.GetRecruitment recruitPlan

        let army = [new OffensiveGeneral(playerView) :> Manager]
        
        let rec apply (managers:list<Manager>) acc =
            match managers with
                | head :: tail ->
                    apply tail (acc @ head.Execute())
                | _ -> acc
          
        apply (economics @ recruiment @ army) []

    member this.GetRecruitment recruitPlan =
        recruitPlan |> List.choose(fun recruit ->
            match gameState.CanAfford recruit with
                | true -> 
                    gameState.PlanBuild recruit
                    Some(new Fabricant(playerView, gameState, (recruit, 1)) :> Manager)
                | _ -> None)

    member this.GetEconomics stage =
        let needed = stage |> List.choose(fun (entity,required) ->
                                let needed = required - (playerView |> View.countOwnUnits entity)
                                match needed > 0 with
                                    | true -> Some(entity, needed)
                                    | _ -> None)
    
        needed |> List.takeWhile(fun (entityType, count) -> 
                    match gameState.CanAfford entityType with
                        | true -> gameState.PlanBuild entityType; true
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
                    | 0 -> Some(new Architect(playerView, gameState, values) :> Manager)
                    | 1 -> Some(new Fabricant(playerView, gameState, values |> List.head) :> Manager)
                    | _ -> None)        
