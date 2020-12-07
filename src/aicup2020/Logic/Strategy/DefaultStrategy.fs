namespace Jaina.Logic.Strategy

open Aicup2020.Model
open Jaina.Logic.Management
open Jaina.Core
open Jaina.State
open Jaina.Logic


type DefaultStrategy(playerView: PlayerView) =
    inherit Strategy(playerView)
    
    static let mutable prevSquads: int list list = []

    let turnState = new TurnState(playerView)
    let stages = [
        // inital state
        [EntityType.BuilderBase, 1;
         EntityType.RangedBase, 1;
         EntityType.MeleeBase, 1;]
        // power-up builders
        [EntityType.BuilderUnit, 7];
        // get some houses
        [EntityType.House, 2;
          EntityType.BuilderUnit, 11];
        [EntityType.House, 4;
            EntityType.BuilderUnit, 15];
        [EntityType.House, 6;
          EntityType.BuilderUnit, 20;];
        [EntityType.House, 9;
          EntityType.BuilderUnit, 30;];
        [EntityType.House, 14;
          EntityType.BuilderUnit, 40;];
        [EntityType.House, 19;
          EntityType.BuilderUnit, 50;];
        [EntityType.House, 24;
          EntityType.BuilderUnit, 60;];
        [EntityType.House, 27;
          EntityType.BuilderUnit, 66;];
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

    let getSquads units =

        let collectSquad squad =
            squad |> List.choose(fun m -> units |> Seq.tryFind(fun u -> u.Id = m))

        let oldSquads = prevSquads |> List.map collectSquad |> List.filter(fun x -> not x.IsEmpty)

        let allAssigned = oldSquads |> List.collect(fun x -> x)
        let unassigned = units |> List.filter(fun x -> not (allAssigned |> List.contains x))

        let groups = unassigned |> List.chunkBySize Config.SquadSize                                

        let newSquads = oldSquads @ groups |> List.filter(fun x -> x.Length = Config.SquadSize)
        let leftAlone = groups |> List.filter(fun x -> x.Length <> Config.SquadSize)
                               |> List.collect(fun x -> x)
        
        prevSquads <- newSquads |> List.map(fun x -> x |> List.map(fun y -> y.Id))
        
        (newSquads |> List.map Squad, leftAlone)        

    override this.Execute() =

        let milUnits = playerView |> View.ownEntities
                           |> View.filterMilitaryUnits
                           |> List.ofSeq
        
        let (squads, recruits) = getSquads milUnits
        
        let currStage = stages |> List.skipWhile stageIsComplete
                               |> List.tryHead
                                
          
        let economics = [new Upkeep(playerView)] @
                        match currStage with
                            | Some stage -> this.GetEconomics stage
                            | _ -> []
                        @ [ RawManager(playerView)]:list<Manager>
        
        let recruiment = this.GetRecruitment recruitPlan

        let influnceAndThreat = turnState |> ThreatCheck.buildInfluenceThreat

        if influnceAndThreat |> Seq.exists(fun (_, _, t) -> t > 0) then
            printfn "There is some threat!"

        let army = [new OffensiveGeneral(playerView, squads);
            new Recruiter(playerView, recruits)]:Manager list
        
        let rec apply (managers:list<Manager>) acc =
            match managers with
                | head :: tail ->
                    apply tail (acc @ head.Execute())
                | _ -> acc
          
        apply (economics @ recruiment @ army) []

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
