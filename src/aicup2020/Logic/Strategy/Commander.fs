namespace Jaina.Logic.Strategy

open Aicup2020.Model
open Jaina.Logic.Tactics
open Jaina.Core
open Jaina.State


type Commander(playerView: PlayerView, turnState: TurnState)=
   inherit Strategy(playerView, turnState)
   
   static let mutable prevSquads: int list list = []
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
   
    let totalThreat = turnState.InfluenceAndThreat |> List.sumBy(fun (_, _, t) -> t)

    let strategy = match totalThreat with
                    | x when x > Config.CommanderThreatWatermark 
                        -> new DefensiveStrategy(playerView, turnState, squads, recruits) :> Strategy
                    | _ 
                        -> new DefaultStrategy(playerView, turnState, squads, recruits) :> Strategy   

    strategy.Execute() @ [new Idler(playerView)]  