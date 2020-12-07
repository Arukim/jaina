namespace Aicup2020

open Aicup2020.Model
open Jaina.Logic
open Jaina.Logic.Tactics
open Jaina.Core
open Jaina.Logic.Strategy

type MyStrategy() =

    let rec doTurn entities (tactics:list<Tactics>) acc =
        match tactics with
            | head :: tail -> 
                let (left, actions) = head.Execute entities
                doTurn left tail (acc @ actions)
            | _ -> acc

    member this.getAction(playerView: PlayerView, debugInterface: Option<DebugInterface>): Action =

        let currMilitary = (playerView |> View.countOwnUnits EntityType.RangedUnit) + 
                           (playerView |> View.countOwnUnits EntityType.MeleeUnit)
      
        let heatMapUpdateTick = playerView.CurrentTick % Config.Attack_Map_Refresh_Rate = 0
        if currMilitary > 0 && heatMapUpdateTick then 
            HeatAttack.Update(playerView)
        
        let strategy = new DefaultStrategy(playerView)

        let turnTactics = strategy.Execute() @ [new Idler(playerView)]        
        
        let myEntities = playerView |> View.ownEntities
                                    |> View.filterActionable
                                    |> List.ofSeq

        let actions = doTurn myEntities turnTactics [] |> Map.ofList            

        { EntityActions = actions}

    member this.debugUpdate(playerView: PlayerView, debugInterface: DebugInterface) =
        debugInterface.send (DebugCommand.Clear(new DebugCommandClear()))
        debugInterface.getState () |> ignore
