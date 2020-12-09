namespace Aicup2020

open Aicup2020.Model
open Jaina.Logic
open Jaina.Logic.Tactics
open Jaina.Core
open Jaina.Logic.Strategy
open Jaina.State

type MyStrategy() =

    let rec doTurn entities (tactics:list<Tactics>) acc =
        match tactics with
            | head :: tail -> 
                let (left, actions) = head.Execute entities
                doTurn left tail (acc @ actions)
            | _ -> acc

    member this.getAction(playerView: PlayerView, debugInterface: Option<DebugInterface>): Action =
        Debug.Instance.Refresh()

        let currMilitary = (playerView |> View.countOwnUnits EntityType.RangedUnit) + 
                           (playerView |> View.countOwnUnits EntityType.MeleeUnit)
      
        let heatMapUpdateTick = playerView.CurrentTick % Config.Attack_Map_Refresh_Rate = 0
        if currMilitary > 0 && heatMapUpdateTick then 
            HeatAttack.Update(playerView)
            
        let turnState = new TurnState(playerView)

        let commander = new Commander(playerView, turnState)

        let turnTactics = commander.Execute()      
        
        let myEntities = playerView |> View.ownEntities
                                    |> View.filterActionable
                                    |> List.ofSeq

        Debug.Instance.FillCellA Palette.Crimson 0.5f {X=0;Y=0}
        let actions = doTurn myEntities turnTactics [] |> Map.ofList

        match debugInterface with
            | Some x -> Debug.Instance.Events |> List.iter x.send
            | _ -> ()

        { EntityActions = actions}

    member this.debugUpdate(playerView: PlayerView, debugInterface: DebugInterface) =
        debugInterface.send (DebugCommand.Clear(new DebugCommandClear()))
        #if DEBUG
        let state = debugInterface.getState()

        //Debug.Instance.Events |> List.iter debugInterface.send

        ()
        #else
        debugInterface.getState () |> ignore
        #endif
