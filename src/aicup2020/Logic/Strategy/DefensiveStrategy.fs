namespace Jaina.Logic.Strategy

open Aicup2020.Model
open Jaina.Algo
open Jaina.Logic.Management
open Jaina.Logic
open Jaina.State
open Jaina.Core


type DefensiveStrategy(playerView: PlayerView, turnState, squads: Squad list, recruits) =
    inherit Strategy(playerView, turnState)

    let stages = Blueprints.Economics.Default;
    
    let recruitPlan = Blueprints.Military.Defensive; 

    override this.Execute() =
        let recruiment = this.GetRecruitment recruitPlan

        let outerSquadSelector (x:Squad) = 
            let tilePos = Cells.toTilePos Config.PotentialFieldTileSize x.Position
            let ownTile = turnState.OwnTerritoryField.TryFind tilePos
            match ownTile with 
                | None -> Some(x)
                | _ -> None

        
        let outerSquads = squads |> List.choose outerSquadSelector

        let military = [new OffensiveGeneral(playerView, outerSquads);
                        new DefensiveGeneral(playerView, turnState)]
                        : Manager list

        let currStage = stages |> this.CurrStage           
        let economics = [new Upkeep(playerView)] @
                        match currStage with
                            | Some stage -> this.GetEconomics stage
                            | _ -> []
                        @ [ RawManager(playerView)]
                        : Manager list

        this.Apply (recruiment @ military @ economics)