namespace Jaina.Logic.Strategy

open Aicup2020.Model
open Jaina.Logic.Management
open Jaina.Logic


type DefaultStrategy(playerView: PlayerView, turnState, squads, recruits) =
    inherit Strategy(playerView, turnState)

    let stages = Blueprints.Economics.Default;

    let recruitPlan = Blueprints.Military.Default; 

    override this.Execute() =
        
        let currStage = stages |> this.CurrStage 
          
        let economics = [new Upkeep(playerView)] @
                        match currStage with
                            | Some stage -> this.GetEconomics stage
                            | _ -> []
                        @ [ RawManager(playerView)]:list<Manager>
        
        let recruiment = this.GetRecruitment recruitPlan

        let military = [new OffensiveGeneral(playerView, squads);
                        new TrainingGeneral(playerView, recruits)]
                        : Manager list
                    
        this.Apply (economics @ recruiment @ military)
