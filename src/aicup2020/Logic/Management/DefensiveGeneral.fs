namespace Jaina.Logic.Management

open Aicup2020.Model
open Jaina.Logic.Tactics

type DefensiveGeneral(playerView: PlayerView, turnState) =
    inherit Manager(playerView)

    override this.Execute() =
        [new Militia(playerView, turnState) :> Tactics]