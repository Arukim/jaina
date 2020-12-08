namespace Jaina.Logic.Management

open Aicup2020.Model
open Jaina.Logic.Tactics


type TrainingGeneral(playerView: PlayerView, recruits) =
    inherit Manager(playerView)

    override this.Execute() =
        [new Logist(playerView, recruits) :> Tactics]        