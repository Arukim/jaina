namespace Jaina.Logic.Management

open Aicup2020.Model
open Jaina.Logic.Tactics


type OffensiveGeneral(playerView: PlayerView) =
    inherit Manager(playerView)

    override this.Execute() =
        [new RushCorporal(playerView)]