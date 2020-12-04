namespace Jaina.Logic.Management

open Aicup2020.Model
open Jaina.Logic.Tactics

type Upkeep(playerView: PlayerView) =
    inherit Manager(playerView)
        
    override this.Execute() =
        [new Mechanic(playerView)]
        
