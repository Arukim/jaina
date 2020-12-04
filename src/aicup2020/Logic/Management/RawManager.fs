namespace Jaina.Logic.Management

open Aicup2020.Model
open Jaina.Logic.Tactics

type RawManager(playerView: PlayerView) =
    inherit Manager(playerView)
        
    override this.Execute() =
        [new Miner(playerView)]
