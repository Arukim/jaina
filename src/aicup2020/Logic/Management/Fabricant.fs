namespace Jaina.Logic.Management

open Aicup2020.Model
open Jaina.Logic.Tactics

type Fabricant(playerView: PlayerView, order: (EntityType*int)) =
    inherit Manager(playerView)
        
    override this.Execute() =
        [new Fabric(playerView, fst order, snd order) :> Tactics]
