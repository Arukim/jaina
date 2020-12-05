namespace Jaina.Logic

open Aicup2020.Model

[<AbstractClass>]
type LogicUnit(playerView: PlayerView) =

    member this.EntityProps entity = 
        playerView.EntityProperties.[entity.EntityType]
    
    member this.EntityTypeProps entityType = 
        playerView.EntityProperties.[entityType]