namespace Jaina.Logic.Strategy

open Jaina.Logic.Tactics
open Aicup2020.Model
open Jaina.Logic

[<AbstractClass>]
type Strategy(playerView: PlayerView) = 
    inherit LogicUnit(playerView)
    abstract member Execute : unit -> list<Tactics>

