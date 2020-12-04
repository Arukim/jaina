namespace Jaina.Logic.Management

open Aicup2020.Model
open Jaina.Logic.Tactics
open Jaina.Logic

[<AbstractClass>]
type Manager(playerView: PlayerView) =
    inherit LogicUnit(playerView)

    abstract member Execute: unit -> list<Tactics>