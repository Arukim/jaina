namespace Jaina.Logic.Management

open Aicup2020.Model
open Jaina.Logic.Tactics
open Jaina.State


type OffensiveGeneral(playerView: PlayerView, squads: Squad list) =
    inherit Manager(playerView)

    override this.Execute() =
        squads |> List.map(fun s -> new RushCorporal(playerView, s) :> Tactics)