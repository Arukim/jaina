namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Algo
open Jaina.Logic

[<AbstractClass>]
type Tactics(playerView: PlayerView) =
    inherit LogicUnit(playerView)

    abstract member Execute: list<Entity> -> list<Entity>*list<int*EntityAction>

    member this.FilterInactive entities actions =
        entities |> List.filter (fun x -> not(actions |> List.exists(fun (e, _) -> e = x.Id)))

    member this.SelectBuilderPos unit pos entityType  =
        let size = playerView.EntityProperties.[entityType].Size
        Cells.outerBorders playerView.MapSize size pos
            |> Seq.sortBy(fun x -> Cells.dist x unit.Position)
            |> Seq.head