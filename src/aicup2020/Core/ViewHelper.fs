namespace Jaina.Core

open Aicup2020.Model

module ViewHelper=
    let countOwnUnits view entityType =
        view.Entities |> Seq.filter(fun x -> x.PlayerId = Some(view.MyId))
                      |> Seq.filter(fun x -> x.EntityType = entityType)
                      |> Seq.length

    let ownEntities view =
        view.Entities |> Seq.filter(fun x -> x.PlayerId = Some(view.MyId))

    let ownEntitiesOf view entityType =
        view.Entities |> Seq.filter(fun x -> x.PlayerId = Some(view.MyId))
                      |> Seq.filter(fun x -> x.EntityType = entityType)

    let filterBuildings entities =
        entities |> Seq.filter(fun x -> match x.EntityType with
                                                    | EntityType.BuilderBase
                                                    | EntityType.MeleeBase
                                                    | EntityType.RangedBase
                                                    | EntityType.Wall
                                                    | EntityType.Turret
                                                    | EntityType.House -> true
                                                    | _ -> false)

