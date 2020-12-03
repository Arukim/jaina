namespace Jaina.Core

open Aicup2020.Model

module View =
    let countOwnUnits entityType view =
        view.Entities |> Seq.filter(fun x -> x.PlayerId = Some(view.MyId))
                      |> Seq.filter(fun x -> x.EntityType = entityType)
                      |> Seq.length

    let ownEntities view =
        view.Entities |> Seq.filter(fun x -> x.PlayerId = Some(view.MyId))


    let ownEntitiesOf view entityType =
        view.Entities |> Seq.filter(fun x -> x.PlayerId = Some(view.MyId))
                      |> Seq.filter(fun x -> x.EntityType = entityType)
                      
    let entitiesOf view entityType =
        view.Entities |> Seq.filter(fun x -> x.EntityType = entityType)

    let filterBuildings entities =
        entities |> Seq.choose(fun x -> match x.EntityType with
                                                    | EntityType.BuilderBase
                                                    | EntityType.MeleeBase
                                                    | EntityType.RangedBase
                                                    | EntityType.Wall
                                                    | EntityType.Turret
                                                    | EntityType.House -> Some(x)
                                                    | _ -> None)
    
    let filterHousing entities =
        entities |> Seq.choose(fun x -> match x.EntityType with
                                                    | EntityType.BuilderBase
                                                    | EntityType.MeleeBase
                                                    | EntityType.RangedBase
                                                    | EntityType.House -> Some(x)
                                                    | _ -> None)    
    
    let filterActionable entities =
        entities |> Seq.choose(fun x -> match x.EntityType with
                                            | EntityType.Resource
                                            | EntityType.Wall
                                            | EntityType.House -> None
                                            | _ -> Some(x))

    let filterMilitaryUnits entities =
        entities |> Seq.choose(fun x -> match x.EntityType with
                                            | EntityType.RangedUnit
                                            | EntityType.MeleeUnit -> Some(x)
                                            | _ -> None)

