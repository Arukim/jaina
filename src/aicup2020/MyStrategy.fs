namespace Aicup2020

open Aicup2020.Model

type MyStrategy() =
    let mutable view:option<PlayerView> = None
    let mutable currBuilders = 0
    let mutable currMelees = 0
    let mutable currRangeds = 0
    let mutable currUnits = 0
    let mutable myId = 0

    let maxBuilders = 6
    let maxUnits = 15

    member this.countUnits view entityType =
        view.Entities |> Array.filter(fun x -> x.PlayerId = Some(view.MyId))
                 |> Array.filter(fun x -> x.EntityType = entityType)
                 |> Array.length

    member this.getAction(playerView: PlayerView, debugInterface: Option<DebugInterface>): Action =
        view <- Some(playerView)
        myId <- playerView.MyId

        currBuilders <- this.countUnits playerView EntityType.BuilderUnit
        currMelees <- this.countUnits playerView EntityType.MeleeUnit
        currRangeds <- this.countUnits playerView EntityType.RangedUnit
        currUnits <- currBuilders + currMelees + currRangeds

        let actions = playerView.Entities |> Array.filter(fun x -> x.PlayerId = Some(myId))
                                          |> Array.map this.entityTurn
                                          |> Map.ofArray
        { EntityActions = actions}

    member this.debugUpdate(playerView: PlayerView, debugInterface: DebugInterface) =
        debugInterface.send (DebugCommand.Clear(new DebugCommandClear()))
        debugInterface.getState () |> ignore

    member this.entityTurn(entity: Entity) =
        let playerView = view.Value
        
        let props = (playerView.EntityProperties.TryFind entity.EntityType).Value

        let moveAction = match entity.EntityType with
                            | EntityType.BuilderUnit -> Some({
                                Target = {
                                    X = playerView.MapSize - 1
                                    Y = playerView.MapSize - 1
                                }
                                FindClosestPosition = true
                                BreakThrough = true         
                            })
                            | EntityType.RangedUnit
                            | EntityType.MeleeUnit -> match currUnits with 
                                                            | x when x >= maxUnits - 4 -> Some({
                                                                Target = {
                                                                    X = playerView.MapSize - 1
                                                                    Y = playerView.MapSize - 1
                                                                }
                                                                FindClosestPosition = true
                                                                BreakThrough = true         
                                                            })
                                                            | _ -> Some({
                                                                Target = {
                                                                    X = 17
                                                                    Y = 17
                                                                }
                                                                FindClosestPosition = true
                                                                BreakThrough = false         
                                                            })
                            | EntityType.RangedUnit
                            | EntityType.MeleeUnit when currUnits = maxUnits -> Some({
                                Target = {
                                    X = playerView.MapSize - 1
                                    Y = playerView.MapSize - 1
                                }
                                FindClosestPosition = true
                                BreakThrough = true         
                            })
                            | _ -> None

        let buildAction = match entity.EntityType with
                            | EntityType.BuilderBase when currBuilders < maxBuilders -> Some({
                                EntityType = enum(LanguagePrimitives.EnumToValue entity.EntityType + 1)
                                Position = {
                                    X = entity.Position.X + props.Size
                                    Y = entity.Position.Y + props.Size - 1
                                }                                
                            })
                            | EntityType.RangedBase -> Some({
                                EntityType = enum(LanguagePrimitives.EnumToValue entity.EntityType + 1)
                                Position = {
                                    X = entity.Position.X + props.Size
                                    Y = entity.Position.Y + props.Size - 1
                                }                                
                            })
                            | EntityType.MeleeBase when playerView.Players.[myId].Resource >= 50 -> Some({
                                EntityType = enum(LanguagePrimitives.EnumToValue entity.EntityType + 1)
                                Position = {
                                    X = entity.Position.X + props.Size
                                    Y = entity.Position.Y + props.Size - 1
                                }                                
                            })
                            | _ -> None

        let attackAction = Some({
            Target = None
            AutoAttack = Some({
                PathfindRange = props.SightRange
                ValidTargets = match entity.EntityType with
                                       | EntityType.BuilderUnit -> [|EntityType.Resource|]
                                       | _ -> [||]
            })
        })

        let move = {
            MoveAction = moveAction
            BuildAction = buildAction
            AttackAction = attackAction
            RepairAction = None
        }
        (entity.Id, move)