namespace Aicup2020

open Aicup2020.Model

type MyStrategy() =
    let mutable view:option<PlayerView> = None
    let mutable currBuilders = 0
    let mutable currMelees = 0
    let mutable currRangeds = 0
    let mutable currUnits = 0
    let mutable myId = 0
    let mutable maxUnits = 15

    let maxBuilders = 6

    member this.countUnits view entityType =
        view.Entities |> Seq.filter(fun x -> x.PlayerId = Some(myId))
                 |> Seq.filter(fun x -> x.EntityType = entityType)
                 |> Seq.length

    member this.getAction(playerView: PlayerView, debugInterface: Option<DebugInterface>): Action =
        view <- Some(playerView)
        myId <- playerView.MyId

        currBuilders <- this.countUnits playerView EntityType.BuilderUnit
        currMelees <- this.countUnits playerView EntityType.MeleeUnit
        currRangeds <- this.countUnits playerView EntityType.RangedUnit
        currUnits <- currBuilders + currMelees + currRangeds

        maxUnits <- playerView.Entities |> Seq.filter(fun x -> x.PlayerId = Some(myId))
                                        |> Seq.filter(fun x -> x.EntityType = EntityType.BuilderBase ||
                                                                 x.EntityType = EntityType.MeleeBase ||
                                                                 x.EntityType = EntityType.RangedBase)
                                        |> Seq.map(fun x -> playerView.EntityProperties.[x.EntityType].PopulationProvide)
                                        |> Seq.sum

        let actions = playerView.Entities |> Seq.filter(fun x -> x.PlayerId = Some(myId))
                                          |> Seq.map this.entityTurn
                                          |> Map.ofSeq
        { EntityActions = actions}

    member this.debugUpdate(playerView: PlayerView, debugInterface: DebugInterface) =
        debugInterface.send (DebugCommand.Clear(new DebugCommandClear()))
        debugInterface.getState () |> ignore

    member this.entityTurn(entity: Entity) =
        let playerView = view.Value
        
        let props = (playerView.EntityProperties.TryFind entity.EntityType).Value

        let globalAttackTarget = {
            X = playerView.MapSize - 1
            Y = playerView.MapSize - 1
        }
        let globalDefenceTarget = {
            X = 17
            Y = 17
        }

        let moveAction = match entity.EntityType with
                            | EntityType.BuilderUnit -> Some({
                                Target = globalAttackTarget
                                FindClosestPosition = true
                                BreakThrough = true         
                            })
                            | EntityType.RangedUnit
                            | EntityType.MeleeUnit -> match currUnits with 
                                                            | x when x >= maxUnits - 4 -> Some({
                                                                Target = globalAttackTarget
                                                                FindClosestPosition = true
                                                                BreakThrough = true         
                                                            })
                                                            | _ -> Some({
                                                                Target = globalDefenceTarget
                                                                FindClosestPosition = true
                                                                BreakThrough = false         
                                                            })
                            | _ -> None

        let getBuildPos entity = {
            X = entity.Position.X + props.Size
            Y = entity.Position.Y + props.Size - 1
        }  

        let buildAction = match entity.EntityType with
                            | EntityType.BuilderBase when currBuilders < maxBuilders -> Some({
                                EntityType = enum(LanguagePrimitives.EnumToValue entity.EntityType + 1)
                                Position = getBuildPos entity                              
                            })
                            | EntityType.RangedBase -> Some({
                                EntityType = enum(LanguagePrimitives.EnumToValue entity.EntityType + 1)
                                Position = getBuildPos entity                         
                            })
                            | EntityType.MeleeBase when playerView.Players.[myId-1].Resource >= 60 -> Some({
                                EntityType = enum(LanguagePrimitives.EnumToValue entity.EntityType + 1)
                                Position = getBuildPos entity                  
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