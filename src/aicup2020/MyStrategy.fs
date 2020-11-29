namespace Aicup2020

open Aicup2020.Model
open Jaina.Algo
open Jaina.Logic.Tactics

type MyStrategy() =
    let mutable view:option<PlayerView> = None
    let mutable currBuilders = 0
    let mutable currMelees = 0
    let mutable currRangeds = 0
    let mutable currUnits = 0
    let mutable myId = 0
    let mutable maxUnits = 15
    let mutable attackHeatMap = Map.empty
    let mutable globalAttackTarget = {X= 0; Y = 0;}
    let mutable fieldSize = {X=0; Y=0;}

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

        globalAttackTarget <- {
            X = playerView.MapSize - 1
            Y = playerView.MapSize - 1
        }

        fieldSize <- {X = playerView.MapSize; Y = playerView.MapSize}

        let tactic = new TargetNearestRangedBase()
        if currMelees + currRangeds > 0 then
            attackHeatMap <- tactic.Run(playerView)

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

    member this.findAttackTarget(entity: Entity) =
        let myPos = attackHeatMap.TryFind(entity.Position)
        match myPos with
            | Some p -> 
                    let (pos, _) = Pathfinder.neighboursOf fieldSize entity.Position 
                                |> Seq.map(fun x -> (x, attackHeatMap.[x]))
                                |> Seq.sortBy(fun x -> snd x)
                                |> Seq.head
                    pos
            | _ -> globalAttackTarget

    member this.entityTurn(entity: Entity) =
        let playerView = view.Value
        
        let props = (playerView.EntityProperties.TryFind entity.EntityType).Value

        let globalDefenceTarget = {
            X = 17
            Y = 17
        }

        let breakThrough = entity.Position.X > 20 || entity.Position.Y > 20

        let moveAction = match entity.EntityType with
                            | EntityType.BuilderUnit -> Some({
                                Target = globalAttackTarget
                                FindClosestPosition = true
                                BreakThrough = true         
                            })
                            | EntityType.RangedUnit
                            | EntityType.MeleeUnit -> match currUnits with 
                                                            | _ -> Some({
                                                                Target = this.findAttackTarget entity
                                                                FindClosestPosition = true
                                                                BreakThrough = breakThrough         
                                                            })
                                                            //| _ -> Some({
                                                            //    Target = globalDefenceTarget
                                                            //    FindClosestPosition = true
                                                            //    BreakThrough = breakThrough         
                                                            //})
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