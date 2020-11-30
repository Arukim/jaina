namespace Aicup2020

open Aicup2020.Model
open Jaina.Algo
open Jaina.Logic
open Jaina.Core

type MyStrategy() =
    let mutable view:option<PlayerView> = None
    let mutable currBuilders = 0
    let mutable currMelees = 0
    let mutable currRangeds = 0
    let mutable currUnits = 0
    let mutable myId = 0
    let mutable me = None
    let mutable maxUnits = 15
    let mutable attackHeatMap = Map.empty
    let mutable foreman = None
    let mutable architect = None
    let mutable maxBuilders = Config.Base_Builders_Count

    member this.getAction(playerView: PlayerView, debugInterface: Option<DebugInterface>): Action =
        view <- Some(playerView)
        myId <- playerView.MyId
        me <- Some(playerView.Players |> Array.find(fun x -> x.Id = myId))

        currBuilders <- ViewHelper.countOwnUnits playerView EntityType.BuilderUnit
        currMelees <- ViewHelper.countOwnUnits playerView EntityType.MeleeUnit
        currRangeds <- ViewHelper.countOwnUnits playerView EntityType.RangedUnit
        currUnits <- currBuilders + currMelees + currRangeds
        maxBuilders <- Config.Base_Builders_Count + (ViewHelper.ownEntitiesOf playerView EntityType.House |> Seq.length)

        if currMelees + currRangeds > 0 && playerView.CurrentTick % Config.Attack_Map_Refresh_Rate = 0 then            
            let tactic = new HeatAttack(playerView)
            attackHeatMap <- tactic.Run(playerView)

        architect <- Some(new Architect(playerView))
        architect.Value.Init()

        foreman <- Some(new Foreman(playerView, architect.Value))
        foreman.Value.Init()

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
                    let (pos, _) = Cells.neighboursOf view.Value.MapSize entity.Position 
                                |> Seq.map(fun x -> (x, attackHeatMap.[x]))
                                // psedo-randomize movement. Instead of always following NNNWWW pattern
                                // use something like NWNWWN. GetHasCode makes it stable for all units,
                                // so they continue to keep together.
                                |> Seq.sortBy(fun x -> snd x, (fst x).GetHashCode())
                                |> Seq.head
                    pos
            | _ -> Config.Global_Attack_Target

    member this.allocateBuilders =
        
        ignore

    member this.entityTurn(entity: Entity) =
        let playerView = view.Value
        let foreman = foreman.Value
        
        let props = (playerView.EntityProperties.TryFind entity.EntityType).Value

        let breakThrough = entity.Position.X > 20 || entity.Position.Y > 20

        let moveAction = match entity.EntityType with
                            | EntityType.BuilderUnit -> foreman.GetMove entity
                            | EntityType.RangedUnit
                            | EntityType.MeleeUnit -> match currUnits with 
                                                            | _ -> Some({
                                                                Target = this.findAttackTarget entity
                                                                FindClosestPosition = true
                                                                BreakThrough = breakThrough         
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
                            | EntityType.MeleeBase when me.Value.Resource >= Config.Build_Warrior_Watermark -> Some({
                                EntityType = enum(LanguagePrimitives.EnumToValue entity.EntityType + 1)
                                Position = getBuildPos entity                  
                            })
                            | EntityType.BuilderUnit -> foreman.GetBuilding entity
                            | _ -> None

        let attackAction = match entity.EntityType with
                            | EntityType.BuilderUnit -> foreman.GetAttack entity
                            | _ -> Some({
                                      Target = None
                                      AutoAttack = Some({
                                          PathfindRange = props.SightRange
                                          ValidTargets = match entity.EntityType with
                                                                 | EntityType.BuilderUnit -> [|EntityType.Resource|]
                                                                 | _ -> [||]
                                      })
                                  })
        
        let repairAction = foreman.GetRepair entity

        let move = {
            MoveAction = moveAction
            BuildAction = buildAction 
            AttackAction = attackAction
            RepairAction = repairAction
        }
        (entity.Id, move)