namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Collections
open Jaina.Logic
open Jaina.Algo
open Jaina.Core
open Jaina.State
open Jaina.Debug

type Militia(playerView: PlayerView, turnState: TurnState) =
    inherit Tactics(playerView)


    override this.Execute entities =
        let size = playerView.MapSize
        let fieldsManager = turnState.FieldsManager
        
        let myHousing = playerView
                            |> View.ownEntities
                            |> View.filterHousing
                        
       // let homelandInfluence = new InfluenceMap(playerView.MapSize)
        let myHouseInterest = myHousing |> Seq.map fieldsManager.GetBuildingInfluence
                                                          
                
       // myHouseInterest |> Seq.iter homelandInfluence.AddLayer
                
       // homelandInfluence.Normalize()
                
        //let staticDefence = playerView |> View.ownEntitiesOf EntityType.Turret
                
        //let turretThreat = staticDefence 
        //                        |> Seq.map fieldsManager.GetUnitThreat
                
        //turretThreat |> Seq.iter homelandInfluence.SubLayer
                
        //let militia = playerView |> View.ownEntities
        //                            |> View.filterMilitaryUnits
        //                            |> Seq.map fieldsManager.GetUnitThreat
                                                                                                    
        //militia |> Seq.iter homelandInfluence.SubLayer
                
        //Dashboard.Main |> homelandInfluence.Render (Palette.BlueViolet, 0.8f)
                                                          
      //  let total = new InfluenceMap(playerView.MapSize)
                        
        let foeThreat = playerView |> View.foeEntities
                                        |> View.filterMilitaryUnits
                                        |> Seq.filter(fun x -> x.Position.X < size / 2 && x.Position.Y < size / 2)
                                        |> Seq.map fieldsManager.GetUnitThreat
                
        //turretThreat |> Seq.iter total.AddLayer
        //militia |> Seq.iter total.AddLayer
        //foeThreat |> Seq.iter total.SubLayer
                        
       // homelandInfluence.Mult total
       // Dashboard.Main |> homelandInfluence.Render2 (Palette.GreenYellow, 1f) (Palette.DarkRed, 0.7f)


        //todo - change later
        
        let sumFoeThreat = new InfluenceMap(playerView.MapSize)
        foeThreat |> Seq.iter sumFoeThreat.AddLayer

        let mostThreatened = new InfluenceMap(playerView.MapSize)
        myHouseInterest |> Seq.iter mostThreatened.AddLayer

        mostThreatened.Mult sumFoeThreat

        
        let free = entities |> View.filterMilitaryUnits
                            |> Seq.filter(fun x -> 
                                            let pos = x.Position
                                            sumFoeThreat.Map.[pos.X, pos.Y] = 0.)
        let busy = entities |> View.filterMilitaryUnits
                            |> Seq.filter(fun x -> 
                                            let pos = x.Position
                                            sumFoeThreat.Map.[pos.X, pos.Y] <> 0.)        
       

        let freeActions = free |> this.TargetFree mostThreatened.MaxPos
        let busyActions = busy |> this.TargetBusy sumFoeThreat


        let actions = freeActions @ busyActions
                          
        let others = this.FilterInactive entities actions
        (others, actions)   
    
    member this.TargetBusy sumFoeThreat busy =
        let selectTarget entity =
            entity.Position |> Cells.neighboursOf playerView.MapSize
                            |> Seq.maxBy(fun x -> sumFoeThreat.Map.[x.X,x.Y])

        let moveAction entity = Some({
                          Target = selectTarget entity
                          FindClosestPosition = true
                          BreakThrough = false         
                      })

        let createPrivate = fun entity ->
                              
            let attackAction = Some({
                Target = None 
                AutoAttack = Some({
                    PathfindRange = (this.EntityProps entity).SightRange
                    ValidTargets = [||]})
            })

            (entity.Id, {
                MoveAction = moveAction entity
                BuildAction = None
                AttackAction = attackAction
                RepairAction = None
            })

        busy |> Seq.map createPrivate 
             |> List.ofSeq 
        

    member this.TargetFree target free =
        let moveAction entity = Some({
                          Target = target
                          FindClosestPosition = true
                          BreakThrough = false         
                      })

        let createPrivate = fun entity ->
                              
            let attackAction = Some({
                Target = None 
                AutoAttack = Some({
                    PathfindRange = (this.EntityProps entity).SightRange
                    ValidTargets = [||]})
            })

            (entity.Id, {
                MoveAction = moveAction entity
                BuildAction = None
                AttackAction = attackAction
                RepairAction = None
            })
        free |> Seq.map createPrivate 
             |> List.ofSeq 
       
