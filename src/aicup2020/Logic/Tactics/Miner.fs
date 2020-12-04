namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Core

type Miner(playerView: PlayerView) =
    inherit Tactics(playerView)

    override this.Execute entities = 
        let miners = entities |> List.filter(fun x -> x.EntityType = EntityType.BuilderUnit)
        
        let createMiner = fun miner ->            
            let moveAction = Some({
                Target = Config.Global_Attack_Targets.[0]
                FindClosestPosition = true
                BreakThrough = true
            })
            let attackAction = Some({
                Target = None
                AutoAttack = Some({
                    PathfindRange = (this.EntityProps miner).SightRange
                    ValidTargets = [|EntityType.Resource|]
                })
            })
            let action = {
                MoveAction = moveAction
                BuildAction = None 
                AttackAction = attackAction
                RepairAction = None
            }
            (miner.Id, action)

        let actions = miners |> List.map createMiner
        let others = this.FilterInactive entities actions

        (others, actions)
        

