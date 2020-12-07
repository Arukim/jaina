namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Collections
open Jaina.Logic
open Jaina.Algo
open Jaina.Core
open Jaina.State

type RushCorporal(playerView: PlayerView, squad: Squad) =
    inherit Tactics(playerView)

    let findNext pos = 
        Cells.neighboursOf playerView.MapSize pos 
            |> Seq.map(fun x -> (x, HeatAttack.AttackMap.[x]))
            // psedo-randomize movement. Instead of always following NNNWWW pattern
            // use something like NWNWWN. GetHasCode makes it stable for all units,
            // so they continue to keep together.
            |> Seq.sortBy(fun x -> snd x, (fst x).GetHashCode())
            |> Seq.head

    member this.findAttackTarget (squadPath:Vec2Int list) entity =
        // find nearest point in squad path
        let nearest = squadPath |> Seq.mapi(fun idx x -> (x, idx))
                                |> Seq.sortBy(fun (pos, _) -> Cells.dist pos entity.Position)
                                |> Seq.head

        match nearest with
            | (x, idx) when idx = squadPath.Length - 1 -> x
            | (x, idx) when x = entity.Position -> squadPath.[idx + 1]
            | _ -> fst nearest
        

    override this.Execute entities =        
        
        let memberPositions = squad.Members |> List.map(fun x -> (x, HeatAttack.AttackMap.[x.Position]))
        let farest = memberPositions |> List.sortByDescending(fun (x, dist) -> (dist, x.Id))
                                     |> Seq.map(fun (x, dist) -> (x.Position, dist))
                                     |> Seq.head

        let leaderPos = squad.Members.Head.Position
        let leaderCoord = (leaderPos, HeatAttack.AttackMap.[leaderPos])

        let pathGenerator (pos, dist) =
            let next = findNext pos
            if snd next < dist then
                Some(fst next, next)
            else None

        let squadPath = [leaderPos] @ (leaderCoord |> List.unfold pathGenerator)

        if squadPath.Length = 0 then 
            failwith "empty squadPath"

        let moveAction entity = Some({
            Target = this.findAttackTarget squadPath entity
            FindClosestPosition = true
            BreakThrough = true         
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

        let actions = entities 
                        |> List.filter(fun e -> squad.Members |> List.contains e)
                        |> List.map createPrivate
                        
        let others = this.FilterInactive entities actions
        (others, actions)
    