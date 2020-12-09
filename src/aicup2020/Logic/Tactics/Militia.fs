namespace Jaina.Logic.Tactics

open Aicup2020.Model
open Jaina.Collections
open Jaina.Logic
open Jaina.Algo
open Jaina.Core
open Jaina.State

type Militia(playerView: PlayerView, turnState: TurnState) =
    inherit Tactics(playerView)

    override this.Execute entities =
        let militia = entities |> View.filterMilitaryUnits
                               |> List.ofSeq


        let ownXs = turnState.OwnTerritoryField |> Seq.filter(fun x -> x.Value > 0)
                                                |> Seq.map(fun x -> x.Key.X)
                                                |> Seq.sort
                                                |> List.ofSeq
        let ownYs = turnState.OwnTerritoryField |> Seq.filter(fun x -> x.Value > 0)
                                                |> Seq.map(fun x -> x.Key.Y)
                                                |> Seq.sort
                                                |> List.ofSeq

        let ownMin = {
            X = ownXs |> List.head
            Y = ownYs |> List.head
        }

        let ownMax = {
            X = ((ownXs |> List.last) + 1) * Config.PotentialFieldTileSize
            Y = ((ownYs |> List.last) + 1) * Config.PotentialFieldTileSize
        }

        let foeIsInMinMax entity =
            let pos = entity.Position
            pos.X >= ownMin.X && pos.X <= ownMax.X &&
            pos.Y >= ownMin.Y && pos.Y <= ownMax.Y

        let foes = playerView
                        |> View.foeEntities
                        |> View.filterMilitaryUnits
                        |> Seq.filter foeIsInMinMax
                        |> List.ofSeq

        let moveCost = turnState.BuildMoveCost

        foes |> List.iter(fun foe ->
                            let pathMap = Pathfinder.pathMap moveCost Config.InfluenceIterations foe.Position
                            let range = match (this.EntityProps foe).Attack with
                                                | Some x -> x.AttackRange
                                                | None _ -> 0

                            let extPathMap = pathMap |> Pathfinder.extendPathMap range
                            let rank = float (extPathMap.Range + 1)
                            let threatMap = extPathMap.ToNormalized (fun x -> 
                                                                        let f = float x
                                                                        (rank - f) / (rank - 1.0))

                            threatMap.Map |> Array2D.iteri(fun x y v-> 
                                                            let pos = threatMap.ToGlobalCoord {X=x;Y=y}
                                                            Debug.Instance.FillCellA Palette.DarkRed (single v * 0.2f) pos)
                            ()
                            )
        
        let foeInfluence = new InfluenceMap(playerView.MapSize)

        //foesProxMap |> List.iter(fun (foe, map) -> foeInfluence.AddLayer())

        let actions = []
                          
        let others = this.FilterInactive entities actions
        (others, actions)