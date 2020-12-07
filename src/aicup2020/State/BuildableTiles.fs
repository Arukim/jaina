namespace Jaina.State

open Aicup2020.Model
open Jaina.Algo
open Jaina.Core

type TileBuildState =
    | Empty = 0
    | Building = 1 
    | SafeZone = 2
    | Resource = 3 

module BuildableTiles =
    let fillTiles pos size mapSize (tiles:TileBuildState[]) = 
        for x in 0..size - 1 do
            for y in 0..size - 1 do
                tiles.[pos + x + y * mapSize] <- TileBuildState.Building

    let construct (playerView:PlayerView) = 
       let mapSize = playerView.MapSize
       let getPos pos = pos.X + pos.Y * mapSize
       let tiles = Array.create (mapSize*mapSize) TileBuildState.Empty
       
       // fill current buildings
       playerView.Entities
           |> View.filterBuildings
           |> Seq.iter(fun building -> let buildingSize = playerView.EntityProperties.[building.EntityType].Size
                                       let pos = getPos building.Position
                                       tiles |> fillTiles pos buildingSize mapSize )
          
       // draw 1-tile 'safe' zone around
       for i in 0..tiles.Length-1 do
           let v = tiles.[i]
           match v with
                | TileBuildState.Building 
                | TileBuildState.Resource -> 
                    Array2d.neighboursPlusOf mapSize i 
                        |> Seq.iter(fun x -> 
                            if tiles.[x] = TileBuildState.Empty then 
                                tiles.[x] <- TileBuildState.SafeZone)
                | _ -> ()
          
       //place resources
       playerView |> View.entitiesOf EntityType.Resource
                  |> Seq.iter(fun x -> tiles.[getPos x.Position] <- TileBuildState.Resource)
       tiles

    let occupy pos size mapSize tiles =
        tiles |> fillTiles (pos.X + pos.Y*mapSize) size mapSize