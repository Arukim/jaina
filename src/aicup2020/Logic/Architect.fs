namespace Jaina.Logic

open Aicup2020.Model
open Jaina.Algo
open Jaina.Core

type TileState =
    | Empty = 0
    | Building = 1 
    | SafeZone = 2
    | Resource = 3 

type Architect(playerView: PlayerView) =
    let myId = playerView.MyId
    let me = playerView.Players |> Array.find(fun x -> x.Id = myId)
    let mapSize = playerView.MapSize

    let getPos pos = pos.X + pos.Y * mapSize
    let posToVec pos = {X = pos % mapSize; Y = pos / mapSize }

    let mutable spaces3x = Array.empty    

    // check if 3x building fits
    let check3X (tiles:TileState[]) i =
        let vec = posToVec i
        let mutable isFree = true
        if vec.X < mapSize - 2 && vec.Y < mapSize - 2 then
            for x in 0..2 do
                for y in 0..2 do
                    if tiles.[i + x + y * mapSize] <> TileState.Empty then
                        isFree <- false
        else isFree <- false
        isFree

    let getSpaces3X (tiles:TileState[]) =
        seq {
            for i in 0..tiles.Length-1 do
                let v = tiles.[i]
                if v = TileState.Empty && check3X tiles i then
                    yield posToVec i
        }
    
    member this.Init() =
        let mutable tiles = Array.create (mapSize*mapSize) TileState.Empty
        
        // fill current buildings
        playerView.Entities
            |> ViewHelper.filterBuildings
            |> Seq.iter(fun building -> let buildingSize = playerView.EntityProperties.[building.EntityType].Size
                                        let pos = getPos building.Position
                                        for x in 0..buildingSize - 1 do
                                            for y in 0..buildingSize - 1 do
                                                tiles.[pos + x + y * mapSize] <- TileState.Building)
        
        // draw 1-tile 'safe' zone around
        for i in 0..tiles.Length-1 do
            let v = tiles.[i]
            if v = TileState.Building then
                Array2d.neighboursPlusOf mapSize i 
                    |> Seq.iter(fun x -> if tiles.[x] = TileState.Empty then tiles.[x] <- TileState.SafeZone)
        
        //place resources
        ViewHelper.entitiesOf playerView EntityType.Resource
            |> Seq.iter(fun x -> tiles.[getPos x.Position] <- TileState.Resource)
        
        // generate all free spaces with 3x size (House)
        spaces3x <- getSpaces3X tiles |> Seq.sortBy(fun x -> x.X + x.Y) |> Array.ofSeq
            
        ignore()

    member this.GetBuildings() =
        seq {
            if me.Resource >= Config.Architect_House_Watermark && spaces3x.Length > 0 
                                && EntityType.House |> ViewHelper.countOwnUnits playerView < Config.Max_Houses then
                yield (EntityType.House, spaces3x |> Array.head)
        }