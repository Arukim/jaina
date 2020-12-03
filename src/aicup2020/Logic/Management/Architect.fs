namespace Jaina.Logic.Management

open Aicup2020.Model
open Jaina.Core
open Jaina.Logic.Tactics
open Jaina.Algo


type Architect(playerView: PlayerView, gameState:GameState, order:(EntityType*int)list) =
    inherit Manager(playerView)
    
    let mapSize = playerView.MapSize
    
    let posToVec pos = {X = pos % mapSize; Y = pos / mapSize }

    // check if 3x building fits
    let check (tiles:TileBuildState[]) size i =
        let vec = posToVec i
        let isFree = ref true
        let pSize = size - 1
        if vec.X < mapSize - pSize && vec.Y < mapSize - pSize then
            for x in 0..pSize do
                for y in 0..pSize do
                    if tiles.[i + x + y * mapSize] <> TileBuildState.Empty then
                        isFree := false
        else isFree := false
        !isFree

    let getSpaces size (tiles:TileBuildState[]) =
       seq {
           for i in 0..tiles.Length-1 do
               let v = tiles.[i]
               if v = TileBuildState.Empty && check tiles size i then
                   yield posToVec i
       }
        
    override this.Execute() =
        let tiles = gameState.BuildableTiles

        let chooseBuilding entityType =
            let size = (this.Props entityType).Size
            let spaces = tiles |> getSpaces size |> Seq.sortBy(fun x -> Cells.dist {X=0;Y=0} x)
            let space = Seq.tryHead spaces
            match space with
                | Some x -> 
                    tiles |> BuildableTiles.occupy x size playerView.MapSize
                    Some(entityType, x)
                | _ -> None

        order |> List.map(fun (entityType, count) -> List.init count (fun _ -> entityType))
              |> List.collect(fun x -> x)
              |> List.choose chooseBuilding
              |> List.map(fun (e,pos) -> new Foreman(playerView, e, pos) :> Tactics)

