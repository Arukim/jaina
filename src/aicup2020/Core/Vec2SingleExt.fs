namespace Jaina.Core

open Aicup2020.Model

module Vec2Single =
    let fromVec2Int pos = { X = single pos.X; Y = single pos.Y} : Vec2Single
    let ToCenter (pos : Vec2Single) =  { X = pos.X + 0.5f; Y = pos.Y + 0.5f} : Vec2Single
    let toCornerNW (pos : Vec2Single) =  { X = pos.X; Y = pos.Y + 1f} : Vec2Single
    let toCornerNE (pos : Vec2Single) =  { X = pos.X + 1f; Y = pos.Y + 1f} : Vec2Single
    let toCornerSE (pos : Vec2Single) =  { X = pos.X + 1f; Y = pos.Y} : Vec2Single