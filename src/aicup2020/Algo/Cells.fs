namespace Jaina.Algo

open Aicup2020.Model

module Cells =
    let dist a b =
        abs(a.X - b.X) + abs(a.Y - b.Y)

    let sum a b =
        {X = a.X + b.X; Y = a.Y + b.Y}
        
    let toVec size pos =
        {X = pos % size; Y = pos / size}

    let fromVec size pos =
        pos.Y * size + pos.X

    let toTilePos size pos = 
        {X = pos.X / size; Y = pos.Y / size}

    let upcastPos size pos =
        {X = pos.X * size + size / 2; Y = pos.Y * size + size / 2}

    let patchSize min max =
        {X = max.X - min.X + 1; Y = max.Y - min.Y + 1}

    let patchEdges size start startSize range =
        let calcMinMax x = (max 0 (x - range)), (min (x + range + startSize - 1) (size - 1))
        let (xMin, xMax) = calcMinMax start.X
        let (yMin, yMax) = calcMinMax start.Y
        ({X = xMin; Y = yMin}, {X = xMax; Y = yMax})

    // use in calculation of shortest path to a building
    let triPointDist a1 a2 b =
        if b < a1 then a1 - b
        elif b > a2 then b - a2
        else 0

    let distToBuilding w b1 bSize =
        let b2 = {X = b1.X + bSize; Y = b1.Y + bSize }

        let distX = triPointDist b1.X b2.X w.X
        let distY = triPointDist b1.Y b2.Y w.Y
        distX + distY

    let outerBorders mapSize size pos =
        seq {
            for i in 0..size-1 do
                // left border
                if pos.X > 0 then yield {X = pos.X - 1; Y = pos.Y + i}
                // right border
                if pos.X < mapSize - size - 1 then yield {X = pos.X + size; Y = pos.Y + i}
                // bottom border
                if pos.Y > 0 then yield {X = pos.X + i; Y = pos.Y - 1}
                // top border
                if pos.Y < mapSize - 1 - size then yield {X = pos.X + i; Y = pos.Y + size}
        }

    let neighboursOf mapSize curr = 
        seq {
            if curr.X > 0 then yield {X = curr.X - 1; Y = curr.Y}
            if curr.X < mapSize-1 then yield {X = curr.X + 1; Y = curr.Y}
            if curr.Y > 0 then yield {X = curr.X; Y = curr.Y - 1}
            if curr.Y < mapSize-1 then yield {X = curr.X; Y = curr.Y + 1}
        }

    let neighboursOf2 mapSize curr = 
        seq {
            if curr.X > 0 then yield {X = curr.X - 1; Y = curr.Y}
            if curr.X < mapSize.X-1 then yield {X = curr.X + 1; Y = curr.Y}
            if curr.Y > 0 then yield {X = curr.X; Y = curr.Y - 1}
            if curr.Y < mapSize.Y-1 then yield {X = curr.X; Y = curr.Y + 1}
        }