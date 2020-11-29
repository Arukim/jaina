namespace Jaina.Algo

module Tiles =
    let neighboursOf size pos =
        seq {
            let (x,y) = pos % size, pos / size
            if x > 0 then yield pos - 1
            if x < size - 1 then yield pos + 1
            if y > 0 then yield pos - size
            if y < size - 1 then yield pos + size
        }

    let neighboursPlusOf size pos =
        seq {
            let (x,y) = pos % size, pos / size
            if x > 0 then yield pos - 1
            if x < size - 1 then yield pos + 1
            if y > 0 then yield pos - size
            if y < size - 1 then yield pos + size
            if x > 0 && y > 0 then yield pos - 1 - size
            if x > 0 && y < size - 1 then yield pos - 1 + size
            if x < size - 1 && y > 0 then yield pos + 1 - size
            if x < size - 1 && y < size - 1 then yield pos + 1 + size
        }

