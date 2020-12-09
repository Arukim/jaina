namespace Jaina.Algo

open Aicup2020.Model

type InfluencePatch<'T> = {
    Min: Vec2Int
    Max: Vec2Int
    Start: Vec2Int
    BaseSize: int
    Range: int
    Map: 'T[,]
} with
    static member Create baseSize start range initValue =        
        let (min, max) = Cells.patchEdges baseSize start range
        let patchSize = Cells.patchSize min max 
        let resMap = Array2D.create patchSize.X patchSize.Y initValue
        {
            Min = min
            Max = max
            Start = start
            BaseSize = baseSize
            Range = range
            Map = resMap
        }

    member this.ToNormalized mapping =
        let patchSize = Cells.patchSize this.Min this.Max
        let initFunc x y = this.Map.[x,y] |> mapping
        let resMap = Array2D.init patchSize.X patchSize.Y initFunc
        {
            Min = this.Min
            Max = this.Max
            Start = this.Start
            BaseSize = this.BaseSize
            Range = this.Range
            Map = resMap
        }

    member this.ToPatchCoord pos =
        {X = pos.X - this.Min.X; Y = pos.Y- this.Min.Y}

    member this.ToGlobalCoord pos =
        {X = pos.X + this.Min.X; Y = pos.Y + this.Min.Y}

    member this.GetExpanded range initValue =
        let newRange = this.Range + range
        let (min, max) = Cells.patchEdges this.BaseSize this.Start newRange
        let newSize = Cells.patchSize min max
        let newMap = Array2D.create newSize.X newSize.Y initValue
        Array2D.blit this.Map 0 0 newMap (this.Min.X - min.X) (this.Min.Y - min.Y) (this.Map.GetLength(0)) (this.Map.GetLength(1))
        {
            Min = min
            Max = max
            Start = this.Start
            BaseSize = this.BaseSize
            Range = newRange
            Map = newMap
        }

    
