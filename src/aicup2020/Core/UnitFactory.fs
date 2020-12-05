namespace Jaina.Core

type UnitFactory = {
    Current: int
    mutable Capacity: int
    mutable Enlisted: int
} with 
    member this.Inflation = this.Current + this.Enlisted + 1
    member this.HasCapacity = this.Enlisted < this.Capacity
    member this.EnlistSingle() = 
        if this.Enlisted < this.Capacity then
            this.Enlisted <- this.Enlisted + 1
        else invalidOp "Capacity reached"