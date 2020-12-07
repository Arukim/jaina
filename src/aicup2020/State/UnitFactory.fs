namespace Jaina.State

type UnitFactory(current, capacity) = 
    let mutable enlisted = 0

    member this.Inflation = current + enlisted + 1
    member this.HasCapacity = enlisted < capacity
    member this.EnlistSingle() = 
        if enlisted < capacity then
            enlisted <- enlisted + 1
        else invalidOp "Capacity reached"