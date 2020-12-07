namespace Jaina.State

open Aicup2020.Model
open Jaina.Algo

type Squad(members: Entity list) = 

    member this.Members with get() = members

    member this.Id with get() = members.Head.Id

    member this.Position with get() =
        match members.Length with
            | 0 -> {X=0;Y=0}
            | _ -> 
                let sumPos = members 
                                |> List.map(fun x -> x.Position)
                                |> List.fold Cells.sum {X=0;Y=0}
                {X=sumPos.X / members.Length; Y = sumPos.Y / members.Length}
