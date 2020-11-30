namespace aicup2020.tests.Algo

open Xunit
open Jaina.Algo
open FsUnit.Xunit

module Cells =

    [<Fact>]
    let ``Cells works with simple data`` () =
        Cells.dist {X=0;Y=0;} {X=2; Y=1} |> should equal 3