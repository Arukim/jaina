﻿namespace Jaina.Collections

module Seq =
    let tryTake (n : int) (s : _ seq) =
        let e = s.GetEnumerator ()
        let i = ref 0
        seq {
            while e.MoveNext () && !i < n do
                i := !i + 1
                yield e.Current
        }

