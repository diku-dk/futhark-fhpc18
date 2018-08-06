-- generate a number of 2d sobol vectors

import "/futlib/sobol"
import "/futlib/array"
import "/futlib/sobol-dir-50"

module S2 = Sobol sobol_dir { let D = 2 }

let indep (n:i32) : [n][2]f64 =
  map (\i -> map (\u -> f64.u32 u / S2.norm) (S2.independent i)) (iota n)

let recur (n:i32) : [n][2]f64 =
  let (tmp,_) =
    loop (arr : *[n][2]u32, p : [2]u32) = (replicate n [0,0],[0,0]) for i < n do
      let new = S2.recurrent i p
      let arr = arr with [i] <- new
    in (arr, new)
  in map (\v -> map (\x -> f64.u32 x / S2.norm) v) tmp


let main (n:i32) : [n][2]f64 =
  indep n
  --recur n
