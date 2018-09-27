import "lib/github.com/diku-dk/sobol/sobol"
import "lib/github.com/diku-dk/sobol/sobol-dir-50"

module sobol =
  Sobol sobol_dir { let D = 2 }

-- ==
-- entry: main
-- input { 1000000 } output { 3.142000f64 }

let main (n:i32) : f64 =
  let hits =
    map (\v -> if v[0]*v[0]+v[1]*v[1]<1.0 then 1.0
               else 0.0)
        (sobol.sobol n)
  in 4.0 * reduce (+) 0.0 hits / r64 n
