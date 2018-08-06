import "/futlib/sobol"
import "/futlib/sobol-dir-50"

module sobol =
  Sobol sobol_dir { let D = 2 }

let main (n:i32) : f64 =
  let hits =
    map (\v -> if v[0]*v[0]+v[1]*v[1]<1.0 then 1.0
               else 0.0)
        (sobol.sobol n)
  in 4.0 * reduce (+) 0.0 hits / r64 n
