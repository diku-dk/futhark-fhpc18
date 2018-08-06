import "/futlib/sobol"
import "/futlib/sobol-dir-50"

module sobol = Sobol sobol_dir { let D = 1 }

let main2 (n:i32) : f64 =
  let xs = map (\v -> v[0]) (sobol.sobol n)
  in reduce (+) 0.0 xs / r64 n

let main (n:i32) : f64 =
  let norm = 2.0**32f64
  let xs = map (\i -> f64.u32((sobol.independent i)[0])/norm) (iota n)
  in reduce (+) 0.0 xs / r64 n