import "lib/github.com/diku-dk/sobol/sobol"
import "lib/github.com/diku-dk/sobol/sobol-dir-50"

module sobol = Sobol sobol_dir { let D = 1 }

-- ==
-- entry: chunked
-- input { 1000000 }
entry chunked (n:i32) =
  map (\v -> v[0]) (sobol.sobol n) |> f64.sum

-- ==
-- entry: independent
-- input { 1000000 }
entry independent (n:i32) =
  let norm = 2.0**32f64
  in map (\i -> f64.u32((sobol.independent i)[0])/norm) (iota n) |> f64.sum
