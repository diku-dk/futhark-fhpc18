import "../lib/github.com/diku-dk/cpprandom/random"

module rng_reduce (rng : rng_engine) = {

  module int = rng.int
  let a64 = int.to_i64 >-> f64.i64

  let redomap 't (D: i32) (f:[]f64->t) (g:t->t->t) (ne:t) (N:i32) : t =
    let seed = [61i32]
    let rng = rng.rng_from_seed seed
    let rngs = rng.split_rng N rng
    let m : [N][D]f64 =
      map1 (\ rng -> let rngs' = rng.split_rng D rng
                     in map1 (\rng -> let (_,v) = rng.rand rng
                                      in a64(v int.- rng.min) / a64(rng.max int.- rng.min)) rngs') rngs
    let ts : [N]t = map1 (\(vec : [D]f64) -> f vec) m
    in reduce g ne ts
}
