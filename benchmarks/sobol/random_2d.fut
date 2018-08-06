-- ==
-- entry: main
-- input { 1000 } output { 3.144000f64 }

import "/futlib/random"
import "/futlib/array"

module rng_reduce (rng : rng_engine with int.t = u32) = {

  let simulate 't (D:i32) (f:[]f64->t) (g:t->t->t) (ne:t) (N:i32) : t =
    let seed = [61i32]
    let rng = rng.rng_from_seed seed
    let rngs = rng.split_rng N rng
    let m : [N][D]f64 =
      map1 (\ rng -> let rngs' = rng.split_rng D rng
                     in map1 (\rng -> let (_,v) = rng.rand rng
                                      in (r64 (i32.u32 v) - r64 (i32.u32 rng.min)) /
                                         (r64 (i32.u32 rng.max) - r64 (i32.u32 rng.min))
                             ) rngs') rngs
    let ts : [N]t = map f m
    in reduce g ne ts

  let random (D:i32) (n:i32) : [n][D]f64 =
    let seed = [61i32]
    let rng = rng.rng_from_seed seed
    let rngs = rng.split_rng n rng
    let m : [n][D]f64 =
      map1 (\ rng -> let rngs' = rng.split_rng D rng
                     in map1 (\rng -> let (_,v) = rng.rand rng
                                      in (r64 (i32.u32 v) - r64 (i32.u32 rng.min)) /
                                         (r64 (i32.u32 rng.max) - r64 (i32.u32 rng.min))
                             ) rngs') rngs
    in m
}

module R = rng_reduce minstd_rand

let main (n:i32) : [n][2]f64 =
  R.random 2 n