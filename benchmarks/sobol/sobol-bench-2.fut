let gray_code (x: i32): i32 = (x >> 1) ^ x

let test_bit (x: i32) (ind: i32): bool =
  let t = (1 << ind) in (x & t) == t

let sobol_ind [n] (dir_v: [n]i32) (x: i32): i32 =
  let reldv_vals =
    map2 (\dv i ->
           if test_bit (gray_code x) i
           then dv else 0)
        dir_v (iota n)
  in reduce (^) 0 reldv_vals

let index_of_least_significant_0 (x: i32): i32 =
  loop i = 0 while i < 32 && ((x>>i)&1) != 0 do i + 1

let rec_m [n] (dir_v: [n]i32) (i: i32): i32 =
  let bit = index_of_least_significant_0 i
  in unsafe dir_v[bit]

let sobol_chunk [n] (dir_v: [n]i32) (x: i32) (chunk: i32): [chunk]f32 =
  let divisor = 2.0 ** r32 n
  let sob_beg = sobol_ind dir_v (x+1)
  let contrbs = map (\i ->
                        if i==0 then sob_beg
                        else rec_m dir_v (i+x))
                    (iota chunk)
  let vct_i32s= scan (^) 0 contrbs
  in map (\y -> r32 y / divisor) vct_i32s

let dir_v = [536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608, 4194304, 2097152, 1048576, 524288, 262144, 131072, 65536, 32768, 16384, 8192, 4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1]

-- ==
-- entry: independent chunked stream_map cpprandom
-- input { 30000000 }

entry stream_map (n: i32): f32 =
  let sobol_nums =
    stream_map (\[c] (xs: [c]i32): [c]f32 ->
                  sobol_chunk dir_v (if c == 0 then 0 else unsafe xs[0]) c)
               (iota n)
  in reduce (+) 0.0 sobol_nums

entry chunked (n: i32): f32 =
  let sobol_nums = sobol_chunk dir_v 0 n
  in reduce (+) 0.0 sobol_nums

entry independent (n:i32) =
  let norm = 2.0**r32 n
  in map (\i -> f32.i32(sobol_ind dir_v i)/norm) (iota n) |> f32.sum

import "lib/github.com/diku-dk/cpprandom/random"
module R = minstd_rand
entry cpprandom (n:i32) =
  let rngs = R.rng_from_seed [n] |> R.split_rng n
  in map R.rand rngs |> map (.2) |> u32.sum
