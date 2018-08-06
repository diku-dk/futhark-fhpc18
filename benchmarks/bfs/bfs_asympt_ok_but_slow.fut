-- A "flattened" version that respects both the work and depth asymptotic
-- but it is very slow indeed.
-- ==
--
-- tags { }
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- input @ data/512nodes_high_edge_variance.in.gz
-- output @ data/512nodes_high_edge_variance.out
-- input @ data/graph1MW_6.in.gz
-- output @ data/graph1MW_6.out.gz
-- input @ data/64kn_32e-var-1-256-skew.in.gz
-- output @ data/64kn_32e-var-1-256-skew.out

-- Paper's tests!
-- input @ data/bin-6kn_2ke-ct.in
-- output @ data/bin-6kn_2ke-ct.out
-- input @ data/bin-6kn_2ke-var.in
-- output @ data/bin-6kn_2ke-var.out
-- input @ data/bin-400kn_30e-ct.in
-- output @ data/bin-400kn_30e-ct.out
-- input @ data/bin-20kn_600e-var.in
-- output @ data/bin-20kn_600e-var.out
-- input @ data/graph1MW_6.in.gz
-- output @ data/graph1MW_6.out.gz
-- input @ data/64kn_32e-var-1-256-skew.in.gz
-- output @ data/64kn_32e-var-1-256-skew.out

import "/futlib/segmented"

let step [n][e]
        (cost: *[n]i32)
        (nodes_start_index: [n]i32)
        (nodes_n_edges: [n]i32)
        (edges_dest: [e]i32)
        (graph_visited: [n]bool)
        (graph_mask: *[n]bool)
        (updating_graph_mask: *[n]bool) : (*[n]i32, *[n]bool, *[n]bool) =
  let active_indices =
    filter (\i -> graph_mask[i]) (iota n)
  let n_indices = length active_indices
  let graph_mask' =
    scatter graph_mask active_indices (replicate n_indices false)

  -----------------------------------------------------------------------------
  -- BEGIN-FLATTENING: needed to ensure the asymptotic work/depth complexity --
  -----------------------------------------------------------------------------
  let active_costs   = map (\tid -> unsafe (cost[tid])) active_indices
  let active_edges   = map (\tid -> unsafe (nodes_n_edges[tid])) active_indices
  let scan_num_edges = scan (+) 0i32 active_edges
  let flat_len       = scan_num_edges[n_indices-1]
  let (tmp1, tmp2, tmp3) = (replicate flat_len false,
                            replicate flat_len 0i32,
                            replicate flat_len 1i32)
  let write_inds     = map (\i -> if i==0i32 then 0i32 else unsafe scan_num_edges[i-1]) (iota n_indices)
  let active_flags   = scatter tmp1 write_inds (replicate n_indices true)
  let track_nodes_tmp= scatter tmp2 write_inds (iota n_indices) --active_indices
  let active_starts  = map (\tid -> unsafe (nodes_start_index[tid])) active_indices
  let track_index_tmp= scatter tmp3 write_inds active_starts

  -- DOUBLE BUG in FUSION:
  -- 1) if the scans are separated (as in commented code)
  --    then they are NOT fused by compiler
  -- 2) even stranger, if I fuse them by hand as below, then
  --    the three scatter above are not fused (but they fuse
  --    the commented version in which the scans are separated)
  let (track_nodes, track_index) = unzip (
      segmented_scan (\(a,b) (c,d) -> (a+c,b+d)) (0,0) active_flags
                     (zip track_nodes_tmp track_index_tmp)           )
    let track_nodes    = segmented_scan (+) 0 active_flags track_nodes_tmp
    let track_index    = segmented_scan (+) 0 active_flags track_index_tmp
  -----------------------------------------------------------------------------
  -- END-FLATTENING:   needed to ensure the asymptotic work/depth complexity --
  -----------------------------------------------------------------------------

  let changes = map2(\row edge_index ->
                      let node_id = unsafe edges_dest[edge_index]
                      in  if !(unsafe graph_visited[node_id])
                          then (node_id, unsafe active_costs[row]+1)
                          else (-1, -1)
                    ) track_nodes track_index

  let (changes_node_ids, changes_costs) = unzip(changes)

  let cost' =
      scatter cost changes_node_ids changes_costs

  let updating_graph_mask' =
      scatter updating_graph_mask changes_node_ids (replicate flat_len true)

  in (cost', graph_mask', updating_graph_mask')

import "common"

let main = common_main step
