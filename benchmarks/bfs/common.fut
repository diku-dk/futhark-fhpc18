-- The outer driving loop is common to all BFS implementations.

type step_fn =  (cost: *[]i32)
             -> (nodes_start_index: []i32)
             -> (nodes_n_edges: []i32)
             -> (edges_dest: []i32)
             -> (graph_visited: []bool)
             -> (graph_mask: *[]bool)
             -> (updating_graph_mask: *[]bool)
             -> (*[]i32, *[]bool, *[]bool)

let common_main [n][e] (step: step_fn)
                       (nodes_start_index: [n]i32)
                       (nodes_n_edges: [n]i32)
                       (edges_dest: [e]i32) : [n]i32 =
    let source = 0
    let is_source = map (==source) (iota n)
    let (graph_mask, graph_visited, cost) = (pick is_source (replicate n true) (replicate n false),
                                             pick is_source (replicate n true) (replicate n false),
                                             pick is_source (replicate n 0) (replicate n (-1)))
    let (cost,_,_,_,_) =
      loop (cost, graph_mask, graph_visited, updating_graph_mask, continue) =
           (cost, graph_mask, graph_visited, replicate n false, true)
      while continue do
        let (cost', graph_mask', updating_graph_mask') =
          step cost nodes_start_index nodes_n_edges edges_dest
               graph_visited graph_mask updating_graph_mask

        let step2_inds = map (\i -> if updating_graph_mask'[i] then i else (-1)) (iota n)

        let graph_visited' =
            scatter graph_visited step2_inds (replicate n true)

        let graph_mask'' =
            scatter graph_mask' step2_inds (replicate n true)

        let updating_graph_mask'' =
            scatter updating_graph_mask' step2_inds (replicate n false)

        let continue_indices = map (\x -> if x>=0 then 0 else -1) step2_inds
        let continue' =
            scatter [false] continue_indices (replicate n true)

        in (cost', graph_mask'', graph_visited', updating_graph_mask'', continue'[0])

    in cost
