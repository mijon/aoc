library(DiagrammeR)
source("08_solution.R")

nodesdf <- parse_nodes_df(input[3:length(input)])

ndf <- nodesdf |> transmute(id = 1:n(), label = name) 
edf <- bind_rows(
  nodesdf |> select(from_label = name,
                    to_label = left) |>
    left_join(ndf |> rename(from_label = label, from = id), by = "from_label") |>
    left_join(ndf |> rename(to_label = label, to = id), by = "to_label"),
  nodesdf |> select(from_label = name,
                    to_label = right) |>
    left_join(ndf |> rename(from_label = label, from = id), by = "from_label") |>
    left_join(ndf |> rename(to_label = label, to = id), by = "to_label")
) |> select(from, to)

g <- create_graph() |>
  add_nodes_from_table(ndf, label_col = label) |>
  add_edges_from_table(edf, from_col = from, to_col = to, from_to_map = id_external)


g |>
  mutate_node_attrs(final_node = endsWith(label, "Z")) |>
  mutate_node_attrs(start_node = endsWith(label, "A")) |>
  select_nodes(conditions = final_node == TRUE) |>
  set_node_attrs_ws(node_attr = fillcolor, value = "orange") |>
  clear_selection() |>
  select_nodes(conditions = start_node == TRUE) |>
  set_node_attrs_ws(node_attr = fillcolor, value = "red") |>
  clear_selection() |>
  render_graph()
