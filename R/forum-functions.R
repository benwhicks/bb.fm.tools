# Functions for analysing forum data

#' make fm graph
#'
#' Makes a graph from raw forum output
#'
#' @param x A data frame
#' @export
make_fm_graph <- function(x, merge_edges = TRUE){
    people = union(x$poster, x$reply_to)
    nodes <- data.frame(person = people) %>%
        tidyr::drop_na()
    edges <- data.frame(
        from = x$poster,
        to = x$reply_to,
        hits = x$hit_count,
        time = x$posted_date,
        text = x$msg_text
    ) %>%
        tidyr::drop_na()
    if (merge_edges) {
        edges <- edges %>%
            dplyr::group_by(from, to) %>%
            dplyr::summarise(hits = sum(hits, na.rm = TRUE),
                             time = mean(time, na.rm = TRUE)
                             # ,
                             # text = dplyr::paste(text, sep = "\n:::new post:::\n")
                             )
    }
    g <- tidygraph::tbl_graph(nodes = nodes, edges = edges)
    return(g)
}

#' plot3d_graph
#'
#' Takes a network object and plots a 3d hairball plot.
#'
#' @param graph A network object
#' @export
#'
#' Ideas: Could include triads as triangles, using rgl
#' package and segments3d for lines and triangles3d for
#' the triangles...
plot3d_graph <- function(graph) {
    # Getting 3d node positions
    np <- igraph::layout.fruchterman.reingold(graph, dim = 3)
    old_nodes <- graph %>%
        tidygraph::activate(nodes) %>%
        dplyr::as_tibble()
    nodes <- dplyr::bind_cols(
        old_nodes,
        np %>% as_tibble(.name_repair = "minimal")
    )
    names(nodes) <- c(names(old_nodes), c('x', 'y', 'z'))
    edges <- graph %>%
        tidygraph::activate(edges) %>%
        dplyr::as_tibble()
    from.xyz <- nodes[edges$from, 2:4]
    to.xyz <- nodes[edges$to, 2:4]
    x.seg <- map2_df(from.xyz$x, to.xyz$x, c)
    y.seg <- map2_dbl(from.xyz$y, to.xyz$y, c)
    z.seg <- map2_dbl(from.xyz$z, to.xyz$z, c)
}
