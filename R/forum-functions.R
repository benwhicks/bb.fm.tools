# Functions for analysing forum data

#' fm graph
#'
#' Makes a graph from raw forum output.
#'
#' @param x A data frame. Needs the fields 'poster' and 'reply_to',
#' and also currently 'hit_count', 'posted_date' and 'msg_text'
#' @merge_edges T / F. If true edges are merged by other fields into a single edge
#' with aggregated context data.
#' @export
fm_graph <- function(x, merge_edges = TRUE){
    x$poster <- as.character(x$poster)
    x$reply_to <- as.character(x$reply_to)
    x$msg_text <- str_squish(x$msg_text)
    x <- x %>%
        mutate(msg_text = lakit::str_strip_html(msg_text))
    people = union(x$poster, x$reply_to)
    people <- people[people != ""]
    nodes <- data.frame(person = people) %>%
        tidyr::drop_na() %>%
        dplyr::left_join(x %>%
                             select(person = poster, poster_role),
                         by = "person") %>%
        dplyr::left_join(data.frame(
            poster_role = c("S", "T", "G", "P", "B", "U"),
            role = c("Student", "Teacher", "Grader", "Instructor", "Builder", "Guest")),
            by = "poster_role") %>%
        dplyr::rename(role_code = poster_role) %>%
        dplyr::mutate(role = factor(role, levels = c("Student", "Instructor", "Teacher", "Grader", "Builder", "Guest")))

    edges <- data.frame(
        from = x$poster,
        to = x$reply_to,
        hits = x$hit_count,
        time = x$posted_date,
        text = x$msg_text
    ) %>%
        tidyr::drop_na() %>%
        dplyr::filter(to != "")
    if (merge_edges) {
        edges <- edges %>%
            dplyr::group_by(from, to) %>%
            dplyr::summarise(hits = sum(hits, na.rm = TRUE),
                             time = mean(time, na.rm = TRUE)
                             # TODO: This not working :(
                              ,
                              text = pmap_chr(list(text), paste)[[1]]
                             )
    }
    g <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
    return(g)
}

# TODO: Triadic Census
# fm_triadic_census <- function(graph,
#                               remove_isolates = TRUE) {
#
# }

# TODO: Look at parallel edge graphs as an option for
# aggregating by which forum the link was from
fm_plot <- function(graph,
                    title = "SNA of forum",
                    remove_isolates = FALSE,
                    layout = "stress") {

    num_of_isolated_points <- graph %>%
        tidygraph::filter(tidygraph::node_is_isolated()) %>%
        tidygraph::activate(nodes) %>%
        tidyr::as_tibble() %>%
        pull(person) %>%
        unique() %>%
        length()

    if (remove_isolates) {
        g <- graph %>%
            tidygraph::activate(nodes) %>%
            tidygraph::filter(!tidygraph::node_is_isolated())
    } else {
        g <- graph
    }

    p <- g %>%
        ggraph::ggraph(layout = layout) +
        ggraph::geom_node_point(aes(colour = role), alpha = 0.7, size = 3) +
        ggraph::geom_edge_link(aes(alpha = hits, color = as.numeric(time)),
                               end_cap = circle(2, 'mm'),
                               start_cap = circle(2, 'mm')) +
        ggraph::scale_edge_alpha(guide = "none")

    if (remove_isolates) {
        p <- p + ggplot2::ggtitle(
            title,
            subtitle = paste0(num_of_isolated_points,
                              " isolated points removed."))
    } else {
        p <- p + ggplot2::ggtitle(title)
    }
    p <- p +
        ggraph::theme_graph() +
        ochRe::scale_colour_ochre(palette = "nolan_ned") +
        ggraph::scale_edge_color_continuous(low = "royalblue3",
                                            high = "orangered1",
                                            guide = "none")

    return(p)
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
