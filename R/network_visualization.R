#' Visualize semantic network.
#'
#' @param model_parameters list of length 2: (a) pi = numeric vector of initial probabilities (b) P = numeric matrix of transition probabilities.
#' @param mode logical indicating whether network is 'directed'/'undirected' (note, models are directed but using undirected can help visualization).
#' @param cluster logical indicating whether to evaluate the presence of clusters (currently only a 3-step walktrap algorithm is available).
#' @param average logical indicating whether to to and from edges should be averaged (becoming an undirected network)
#' @param threshold a threshold applied to edges (edges below the threshold are left transparent).
#' @param edge.width.mult a multiplier to increase the width of edges.
#' @param node.size.mult a multiplier of node.size to greater differentiate between different initial probabilities.
#' @param node.size.sum a constant added to node.size to increase the baseline size of nodes.
#' @return visualization of semantic network.
#' @export
visualize_sn <- function(model_parameters, mode = "undirected", average = FALSE, cluster = TRUE, threshold = NULL, edge.width.mult = 15, node.size.mult = 115, node.size.sum = 0.03) {
    # get model parameters
    initial <- model_parameters[["pi"]]
    transitions <- model_parameters[["P"]]
    # average
    if (average) {
        transitions <- (transitions + t(transitions))/2
        
    }
    # get graph
    net <- graph_from_adjacency_matrix(transitions, mode = mode, weighted = TRUE, diag = FALSE)
    # compute clusters if TRUE
    if (cluster) {
        net_groups <- cluster_walktrap(net, weights = E(net)$weight, steps = 3, membership = TRUE)
        # initialize color palette
        color_palette <- brewer.pal(max(3, max(membership(net_groups))), "Set3")
        # cluster tbl
        clusters <- tibble(state = colnames(transitions), cluster = membership(net_groups), color = color_palette[membership(net_groups)])
        # arrange by clusters
        clusters <- clusters %>% arrange(cluster)
        # arrange transitions to group clustered states in viz
        transitions <- transitions[clusters$state, clusters$state]
        initial <- initial[clusters$state]
        net <- graph_from_adjacency_matrix(transitions, mode = mode, weighted = TRUE, diag = FALSE)
    }
    if (!is.null(threshold)) {
        transitions[transitions < threshold] <- 0
        net <- graph_from_adjacency_matrix(transitions, mode = mode, weighted = TRUE, diag = FALSE)
    }
    
    # label locations kudos to: https://gist.github.com/ajhmohr/5337a5c99b504e4a243fad96203fa74f
    V(net)$name = names(initial)
    V(net)$size = (initial + node.size.sum) * node.size.mult  # node size a function of initial probability
    la <- layout_in_circle(net)  # lay out as circle
    par(mar = c(9, 7, 7, 7))
    # plot
    if (cluster) {
        plot(net, layout = la, edge.color = "darkgreen", edge.width = (E(net)$weight) * edge.width.mult, vertex.color = clusters$color, edge.curved = TRUE, vertex.label = "", edge.arrow.width = 2, 
            edge.arrow.size = 0.05)
    } else {
        plot(net, layout = la, edge.color = "darkgreen", edge.width = (E(net)$weight) * edge.width.mult, vertex.label = "", edge.curved = TRUE, edge.arrow.width = 2, edge.arrow.size = 0.05)
    }
    
    # apply labels manually x and y coordinates of labels, adjust outward as desired
    x = la[, 1] * 1.3
    y = la[, 2] * 1.3
    
    # create vector of angles for text based on number of nodes (flipping the orientation of the words half way around so none appear upside down)
    angle = ifelse(atan(-(la[, 1]/la[, 2])) * (180/pi) < 0, 90 + atan(-(la[, 1]/la[, 2])) * (180/pi), 270 + atan(-la[, 1]/la[, 2]) * (180/pi))
    
    # apply the text labels with a loop with angle as srt
    for (i in 1:length(x)) {
        text(x = x[i], y = y[i], labels = V(net)$name[i], adj = NULL, pos = NULL, cex = 1, col = "black", srt = angle[i], xpd = T)
    }
}

#' Visualize semantic network difference (difference in initial and transition probabilities given parameters for two groups.)
#'
#' @param model_parameters list of length 2: (a) pi = numeric vector of initial probabilities (b) P = numeric matrix of transition probabilities.
#' @param mode whether network is 'directed'/'undirected' (note, models are directed but using undirected can help visualization).
#' @param threshold a threshold applied to edges (edges below the threshold are left transparent).
#' @param edge.width.mult a multiplier to increase the width of edges.
#' @param node.size.mult a multiplier of node.size to greater differentiate between different initial probabilities.
#' @param node.size.sum a constant added to node.size to increase the baseline size of nodes.
#' @return visualization of semantic network.
#' @export
visualize_diff <- function(model_parameters, groups, mode = "undirected", threshold = NULL, edge.width.mult = 8, node.size.mult = 115, node.size.sum = 0.03) {
    # get model parameters diffs
    initial <- model_parameters[[groups[1]]][["pi"]] - model_parameters[[groups[2]]][["pi"]]
    initial <- initial[order(initial)]
    transitions <- model_parameters[[groups[1]]][["P"]] - model_parameters[[groups[2]]][["P"]]
    transitions <- transitions[names(initial), names(initial)]
    diag(transitions) <- 0
    
    # get graph
    if (!is.null(threshold)) {
        transitions[abs(transitions) < threshold] <- 0
        net <- graph_from_adjacency_matrix(abs(transitions), mode = mode, weighted = TRUE, diag = FALSE)
    } else {
        net <- graph_from_adjacency_matrix(abs(transitions), mode = mode, weighted = TRUE, diag = FALSE)
    }
    # label locations kudos to: https://gist.github.com/ajhmohr/5337a5c99b504e4a243fad96203fa74f
    V(net)$name = names(initial)
    V(net)$size = (abs(initial) + node.size.sum) * node.size.mult  # node size a function of initial probability
    la <- layout_in_circle(net)  # lay out as circle
    vertex.color = ifelse(initial > 0, "blue", "red")
    edge_colors <- transitions
    edge_colors[transitions < 0] <- "blue"
    edge_colors[transitions > 0] <- "red"
    edge_colors[transitions == 0] <- rgb(0, 0, 255, max = 255, alpha = 0, names = "darkgreen")
    edge.color = edge_colors
    
    # plot
    par(mar = c(9, 7, 7, 7))
    plot(net, layout = la, edge.color = edge.color, edge.width = E(net)$weight * edge.width.mult, vertex.color = vertex.color, edge.curved = TRUE, vertex.label = "", edge.arrow.width = 2, edge.arrow.size = 0.05)
    
    # apply labels manually x and y coordinates of labels, adjust outward as desired
    x = la[, 1] * 1.3
    y = la[, 2] * 1.3
    
    # create vector of angles for text based on number of nodes (flipping the orientation of the words half way around so none appear upside down)
    angle = ifelse(atan(-(la[, 1]/la[, 2])) * (180/pi) < 0, 90 + atan(-(la[, 1]/la[, 2])) * (180/pi), 270 + atan(-la[, 1]/la[, 2]) * (180/pi))
    
    # apply the text labels with a loop with angle as srt
    for (i in 1:length(x)) {
        text(x = x[i], y = y[i], labels = V(net)$name[i], adj = NULL, pos = NULL, cex = 1, col = "black", srt = angle[i], xpd = T)
    }
}
