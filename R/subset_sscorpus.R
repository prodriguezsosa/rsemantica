#' Subset and sscorpus by cue and tags.
#'
#' @param sscorpus sscorpus object.
#' @param cue_set the cues of interest.
#' @param tag_set the tags that characterize the subjects of interest.
#' @param tags_AND logical function specifying whether tags apply with an AND or an OR condition (e.g. 'republican' and/or 'male').
#' @return an sscorpus (subsetted if specified).
#' @export
subset_sscorpus <- function(sscorpus, cue_set = NULL, tag_set = NULL, tags_AND = TRUE) {
    if (all(c(is.null(cue_set), is.null(tag_set)))) {
        print("both ss_cue and tag_set are NULL, returning original sscorpus")
        return(sscorpus)
    } else {
        # subset cue if defined, check it is part of set
        if (!is.null(cue_set)) {
            # subset fluency data
            sscorpus_subset <- sscorpus
            fluency <- sscorpus$fluency %>% filter(cue %in% cue_set)
            sscorpus_subset[["fluency"]] <- fluency
        }
        
        # subset by tags if defined
        if (!is.null(tag_set)) {
            # if tags are defined, identify relevant observations
            if (tags_AND) {
                obs <- lapply(tag_set, function(x) grepl(paste0("\\<", x, "\\>"), sscorpus_subset$tags$tags)) %>% do.call(cbind, .) %>% apply(., 1, all)
            } else {
                # identify obs for which ALL the tags apply
                obs <- lapply(tag_set, function(x) grepl(paste0("\\<", x, "\\>"), sscorpus_subset$tags$tags)) %>% do.call(cbind, .) %>% apply(., 1, any)  # else identify obs for which ANY of the tags apply
            }
            # subset data
            obs <- sscorpus_subset$tags$pid[obs]
            sscorpus_subset <- lapply(sscorpus_subset, function(x) x %>% filter(pid %in% obs))
        }
        return(sscorpus_subset)
    }
}

#' Subset and sscorpus by ids (used in classifier).
#'
#' @param sscorpus sscorpus object.
#' @param ids chracter vector of pids.
#' @return an sscorpus (subsetted if specified).
#' @export
subset_sscorpus_ids <- function(sscorpus, ids) {
    sscorpus_subset <- sscorpus
    sscorpus_subset <- lapply(sscorpus_subset, function(x) x %>% filter(pid %in% ids$pid))
    return(sscorpus_subset)
}

