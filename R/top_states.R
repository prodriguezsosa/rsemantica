#' Identify set of top tokens based on counts.
#'
#' @param sscorpus sscorpus object.
#' @param N the number of top tokens to identify.
#' @param cue_set the cues of interest.
#' @param tag_set the tags that characterize the subjects of interest.
#' @param tags_AND logical function specifying whether tags apply with an AND or an OR condition (e.g. "republican" and/or "male").
#' @param min_count integer value specifying if only states with a given minimum count should be selected.
#' @param remove_cue logical value specifying whether cue should be removed from the top of every list.
#' @param min_states an integer specifying a minimum semantic fluency list length.
#' @return a data.frame with 2 columns: (1) fluency = token (2) n = count.
#' @export
top_states <- function(sscorpus, N = NULL, cue_set = NULL, tag_set = NULL, tags_AND = TRUE, remove_cue = TRUE, min_count = NULL){
  fluency <- sscorpus[["fluency"]]  # extract fluency data

  # subset by cue if specified -------------------------------------------------------
  if(!is.null(cue_set)){fluency <- fluency %>% filter(cue %in% cue_set)}

  # remove cue from top of lists if remove_cue = TRUE --------------------------------
  if(remove_cue){fluency <- fluency %>% group_by(pid) %>% slice(-1) %>% ungroup()}

  # subset by tags if specified ------------------------------------------------------
  if(is.null(tag_set)){  # if no tags are specified, identify top tokens for full data
    top_states <- fluency %>%
      group_by(fluency) %>%  # group by states
      tally(sort = TRUE)
  }else{  # if a tag set is specified, identify pids with the relevant tags
    if(tags_AND){
      obs <-  lapply(tag_set, function(x) grepl(paste0("\\<", x, "\\>"), sscorpus$tags$tags)) %>% do.call(cbind, .) %>% apply(., 1, all)}else{  # identify obs for which ALL the tags apply
      obs <-  lapply(tag_set, function(x) grepl(paste0("\\<", x, "\\>"), sscorpus$tags$tags)) %>% do.call(cbind, .) %>% apply(., 1, any)  # else identify obs for which ANY of the tags apply
      }
    obs <-sscorpus$tags$pid[obs]
    top_states <- fluency %>%
      filter(pid %in% obs) %>%  # keep only subjects that fit the tags
      group_by(fluency) %>%  # group by states
      tally(sort = TRUE)}  # tally each token and order by tally

  # subset by min_states if specified ---------------------------------------------------
  if(!is.null(min_count)){top_states <- top_states %>% filter(n > min_count)}

  # subset by top N if specified --------------------------------------------------------
  if(!is.null(N)){top_states <- top_states %>% slice(1:min(N, nrow(top_states)))}

  # keep only fluency data
  #top_states <- top_states %>% select(fluency) %>% unlist() %>% unname()
  return(top_states)
}
