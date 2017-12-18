#' Return random document IDs with a limit.
#'
#' This function returns randomly sampled document IDs. It is intended for users who don't know what to look for, and it lets them explore and view the documents.
#'
#' @param limit The number of document IDs to be randomly sampled. Default setting is 10 ids. Maximum is set to be 1024, so as not to crash the API server.
#' @return Document IDs.
#' @examples
#' declass_query_random_ids(32)
#' declass_query_random_ids(1100)
#' @export
declass_query_random_ids <- function(limit = 10) {
  base_url <- "http://api.declassification-engine.org/declass/v0.4/random/?limit="
  if (limit <= 1024) {
    url <- paste(base_url, limit, sep = "")
    ids <- fromJSON(url)$results
    return(ids)
  } else {
    notice <- "Limit exceeds the maximum 1024"
    return(notice)
  }
}
