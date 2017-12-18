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



#' Query document by ID.
#'
#' This function queries a specific document or documents by user-inputed ID. It is intended for users who know what they want and have specific IDs in mind.
#'
#' @param id_list A list of IDs
#' @return Content of the entire document
#' @examples
#' declass_query_id_data("Clinton-74931")
#' declass_query_id_data("1977LONDON17830,1975MOSCOW15238,P790162-1195")
#' @export
declass_query_id_data <- function(id_list) {
  base_url <- "http://api.declassification-engine.org/declass/v0.4/?ids="
  url <- paste(base_url, id_list, sep = "")
  doc <- fromJSON(url)
  return(doc)
}




#' Query documents based on randomly sampled IDs.
#'
#' This function queries documents by randomly sampled IDs.
#'
#' @param limit The number of randomply sampled IDs.
#' @return Content of the documents
#' @examples
#' declass_query_random_ids_data(3)
#' @export
declass_query_random_ids_data <- function(limit) {
  ids <- declass_query_random_ids(limit)
  id_list <- paste(ids$id, collapse = ",")
  id_list <- URLencode(id_list)
  return (declass_query_id_data(id_list))
}
