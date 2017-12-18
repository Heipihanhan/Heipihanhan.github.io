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
