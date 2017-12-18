
#' List collections.
#'
#' This functions lists all collections that're avaialble in the Declassification Engine API.
#'
#' @param return_type The format of content displayed, in list or raw.
#' @return The name of collections.
#' @examples
#' declass_collection("list")
#' declass_collection("raw")
#' @export
declass_collection <- function(return_type = "list"){
  url <- "http://api.declassification-engine.org/declass/v0.4/collections"
  parsed_json <- fromJSON(url)
  collections <- parsed_json$results$collections
  if (return_type == "list") {
    return(collections)
  } else if (return_type == "raw") {
    raw <- content(GET(url))
    return(raw)
  } else {
    notice <- "Return_type not supported"
    return(notice)
  }
}
