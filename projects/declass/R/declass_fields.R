

#' Display document metadata fields.
#'
#' This function displays the document metadata fields for all collections.
#'
#' @param return_type The format of content displayed, in list or raw.
#' @return The metadata fields for all collections of data.
#' @examples
#' declass_fields("list")
#' declass_fields("raw")
#' @export
declass_fields <- function(return_type = "list") {
  url <- "http://api.declassification-engine.org/declass/v0.4/fields"
  if (return_type == "list") {
    parsed_json <- fromJSON(url)
    fields <- parsed_json$results$fields
    return(fields)
  } else if (return_type == "raw") {
    raw <- content(GET(url))
    return(raw)
  } else {
    notice <- "Return_type not supported"
    return(notice)
  }
}

