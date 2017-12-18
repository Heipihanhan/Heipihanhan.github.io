#' Dispaly the welcome page.
#'
#' This function displays the welcome page of Declassification Engine API.
#'
#' @param return_type The format of content displayed, in string or html
#' @return The content of welcome page, in either string  or html format.
#' @examples
#' declass_welcome("html")
#' declass_welcome("string")
#' @export
declass_welcome <- function(return_type = "string"){
  url <- "http://api.declassification-engine.org/declass"
  message <- content(GET(url))
  if (return_type == "html") {
    return(message)
  } else if (return_type == "string") {
    text <- html_text(message)
    return(text)
  } else {
    notice <- "Return_type not supported"
    return(notice)
  }
}

