#' Query data of an entity in a collection.
#'
#' This functions queries data of a specified entitiy in a specified collection.
#'
#' @param collection The collection name
#' @param entity The entity name
#' @param page_size The number of rows displayed per page. Default setting is 25 records per page.
#' @param page The starting page number to be queried. Default setting is page No.1.
#' @param num_pages The number of pages to be quried. Default setting is 10 pages.
#' @return Fields of data for the specified entity in that collection, and urls.
#' @examples
#' declass_entity_data("kissinger", "countries", page_size = 30, page = 1, num_pages = 5)
#' @export
declass_entity_data <- function(collection, entity, page_size = 25, page = 1, num_pages = 10) {
  url <- paste("http://api.declassification-engine.org/declass/v0.4/entity_info/?collection=", collection, "&entity=", entity, "&page_size=", page_size, "&page=", page, sep = "")

  if (collection %in% c("cpdoc",  "kissinger", "cabinet") & entity %in% c("countries", "persons", "topics")) {
  }
  else if (collection %in% c("statedeptcables", "frus", "ddrs", "pdb") & entity %in% c("countries", "persons", "topics", "classifications")) {
  }
  else if (collection == "clinton" & entity %in% c("countries", "persons", "classifications")) {
  }
  else {
    notice <- "Collection or entity name not found"
    return(notice)
  }
  print(url)
  parsed_json <- fromJSON(url)
  entity_data <- parsed_json$results
  num_pages <- num_pages - 1

  while (is.null(parsed_json$next_page) == FALSE & num_pages > 0) {
    print(parsed_json$next_page)
    parsed_json <- fromJSON(parsed_json$next_page)
    entity_data <- rbind(entity_data, parsed_json$results)
    num_pages <- num_pages - 1
  }
  return(entity_data)
}
