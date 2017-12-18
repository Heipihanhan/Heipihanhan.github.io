#' Display entities for a collection.
#'
#' This functions display all available entities for a specified collection.
#'
#' @param collection The collection name.
#' @return The entities for the collection.
#' @examples
#' declass_collection_entity("clinton")
#' declass_collection_entity("kissinger")
#' declass_collection_entity("frus")
#' @export
declass_collection_entity <- function(collection) {
  if (collection %in% c("cpdoc", "clinton", "kissinger", "statedeptcables", "frus", "ddrs", "cabinet", "pdb")) {
    url <- paste("http://api.declassification-engine.org/declass/v0.4/entity_info/?collection=", collection, sep = "")
    parsed_json <- fromJSON(url)
    collection <- parsed_json$results$entity
    return(collection)
  } else {
    notice <- "Collection name and entities not found"
    return(notice)
  }
}
