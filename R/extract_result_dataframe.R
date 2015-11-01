#' Extracts and flattens/unnests a data.frame from a JSON response
#' @param l A list of JSON responses of the same kind
#' @return A dataframe
extract_result_dataframe <- function(l) {
  if(!is.list(l)) stop("Object must be a list of responses!")
  df_list <- lapply(l, function(x) {
    if(identical(fromJSON(content(x, "text"))$results, list())) {
      NULL
    } else {
      flatten(fromJSON(content(x, "text"))$results)
    }
  })
  bind_rows(df_list)
}
