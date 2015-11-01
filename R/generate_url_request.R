#' Generates the URL for the API call
#' @param url_prefix
#' @param service
#' @param key
#' @param group_urlname
#' @param group_id
#' @param event_id
#' @param page
#' @param offset
#' @param order
#' @param authentication
#' @return URL for API call
#' @export
generate_url_request <- function (url_prefix = "https://api.meetup.com/2/",
                                  service,
                                  key = NULL,
                                  group_urlname = NULL,
                                  group_id = NULL,
                                  event_id = NULL,
                                  page = NULL,
                                  offset = NULL,
                                  order = NULL,
                                  authentication = "oauth") {
  url <- sprintf("%s%s?", url_prefix, service)
  key <- if (authentication != "oauth" & !is.null(key)) {
    sprintf("key=%s&sign=true", url_prefix, service, key)
  }
  group_urlname <- if (!is.null(group_urlname)) {
    sprintf("group_urlname=%s", group_urlname)
  }
  group_id  <- if (!is.null(group_id)) {
    sprintf("group_id=%s", as.character(group_id))
  }
  event_id <- if (!is.null(event_id)) {
    sprintf("event_id=%s", as.character(event_id))
  }
  page <- if (!is.null(page)) {
    sprintf("page=%d", page)
  }
  offset <- if (!is.null(offset)) {
    sprintf("offset=%d", offset)
  }
  order <- if(!is.null(order)) {
    sprintf("order=%s", order)
  }

  query_string <- paste(c(key, group_urlname, group_id, event_id, page, offset,
                          order),
                        collapse = "&")
  url <- paste0(url, query_string)

  if (service == "events") {
    url <- paste0(url, "&status=upcoming,past,proposed,suggested")
  }
  url
}
