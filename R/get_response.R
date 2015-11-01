#' Makes the GET call to the meetup API
#' @param service
#' @param group_urlname
#' @param meetup_token
#' @param default_ct
#' @param group_id
#' @param order
#' @param event_id
#' @param page
#' @param offset
#' @return Response object for the URL
#' @export
get_response <- function(service,
                         group_urlname = NULL,
                         meetup_token,
                         default_ct = 800,
                         group_id = NULL,
                         order = NULL,
                         event_id = NULL,
                         offset = NULL) {

  request <- generate_url_request(service = service,
                                  group_urlname = group_urlname,
                                  group_id = group_id,
                                  event_id = event_id,
                                  order = order,
                                  offset = offset)
  cat(sprintf("Getting %s\n", request))
  response <- GET(request,
                  config(token = meetup_token))

  ct <- content(response)$meta$total_count

  response <- list(response)

  if(ct > default_ct) {
    splits <- floor(ct / default_ct)
    additional_requests <- lapply(1:splits,
                                  function(i)
                                    generate_url_request(service = service,
                                                         group_urlname = group_urlname,
                                                         group_id = group_id,
                                                         event_id = event_id,
                                                         order = order,
                                                         offset = i))

    additional_response <- lapply(additional_requests,
                                  function(x) {
                                    cat(sprintf("Getting %s\n", x))
                                    GET(x,
                                        config(token = meetup_token))
                                    })
    response <- c(response, additional_response)
  }
  response
}
