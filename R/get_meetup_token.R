#' Obtains the oath1.0 token required to get data from meetup.com
#' @param key Your meetup.com oauth key
#' @param secret Your meetup.com oauth secret key
#' @return token object required for other functions
#' @export
get_meetup_token <- function(key, secret) {
  app <- oauth_app("meetup", key = key, secret = secret)
  token <- oauth1.0_token(oauth_endpoint(request = "https://api.meetup.com/oauth/request/",
                                              authorize = "http://www.meetup.com/authorize",
                                              access = "https://api.meetup.com/oauth/access",
                                              base_url = "https://api.meetup.com"),
                               app)
  return(token)
}