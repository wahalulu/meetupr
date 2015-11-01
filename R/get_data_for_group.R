#' Retrieves json objects for group membership, event and rsvp information
#' @param group_urlname The group name as shown in the meetup.com URL
#' @param meetup_token The token object
#' @return list of responses for group, members, profiles, events, and time
#' @export
get_data_for_group <- function(group_urlname, meetup_token) {
  cat(sprintf("Getting Group Info for %s.\n", group_urlname))
  group_response <- get_response(service = "groups",
                                 group_urlname = group_urlname,
                                 meetup_token = meetup_token
  )
  group_id <- content(group_response[[1]])$results[[1]]$id
  cat(sprintf("Getting Member Info for %s.\n", group_urlname))
  members_response <- get_response(service = "members",
                                   group_id = group_id,
                                   meetup_token = meetup_token,
                                   order = "joined"
  )
  cat(sprintf("Getting Profile Info for %s.\n", group_urlname))
  profiles_response <- get_response(service = "profiles",
                                    group_id = group_id,
                                    meetup_token = meetup_token,
                                    order = "joined"
  )
  cat(sprintf("Getting Event Info for %s.\n", group_urlname))
  events_response <- get_response(service = "events",
                                  group_id = group_id,
                                  meetup_token = meetup_token
  )

  events <-extract_result_dataframe(events_response)
  #profiles <- extract_result_dataframe_from_response_list(profiles_response)
  #members <- extract_result_dataframe_from_response_list(members_response)

  cat(sprintf("Getting RSVP Info for %s.\n", group_urlname))

  eventid_string <- lapply(split_vector(events$id, 200),
                           paste,
                           collapse = ",")

  rsvps_response <- lapply(eventid_string,
                           function(x)
                             get_response(service = "rsvps",
                                event_id = x,
                                group_id = group_id,
                                meetup_token = meetup_token))
  rsvps_response <- unlist(rsvps_response,recursive = F)

  list(group = group_response,
       members = members_response,
      profiles = profiles_response,
      events = events_response,
     rsvps = rsvps_response,
     timestamp = now()
       )
}
