#' Retrieves json objects for group membership, event and rsvp information
#' and creates dataframes
#' @param group_urlname The group name as shown in the meetup.com URL
#' @param meetup_token The token object
#' @return list of data.frames for group, members, profiles, events, and time
get_and_process_group_data <- function(group_urlname, meetup_token) {
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
                                  meetup_token = meetup_token,
  )

  events <-extract_result_dataframe_from_response_list(events_response)
  profiles <- extract_result_dataframe_from_response_list(profiles_response)
  members <- extract_result_dataframe_from_response_list(members_response)

  cat(sprintf("Getting RSVP Info for %s.\n", group_urlname))

  eventid_string <- paste(events$id, collapse = ",")
  rsvps_response <- get_response(service = "rsvps",
                                event_id = eventid_string,
                                group_id = group_id,
                                meetup_token = meetup_token)
  rsvps <- extract_result_dataframe_from_response_list(rsvps_response)

  cat("Processing the rest of the results.\n")

  group <- extract_result_dataframe_from_response_list(group_response) %>%
    rename(url_name = urlname) %>%
    mutate(created = get_real_time(created),
           url_name = tolower(url_name))

  answers <- if(is.null(profiles$answers)) {
    NULL
  } else {
    profiles %>%
    rename(url_name = group.urlname) %>%
    mutate(url_name = tolower(url_name)) %>%
    group_by(url_name, member_id) %>%
    do(data.frame(#member_id = .$member_id,
      #url_name = .$group.urlname,
      unnest(.$answers)))
  }

  m <- members %>% select(country, city, state, joined, lon, member_name = name, visited, member_id = id,
                                            lat, bio, starts_with("other_"))
  p <- profiles %>% select(member_id, updated, group_name = group.name,
                             url_name = group.urlname)

  current_timestamp <- now()

  membership <- inner_join(m,p, by = "member_id") %>%
    mutate(url_name = tolower(url_name),
           joined = get_real_time(joined),
           visited = get_real_time(visited),
           time_since_last_visit = difftime(current_timestamp, visited, units = "days"))

  if(!"announced" %in% names(events)) events$announced <- NA

  events <- events %>%
    select(event_id = id,
           event_name = name,
           event_status = status,
           utc_offset, headcount, waitlist_count,
           maybe_rsvp_count,
           name,
           yes_rsvp_count,
           announced,
           time,
           url_name = group.urlname,
           group_name = group.name) %>%
    mutate(time = get_real_time(time),
           url_name = tolower(url_name))

  rsvps <- rsvps %>%
    select(created, response, modified = mtime, member_id = member.member_id,
           url_name = group.urlname,
           event_id = event.id,
           event_name = event.name) %>%
    mutate(created = get_real_time(created),
           modified = get_real_time(modified),
           original_rsvp = created == modified,
           url_name = tolower(url_name)
    )

  all_dates <- data.frame(url_name = group_urlname,
                          date = seq.Date(as.Date(group$created),
                                          as.Date(current_timestamp),
                                          1))


  list(group = group,
       membership = membership,
       events = events,
       rsvps = rsvps,
       answers = answers,
       all_dates = all_dates,
       timestamp = current_timestamp,
       responses = list(group = group_response,
                        members = members_response,
                        profiles = profiles_response,
                        events = events_response,
                        rsvps = rsvps_response))
}

