#' Converts the response objects obtained by the get_data_for_group function
#' into a list of data.frames. Also merges the membership and profiles.
#' @param grl A group response list generated by the get_data_for_group function
#' @return A list of data.frames of group, membership, rsvps, event info
#' @export

process_group_response <- function(grl) { #grl is group_response_list

  events <- extract_result_dataframe(grl$events)
  profiles <- extract_result_dataframe(grl$profiles)
  members <- extract_result_dataframe(grl$members)
  rsvps <- extract_result_dataframe(grl$rsvps)
  group <- extract_result_dataframe(grl$group) %>%
    rename(url_name = urlname) %>%
    mutate(created = get_real_time(created),
           url_name = tolower(url_name))

#   answers <- if(is.null(profiles$answers)) {
#     NULL
#   } else {
#     profiles %>%
#     rename(url_name = group.urlname) %>%
#     mutate(url_name = tolower(url_name)) %>%
#     group_by(url_name, member_id) %>%
#     do(data.frame(unnest(.$answers)))
#   }

  m <- members %>% select(country, city, state, joined, lon, member_name = name, visited, member_id = id,
                                            lat, bio, starts_with("other_"))
  p <- profiles %>% select(member_id, updated, group_name = group.name,
                             url_name = group.urlname)

  current_timestamp <- grl$timestamp

  membership <- inner_join(m,p, by = "member_id") %>%
    mutate(url_name = tolower(url_name),
           joined = get_real_time(joined),
           visited = get_real_time(visited))


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

  list(group = group,
       membership = membership,
       events = events,
       rsvps = rsvps,
       # answers = answers,
       timestamp = current_timestamp)
}

