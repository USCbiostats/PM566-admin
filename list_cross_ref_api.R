library(data.table)
library(httr)
library(stringr)


list_cross_ref_api <- function(
  issue,
  repo    = "USCbiostats/PM566",
  baseurl = "https://api.github.com/repos",
  ...
  ) {
  
  # Checking the arguments of curl -X GET
  dots <- list(...)
  if ("config" %in% names(dots))
    dots$config <- c(dots$config, httr::add_headers(
      Accept = "application/vnd.github.mockingbird-preview+json"
    ))
  else
    dots$config <- httr::add_headers(
      Accept = "application/vnd.github.mockingbird-preview+json"
    )
  
  dots$url <- sprintf("%s/%s/issues/%d/timeline", baseurl, repo, issue)
  
  response <- tryCatch(do.call(httr::GET, dots), error = function(e) e)
  
  if (inherits(response, "error")) {
    warning("httr::GET failed.")
    return(NULL)
  }
  
  response <- httr::content(response)
  
  # Basic information
  timeline <- data.table(
    user       = sapply(response, function(r) r$actor$login),
    created_at = sapply(response, "[[", "created_at"),
    event      = sapply(response, "[[", "event"),
    commit_url = sapply(response, function(r) {
      if (length(r$commit_url))
        r$commit_url
      else
        NA_character_
    }),
    commit_id = sapply(response, function(r) {
      if (length(r$commit_id))
        r$commit_id
      else
        NA_character_
    })
  )
  
  timeline[, repo := str_extract(commit_url, "(?<=\\.com/repos/).+(?=/commits/)")]
  timeline[, commit_url := fifelse(
    !is.na(repo),
    sprintf("https://github.com/%s/commit/%s", repo, commit_id),
    NA_character_)
    ]
  
  # Now getting the contents of the commit:
  message("Getting the details of the commits...", appendLF = FALSE)
  files <- vector("list", nrow(timeline))
  for (i in 1:nrow(timeline)) {
    message(i, ", ", appendLF = FALSE)
    files[[i]] <- commit_info(
      timeline$commit_id[i], timeline$repo[i], ...)
  }
  message("done.")
  
  timeline <- cbind(timeline, rbindlist(files))
  
  # Checking which ones were about a lab
  timeline[, type := fifelse(
    grepl("lab", tolower(message)), "lab",
    fifelse(grepl("hw|assignme", tolower(message)), "homework", NA_character_)
  )]
  
  # Filling the emptyones
  timeline[, type := fcoalesce(type, sapply(fnames, function(f) {
    if (any(grepl("assig|hw|homew", tolower(f))))
      "homework"
    else
      NA_character_
  }))]
  timeline[, type := fcoalesce(type, sapply(fnames, function(f) {
    if (any(grepl("lab", tolower(f))))
      "lab"
    else
      NA_character_
  }))]
  
  timeline
  
}

commit_info <- function(
  commit,
  repo    = "USCbiostats/PM566",
  baseurl = "https://api.github.com/repos",
  ...
  ) {
  
  # Checking the arguments of curl -X GET
  dots <- list(...)
  if ("config" %in% names(dots))
    dots$config <- c(dots$config, httr::add_headers(
      Accept = "application/vnd.github.mockingbird-preview+json"
    ))
  else
    dots$config <- httr::add_headers(
      Accept = "application/vnd.github.mockingbird-preview+json"
    )
  
  dots$url <- sprintf("%s/%s/commits/%s", baseurl, repo, commit)
  
  response <- tryCatch(do.call(httr::GET, dots), error = function(e) e)
  
  if (inherits(response, "error")) {
    warning("httr::GET failed.")
    return(data.table(
      message = NA_character_,
      flinks  = list(),
      fnames  = list()
    ))
  }
  
  commit <- httr::content(response)
  
  msg    <- commit$commit$message
  fnames <- lapply(commit$files, "[[", "filename")
  flinks <- lapply(commit$files, "[[", "blob_url")
  
  
  data.table(
    message = if (length(msg) == 0) NA_character_ else msg,
    flinks  = list(flinks),
    fnames  = list(fnames)
  )
  
}

# ans <- list_cross_ref_api(25)
