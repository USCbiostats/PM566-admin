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
    warning("httr::GET failed:", response)
    return(data.table(
      date     = NA_character_,
      message  = NA_character_,
      comments_url = NA_character_,
      comments = NA_character_,
      flinks   = list(),
      fnames   = list()
    ))
  }
  
  commit <- httr::content(response)
  
  msg    <- commit$commit$message
  comments <- commit$comments_url
  fnames <- lapply(commit$files, "[[", "filename")
  flinks <- lapply(commit$files, "[[", "blob_url")
  
  
  data.table(
    date    = commit$commit$author$date,
    message = if (length(msg) == 0) NA_character_ else msg,
    comments_url = if (length(comments) == 0) NA_character_ else comments,
    flinks  = list(flinks),
    fnames  = list(fnames)
  )
  
}

#' Knit a (R)markdown document from a given commit
#' This function will download a file from github in the form of
#' https://github.com/[usrname]/(raw|blob)/[commit id]/[path to doc].(Rmd|md)
download_and_knit <- function(url, ...) {
  
  fn <- stringr::str_extract(url, "(?<=/(raw|blob)/[[:alnum:]]{10,500}/).+")
  gh <- gsub(paste0("/?",fn), "", url)
  gh <- paste0(gsub("/(raw|blob)/", "/archive/", gh), ".zip")
  
  # Downloading
  tmpzip <- tempfile(fileext = ".zip")
  download.file(gh, destfile = tmpzip)
  unzip(tmpzip, exdir = tempdir())
  
  # Getting the file name
  fn_gh <- stringr::str_extract(
    gh,
    "(?<=/)[[:alnum:]-_]+/archive/[[:alnum:]]+"
  )
  
  fn_gh <- paste0(tempdir(), "/", gsub("/archive/", "-", fn_gh))
  
  rmarkdown::render(
    input = file.path(fn_gh, fn),
    ...
  )
  
}


list_files <- function(
  repo,
  commit = "master",
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
  
  dots$url <- sprintf("%s/%s/contents?ref=%s", baseurl, repo, commit)
  
  response <- tryCatch(do.call(httr::GET, dots), error = function(e) e)
  
  if (inherits(response, "error")) {
    warning("httr::GET failed:", response)
    return(data.table(
      name = NA_character_,
      path = NA_character_,
      download_url = NA_character_,
      html_url = NA_character_
    ))
  }
  
  dat <- content(response)
  
  dat <- lapply(dat, "[", c("name", "path", "download_url", "html_url"))
  rbindlist(dat, fill = TRUE)
  
  
}
