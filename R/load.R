#' Get Path of cache file for a given name
#' @param name character vector
#' @export
pubweb_cache = function(name) {
  dir = config[["cache_dir"]]
  if(is.null(dir) ) {
    dir = rappdirs::user_cache_dir("pubweb")
    if(!file.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
  }
  file.path(dir, name)
}

#' Load RDS with cache
#' @param file file name to load
#' @param ttl int time to live in seconds
load_with_cache = function(file, ttl) {
  if(!file.exists(file)) {
    return(NULL)
  }
   if(!is.integer(ttl) || length(ttl) != 1) {
    rlang::abort("`ttl` must be integer with single value")
  }
  mtime = file.mtime(file)
  d = as.integer(difftime(Sys.time(), mtime, units="secs"))
  if(d > ttl) {
    return(NULL)
  }
  readRDS(file)
}

#' Cache loader from file
#' Expose load() & save() function to simply load from cache
#' @param name file name (without path)
#' @param ttl Time to live in seconds, if NULL default from config is used
cache_file = function(name, ttl) {
  cache_file = pubweb_cache(name)
  if(is.na(ttl)) {
    ttl = as.integer(config$default_ttl)
  }
  if(is.double(ttl)) {
    ttl = as.integer(ttl)
  }
  if(length(ttl) != 1) {
    rlang::abort("`ttl` must be a single value integer")
  }
  if(is.na(ttl) || !rlang::is_integer(ttl, n=1)) {
    rlang::abort("ttl must be a single integer value")
  }
  list(
    load=function() {
      if(ttl > 0L) {
        load_with_cache(cache_file, ttl)
      } else {
        NULL
      }
    },
    save=function(data) {
      saveRDS(data, cache_file)
    },
    file=cache_file
  )
}

#' Load JSON data from URL
#' @param url URL to load
load_json_from_url = function(url) {
  h = curl::new_handle()
  curl::handle_setopt(h, verbose=config$curl_verbose)
  if(config$curl_insecure) {
    curl::handle_setopt(h, ssl_verifypeer=0L)
  }
  bib = curl::curl_fetch_memory(url, handle=h)
  status = bib$status_code
  if(status != 200L) {
    rlang::abort(paste("Unable to load records, server responded with errors status", status), response=bib)
  }
  jsonlite::fromJSON(rawToChar(bib$content))
}

#' Load Pubweb Records using pagination
#' @param page integer page number to load
#' @param page_size integer number of records by page
#' @param with_team logical if TRUE add teams list in results
load_records_page = function(offset, page_size, with_team=FALSE) {
  url = paste0(config$root_url, "/public/data","?limit=", as.integer(page_size),"&offset=", as.integer(offset))
  if(with_team) {
    url = paste0(url, "&team=1")
  }
  bib = load_json_from_url(url)
  if(is.list(bib) && !is.null(bib$records)) {
    message(url)
    message("Loaded ", nrow(bib$records), " from", bib$offset, " / ", bib$total_records, " by ", bib$page_size)
  } else {
    str(bib)
    rlang::abort(paste("Abnormal response structure from", url), response=bib)
  }
  bib
}

#' Load all records using pagination
#' @param page_size integer, number of records by page
load_records = function(page_size) {
  teams = NULL
  records = NULL
  total_records = 1L
  offset = 0L
  while(offset < total_records) {
    p = load_records_page(offset=offset, page_size=page_size, with_team = offset == 0L)
    if(offset == 0L) {
      teams = p$teams
      total_records = p$total_records
    }
    if(!is.null(p$records) && nrow(p$records) > 0L) {
      records = dplyr::bind_rows(records, p$records)
    }
    offset = offset + page_size
  }
  list(
    records=records,
    teams=teams,
    meta=list(
      total_records=total_records,
      page_size=page_size
    )
  )
}

#' Install teams list loaded from server in config
#' @param teams list
install_teams = function(teams) {
  if(is.null(teams)) {
    return()
  }
  acronyms = lapply(teams, \(t) t$acronym)
  tt = as.integer(names(acronyms))
  names(tt) <- unlist(acronyms)
  message(paste( paste0(sQuote(names(tt)), "=", tt), collapse = ", "))
  set_teams(tt)
}

#' Cache for teams
#' @param ttl integer Time to live in seconds
teams_cache = function(ttl) {
  cache_file("teams.Rds", ttl)
}

#' Load team list from server (cached)
#' @param ttl Time to live, if NA default ttl in config will be used, O will disable cache
#' @export
pubweb_load_teams = function(ttl=NA) {
  cache = teams_cache(ttl)
  teams = cache$load()
  if(is.null(teams)) {
    message("Loading teams from server")
    url = paste0(config$root_url, "/public/teams")
    r = load_json_from_url(url)
    if(is.null(r$teams)) {
      rlang::abort("Server returned an empty team list")
    }
    teams = r$teams
    cache$save(teams)
  } else {
    message("Teams loaded from cache")
  }
  install_teams(teams)
}

#' Load Pubweb databases
#' @param ttl int cache time (if 0 will disable cache, NA will use default)
#' @export
pubweb_load_db = function(ttl=NA) {
  bib = pubweb_load_from_server(ttl)
  pubweb_load_teams(ttl)
  bib
}

#' Load Pubweb databases froms server
#' @param ttl int cache time (if 0 will disable cache)
pubweb_load_from_server = function(ttl=NA) {
  cache = cache_file("pubweb.Rds", ttl)
  bib = cache$load()
  if(is.null(bib)) {
    message("Loading records from server")
    r = load_records(config$records_page_size)
    bib = r$records
    message(nrow(bib), "records loaded")
    teams = r$teams
    bib$hal_ids = sapply(bib$hal, paste0, collapse=',')
    bib$authors = lapply(bib$authors, pubweb_fix_authors)
    attr(bib, "loaded_at") <- Sys.time()
    attr(bib, "meta") <- r$meta
    cache$save(bib)

    ## Update teams
    if(length(teams) > 0) {
      cache2 = teams_cache(ttl)
      cache2$save(teams)
    }
  } else {
    message(nrow(bib)," records loaded from cache")
  }
  bib
}

#' Fix authors
#' @param authors data.frame fix authors data
pubweb_fix_authors = function(authors) {
  authors = data.frame(authors)
  for(col in c('iplesp','wild','corresp','group', 'equal')) {
    if(hasName(authors, col)) {
      authors[[col]] = as.integer(authors[[col]]) > 0
    }
  }
  authors
}
