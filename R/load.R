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

#' Load Pubweb databases
#' @param ttl int cache time (if 0 will disable cache)
#' @export
pubweb_load_db = function(ttl=3600L*3L) {
  cache_file = pubweb_cache("pubweb.Rds")
  if(ttl > 0) {
    bib = load_with_cache(cache_file, ttl=ttl)
    if(!is.null(bib)) {
      return(bib)
    }
  }
  h = curl::new_handle()
  curl::handle_setopt(h, verbose=TRUE)
  url = paste0(config$root_url, "/public/data")
  bib = curl::curl_fetch_memory(url, handle=h)
  bib = jsonlite::fromJSON(rawToChar(bib$content))
  bib = bib$records
  bib$hal_ids = sapply(bib$hal, paste0, collapse=',')
  bib$authors = lapply(bib$authors, pubweb_fix_authors)
  attr(bib, "url") <- url
  attr(bib, "loaded_at") <- Sys.time()
  if(ttl > 0) {
    saveRDS(bib, cache_file)
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
