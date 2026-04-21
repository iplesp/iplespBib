#' IplespBib package
#' Load Bibliography database from Iplesp Database
#' @importFrom dplyr %>%
#' @importFrom utils hasName
"_PACKAGE"

#' Configuration
#' \describe{
#'  \item{root_url}{Root of the pubweb database site}
#'  \item{cache_dir}{Directory where to place cache files (loaded from server)}
#'  \item{curl_verbose}{Show verbose output when loading data from server}
#'  \item{curl_insecure}{Disable certificate check when loaded data from server}
#'  \item{records_page_size}{Number of records to load from the server at each step}
#'  \item{teams}{vector of int, List of teams with numerical code as value, and team label as name}
#' }
config = rlang::new_environment(list(
  cache_dir = NULL,
  root_url = "https://pubweb.iplesp.fr",
  curl_verbose=FALSE,
  curl_insecure=FALSE,
  records_page_size=2000,
  default_ttl=3600L*3L,
  teams = c()
))

set_root_url = function(value) {
  ok = grepl("(https?|ftp)://[^ /$.?#].[^\\s]*", value, perl=TRUE)
  if(!ok) {
    rlang::abort("root_url must be a valid URL")
  }
  value = gsub("/+$","", value) # Remove ending slash
  config$root_url = value
}

set_teams = function(value) {
  if( !is.integer(value) || length(value) == 0) {
    rlang::abort("`teams` must be an integer vector")
  }
  if(is.null(names(value))) {
    rlang::abort("Values for teams must be named")
  }
  config$teams = value
}

set_cache_dir = function(value) {
  fi = file.info(value)
  if(is.na(fi$isdir) || !fi$isdir) {
    rlang::abort(paste("cache_dir must exist and be a directory"))
  }
  config$cache_dir = value
}

#' Define config values
#' @param ... named value
#' @export
#' @examples
#'  \dontrun{pubweb_set_config(root_url="https://pubweb.fr")}
pubweb_set_config = function(...) {
  oo = list(...)
  if(is.null(names(oo))) {
    rlang::abort("Each parameter must be named")
  }

  set_config = \(name, value) {
    config[[name]] = value
  }

  for(n in names(oo)) {
    switch(n,
      root_url=set_root_url(oo[[n]]),
      team=set_teams(oo[[n]]),
      cache_dir=set_cache_dir(oo[[n]]),
      set_config(n, oo[[n]])
    )
  }
}

#' Get current configuration
#' @return list
#' @export
pubweb_config = function() {
  as.list(config)
}
