#' IplespBib package
#' Load Bibliography database from Iplesp Database
#' @importFrom dplyr %>%
#' @importFrom utils hasName
"_PACKAGE"

#' Configuration
#' \describe{
#'  \item{root_url}{Root of the pubweb database site}
#'  \item{teams}{vector of int, List of teams with numerical code as value, and team label as name}
#' }
config = rlang::new_environment(list(
  cache_dir = NULL,
  root_url = "https://pubweb.iplesp.fr",
  teams = c('SUMO'=1L,'CLEPIVIR'=2L,'THERAVIR'=4L,'EPAR'=5L,'PEPITES'=6L,'ERES'=7L,'NEMESIS'=10L, 'CIPHOD'=11L)
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
  for(n in names(oo)) {
    switch(n,
      root_url=set_root_url(oo[[n]]),
      team=set_teams(oo[[n]]),
      cache_dir=set_cache_dir(oo[[n]]),
      rlang::abort(paste("Unknown config", sQuote(n)))
    )
  }
}

#' Get current configuration
#' @return list
#' @export
pubweb_config = function() {
  as.list(config)
}
