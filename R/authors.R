#' Extract authors list from th database
#' @param .db database loaded from \code{\link{pubweb_load_db}()}
#' @param with columns to keep
#' @export
#' @return data.frame()
#' @details
#' \describe{
#'  \item{id}{Publication id}
#'  \item{order}{Order of the authors in the publication}
#'  \item{name}{First Name of the author. Depending of the data source, this field can contain the full name or the family name}
#'  \item{given}{Given name of the author, sometimes not provided}
#'  \item{iplesp}{This author is recognized as an Iplesp author with corresponding affiliation}
#'  \item{wild}{This author is part of Iplesp, but affiliation is not mentionning ig}
#'  \item{affiliation}{Affiliation provided, several affiliations has been concatenated with `|` character}
#'  \item{hal}{halId of the author, rarely provided}
#'  \item{orcid}{Orcid number of the authors, as provided by the metadata source (pubmed, wos,...)}
#'  \item{corresp}{Authors is corresponding author}
#'  \item{group}{Author is a group}
#'  \item{equal}{Equal authorship with the adjacent author (first or last)}
#' }
pubweb_authors = function(.db, with=c()) {

  cols = c('id', 'authors', with)

  authors = tidyr::unnest(dplyr::select(.db, !!!rlang::syms(cols)), cols='authors')

  fill_na = function(x) {
    dplyr::if_else(is.na(x), FALSE, x)
  }

  authors = authors %>% dplyr::mutate_at(c('iplesp','wild','corresp','group', 'equal'), fill_na)

  authors
}

#' Recode an author name to full name
#' This function works with data.frame
#' @param .row author data raw
recode_author_name = function(.row) {
  ifelse(is.na(.row$given) | .row$given == "", .row$name, paste(.row$name, .row$given))
}

#' Compute author full name from given & name fields
#' @param given given name field
#' @param name name field
#' @export
author_fullname = function(given, name) {
  ifelse(is.na(given) | given == "", name, paste(name, given))
}

#' Compute authors full name from authors data.frame
#' @param aa authors data.frame()
recode_authors = function(aa) {
  paste(recode_author_name(aa), collapse=", ")
}

#' Create list of authors (coma separated list of authors)
#'
#' @param authors authors column
#' @export
pubweb_authorlist = function(authors) {
  sapply(authors, recode_authors)
}
