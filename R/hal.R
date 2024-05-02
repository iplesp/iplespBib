
#' Create data.frame with correspondance with HAL ids and iplesp id
#' @param .db database
#' @param with_aliases if TRUE include HAL aliases in the list
#' @export
pubweb_extract_hal = function(.db, with_aliases=TRUE) {
  hh = tidyr::unnest(.db[, c('id','hal')], cols='hal')
  hh = hh %>% dplyr::filter(!is.na(hal))
  if(with_aliases) {
    has = sapply(bib$hal_alias, length) > 0
    aliases = tidyr::unnest(.db[has, c('id','hal_alias')], cols='hal_alias')
    aliases = aliases %>% dplyr::filter(!is.na(hal_alias)) %>% dplyr::rename(hal=hal_alias) %>% dplyr::mutate(alias=TRUE)
    hh$alias = FALSE
    hh = dplyr::bind_rows(hh, aliases)
  }
  hh
}
