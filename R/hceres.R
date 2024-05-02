#' Recode HCERES field into meaningful labels
#' @param y hceres field values in numerical encoding
#' @return factor vector
#' @export
pubweb_recode_hceres = function(y) {
  if(is.factor(y)) {
    warning("Cannot recode a factor")
    return(y)
  }
  if(!is.integer(y)) {
    warning(paste(deparse(substitute(y, parent.frame())), "Is not a integer"))
    return(y)
  }
  factor(y, c(0,1,2), c('Non','Oui (avec Aff.)', "Oui (sans Aff)."))
}
