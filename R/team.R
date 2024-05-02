#' Compute Matrix of team combinations in the provided db
#' @importFrom utils combn
#' @importFrom stats xtabs
#' @param .db pubweb database
#' @param without_team numercical code of team to exclude from the matrix
#' @export
team_combinations = function(.db, without_team=NULL) {
  compute_combinations = function(article, .k=NULL) {
    equipes = unlist(article$equipes)
    if(length(equipes) > 0 && !is.null(without_team)) {
      equipes = equipes[!equipes %in% without_team]
    }
    #str(equipes)
    if(length(equipes) == 0) {
      return(NULL)
    }
    if(length(equipes) > 1) {
      ee = combn(equipes, 2, simplify = FALSE)
      #str(ee)
      ee = dplyr::bind_rows(lapply(ee, function(e) {
        data.frame(col=e[[1]], row=e[[2]])
      }))
    } else {
      ee = data.frame(col=equipes[1], row=equipes[1])
    }
    ee
  }

  m = lapply(.db$id, function(i) {
    b = .db[ .db$id == i, ]
    m = compute_combinations(b)
    if(is.null(m)) {
      return(NULL)
    }
    m$id = i
    m
  })

  m = dplyr::bind_rows(!!!m)

  m = m %>% dplyr::group_by(col, row) %>% dplyr::count()
  mc = xtabs(n ~ row + col, data=m)
  rownames(mc) <- recode_team(as.integer(rownames(mc)))
  colnames(mc) <- recode_team(as.integer(colnames(mc)))
  attr(mc, "data") <- m
  mc
}

#' Recode team
#' @param x team value as numerical code
#' @return factor with team label as level
#' @export
recode_team = function(x) {
  teams = config$teams
  factor(x, levels = teams, names(teams))
}

#' Extract teams attached of each publication of the provided db
#' @param .db data.frame() of publication (returned by pubweb_load_db() or a subset)
#' @param recode if TRUE numerical team id will be recoded to factor
#' @return data.frame(pub_id, team)
pubweb_teams = function(.db, recode=TRUE) {
  hh = tidyr::unnest(.db[, c('id','equipes')], cols='equipes')
  hh = hh %>% dplyr::rename(team=equipes, pub_id=id)
  if(recode) {
    hh$team = recode_team(hh$team)
  }
  hh

}
