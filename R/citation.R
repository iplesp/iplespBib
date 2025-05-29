
author_to_person = function(author) {
  person(given=author$given, family=author$name)
}

#' Create bibentry from a publication row
#' @param p row of a publications dataframe
#' @returns bibentry
#' @export
pub_to_bib = function(p) {
  aa = p$authors

  if(is.list(aa) && !is.data.frame(aa)) {
    aa = aa[[1]]
  }

  authors = lapply(seq_len(nrow(aa)), \(i) {
    a = aa[i, ]
    author_to_person(a)
  })

  authors = do.call(personList, authors)

  bibtype = switch(p$doctype,
                   article="Article",
                   "Article"
  )

  utils::bibentry(
    bibtype = bibtype,
    author=authors,
    journal=p$journal,
    number=p$issue,
    title=p$title,
    volume=p$volume,
    year=p$year,
    pages=p$pages,
    doi=p$doi
  )
}

#' Format publication to a textual citation
#' @param p data.frame row of a publication table
#' @param authorlist all, first or "first_last"
#' @returns character string of citation
format_citation = function(p, authorlist="all") {

  not_empty = \(x) !is.null(x) && x != ""

  parts = c()

  parts = c(parts, paste0("#", p$id))

  if(not_empty(p$title)) {
    title = paste0(p$title, if(!stringr::str_ends(p$title, stringr::fixed("."))) ".")
    parts = c(parts,  dQuote(title))
  }

  aa = p$authors

  if(is.list(aa) && !is.data.frame(aa)) {
    aa = aa[[1]]
  }

  if(nrow(aa) > 0) {
    aix = switch(authorlist,
                 all=seq_len(nrow(aa)),
                 first=1,
                 first_last=unique(c(1, nrow(aa)))
    )
    authors = lapply(aix, \(i) {
      a = aa[i, ]
      author_to_person(a)
    })
  } else {
    authors = NULL
  }

  if(length(authors) > 0) {
    authors = sapply(authors, format)
    if(authorlist == "first_last") {
      al = paste0(authors, collapse = ",...,")
    } else {
      al = paste(authors, collapse = ", ")
    }
    parts = c(parts, al)
  }

  parts = c(parts, sQuote(p$journal))

  parts = c(parts, p$year)

  if(not_empty(p$volume)) {
    r = p$volume
    if(not_empty(p$issue)) {
      r = paste0(r, ":", p$issue)
    }
    parts = c(parts, r)
  }

  if(not_empty(p$conf_title)) {
    cf = p$conf_title

    if(not_empty(p$date)) {
      cf = c(cf, p$date)
    }
    if(not_empty(p$city)) {
      cf = c(cf, p$city)
    }
    if(not_empty(p$country)) {
      cf = c(cf, p$country)
    }
    parts = c(parts, paste0(cf, collapse = ", "))
  }

  if(not_empty(p$doi)) {
    parts = c(parts, paste0("doi:", p$doi))
  }

  if(not_empty(p$pmid)) {
    parts = c(parts, paste0("PubMed:", p$pmid) )
  }

  if(not_empty(p$dblp)) {
    parts = c(parts, paste0('dblp:', p$dblp) )
  }

  paste(parts, collapse = " ")
}
