#' Response parser
#'
#' Should not be externally used
#'
#' @title parse_response: Parse WikipediR responses internally
#' @param x result from a WikipediR query
#' @export
parse_response <- function(x){
  UseMethod("parse_response", x)
}

#' @export
parse_response.rchanges <- function(x){
  x <- x$query$recentchanges
  return(x)
}

#' @export
parse_response.rcontent <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  return(x)
}

#' @export
parse_response.pcontent <- function(x){
  x <- x$parse
  return(x)
}

#' @export
parse_response.rdiff <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  return(x)
}

#' @export
parse_response.uinfo <- function(x){
  x <- x$query$users
  return(x)
}

#' @export
parse_response.ucontribs <- function(x){
  x <- x$query$usercontribs
  results <- unlist(x)
  results <- data.frame(matrix(results, nrow = length(x), byrow = TRUE),
                        stringsAsFactors = FALSE)
  names(results) <- names(x[[1]])
  return(results)
}

#' @export
parse_response.catpages <- function(x){
  x <- x$query$categorymembers
  results <- unlist(x)
  results <- data.frame(matrix(results, nrow = length(x), byrow = TRUE),
                        stringsAsFactors = F)
  names(results) <- names(x[[1]])
  return(results)
}

#' @export
parse_response.pagecats <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  results <- lapply(x,function(x){
    cats <- unlist(x$categories)
    cats <- data.frame(matrix(cats, nrow = length(x$categories), byrow = TRUE),
                       stringsAsFactors = FALSE)
    names(cats) <- names(x$categories[[1]])
    x$categories <- cats
    return(x)
  })
  return(results)
}

#' @export
parse_response.blink <- function(x){
  x <- x$query$backlinks
  results <- lapply(x,unlist)
  return(results)
}

#' @export
parse_response.plink <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  results <- lapply(x, function(x){
    x$links <- lapply(x$links,unlist)
    return(x)
  })
  return(results)
}

#' @export
parse_response.elink <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  results <- lapply(x, function(x){
    x$extlinks <- unlist(x$extlinks)
    names(x$extlinks) <- NULL
    return(x)
  })
  return(results)
}

#' @export
parse_response.pageinfo <- function(x){
  x <- x$query$pages
  names(x) <- NULL
  results <- lapply(x, function(x){
    x$restrictiontypes <- unlist(x$restrictiontypes)
    return(x)
  })
  return(results)
}

#' @export
parse_response.prelogintoken <- function(x){
  x <- x$login$token
  return(x)
}

#' @export
parse_response.login <- function(x){
  x <- x$clientlogin$status == "PASS"
  return(x)
}

#' @export
parse_response.actiontoken <- function(x){
  x <- x$query$tokens$csrftoken
  return(x)
}

#' @export
parse_response.createpage <- function(x){
  x <- x$edit$result == "Success"
  return(x)
}