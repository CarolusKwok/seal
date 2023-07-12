#' Printing the factor matrix in SED
#'
#' @param x an object
#'
#' @return A visual print of the SED
#' @keywords internal
#' @noRd
#'
#' @examples print.sed_factor(SED_factor)
print.sed_factor = function(x, name){
  cli::cli_div(theme = list(span.grey = list(color  = "#9E9E9E")))
  factors = seal:::sys_obtain_factor(x$factor) %>%
    unique()
  nonflex_factors = dplyr::filter(.data = x,
                                  label != "###") %>%
    .$factor %>%
    seal:::sys_obtain_factor() %>%
    unique()
  flex_factors = factors[!(factors %in% nonflex_factors)]
  len = length(factors)
  len_nonflex = length(nonflex_factors)

  #Actually printing the data ####
  cli::cli_text("{.grey {stringr::str_dup('#', ceiling(log10(len)))} SED factor: {length(factors)}}")
  for(i in 1:len_nonflex){
    index = sprintf(paste0("%0", ceiling(log10(len)), "d"), i)
    sel_factor = nonflex_factors[i]
    sel_label = dplyr::filter(.data = x,
                              factor == sel_factor)$label
    cli::cli_text("{.grey {index}} {sel_factor}: {stringr::str_flatten(sel_label, '/ ')}")
  }

  if(length(flex_factors) != 0){
    cli::cli_text("{.grey {stringr::str_dup('>', ceiling(log10(len)))} flexible factors}")
    cli::cli_text("{.grey {len}} {(stringr::str_flatten(string = flex_factors, ', '))}")
  }
}



#' Printing the item matrix in SED
#'
#' @param x an object
#'
#' @return A visual print of the SED
#' @keywords internal
#' @noRd
#'
#' @examples print.sed_item(SED_item)
print.sed_item = function(x){
  cli::cli_div(theme = list(span.grey = list(color  = "#9E9E9E")))

  datatype = unique(x$datatype)
  cli::cli_text("{.grey type items}")
  for(i in datatype){
    item = dplyr::filter(.data = x, datatype == i)$item
    cli::cli_text("{.grey {i}...} {stringr::str_flatten(item, ', ')}")
  }
  col = colnames(x)
  if(length(col) > 2){
    col = col[col != "item" & col != "datatype"]
    cli::cli_text("{.grey Extra:} {stringr::str_flatten(col, ', ')}")
  }
}



#' Printing the data matrix in SED
#'
#' @param x an object
#'
#' @return A visual print of the SED
#' @keywords internal
#' @noRd
#'
#' @examples print.sed_data(SED_data)
print.sed_data = function(x){
  print.data.frame(x)
}
