#' Extract matrix `factor` from an SED
#'
#' @param SED SED object
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name that called this function, which is to be passed to `sys_msg_error` or `sys_msgwarning`. Default as `NULL`, the function name will not display.
#'
#' @return matrix `factor`
#' @export
#'
#' @examples extract_factor(SED)
extract_factor = function(SED, silent = FALSE, func = "extract_factor"){
  if(!seal::is_sed(SED)){
    return(invisible(seal:::sys_msg_error(title = "Inputing `SED` with the wrong class",
                                          error = c("Please check if `SED` is `sed`"),
                                          func = func, silent = silent)))
  }

  #Find where the sed_factor is ####
  tmp = lapply(SED, seal::is_sed_factor) %>%
    unlist() %>%
    match(TRUE, .)

  #Extract the sed_factor ####
  list = SED[tmp] %>% unname
  return(invisible(list))
}



#' Extract `SED set` from an SED
#'
#' @param SED SED object
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name that called this function, which is to be passed to `sys_msg_error` or `sys_msgwarning`. Default as `NULL`, the function name will not display.
#'
#' @return a list of SED set
#' @export
#'
#' @examples extract_sed_set(SED)
extract_sed_set = function(SED, silent = FALSE, func = "extract_factor"){
  if(!seal::is_sed(SED)){
    return(invisible(seal:::sys_msg_error(title = "Inputing `SED` with the wrong class",
                                          error = c("Please check if `SED` is `sed`"),
                                          func = func, silent = silent)))
  }

  #Find where the sed_sets are ####
  tmp = lapply(SED, seal::is_sed_set) %>%
    unlist() %>%
    match(TRUE, .)

  #Extract the sed_set ####
  list = SED[tmp] %>% unname
  return(invisible(list))
}
