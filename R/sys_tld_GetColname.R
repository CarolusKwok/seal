#' System tools: Get a non-clashing column name within a dataframe, by adding a numeric suffix ("_x")
#'
#' @param value Column name you are expecting to use. Can be a symbol or a character.
#' @param data The dataframe you are using
#'
#'
#' @return The column name, in character
#' @keywords internal
#' @noRd
#'
#' @examples sys_tld_GetColname(x, data = tibble::tibble(x = 0)) #Returns "x_1"
sys_tld_GetColname = function(value, data){
  #Check ####
  if(!hasArg(value)){
    return(invisible(seal:::sys_msgerror(title = "`value` has no input",
                                         error = c("x" = "Please input a character or a symbol."),
                                         func = "sys_tld_GetColname", silent = silent, return_flag = return_flag)))
  }
  if(!hasArg(data)){
    return(invisible(seal:::sys_msgerror(title = "`data` has no input",
                                         error = c("x" = "Please input a dataframe."),
                                         func = "sys_tld_GetColname", silent = silent, return_flag = return_flag)))
  }
  #Work ####
  value = seal:::sys_hp_sym2chr({{value}})
  colnames_data = colnames(data)
  name_trial = value
  n = 0
  while(name_trial %in% colnames_data){
    n = n + 1
    name_trial = paste0(value, "_", n)
  }
  return(name_trial)
}
