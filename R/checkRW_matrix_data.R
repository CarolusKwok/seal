#' Pre-check for r/w: Check the `data` matrix if it suits the requirements
#'
#' @param data the `data` matrix in dataframe or tibble.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name that called this function, which is to be passed to `sys_msgerror` or `sys_msgwarning`. Default as `"ckrw_data"`. If `NULL`, the function name will not display.
#'
#' @return a logical value, with `TRUE` representing an error occurred; and a standardized message if `silent` is `FALSE`.
#' @export
#'
#' @examples ckrw_data(data)
ckrw_data = function(data, silent = FALSE, func = "ckrw_data"){
  #Check if there is an input ####
  if(!hasArg(data)){return(invisible(seal:::sys_msgs_noinput(arg = "data",
                                                             expect = c("data.frame", "tibble"))))}

  #Check if `data` is a dataframe or tibble ####
  if((!is.data.frame(data))){return(invisible(seal:::sys_msgs_wrongclass(arg = "data",
                                                                         expect = c("data.frame", "tibble"))))}

  #Check if the dataframe columns are unique ####
  columns = colnames(data)
  if(sum(duplicated(columns))){
    col = unique(columns[duplicated(columns)])
    return(invisible(seal:::sys_msgsdf_notuniquecolname(col = col, dataframe = "data",
                                                        silent = silent, func = func)))
  }
  #Check if the dataframe consist of @code, >=1 factor, and >=1 item ####
  col_factor = seal:::sys_obtain_factor(columns)
  col_items = seal:::sys_obtain_item(columns)

  if(!("@code" %in% columns)){return(invisible(seal:::sys_msgsdf_nocolumn(col = "@code", dataframe = "data",
                                                                          silent = silent, func = func)))}
  if(length(col_factor) <= 0){return(invisible(seal:::sys_msgsdf_nocolumn(col = "factors", dataframe = "data",
                                                                          silent = silent, func = func)))}
  if(length(col_items) <= 0){return(invisible(seal:::sys_msgsdf_nocolumn(col = "items", dataframe = "data",
                                                                         silent = silent, func = func)))}
  #Check if all the factor columns are filled ####
  data_factors = dplyr::select(.data = data, {{col_factor}})
  if(anyNA(data_factors)){
    return(invisible(seal:::sys_msgsdf_columnnotfull(col = "factors", dataframe = "data", silent = silent, func = func)))
  }
  #Check if @code is all filled or all empty ####
  data_code = dplyr::select(data, `@code`)$`@code`
  if(length(data_code) != sum(is.na(data_code))){
    if(length(data_code) != sum(!is.na(data_code))){
      return(invisible(seal:::sys_msgsdf_colnotfullempty(col = "@code", dataframe = "data", silent = silent, func = func)))
    }
  }
  #Check if @code is unique ####
  if(sum(!is.na(data_code))){
    if(sum(duplicated(data_code))){
      return(invisible(seal:::sys_msgsdf_itemnotunique(col = "@code", dataframe = "data", silent = silent, func = func)))
    }
  }
  #Return F if all ok ####
  return(invisible(FALSE))
}
