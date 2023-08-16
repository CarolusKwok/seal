#' Pre-check for r/w: Check `data` matrix fulfills requirement
#'
#' @param data the `data` matrix in dataframe or tibble.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name that called this function, which is to be passed to `sys_msg_error` or `sys_msgwarning`. Default as `NULL`, the function name will not display.
#'
#' @return a logical value, with `TRUE` representing an error occurred; and a standardized message if `silent` is `FALSE`.
#' @keywords internal
#' @noRd
#'
#' @examples ckrw_data(data)
ckrw_data = function(data, silent = FALSE, func = NULL){
  #Check about the class ####
  if((!is.data.frame(data))){
    return(invisible(seal:::sys_msg_error(title = "Inputing `data` with the wrong class",
                                          error = c("Please check if `data` is `data.frame`"),
                                          func = func, silent = silent)))}

  #Check if the dataframe columns are unique ####
  columns = colnames(data)
  if(sum(duplicated(columns))){
    col = unique(columns[duplicated(columns)])
    return(invisible(seal:::sys_msg_error(title = "`data` has duplicate columns",
                                          error = c("Please check if `data` is all unique",
                                                    "Duplicated columns include...",
                                                    col),
                                          func = func, silent = silent)))
  }

  #Check if the dataframe consist of @code, >=1 factor, and >=1 item ####
  col_factor = seal:::sys_grab_factor(columns)
  col_items = seal:::sys_grab_item(columns)

  if(!("@code" %in% columns)){
    return(invisible(seal:::sys_msg_error(title = "`data` must have a column `@code`",
                                          error = c("Please check if `@code` is present in `data`"),
                                          func = func, silent = silent)))
  }
  if(length(col_factor) <= 0){
    return(invisible(seal:::sys_msg_error(title = "`data` must have column(s) representing factors",
                                          error = c("Please check if there are any factors"),
                                          func = func, silent = silent)))
  }
  if(length(col_items) <= 0){
    return(invisible(seal:::sys_msg_error(title = "`data` must have column(s) representing items",
                                          error = c("Please check if there are any items"),
                                          func = func, silent = silent)))
  }

  #Check if all the factor columns are filled ####
  data_factors = dplyr::select(.data = data, {{col_factor}})
  if(anyNA(data_factors)){
    return(invisible(seal:::sys_msg_error(title = "Factors in `data` must be all filled",
                                          error = c("Please check `data`"),
                                          func = func, silent = silent)))
  }

  #Check if all the item column classes make sense ####
  data_items = dplyr::select(.data = data, {{col_items}})
  data_items_class = lapply(data_items, FUN = class) %>% unlist()
  if(sum(!(data_items_class %in% c("logical", "integer", "numeric", "character")))){
    return(invisible(seal:::sys_msg_error(title = "Items in `data` must be of some classes",
                                          error = c("Please check `data`, accepted classes are",
                                                    c("logical", "integer", "numeric", "character")),
                                          func = func, silent = silent)))
  }

  #Check if @code is all filled or all empty####
  data_code = dplyr::select(data, `@code`)$`@code`
  len = sum(is.na(data_code))
  if(len != 0 & len != length(data_code)){
    return(invisible(seal:::sys_msg_error(title = "`@code` in `data` must be all filled or all empty",
                                          error = "Please check `data`",
                                          func = func, silent = silent)))
  }
  #Check if @code is unique if @code is filled####
  if(len == 0){
    if(sum(duplicated(data_code))){
      cod = unique(data_code[duplicated(data_code)])
      return(invisible(seal:::sys_msg_error(title = "`@code` must be all unique in `data`",
                                            error = c("Please check if `data` is all unique",
                                                      "Duplicated columns include...",
                                                      cod),
                                            func = func, silent = silent)))
    }
  }
  #Return F if all ok ####
  return(invisible(FALSE))
}
