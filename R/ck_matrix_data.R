#' SED: Check `data` matrix fulfills requirement
#'
#' This function checks if data in the subclass `sed_data` fulfills the SED requirements.
#' This check ignores cooperative nature of items in `sed`, and only focuses if the data matrix makes sense.
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
ck_data = function(data, silent = FALSE, func = NULL){
  #Check about the class ####
  if((!seal:::is_sed_data(data))){
    return(invisible(seal:::sys_msg_error(title = "Inputing `data` with the wrong class",
                                          error = c("Please check if `data` is `sed_data`"),
                                          func = func, silent = silent)))}
  if((!is.data.frame(data))){
    return(invisible(seal:::sys_msg_error(title = "Inputing `data` with the wrong class",
                                          error = c("Please check if `data` is `data.frame`"),
                                          func = func, silent = silent)))}

  #Temporarily remove the sed_data class for easier process ####
  class_sed = class(data)
  class(data) = class_sed[class_sed != "sed_data"]

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

  #Check integer and numeric item columns are all filled ####
  sel_data_items = data_items[, data_items_class %in% c("integer", "numeric")]
  if(sum(anyNA(sel_data_items))){
    return(invisible(seal:::sys_msg_error(title = "Items in `data` with integer/ numeric must be filled",
                                          error = c("Please check `data`"),
                                          func = func, silent = silent)))
  }

  #Check if @code is all filled####
  data_code = dplyr::select(data, `@code`)$`@code`
  if(sum(is.na(data_code))){
    return(invisible(seal:::sys_msg_error(title = "`@code` in `data` must be all filled",
                                          error = "Please check `data`",
                                          func = func, silent = silent)))
  }
  #Check if @code is unique ####
  if(sum(duplicated(data_code))){
    cod = unique(data_code[duplicated(data_code)])
    return(invisible(seal:::sys_msg_error(title = "`@code` must be all unique in `data`",
                                          error = c("Please check if `data` is all unique",
                                                    "Duplicated columns include...",
                                                    cod),
                                          func = func, silent = silent)))
  }
  #Return F if all ok ####
  return(invisible(FALSE))
}
