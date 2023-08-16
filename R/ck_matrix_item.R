#' SED: Check `item` matrix fulfills requirement
#'
#' This function checks if data in the subclass `sed_item` fulfills the SED requirements.
#' This check ignores cooperative nature of items in `sed`, and only focuses if the factor matrix makes sense.
#'
#' @param item the `item` matrix in dataframe or tibble.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name that called this function, which is to be passed to `sys_msg_error` or `sys_msgwarning`. Default as `NULL`, the function name will not display.
#'
#' @return a logical value, with `TRUE` representing an error occurred; and a standardized message if `silent` is `FALSE`.
#' @keywords internal
#' @noRd
#'
#' @examples ck_item(item)
ck_item = function(item, silent = FALSE, func = NULL){
  #Check about the class ####
  if((!seal:::is_sed_item(item))){
    return(invisible(seal:::sys_msg_error(title = "Inputing `item` with the wrong class",
                                          error = c("Please check if `item` is `sed_item`"),
                                          func = func, silent = silent)))}
  if((!is.data.frame(item))){
    return(invisible(seal:::sys_msg_error(title = "Inputing `item` with the wrong class",
                                          error = c("Please check if `item` is `data.frame`"),
                                          func = func, silent = silent)))}

  #Remove the sed_factor class for easier process ####
  class_sed = class(item)
  class(item) = class_sed[class_sed != "sed_item"]

  #Check if the dataframe contains `item`, `datatype`####
  columns = colnames(item)

  if(!("item" %in% columns)){
    return(invisible(seal:::sys_msg_error(title = "`item` is not present.",
                                          error = "Please check `item` and include the column",
                                          func = func, silent = silent)))
  }
  if(!("datatype" %in% columns)){
    return(invisible(seal:::sys_msg_error(title = "`datatype` is not present.",
                                          error = "Please check `item` and include the column",
                                          func = func, silent = silent)))
  }

  #Check if the dataframe columns are unique ####
  if(sum(duplicated(columns))){
    col = unique(columns[duplicated(columns)])
    return(invisible(seal:::sys_msg_error(title = "`factor` has duplicate columns",
                                          error = c("Please check if `factor` is all unique",
                                                    "Duplicated columns include...",
                                                    col),
                                          func = func, silent = silent)))
  }

  #Check if all columns are "characters" ####
  col_class = unlist(lapply(item, class)) %>% unique

  if(length(col_class) != 1 | col_class[1] != "character"){
    return(invisible(seal:::sys_msg_error(title = "All columns in `item` must be `character`",
                                          error = 'Please make sure all columns in `item` are "character".',
                                          silent = silent, func = func)))
  }

  #Check if all columns are filled ####
  if(anyNA(item)){
    return(invisible(seal:::sys_msg_error(title = "`item` must be all filled.",
                                          error = "Please check `item` and remove any blanks.")))
  }

  #Check if `datatype` is p n i c ####
  if(sum(!(item$datatype %in% c("p", "n", "i", "c")))){
    return(invisible(seal:::sys_msg_error(title = "Invalid inputs in `datatype`",
                                          error = c("Please check `datatype` in item",
                                                    "There could only be `p`, `n`, `i`, `c`",
                                                    "p = presence/ absence (logical)",
                                                    "n = numerical",
                                                    "i = integer",
                                                    "c = character"))))
  }

  #Return F if all ok ####
  return(invisible(FALSE))
}
