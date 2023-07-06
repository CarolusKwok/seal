#' Standard message: When the column is not within a dataframe
#'
#' @param col the column name itself in `character`.
#' @param dataframe the dataframe name itself in character
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#' @param return_flag Will this function silently return a logical value to indicate an error occurred? `TRUE` indicates an error occurred. Default as `TRUE`, which will return the value.
#'
#' @return a standardized text message and logical value
#' @keywords internal
#' @noRd
#'
#' @examples sys_msgsdf_nocolumn(col = "item", dataframe = "item")
sys_msgsdf_nocolumn = function(col, dataframe,
                               silent = FALSE,
                               func = NULL,
                               return_flag = TRUE){
  #Check for input####
  if(!hasArg(col)){return(invisible(seal:::sys_msgs_noinput(arg = "col", expect = "character")))}
  if(!hasArg(dataframe)){return(invisible(seal:::sys_msgs_noinput(arg = "dataframe", expect = "character")))}

  #magic ####
  title = paste0("Column",
                 stringr::str_flatten(string = paste0("`", col, "`"), collapse = "/ "),
                 " not present in `", dataframe, "`")
  error = paste0("Please include column ", stringr::str_flatten(string = paste0("`", col, "`"), collapse = "/ "), ".")
  return(invisible(seal:::sys_msgerror(title = title,
                                       error = c("x" = error),
                                       func = func, silent = silent, return_flag = return_flag)))
}


#' Standard message: When column names are not unique within a dataframe
#'
#' @param col the column name itself in `character`.
#' @param dataframe the dataframe name itself in character
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#' @param return_flag Will this function silently return a logical value to indicate an error occurred? `TRUE` indicates an error occurred. Default as `TRUE`, which will return the value.
#'
#' @return a standardized text message and logical value
#' @keywords internal
#' @noRd
#'
#' @examples sys_msgsdf_notuniquecolname(dataframe = "item")
sys_msgsdf_notuniquecolname = function(col, dataframe,
                                       silent = FALSE,
                                       func = NULL,
                                       return_flag = TRUE){
  #Check for input####
  if(!hasArg(col)){return(invisible(seal:::sys_msgs_noinput(arg = "col", expect = "character")))}
  if(!hasArg(dataframe)){return(invisible(seal:::sys_msgs_noinput(arg = "dataframe", expect = "character")))}

  #magic ####
  title = paste0("Column ",
                 stringr::str_flatten(string = paste0("`", col, "`"), collapse = "/ "),
                 " is duplicated in `", dataframe, "`")
  error = paste0("Please check the column ",
                 stringr::str_flatten(string = paste0("`", col, "`"), collapse = "/ "))
  return(invisible(seal:::sys_msgerror(title = title,
                                       error = c("x" = error),
                                       func = func, silent = silent, return_flag = return_flag)))
}


#' Standard message: When columns are not fully filled
#'
#' @param col the column name itself in `character`.
#' @param dataframe the dataframe name itself in character
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#' @param return_flag Will this function silently return a logical value to indicate an error occurred? `TRUE` indicates an error occurred. Default as `TRUE`, which will return the value.
#'
#' @return a standardized text message and logical value
#' @keywords internal
#' @noRd
#'
#' @examples sys_msgsdf_columnnotfull(col = "item", dataframe = "item")
sys_msgsdf_columnnotfull = function(col, dataframe,
                                    silent = FALSE,
                                    func = NULL,
                                    return_flag = TRUE){
  #Check for input####
  if(!hasArg(col)){return(invisible(seal:::sys_msgs_noinput(arg = "col", expect = "character")))}
  if(!hasArg(dataframe)){return(invisible(seal:::sys_msgs_noinput(arg = "dataframe", expect = "character")))}

  #magic ####
  title = paste0("Column ",
                 stringr::str_flatten(string = paste0("`", col, "`"), collapse = "/ "),
                 " is not fully filled in `", dataframe, "`.")
  error = paste0("Please check the column ",
                 stringr::str_flatten(string = paste0("`", col, "`"), collapse = "/ "), ".")
  return(invisible(seal:::sys_msgerror(title = title,
                                       error = c("x" = error),
                                       func = func, silent = silent, return_flag = return_flag)))
}

#' Standard message: When items in a column are not unique
#'
#' @param col the column name itself in `character`.
#' @param dataframe the dataframe name itself in character
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#' @param return_flag Will this function silently return a logical value to indicate an error occurred? `TRUE` indicates an error occurred. Default as `TRUE`, which will return the value.
#'
#' @return a standardized text message and logical value
#' @keywords internal
#' @noRd
#'
#' @examples sys_msgsdf_columnnotfull(col = "item", dataframe = "item")
sys_msgsdf_itemnotunique = function(col, dataframe,
                                    silent = FALSE,
                                    func = NULL,
                                    return_flag = TRUE){
  #Check for input####
  if(!hasArg(col)){return(invisible(seal:::sys_msgs_noinput(arg = "col", expect = "character")))}
  if(!hasArg(dataframe)){return(invisible(seal:::sys_msgs_noinput(arg = "dataframe", expect = "character")))}

  #magic ####
  title = paste0("Items in column ",
                 stringr::str_flatten(string = paste0("`", col, "`"), collapse = "/ "),
                 " are not unique in `", dataframe, "`.")
  error = paste0("Please check the column ",
                 stringr::str_flatten(string = paste0("`", col, "`"), collapse = "/ "), ".")
  return(invisible(seal:::sys_msgerror(title = title,
                                       error = c("x" = error),
                                       func = func, silent = silent, return_flag = return_flag)))
}



#' Standard message: When the column is not fully filled or fully emptied
#'
#' @param col the column name itself in `character`.
#' @param dataframe the dataframe name itself in character
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#' @param return_flag Will this function silently return a logical value to indicate an error occurred? `TRUE` indicates an error occurred. Default as `TRUE`, which will return the value.
#'
#' @return a standardized text message and logical value
#' @keywords internal
#' @noRd
#'
#' @examples sys_msgsdf_columnnotfull(col = "item", dataframe = "item")
sys_msgsdf_colnotfullempty = function(col, dataframe,
                                      silent = FALSE,
                                      func = NULL,
                                      return_flag = TRUE){
  #Check for input####
  if(!hasArg(col)){return(invisible(seal:::sys_msgs_noinput(arg = "col", expect = "character")))}
  if(!hasArg(dataframe)){return(invisible(seal:::sys_msgs_noinput(arg = "dataframe", expect = "character")))}

  #magic ####
  title = paste0("Items in column ",
                 stringr::str_flatten(string = paste0("`", col, "`"), collapse = "/ "),
                 " are not fully filled/ emptied in `", dataframe, "`.")
  error = paste0("Please check the column ",
                 stringr::str_flatten(string = paste0("`", col, "`"), collapse = "/ "), ".")
  return(invisible(seal:::sys_msgerror(title = title,
                                       error = c("x" = error),
                                       func = func, silent = silent, return_flag = return_flag)))
}
