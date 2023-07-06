#' Standard message: When there is no input for an argument
#'
#' @param arg the argument name itself in `character`.
#' @param expect expected value, e.g. `character`, `data.frame`, `list`
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#' @param return_flag Will this function silently return a logical value to indicate an error occurred? `TRUE` indicates an error occurred. Default as `TRUE`, which will return the value.
#'
#' @return a standardized text message and logical value
#' @keywords internal
#' @noRd
#'
#' @examples sys_msgs_noinput(arg = "data", expect = "data.frame")
sys_msgs_noinput = function(arg, expect,
                            silent = FALSE,
                            func = NULL,
                            return_flag = TRUE){
  #check for input####
  if(!hasArg(arg)){
    return(seal:::sys_msgerror(title = "`arg` must have an input.",
                               error = c("x" = "Please input `arg`."),
                               func = "sys_msgerror_noinput", silent = FALSE) %>% invisible)
  }
  if(!hasArg(expect)){
    return(seal:::sys_msgerror(title = "`expect` must have an input.",
                               error = c("x" = "Please input `expected`."),
                               func = "sys_msgerror_noinput", silent = FALSE) %>% invisible)
  }

  #magic ####
  error = paste0("Please input ", stringr::str_flatten(expect, collapse = "/ "), ".")
  return(invisible(seal:::sys_msgerror(title = paste0("`", arg, "` must have an input."),
                                       error = c("x" = error),
                                       silent = silent, func = func, return_flag = return_flag)))
}

#' Standard message: When the input for an argument is of the wrong class
#'
#' @param arg the arg name itself in `character`.
#' @param expect expected class, e.g. `character`, `data.frame`, `list`
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#' @param return_flag Will this function silently return a logical value to indicate an error occurred? `TRUE` indicates an error occurred. Default as `TRUE`, which will return the value.
#'
#' @return a standardized text message and logical value
#' @keywords internal
#' @noRd
#'
#' @examples sys_msgs_wrongclass("item", "dataframe")
sys_msgs_wrongclass = function(arg, expect,
                               silent = FALSE,
                               func = NULL,
                               return_flag = TRUE){
  #Check for input####
  if(!hasArg(arg)){return(invisible(seal:::sys_msgs_noinput(arg = "arg", expect = "character")))}
  if(!hasArg(expect)){return(invisible(seal:::sys_msgs_noinput(arg = "expect", expect = "character")))}

  #magic ####
  title = paste0("`", arg, "` must be a ", stringr::str_flatten(string = expect, collapse = "/ "), ".")
  error = paste0("Please input a ", stringr::str_flatten(string = expect, collapse = "/ "), ".")
  return(invisible(seal:::sys_msgerror(title = title,
                                       error = c("x" = error),
                                       func = func, silent = silent, return_flag = return_flag)))
}
