#' Create a standardized error message for the package
#'
#' @param title Title of the error in `character`. This will be passed into `cli::cli_text`.
#' @param error Details of the error in  `character`. This will be passed into `cli::cli_bullets`. Use `"x"` for each bullet.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#' @param return_flag Will this function silently return a logical value to indicate an error occurred? `TRUE` indicates an error occurred. Default as `TRUE`, which will return the value.
#'
#' @return a standardized text message and logical value
#' @keywords internal
#' @noRd
#'
#' @examples seal:::sys_msgerror("Data incomplete", c("x" = "Please fill in the data"), func = "data_check")
sys_msgerror = function(title,
                        error,
                        silent = FALSE,
                        func = NULL,
                        return_flag = TRUE){
  #Check if sys_msgerror has error ####
  if(!hasArg(title) | !hasArg(error)){
    cli::cli_text(cli::bg_yellow(cli::col_black("Error in {.fn sys_msgerror}: No {.code title} or {.code error}")))
    cli::cli_bullets(c("x" = "Please fill in the {.code title}.",
                       "x" = "Please fill in the {.code error}."))
    return(invisible(TRUE))
  }

  #Send the error ####
  if(!silent){
    if(!is.null(func)){
      cli::cli_text(cli::bg_yellow(cli::col_black("Error in {.fn {func}}: {title}")))
    } else {
      cli::cli_text(cli::bg_yellow(cli::col_black("Error: {title}")))
    }
    cli::cli_bullets(error)
  }
  if(return_flag){
    return(invisible(TRUE))
  }
}
