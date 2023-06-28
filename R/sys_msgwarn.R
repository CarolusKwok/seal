#' Create a standardized error message for the package
#'
#' @param title Title of the error in `character`. This will be passed into `cli::cli_text`.
#' @param error Details of the error in  `character`. This will be passed into `cli::cli_bullets`. Use `"!"` for each bullet.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#'
#' @internal
#' @export
#'
#' @examples seal::sys_msgwarning("Data incomplete", c("!" = "Please fill in the data"), func = "data_check")
sys_msgwarning = function(title,
                          error,
                          func = NULL){
  if(!hasArg(title)){
    cli::cli_text(cli::bg_yellow(cli::col_black("Error in {.fn sys_msgerror}: No {.code title}")))
    cli::cli_bullets(c("x" = "Please fill in the {.code title}"))
    return(invisible())
  }
  if(!hasArg(error)){
    cli::cli_text(cli::bg_yellow(cli::col_black("Error in {.fn sys_msgerror}: No {.code error}")))
    cli::cli_bullets(c("x" = "Please fill in the {.code error}"))
    return(invisible())
  }

  if(!is.null(func)){
    cli::cli_text(cli::bg_yellow(cli::col_black("Warning in {.fn {func}}: {title}")))
  } else {
    cli::cli_text(cli::bg_yellow(cli::col_black("Warning: {title}")))
  }
  cli::cli_bullets(error)
  return(invisible())
}
