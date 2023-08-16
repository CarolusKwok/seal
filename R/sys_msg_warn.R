#' Create a standardized error message for the package
#'
#' @param title Title of the error in `character`. This will be passed into `cli::cli_text`.
#' @param error Details of the error in  `character`. Accepts a vector of text for multiple lines.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#' @param flag Will this function silently return a logical value to indicate an error occurred? `TRUE` indicates an error occurred. Default as `TRUE`, which will return the value.
#'
#' @return a standardized text message and logical value
#' @keywords internal
#' @noRd
#'
#' @examples seal:::sys_msg_warn("Data incomplete", c("Please fill in the data"), func = "data_check")
sys_msg_warn = function(title, error, func = NULL, silent = FALSE, flag = TRUE){
  if(!silent){
    if(!is.null(func)){
      cli::cli_alert_warning("Warning in {.fn {func}}: {title}")
    } else {
      cli::cli_alert_warning("Warning: {title}")
    }
    for(i in error){
      cli::cli_bullets(c("i" = paste0(" ", i)))
    }
  }
  if(flag){return(invisible(TRUE))}
}
