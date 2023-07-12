#' Printing SED data
#'
#' @param x an object
#'
#' @return A visual print of the SED data
#' @export
#'
#' @examples print(SED)
print.sed = function(x){
  name = names(x)
  for(i in 1:length(x)){
    print(x[[i]], name = name[[i]])
    if(i < length(x)){
      cli::cli_text("")
    }
  }
}

#' Printing SED dataset
#'
#' @param x an object
#' @param name name of the SED dataset
#'
#' @return A visual print of the SED data
#' @keywords internal
#' @noRd
#'
#' @examples print(SED_set)
print.sed_set = function(x, name){
  cli::cli_div(theme = list(span.grey = list(color  = "#9E9E9E")))
  cli::cli_text("{.grey Dataset: {name}}")
  for(i in 1:length(x)){
    print(x[[i]])
  }
}
