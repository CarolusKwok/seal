#' Returns a string value from symbols
#'
#' @param x value
#'
#' @noRd
#'
#' @examples sys_hp_sym2chr(hiiiii)
sys_hp_sym2chr = function(x){
  return(rlang::as_name(rlang::quo({{x}})))
}
