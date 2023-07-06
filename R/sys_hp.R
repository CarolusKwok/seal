#' System tool: Returns a string value from symbols
#'
#' *This is a highly unstable function that does not perform any checks. Please check by yourself*
#'
#' @param x value
#'
#' @return The symbol in in character
#' @keywords internal
#' @noRd
#'
#' @examples sys_hp_sym2chr(hiiiii)
sys_hp_sym2chr = function(x){
  return(rlang::as_name(rlang::quo({{x}})))
}
