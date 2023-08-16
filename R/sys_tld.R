#' System tools: Convert all columns in a dataframe to characters, regardless
#'
#' @param x A dataframe
#'
#' @return A dataframe, with all columns being characters
#' @keywords internal
#' @noRd
#'
#' @examples sys_tld_dfconv2char(x)
sys_tld_dfconv2char = function(x){
  x = dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character))
  return(x)
}


#' System tools: Swap between Presence/Absence and Logical data
#'
#' @param x A vector
#'
#' @return A vector
#' @keywords internal
#' @noRd
#'
#' @examples sys_tld_plswap(x)
sys_tld_plswap = function(x){
  class = class(x)
  if(class == "logical"){return(seal:::sys_tld_logical2p(x))}
  if(class == "character"){return(seal:::sys_tld_p2logical(x))}
  return()
}

#' System tools: Convert Presence/Absence to Logical data
#'
#' @param x A vector of Presence/Absence data
#'
#' @return A vector of Logical data
#' @keywords internal
#' @noRd
#'
#' @examples sys_tld_p2logical(x)
sys_tld_p2logical = function(x){
  x = (x == "p" & !is.na(x))
  return(x)
}

#' System tools: Convert Logical to Presence/Absence data
#'
#' @param x A vector of Logical data
#'
#' @return A vector of Presence/Absence data
#' @keywords internal
#' @noRd
#'
#' @examples sys_tld_logical2p(x)
sys_tld_logical2p = function(x){
  x = ifelse(x, "p", NA)
  return(x)
}
