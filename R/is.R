#' Test if the object is a `SED`
#'
#' Returns `TRUE` for SED data, and `FALSE` for all other objects, including lists.
#'
#' @param x An object
#'
#' @return A logical value. `TRUE` for SED data.
#' @export
#'
#' @examples is_sed(x)
#'
is_sed = function(x){inherits(x, "sed")}

#' Test if the object is a `SED_set`
#'
#' Returns `TRUE` for SED dataset, and `FALSE` for all other objects, including lists.
#'
#' @param x An object
#'
#' @return A logical value. `TRUE` for SED dataset.
#' @export
#'
#' @examples is_sed_set(x)
is_sed_set = function(x){inherits(x, "sed_set")}

#' Test if the object is a `SED_factor`
#'
#' Returns `TRUE` for SED factor matrix, and `FALSE` for all other objects, including lists.
#'
#' @param x An object
#'
#' @return A logical value. `TRUE` for SED factor matrix.
#' @export
#'
#' @examples is_sed_factor(x)
is_sed_factor = function(x){inherits(x, "sed_factor")}

#' Test if the object is a `SED_item`
#'
#' Returns `TRUE` for SED item matrix, and `FALSE` for all other objects, including lists.
#'
#' @param x An object
#'
#' @return A logical value. `TRUE` for SED item matrix.
#' @export
#'
#' @examples is_sed_item(x)
is_sed_item = function(x){inherits(x, "sed_item")}

#' Test if the object is a `SED_data`
#'
#' Returns `TRUE` for SED data matrix, and `FALSE` for all other objects, including lists.
#'
#' @param x An object
#'
#' @return A logical value. `TRUE` for SED data matrix.
#' @export
#'
#' @examples is_sed_data(x)
is_sed_data = function(x){inherits(x, "sed_data")}
