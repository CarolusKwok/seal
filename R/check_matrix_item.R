#' SED: Check `item` matrix fulfills requirement
#'
#' @param item the `item` matrix in dataframe or tibble.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name that called this function, which is to be passed to `sys_msgerror` or `sys_msgwarning`. Default as `"ck_item"`. If `NULL`, the function name will not display.
#'
#' @return a logical value, with `TRUE` representing an error occurred; and a standardized message if `silent` is `FALSE`.
#' @keywords internal
#' @noRd
#'
#' @examples ck_item(item)
ck_item = function(item, silent = FALSE, func = "ck_item"){
  #Check if there is an input ####
  if(!hasArg(item)){return(invisible(seal:::sys_msgs_noinput(arg = "item",
                                                             expect = c("data.frame", "tibble"))))}

  #Check about the class ####
  if((!seal::is_sed_item(item))){
    return(invisible(seal:::sys_msgs_wrongclass(arg = "item",
                                                expect = c("sed_item"),
                                                silent = silent, func = func)))}
  if((!is.data.frame(item))){return(invisible(seal:::sys_msgs_wrongclass(arg = "item",
                                                                         expect = c("data.frame", "tibble"))))}

  #Temporarily remove the sed_item class for easier process ####
  class_sed = class(item)
  class(item) = class_sed[class_sed != "sed_item"]

  #Check if column names include `item`, `datatype` ####
  columns = colnames(item)
  if(!("item" %in% columns)){return(invisible(seal:::sys_msgsdf_nocolumn(col = "item", dataframe = "item",
                                                                         silent = silent, func = func)))}
  if(!("datatype" %in% columns)){return(invisible(seal:::sys_msgsdf_nocolumn(col = "datatype", dataframe = "item",
                                                                             silent = silent, func = func)))}

  #Check if the column names are unique ####
  if(sum(duplicated(columns))){
    col = unique(columns[duplicated(columns)])
    return(invisible(seal:::sys_msgsdf_notuniquecolname(col = col, dataframe = "item",
                                                        silent = silent, func = func)))
  }

  #Check if all columns are "characters"
  colclass = item %>%
    lapply(class) %>%
    unclass() %>%
    unique()

  if(length(colclass) != 1 | colclass[1] != "character"){
    return(invisible(seal:::sys_msgerror(title = "All columns in `factor` must be `character`",
                                         error = 'Please make sure all columns in {.code factor} are "character".',
                                         silent = silent, func = func)))
  }

  #Check if there is NA ####
  if(sum(is.na(item$item))){return(invisible(seal:::sys_msgsdf_columnnotfull(col = "item", dataframe = "item",
                                                                             silent = silent, func = func)))}
  if(sum(is.na(item$datatype))){return(invisible(seal:::sys_msgsdf_columnnotfull(col = "datatype", dataframe = "item",
                                                                                 silent = silent, func = func)))}
  #Check if datatype only contains "p", "n", "i", "c" ####
  if(sum(!(item$datatype %in% c("p", "n", "i", "c")))){
    return(seal:::sys_msgerror(title = "`datatype` must only contain p, n, i, c",
                               error = c("x" = "Please fill in the datatype correctly"),
                               func = func, silent = silent) %>% invisible)
  }
  #Return `F` if nothing wrong ####
  return(invisible(FALSE))
}
