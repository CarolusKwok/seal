#' Pre-check for r/w: Check the `item` matrix if it suits the requirements
#'
#' @param item the `item` matrix in dataframe or tibble.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name that called this function, which is to be passed to `sys_msgerror` or `sys_msgwarning`. Default as `"ckrw_item"`. If `NULL`, the function name will not display.
#'
#' @return a logical value, with `TRUE` representing an error occurred; and a standardized message if `silent` is `FALSE`.
#' @export
#'
#' @examples ckrw_item(item)
ckrw_item = function(item, silent = FALSE, func = "ckrw_item"){
  #Check if there is an input ####
  if(!hasArg(item)){return(invisible(seal:::sys_msgs_noinput(arg = "item",
                                                             expect = c("data.frame", "tibble"))))}

  #Check if `item` is a dataframe or tibble ####
  if((!is.data.frame(item))){return(invisible(seal:::sys_msgs_wrongclass(arg = "item",
                                                                         expect = c("data.frame", "tibble"))))}

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
