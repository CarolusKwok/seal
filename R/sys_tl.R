#' System tools: Obtain the `data` sheets in xlsx file
#'
#'
#' @param sheet a vector of sheet names
#'
#' @return a vector of characters, that are `data` sheets
#' @keywords internal
#' @noRd
#'
#' @examples sys_grab_sheetdata(sheet = c("factor", "item", "data_1", "data_2")) #Should retrun "data_1" and "data_2"
sys_grab_sheetdata = function(sheet){
  sheetdata = stringr::str_sub(sheet, start = 1L, end = 5L)
  sheetdata = sheet[sheetdata == "data" | sheetdata == "data_"]
  return(sheetdata)
}

#' System tools: Obtain the `item` sheets in xlsx file
#'
#' @param sheet a vector of sheet names
#'
#' @return a vector of characters, that are `item` sheets
#' @keywords internal
#' @noRd
#'
#' @examples sys_grab_sheetitem(sheet = c("factor", "item", "data_1", "data_2")) #Should return "item"
sys_grab_sheetitem = function(sheet){
  sheetitem = stringr::str_sub(sheet, start = 1L, end = 5L)
  sheetitem = sheet[sheetitem == "item" | sheetitem == "item_"]
  return(sheetitem)
}




#' System tools: Obtain the `factors` from `data` sheets
#'
#' @param colnames The column names from the `data` sheet
#'
#' @return a vector of characters, that are `data` sheets
#' @keywords internal
#' @noRd
#'
#' @examples sys_grab_factor(colnames = c('@code', '#factor', 'item')) #Should return "#factor"
sys_grab_factor = function(colnames){
  factors = stringr::str_sub(string = colnames, start = 1L, end = 1L) %>%
    stringr::str_detect(pattern = "#")
  factors = colnames[factors]
  return(factors)
}

#' System tools: Obtain the `items` from `data` sheets
#'
#' @param colnames The column names from the `data` sheet
#'
#' @return a vector of characters, that are `data` sheets
#' @keywords internal
#' @noRd
#'
#' @examples sys_grab_item(colnames = c('@code', '#factor', 'item')) #Should return "item"
sys_grab_item = function(colnames){
  items = stringr::str_sub(string = colnames, start = 1L, end = 1L) %>%
    stringr::str_detect(pattern = "#|@", negate = T)
  items = colnames[items]
  return(items)
}
