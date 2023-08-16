#' Pre-check for r/w: Check all matrixs fulfills requirement
#'
#' @param factor the `factor` matrix in dataframe or tibble.
#' @param item the `item` matrix in dataframe or tibble.
#' @param data the `data` matrix in dataframe or tibble.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func he function name that called this function, which is to be passed to `sys_msg_error` or `sys_msgwarning`. Default as `NULL`, the function name will not display.
#'
#' @return a logical value, with `TRUE` representing an error occurred; and a standardized message if `silent` is `FALSE`.
#' @keywords internal
#' @noRd
#'
#' @examples ckrw_dataset(factor, item, data)
ckrw_dataset = function(factor, item, data, silent = FALSE, func = NULL){
  if(seal:::ckrw_factor(factor = factor, silent = silent, func = func)){return(invisible())}
  if(seal:::ckrw_item(item = item, silent = silent, func = func)){return(invisible())}
  if(seal:::ckrw_data(data = data, silent = silent, func = func)){return(invisible())}

  #Check compatibility #####
  data_colnames = colnames(data)
  used_factor = seal:::sys_grab_factor(data_colnames)
  used_item = seal:::sys_grab_item(data_colnames)
  ava_factor = unique(factor$factor)
  ava_item = unique(item$item)
  ##Check if factors in data are described in `factor` ####
  if(sum(!(used_factor %in% ava_factor))){
    return(invisible(seal:::sys_msg_error(title = "Not all factors in data are described in `factor`",
                                          error = c("Please check `factor`"),
                                          func = func, silent = silent)))
  }

  ##Check if items in data are described in `item` ####
  if(sum(!(used_item %in% ava_item))){
    return(invisible(seal:::sys_msg_error(title = "Not all items in data are described in `item`",
                                          error = c("x" = "Please double check {.code item}"),
                                          func = func, silent = silent)))
  }
  ##Check if factors are correctly described with labels ####
  custom_message = function(factor, func, silent){
    title = paste0("Factor `", factor, "` is described incorrectly")
    return(invisible(seal:::sys_msg_error(title = title,
                                         error = c("x" = "Please check the `factor` and `data` matrix"),
                                         func = func, silent = silent)))
  }
  for(i in used_factor){
    df_label_sel = dplyr::filter(factor, factor == i)$label
    df_data_sel = dplyr::select(.data = data, {{i}})[,1]
    if(df_label_sel[1] != "###"){
      if(sum(!(df_data_sel %in% df_label_sel))){
        return(invisible(custom_message(factor = i, func = func, silent = silent)))
      }
    }
  }
  ##Check if items are correctly described with datatype ####
  custom_message = function(item, func, silent){
    title = paste0("Item `", item, "` is described incorrectly")
    return(invisible(seal:::sys_msg_error(title = title,
                                         error = c("x" = "Please check the `item` and `data` matrix"),
                                         func = func, silent = silent)))
  }
  for(i in used_item){
    df_item_sel = dplyr::filter(item,
                                item == i)$datatype
    df_data_sel = dplyr::select(.data = data, a = {{i}})$a
    if(df_item_sel == "p"){
      if(sum((df_data_sel == "p" | is.na(df_data_sel))) != length(df_data_sel)){
        return(invisible(custom_message(item = i, func = func, silent = silent)))
      }
    }
    if(df_item_sel == "n"){
      if(sum(!is.numeric(df_data_sel))){
        return(invisible(custom_message(item = i, func = func, silent = silent)))
      }
    }
    if(df_item_sel == "i"){
      if(sum(!is.numeric(df_data_sel))){
        return(invisible(custom_message(item = i, func = func, silent = silent)))
      } else if(sum(as.integer(df_data_sel) != as.numeric(df_data_sel))) {
        return(invisible(custom_message(item = i, func = func, silent = silent)))
      }
    }
    if(df_item_sel == "c"){
      if(sum(!is.character(df_dta_sel))){
        return(invisible(custom_message(item = i, func = func, silent = silent)))
      }
    }
  }

  #Return FALSE if correct ####
  return(FALSE)
}
