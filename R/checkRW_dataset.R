#' Pre-check for r/w: Check the dataset for cooperativeness
#'
#' @param factor the `factor` matrix in dataframe or tibble.
#' @param item the `item` matrix in dataframe or tibble.
#' @param data the `data` matrix in dataframe or tibble.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name that called this function, which is to be passed to `sys_msgerror` or `sys_msgwarning`. Default as `"ckrw_dataset"`. If `NULL`, the function name will not display.
#'
#' @return a logical value, with `TRUE` representing an error occurred; and a standardized message if `silent` is `FALSE`.
#' @keywords internal
#' @noRd
#'
#' @examples ckrw_dataset(factor, item, data)
ckrw_dataset = function(factor, item, data, silent = FALSE, func = "ckrw_dataset"){
  #Run basic check of the matrix ####
  if(seal:::ckrw_factor(factor, silent = silent, func = func)){return(invisible(TRUE))}
  if(seal:::ckrw_item(item, silent = silent, func = func)){return(invisible(TRUE))}
  if(seal:::ckrw_data(data, silent = silent, func = func)){return(invisible(TRUE))}

  #Run cooperative check of the matrix, Obtain factors and items from data####
  data_colnames = colnames(data)
  used_factor = seal:::sys_obtain_factor(data_colnames)
  used_item = seal:::sys_obtain_item(data_colnames)
  ava_factor = unique(factor$factor)
  ava_item = unique(item$item)

  ##Check if factors in data are described in `factor` ####
  if(sum(!(used_factor %in% ava_factor))){
    return(invisible(seal:::sys_msgerror(title = "Not all factors in data are described in `factor`",
                                         error = c("x" = "Please double check {.code factor}"),
                                         func = func, silent = silent)))
  }
  ##Check if items in data are described in `item` ####
  if(sum(!(used_item %in% ava_item))){
    return(invisible(seal:::sys_msgerror(title = "Not all items in data are described in `item`",
                                         error = c("x" = "Please double check {.code item}"),
                                         func = func, silent = silent)))
  }

  ##Check if factors are correctly described with labels ####
  custom_message = function(factor, func, silent){
    title = paste0("Factor `", factor, "` is described incorrectly")
    return(invisible(seal:::sys_msgerror(title = title,
                                         error = c("x" = "Please check the `factor` and `data` matrix"),
                                         func = func, silent = silent)))
  }
  for(i in used_factor){
    df_label_sel = dplyr::filter(factor,
                                 factor == i)$label
    df_data_sel = dplyr::select(.data = data,
                                {{i}})[,1]
    if(df_label_sel[1] != "###"){
      if(sum(!(df_data_sel %in% df_label_sel))){
        return(invisible(custom_message(factor = i, func = func, silent = silent)))
      }
    }
  }

  ##Check if items are correctly described with datatype ####
  custom_message = function(item, func, silent){
    title = paste0("Item `", item, "` is described incorrectly")
    return(invisible(seal:::sys_msgerror(title = title,
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

  #Return F if ok ####
  return(invisible(FALSE))
}
