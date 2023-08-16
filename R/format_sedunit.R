#' Format the data, read-in, accordingly
#'
#' @param factor The `factor` matrix in dataframe or tibble.
#' @param item The `item` matrix in dataframe or tibble.
#' @param data The `data` matrix in dataframe or tibble.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#'
#' @return a list of formatted data frames, in the order of `factor`, `item`, `data`.
#' @keywords internal
#' @noRd
#'
#' @examples format_sedunit(factor, item, data)
format_sedunit = function(factor, item, data, func = NULL){
  #Perform a check ####
  if(seal:::ckrw_dataset(factor, item, data, func = func)){return(invisible(T))}

  #Clean up and Extract data ####
  factor = dplyr::arrange(factor, factor)
  data$`@code` = ifelse(is.na(data$`@code`), "", data$`@code`)
  data_factors = seal:::sys_grab_factor(colnames(data))
  data_items = seal:::sys_grab_item(colnames(item))
  ava_factors = unique(factor$factor)
  ava_items = unique(item$item)
  #Start format the data set ####
  ##Make all columns in factor and item as characters ####
  factor = seal:::sys_tld_dfconv2char(factor)
  item = seal:::sys_tld_dfconv2char(item)

  ##Format data's factors into characters if not flexible ####
  for(i in data_factors){
    sel_label = dplyr::filter(factor, factor == i)$label
    if(length(sel_label) == 1){
      if(sel_label == "###"){
        next
      }
    } else {
      tmp_data = unlist(dplyr::select(data, {{i}}))
      data = dplyr::mutate(data, "{i}" := as.character(tmp_data))
    }
  }
  ##Create abbr if there isn't ####
  if(anyNA(factor$abbr)){
    tmp_factor = dplyr::select(.data = factor, factor, label, abbr) %>%
      dplyr::mutate(nchar = nchar(label))
    for(i in ava_factors){
      tmp_factor_sel = dplyr::filter(.data = tmp_factor,
                                     factor == i)
      max_nchar = max(tmp_factor_sel$nchar)
      for(j in 1:max_nchar){
        tmp_factor_sel = dplyr::mutate(tmp_factor_sel,
                                       abbr = stringr::str_sub(label, start = 1L, end = j),
                                       sub = ifelse(j - nchar < 0, 0, j - nchar),
                                       abbr = paste0(abbr, stringr::str_dup("_", sub)))
        if(length(unique(tmp_factor_sel$abbr)) == nrow(tmp_factor_sel)){break}
      }
      tmp_factor = dplyr::mutate(tmp_factor,
                                 tmp_abbr = stringr::str_sub(label, start = 1L, end = j),
                                 sub = ifelse(j - nchar < 0, 0, j - nchar),
                                 tmp_abbr = paste0(tmp_abbr, stringr::str_dup("_", sub)),
                                 abbr = ifelse(factor == i, tmp_abbr, abbr))
    }
    factor$abbr = tmp_factor$abbr
  }
  ##Create @code in data if there isn't ####
  if(sum(data$`@code` == "")){
    tmp_factor = dplyr::select(.data = factor, factor, label, abbr)
    tmp_data = data
    factor_ava = seal:::sys_grab_factor(colnames(tmp_data))
    tmp_data = dplyr::select(.data = tmp_data,
                             `@code`, {{factor_ava}})

    for(i in factor_ava){
      tmp_factor_sel = dplyr::filter(.data = tmp_factor, factor == i) %>%
        dplyr::select(-factor)
      flag_hashed = as.logical(sum(tmp_factor_sel$label == "###"))
      tmp_factor_sel = dplyr::rename(tmp_factor_sel, "{i}" := label)
      if(flag_hashed){
        tmp_data = dplyr::mutate(tmp_data, abbr = "#")
      } else {
        tmp_data = dplyr::left_join(tmp_data, tmp_factor_sel, by = {{i}})
      }
      tmp_data = dplyr::mutate(tmp_data,
                               `@code` = paste0(`@code`, abbr)) %>%
        dplyr::select(-abbr)
    }
    data$`@code` = tmp_data$`@code`
  }
  ##Format factors in data ####
  tmp_data = data
  tmp_factor = dplyr::select(.data = factor, factor, label, abbr)
  factor_ava = seal:::sys_grab_factor(colnames(tmp_data))

  for(i in factor_ava){
    tmp_factor_sel = dplyr::filter(.data = tmp_factor, factor == i)
    data_sel = dplyr::select(.data = data, a = {{i}})$a
    if(sum(tmp_factor_sel$label == "###")){
      next
    } else {
      data_sel = as.factor(data_sel)
      data = dplyr::mutate(data, "{i}" := data_sel)
    }
  }

  ##Format items in data####
  item_ava = seal:::sys_grab_item(colnames(data))
  tmp_data = data
  for(i in item_ava){
    item_sel = dplyr::filter(.data = item, item == i)
    data_sel = dplyr::select(.data = data, a = {{i}})$a
    type_sel = item_sel$datatype[1]
    if(item_sel$datatype[1] == "p"){
      data_sel = (data_sel == "p") %>%
        ifelse(is.na(.), FALSE, .)
      data = dplyr::mutate(data, "{i}" := data_sel)
    }
    if(item_sel$datatype[1] == "n"){
      data_sel = as.numeric(data_sel)
      data = dplyr::mutate(data, "{i}" := data_sel)
    }
    if(item_sel$datatype[1] == "i"){
      data_sel = as.integer(data_sel)
      data = dplyr::mutate(data, "{i}" := data_sel)
    }
    if(item_sel$datatype[1] == "c"){
      data_sel = as.character(data_sel)
      data = dplyr::mutate(data, "{i}" := data_sel)
    }
  }

  #Describe the items with classes####
  normal_class = c("tbl_df", "tbl", "data.frame")
  class(factor) = c("sed_factor", normal_class)
  class(item) = c("sed_item", normal_class)
  class(data) = c("sed_data", normal_class)

  #Return the item
  return(invisible(list(factor, item, data)))
}
