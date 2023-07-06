#' Format the data, read-in, accordingly
#'
#' @param factor The `factor` matrix in dataframe or tibble.
#' @param item The `item` matrix in dataframe or tibble.
#' @param data The `data` matrix in dataframe or tibble.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#'
#' @return a list of formatted data frames, in the order of `factor`, `item`, `data`.
#' @export
#'
#' @examples form_read(factor, item, data)
form_read = function(factor, item, data, func = "form_read"){
  #Perform
  if(seal::ckrw_dataset(factor, item, data, func = func)){return(invisible())}

  #Extract data ####
  factor = factor %>% dplyr::arrange(factor)
  data$`@code` = ifelse(is.na(data$`@code`), "", data$`@code`)
  #Start format the data set ####
  ##Create abbr if there isn't ####
  if(anyNA(factor$abbr)){
    factor_ava = unique(factor$factor)
    tmp_factor = dplyr::select(.data = factor,
                               factor, label, abbr) %>%
      dplyr::mutate(nchar = nchar(label))
    for(i in factor_ava){
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
    tmp_factor = dplyr::select(factor, factor, label, abbr)
    tmp_data = data
    factor_ava = colnames(tmp_data)
    factor_ava = factor_ava[stringr::str_sub(factor_ava, start = 1L, end = 1L) %>%
                              stringr::str_detect(pattern = "#")]
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
  tmp_factor = dplyr::select(.data = factor,
                             factor, label, abbr)
  factor_ava = colnames(tmp_data)
  factor_ava = factor_ava[stringr::str_sub(factor_ava, start = 1L, end = 1L) %>%
                            stringr::str_detect(pattern = "#")]

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
  item_ava = colnames(data)
  item_ava = item_ava[stringr::str_sub(item_ava, start = 1L, end = 1L) %>%
                        stringr::str_detect(pattern = "#|@", negate = TRUE)]
  tmp_data = data

  for(i in item_ava){
    item_sel = dplyr::filter(.data = item, item == i)
    data_sel = dplyr::select(.data = data, a = {{i}})$a
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
  #Final check the data set####
  seal::ck_dataset(factor = factor,
                 item = item,
                 data = data, func = func)
  #Return the item
  return(invisible(list(factor, item, data)))
}



#' Format a list of data, read-in, accordingly
#'
#' This function is a substitute for `form_read`, allowing to read in a `list` of dataframe. The order of the list must be as follow.
#' * item 1: the dataframe containing matrix `factor`
#' * item 2: the dataframe containing matrix `item`
#' * item 3: the dataframe containing matrix `data`
#' for more `item` and `data` matrixs, the pattern of `item`-`data` should continue.
#'
#' @param list The list of dataframes
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#'
#' @return a list of formatted data frames, in the order of same as input.
#' @export
#'
#' @examples form_read_multi(list(factor, item1, data1, item2, data2))
form_read_multi = function(list, func = "form_read_multi"){
  #Check if input list length is >=3 and odd####
  len = length(list)
  if(len < 3){
    return(invisible(seal:::sys_msgerror(title = "Length of input is insufficient",
                                         error = "Please input at least 3 items in list",
                                         func = func)))
  }
  if(len%%2 == 0){
    return(invisible(seal:::sys_msgerror(title = "Length of input must be odd.",
                                         error = "Please check the list",
                                         func = func)))
  }

  #magic ####
  factor = list[[1]]
  item = NA
  data = NA

  len = (length(list)-1)/2
  for(i in (1:len)*2){
    item = list[[i]]
    data = list[[i+1]]

    format_list = seal::form_read(factor = factor, item = item, data = data, func = "form_read_multi")
    if(is.logical(format_list[[1]])){if(format_list[[1]]){break}}
    if(i == 2){list[[1]] = format_list[[1]]}
    list[[i]] = format_list[[2]]
    list[[i+1]] = format_list[[3]]
  }
  return(invisible(list))
}
