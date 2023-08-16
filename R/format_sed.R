#' Format a list of data, read-in, accordingly
#'
#' Converting a list of dataframe into SED. The order of the list must be as follow.
#' * item 1: the dataframe containing matrix `factor`
#' * item 2: the dataframe containing matrix `item` for item 2
#' * item 3: the dataframe containing matrix `data`
#' * item 4: the dataframe containing matrix `item` for item 5
#' * item 5: the dataframe containing matrix `data`
#' ...
#' for more `item` and `data` matrixs, the pattern of `item`-`data` should continue.
#'
#' @param list The list of dataframes described above
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#'
#' @return a list of formatted data frames, in the order of same as input.
#' @keywords internal
#' @noRd
#'
#' @examples format_sed(list(factor, item1, data1, item2, data2))
format_sed = function(list, func = NULL){
  #Check if input list length is >=3 and odd####
  len = length(list)
  if(len < 3){
    return(invisible(seal:::sys_msg_error(title = "Length of input is insufficient",
                                         error = "Please input at least 3 items in list",
                                         func = func)))
  }
  if(len%%2 == 0){
    return(invisible(seal:::sys_msg_error(title = "Length of input must be odd.",
                                         error = "Please check the list",
                                         func = func)))
  }

  #Start formatting the data, according to `format_sedunit` ####
  factor = list[[1]]
  item = NA
  data = NA

  len = (length(list)-1)/2
  for(i in (1:len)*2){
    item = list[[i]]
    data = list[[i+1]]

    format_list = seal:::format_sedunit(factor = factor, item = item, data = data, func = func)
    if(is.logical(format_list[[1]])){if(format_list[[1]]){break}}
    if(i == 2){list[[1]] = format_list[[1]]}
    list[[i]] = format_list[[2]]
    list[[i+1]] = format_list[[3]]
  }


  #Start formatting the data, according to standard SED format ####
  SED = list(factor = list[[1]])
  for(i in 1:len){
    item = list[[(i*2)]]
    data = list[[(i*2)+1]]
    SED_unit = list(list(item = item,
                         data = data))
    class(SED_unit[[1]]) = c("sed_set")
    names(SED_unit) = paste0("data_", i)
    SED = append(SED, values = SED_unit)
  }
  class(SED) = c("sed")
  return(invisible(SED))
}
