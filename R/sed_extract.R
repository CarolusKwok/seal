#' Extracting data sets within a SED
#'
#' @param SED SED
#' @param data Names of the data sets in `character`
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#'
#' @return a SED
#' @export
#'
#' @examples sed_extract(SED, data = "data_1")
sed_extract = function(SED, data, func = "sed_extract", silent = FALSE){
  #Check the inputs ####
  if(!seal::is_sed(SED)){
    return(invisible(seal:::sys_msg_error(title = "Inputing `SED` with the wrong class",
                                          error = c("Please check if `SED` is `sed`"),
                                          func = func, silent = silent)))
  }
  name = names(SED)
  name_set = name[unlist(lapply(SED, seal::is_sed_set))]
  if(!hasArg(data)){data = name_set}
  if(sum(!(data %in% name_set))){
    return(invisible(seal:::sys_msg_error(title = "`data` contains dataset not present in `SED`",
                                          error = c("Please double check `SED` and `data`"),
                                          func = func, silent = silent)))
  }

  #Start extracting datasets ####
  ##Turn data into indexs ####
  data = match(x = data, table = name)
  factor = which(unlist(lapply(SED, seal::is_sed_factor)))
  SED = SED[c(factor, data)]
  class(SED) = "sed"
  return(invisible(SED))
}
