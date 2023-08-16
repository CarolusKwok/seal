#' Drop all grouping from data matrices
#'
#' @param SED SED
#' @param data The name of dataset to be converted, in `character`. By default (`NULL`), all datasets will be converted.
#'
#' @return SED
#' @export
#'
#' @examples cal_ungroup(SED)
cal_ungroup = function(SED, data = NULL){
  #Check ####
  if(!seal::is_sed(SED)){
    return(invisible(seal:::sys_msg_error(title = "Inputing `SED` with the wrong class",
                                          error = c("Please check if `SED` is `sed`"),
                                          func = "cal_group", silent = FALSE)))
  }
  name_set = names(SED)
  name_set = name_set[unlist(lapply(SED, seal::is_sed_set))]
  if(is.null(data)){data = name_set}
  if(sum(!(data %in% name_set))){
    return(invisible(seal:::sys_msg_error(title = "`data` contains dataset not present in `SED`",
                                          error = c("Please double check `SED` and `data`"),
                                          func = "cal_group", silent = FALSE)))
  }

  #Start ####
  for(i in data){
    ##Return it ####
    SED[[i]]$data = dplyr::ungroup(SED[[i]]$data)
  }

  #Return everything ####
  rm(list = ls()[ls() != "SED"])
  return(invisible(SED))
}
