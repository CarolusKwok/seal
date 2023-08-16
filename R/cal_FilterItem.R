#' Filter items based on properties of said item
#'
#' Note that `...` expressions must be comprehensible in the dataset. Non-comprehensible expressions will return an error.
#'
#' @param SED SED
#' @param data The name of dataset to be converted, in `character`. By default (`NULL`), all datasets will be converted.
#' @param ... An expression that returns a logical value, that terms are defined by properties set in `sed_item` of the selected `data`. Only item with an evaluation of `TRUE` will be kept. For more information, please check `dplyr::filter`, which this function is based on.
#'
#' @return SED
#' @export
#'
#' @examples cal_FilterItem(SED, data = NULL, taxonomic_group == "Civet")
cal_FilterItem = function(SED, data = NULL, ...){
  #Check ####
  if(!seal::is_sed(SED)){
    return(invisible(seal:::sys_msg_error(title = "Inputing `SED` with the wrong class",
                                          error = c("Please check if `SED` is `sed`"),
                                          func = func, silent = silent)))
  }
  name_set = names(SED)
  name_set = name_set[unlist(lapply(SED, seal::is_sed_set))]
  if(is.null(data)){data = name_set}
  if(sum(!(data %in% name_set))){
    return(invisible(seal:::sys_msg_error(title = "`data` contains dataset not present in `SED`",
                                          error = c("Please double check `SED` and `data`"),
                                          func = func, silent = silent)))
  }
  #Start ####
  for(i in data){
    sel_dataset = SED[[i]]
    sel_mtx_data = sel_dataset$data
    sel_mtx_item = sel_dataset$item

    sel_mtx_item = sel_mtx_item[sel_mtx_item$item %in% colnames(sel_mtx_data),] %>%
      dplyr::filter(...)
    names_item = sel_mtx_item$item
    names_factor = seal:::sys_grab_factor(colnames(sel_mtx_data))
    sel_mtx_data = sel_mtx_data[,c("@code", names_factor, names_item)]

    #return sel_mtx_data back to SED ####
    SED[[i]]$data = sel_mtx_data
  }

  #Return ####
  rm(list = ls()[ls()!= "SED"])
  return(invisible(SED))
}
