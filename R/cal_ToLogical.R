#' Convert data to logical data
#'
#' @param SED SED
#' @param data The name of dataset to be converted, in `character`. By default (`NULL`), all datasets will be converted.
#' @param item The name of item to be converted across all of `data`, in `character`. By default (`NULL`), all convertible items will be converted into logical data
#'
#' @return SED
#' @export
#'
#' @examples cal_ToLogical(SED, data = "1", item = "a")
cal_ToLogical = function(SED, data = NULL, item){
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

  #Start formatting ####
  for(i in data){
    sel_dataset = SED[[i]]
    sel_mtx_item = sel_dataset$item
    sel_mtx_data = sel_dataset$data
    if(!hasArg(item)){
      sel_item = seal:::sys_grab_item(colnames(sel_mtx_data))
    } else {
      sel_item = item
    }

    #Start the logical conversion ####
    ##Select the items to convert
    sel_item = tibble::tibble(item = sel_item) %>%
      dplyr::mutate(present = item %in% colnames(sel_mtx_data)) %>%
      dplyr::filter(present) %>%
      dplyr::left_join(dplyr::select(sel_mtx_item, item, datatype), by = "item") %>%
      dplyr::filter(datatype %in% c("p", "n", "i"))
    ##Convert the item ####
    sel_mtx_data = dplyr::mutate(sel_mtx_data,
                                 dplyr::across(sel_item$item, .fns = ~ as.logical(.x)))

    ##Mark it's conversion in item ####
    new_type = dplyr::select(sel_mtx_item, item, datatype) %>%
      dplyr::mutate(new_datatype = (item %in% sel_item$item),
                    datatype = ifelse(new_datatype, "p", datatype))
    sel_mtx_item = dplyr::mutate(sel_mtx_item, datatype = new_type$datatype)
    #Return the conversion ####
    SED[[i]]$item = sel_mtx_item
    SED[[i]]$data = sel_mtx_data
  }

  #Return the SED ####
  rm(list = ls()[ls()!= "SED"])
  return(invisible(SED))
}
