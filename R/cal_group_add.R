#' Add grouping to data matrices
#'
#' @param SED SED
#' @param data The name of dataset to be converted, in `character`. By default (`NULL`), all datasets will be converted.
#' @param factor Factors within a dataset to be added to grouping, in `character`. By default (`NULL`), all factors will be added to grouping. If the factor is unavailable, said factor will be ignored.
#' @param overwrite Should this function overwrite previous grouping? By default (`FALSE`), grouping will overwrite previous grouping. For more information, please review the `.add` argument in `dplyr::group_by`
#'
#' @return SED
#' @export
#'
#' @examples cal_group(SED, data = NULL, factor = "#region")
cal_group = function(SED, data = NULL, factor = NULL, overwrite = FALSE){
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
    sel_dataset = SED[[i]]
    sel_mtx_data = sel_dataset$data

    ##Get the factors available, if not available, discard it ####
    ava_factor = seal:::sys_grab_factor(colnames(sel_mtx_data))
    if(is.null(factor)){
      sel_factor = ava_factor
    } else {
      sel_factor = ava_factor[ava_factor %in% factor]
    }

    ##Group it! ####
    if(overwrite){sel_mtx_data = dplyr::ungroup(x = sel_mtx_data)}
    for(f in sel_factor){sel_mtx_data = dplyr::group_by(.data = sel_mtx_data,
                                                        !!rlang::sym({{f}}),
                                                        .add = TRUE)}

    ##Return it ####
    SED[[i]]$data = sel_mtx_data
  }

  #Return everything ####
  rm(list = ls()[ls() != "SED"])
  return(invisible(SED))
}
