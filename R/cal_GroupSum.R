#' Calculate the sum of each group
#'
#' Different items with different properties will have different interpretation of *"sum"*.
#' *Logical: OR operation, where a single TRUE will result in TRUE
#' *Integer/ Numeric: Numerical add operation
#' *Character: String conjunction
#'
#' @param SED The SED
#' @param data data
#'
#' @return A SED
#' @export
#'
#' @examples cal_GroupSum(SED, data = NULL)
cal_GroupSum = function(SED, data = NULL){
  #Check ####
  if(!seal::is_sed(SED)){
    return(invisible(seal:::sys_msg_error(title = "Inputing `SED` with the wrong class",
                                          error = c("Please check if `SED` is `sed`"),
                                          func = "cal_GroupSum", silent = FALSE)))
  }
  name_set = names(SED)
  name_set = name_set[unlist(lapply(SED, seal::is_sed_set))]
  if(is.null(data)){data = name_set}
  if(sum(!(data %in% name_set))){
    return(invisible(seal:::sys_msg_error(title = "`data` contains dataset not present in `SED`",
                                          error = c("Please double check `SED` and `data`"),
                                          func = "cal_GroupSum", silent = FALSE)))
  }
  for(i in data){
    sel_dataset = SED[[i]]
    sel_mtx_data = sel_dataset$data
    if(!dplyr::is_grouped_df(sel_mtx_data)){
      custom_error = paste0("Please group ", i)
      return(invisible(seal:::sys_msg_error(title = "`data` contains non-grouped dataset.",
                                            error = custom_error,
                                            func = "cal_GroupSum", silent = FALSE)))
    }
  }

  #Start ####
  for(i in data){
    sel_dataset = SED[[i]]
    sel_mtx_data = sel_dataset$data
    ava_items = seal:::sys_grab_item(sel_mtx_data)
    ava_factors = seal:::sys_grab_factor(sel_mtx_data)
    pro_data = "0"

    sel_mtx_data_grp = lapply(X = dplyr::groups(sel_mtx_data),
                              FUN = seal:::sys_hp_sym2chr) %>%
      unlist
    sel_mtx_data = dplyr::ungroup(sel_mtx_data)

    ##Start ####
    for(j in ava_items){
      sel_data = dplyr::select(.data = sel_mtx_data, dplyr::all_of(c(sel_mtx_data_grp, j)))
      sel_class = dplyr::select(.data = sel_data, {{j}}) %>% unlist %>% class
      for(k in sel_mtx_data_grp){
        sel_data = dplyr::group_by(.data = sel_data, .add = TRUE, !!rlang::sym({{k}}))
      }

      #Start the transformation ####
      if(sel_class == "logical"){
        sel_data = dplyr::reframe(.data = sel_data,
                                  "{j}" := as.logical(sum(!!rlang::sym({{j}}))))
      }
      if(sel_class == "numeric" | sel_class == "integer"){
        sel_data = dplyr::reframe(.data = sel_data,
                                  "{j}" := sum(!!rlang::sym({{j}})))
      }
      if(sel_class == "character"){
        sel_data = dplyr::reframe(.data = sel_data,
                                  "{j}" := paste0(!!rlang::sym({{j}})))
      }


      if(identical(x = pro_data, y = "0")){
        pro_data = sel_data
      } else {
        pro_data = suppressMessages(dplyr::left_join(x = pro_data, y = sel_data))
      }
    }

    #Add "@code" back to pro_data ####
    pro_data = dplyr::mutate(.data = pro_data, `@code` = as.character(NA)) %>%
      dplyr::relocate(`@code`, .before = dplyr::everything()) %>%
      seal:::format_sed_GenCode(factor = SED$factor, data = .)

    #Add group to pro_data
    for(k in sel_mtx_data_grp){
      pro_data = dplyr::group_by(.data = pro_data, .add = TRUE, !!rlang::sym({{k}}))
    }

    #Return pro_data back to SED
    SED[[i]]$data = pro_data
  }

  #return everything ####
  rm(list = ls()[ls() != "SED"])
  return(invisible(SED))
}
