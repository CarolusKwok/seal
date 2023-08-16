#' Binding 2 SEDs into 1 SED
#'
#' @param SED1 SED 1
#' @param SED2 SED 2
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#'
#' @return a SED
#' @export
#'
#' @examples sed_bind(SED1, SED2)
sed_bind = function(SED1, SED2, func = "sed_bind", silent = FALSE){
  #Check SED ####
  if(!seal::is_sed(SED1)){
    return(invisible(seal:::sys_msg_error(title = "Inputing `SED1` with the wrong class",
                                          error = c("Please check if `SED1` is `sed`"),
                                          func = func, silent = silent)))
  }
  if(!seal::is_sed(SED2)){
    return(invisible(seal:::sys_msg_error(title = "Inputing `SED2` with the wrong class",
                                          error = c("Please check if `SED2` is `sed`"),
                                          func = func, silent = silent)))
  }

  #Start binding ####
  ##Recreate factor ####
  SED1_factor = seal::extract_factor(SED1, silent = FALSE, func = func)
  SED2_factor = seal::extract_factor(SED2, silent = FALSE, func = func)
  factor = seal:::sys_tld_dfconv2char(tibble::tibble(factor = NA, label = NA, abbr = NA, .rows = 0))
  tmp_factor1 = dplyr::distinct(dplyr::bind_rows(SED1_factor, SED2_factor))
  for(i in seal:::sys_grab_factor(tmp_factor1$factor) %>% unique){
    tmp_factor2 = dplyr::filter(tmp_factor1, factor == {{i}})
    if("###" %in% tmp_factor2$label){tmp_factor2 = dplyr::filter(tmp_factor2, label == "###")}
    factor = dplyr::bind_rows(factor, tmp_factor2)
  }
  factor = dplyr::arrange(.data = factor, factor, label)
  class(factor) = c("sed_factor", class(factor))

  ##Recreate datasets ####
  SED1_data = seal::extract_sed_set(SED = SED1, silent = FALSE, func = func)
  SED2_data = seal::extract_sed_set(SED = SED2, silent = FALSE, func = func)

  SED = list(factor)
  for(i in 1:length(SED1_data)){
    SED = append(x = SED, SED1_data[i])
  }
  for(i in 1:length(SED2_data)){
    SED = append(x = SED, SED2_data[i])
  }
  ##Rename datasets ####
  name1 = names(SED1)
  name2 = names(SED2)
  tmp_names = c("factor", name1[name1 != "factor"], name2[name2 != 'factor'])
  names = c()
  for(i in tmp_names){
    if(!(i %in% names)){
      names = append(names, i)
      next
    } else {
      j = 0
      flag = TRUE
      while(flag){
        j = j + 1
        if(!(paste0(i, "_",j) %in% names)){flag = FALSE}
      }
      names = append(names, values = paste0(i, "_", j))
    }
  }
  names(SED) = names

  #Return SED ####
  class(SED) = "sed"
  return(invisible(SED))
}
