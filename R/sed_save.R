#' Save a SED object into a xlsx file
#'
#' @param SED a SED object
#' @param file The file directory itself, with `characters`
#'
#' @return A xlsx file in the specified directory
#' @export
#'
#' @examples sed_save(SED, directory)
sed_save = function(SED, file){
  #Check SED ####
  if(!seal::is_sed(SED)){
    return(invisible(seal:::sys_msg_error(title = "Inputing `SED` with the wrong class",
                                          error = c("Please check if `SED` is `sed`"),
                                          func = "sed_save", silent = F)))
  }
  if(!hasArg(file)){
    return(invisible(seal:::sys_msg_error(title = "No file directory",
                                          error = "Please check the `file` argument.",
                                          func = "sed_save", silent = F)))
  }
  if(stringr::str_sub(string = file, start = -5L, end = -1L) != ".xlsx"){
    return(invisible(seal:::sys_msg_error(title = "File directory is not '.xlsx' file",
                                          error = "Please check the `file` argument.",
                                          func = "sed_save", silent = F)))
  }

  #Ask for permission to overwrite ####
  if(file.exists(file)){
    seal:::sys_msg_warn(title = "File exist!",
                        error = "Are you sure to overwrite it?",
                        func = "sed_save", silent = F)
    answer = readline(prompt="Overwrite (y/n): ")
    if(answer != "y"){
      return(invisible(seal:::sys_msg_error(title = "Overwrite denied",
                                            error = c("Please double check."))))
    }
  }

  #Start writing ####
  ##Reformat the SED into xlsxs ####
  for(i in 1:length(SED)){
    if(seal::is_sed_set(SED[[i]])){
      sel_item = SED[[i]]$item
      sel_data = SED[[i]]$data

      for(item_sel in seal:::sys_grab_item(colnames(sel_data))){
        datatype = dplyr::filter(.data = sel_item, item == item_sel)$datatype[[1]]
        if(datatype == "p"){
          data_sel = dplyr::select(.data = sel_data, a = {{item_sel}})$a
          data_sel = ifelse(data_sel == TRUE, "p", NA)
          sel_data = dplyr::mutate(.data = sel_data, "{item_sel}" := data_sel)
        }
      }

      SED[[i]]$data = sel_data
    }
  }

  ##Create a list of objects ####
  list = list()
  for(i in SED){
    if(seal::is_sed_factor(i)){list = append(x = list, values = list(i))}
    if(seal::is_sed_set(i)){
      list = append(x = list, values = list(i$item))
      list = append(x = list, values = list(i$data))
    }
  }
  ##Create the name ####
  name = c()
  name_sed = names(SED)
  for(i in 1:length(SED)){
    if(seal::is_sed_factor(SED[[i]])){name = append(x = name, values = "factor")}
    if(seal::is_sed_set(SED[[i]])){
      name = append(x = name, values = paste0("item_", name_sed[[i]]))
      name = append(x = name, values = paste0("data_", name_sed[[i]]))
    }
  }
  names(list) = name

  #Return the list as a .xlsx file ####
  openxlsx::write.xlsx(x = list, file = file, overwrite = TRUE)
}
