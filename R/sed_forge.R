#' Forge a `data` matrix into an SED
#'
#' @param obj An object
#' @param ...
#'
#' @return A SED
#' @export
#'
#' @examples sed_forge(data)
sed_forge = function(x, ...){
  UseMethod("sed_forge")
}

#' Read a dataframe and convert it into SED
#'
#' @param x The dataframe itself
#' @param flexible_factor Name of any flexible factors. By default (`NA`), there are no flexible factors.
#'
#' @return A SED
#' @keywords internal
#' @export
#'
#' @examples sed_forge.data.frame(data)
sed_forge.data.frame = function(x, flexible_factor = NA){
  data = x
  #Check the dataframe for rw ####
  if(seal:::ckrw_data(data, silent = FALSE, func = "sed_forge")){return(invisible())}

  #Transform `data` into sed ####
  ava_colname = colnames(data)
  ava_factor = seal:::sys_grab_factor(ava_colname)
  ava_item = seal:::sys_grab_item(ava_colname)

  ##Generate `factor`
  factor = dplyr::select(.data = data, {{ava_factor}}) %>%
    seal:::sys_tld_dfconv2char() %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "factor", values_to = "label") %>%
    dplyr::mutate(abbr = as.character(NA),
                  label = ifelse((factor %in% flexible_factor), "###", label)) %>%
    dplyr::arrange(factor) %>%
    dplyr::distinct()

  ##Generate `item`
  item = data.frame(item = ava_item, datatype = as.character(NA))
  for(i in 1:nrow(item)){
    sel_item = item$item[i]
    sel_content = unname(unlist(dplyr::select(data, {{sel_item}})))
    sel_datatype= seal:::sys_tld_autodatatype(sel_content)
    item$datatype[i] = sel_datatype
  }

  #Forge the SED ####
  SED = seal:::format_sed(list(factor, item, data), func = "sed_forge")

  #Return the SED ####
  return(invisible(SED))
}

#' Read a incomplete SED file, and convert it into SED
#'
#' @param x The file directory itself, with `characters`
#' @param sheet Data sheets to be read. By default, it will read all the data sheets found.
#' @param flexible_factor Name of any flexible factors. By default (`NA`), there are no flexible factors.
#' @param fillMergedCells Will the function fill in all the merged cells in `.xlsx` file? By default (`TRUE`), the function will fill merged cell.
#'
#' @return A SED
#' @keywords internal
#' @export
#'
#' @examples sed_forge.character(choose.files())
sed_forge.character = function(x, sheet, flexible_factor = NA, fillMergedCells = TRUE){
  file = x
  #Check the input ####
  if(!file.exists(file)){
    return(invisible(seal:::sys_msg_error(title = "File doesn't exist.",
                                          error = "Please check `obj`.",
                                          func = "sed_forge", silent = F, flag = F)))
  }
  if(stringr::str_sub(file, start = -4, end = -1) != "xlsx"){
    return(invisible(seal:::sys_msg_error(title = "File is not an .xlsx file",
                                          error = "Please check `obj`.",
                                          func = "sed_forge", silent = F, flag = F)))
  }
  sheets = openxlsx::getSheetNames(file)
  if(length(seal:::sys_grab_sheetdata(sheets)) == 0){
    return(invisible(seal:::sys_msg_error(title = "File doesn't contain any `data`",
                                          error = "Please check the file.",
                                          func = "sed_forge", silent = F, flag = F)))
  }
  #Get the sheets that need to be done ####
  if(!hasArg(sheet)){
    sheet = seal:::sys_grab_sheetdata(sheets)
  } else {
    if(sum(!(sheet %in% seal:::sys_grab_sheetdata(sheets)))){
      return(invisible(seal:::sys_msg_error(title = "`sheet` contains unavailable sheets in file",
                                            error = "Please check the file and the `sheet` argument.",
                                            func = "sed_forge", silent = F, flag = F)))
    }
  }

  #Read the sheets accordingly ####
  list_SED = sheet %>%
    lapply(FUN = openxlsx::read.xlsx, xlsxFile = file, fillMergedCells = fillMergedCells) %>%
    lapply(FUN = seal::sed_forge, flexible_factor = flexible_factor)

  #Turn into an SED ####
  len = length(list_SED)
  SED = list_SED[[1]]
  if(len != 1){
    for(i in 2:len){
      SED = seal::sed_bind(SED1 = SED, SED2 = list_SED[[i]])
    }
  }

  #Return SED####
  return(invisible(SED))
}
