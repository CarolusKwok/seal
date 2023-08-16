#' Read a xlsx file into a SED object
#'
#' Standard Ecological Data (SED) files are Excel files with a fixed format.
#'
#' @param file The file directory itself, with `characters`
#' @param sheet Data sheets to be read. By default, it will read all the data sheets found.
#' @param fillMergedCells Will the function fill in all the merged cells in `.xlsx` file? By default (`TRUE`), the function will fill merged cell.
#'
#' @return A SED
#' @export
#'
#' @examples sed_read.character(choose.files())
sed_read = function(file, sheet, fillMergedCells = TRUE){
  #Check the input ####
  if(!file.exists(file)){
    return(invisible(seal:::sys_msg_error(title = "File doesn't exist.",
                                          error = "Please check `obj`.",
                                          func = "sed_read", silent = F, flag = F)))
  }
  if(stringr::str_sub(file, start = -4, end = -1) != "xlsx"){
    return(invisible(seal:::sys_msg_error(title = "File is not an .xlsx file",
                                          error = "Please check `obj`.",
                                          func = "sed_read", silent = F, flag = F)))
  }
  if(seal:::ckrw_sheet(file = file, sheet = sheet, silent = F, func = "sed_read")){return(invisible())}

  #Start formatting into SED ####
  ##Find all sheets used in SED ####
  sheets = openxlsx::getSheetNames(file)
  sheet_data = seal:::sys_grab_sheetdata(sheets)
  sheet_item = seal:::sys_grab_sheetitem(sheets)
  if(!hasArg(sheet)){
    sheet = seal:::sys_grab_sheetdata(openxlsx::getSheetNames(file))
  }

  ##Order different dataframes into list ####
  list = list(openxlsx::read.xlsx(xlsxFile = file, sheet = "factor") %>% seal:::sys_tld_dfconv2char())
  for(i in 1:length(sheet)){
    sel_sheet_data = sheet[i]
    sel_sheet_item = paste0("item_", stringr::str_sub(sel_sheet_data, start = 6L, end = -1L))
    ###Find the item and data sheet ####
    if(sel_sheet_item %in% sheet_item){
      list[[i*2]] = openxlsx::read.xlsx(xlsxFile = file, sheet = sel_sheet_item, fillMergedCells = fillMergedCells)
      list[[i*2+1]] = openxlsx::read.xlsx(xlsxFile = file, sheet = sel_sheet_data, fillMergedCells = fillMergedCells)
    } else {
      list[[i*2]] = openxlsx::read.xlsx(xlsxFile = file, sheet = "item", fillMergedCells = fillMergedCells)
      list[[i*2+1]] = openxlsx::read.xlsx(xlsxFile = file, sheet = sel_sheet_data, fillMergedCells = fillMergedCells)
    }
  }
  #Convert to SED format ####
  list = seal:::format_sed(list = list, func = NULL)

  #Convert to a named SED ####
  tmp_names = c("factor",
                ifelse(sheet_data == "data",
                       "data",
                       stringr::str_sub(string = sheet_data, start = 6L, end = -1L)))
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
        if(!(paste0(i,j) %in% names)){flag = FALSE}
      }
      names = append(names, values = paste0(i, j))
    }
  }
  names(list) = names
  return(invisible(list))
}
