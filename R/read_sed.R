#' Read a SED file
#'
#' Standard Ecological Data (SED) files are Excel files with a fixed format.
#'
#' @param file The file directory itself, with `characters`
#' @param sheet Data sheets to be read. By default, it will read all the data sheets found.
#' @param fillMergedCells Will the funciton fill in all the merged cells in `.xlsx` file.
#'
#' @return A SED item
#' @export
#'
#' @examples read_sed(choose.files())
read_sed = function(file, sheet, fillMergedCells = TRUE){
  sheets = openxlsx::getSheetNames(file)
  sheet_data = seal:::sys_obtain_sheetdata(sheets)
  sheet_item = seal:::sys_obtain_sheetitem(sheets)

  #Check the dataset structure
  seal:::ckrw_sheet(file = file, sheet = sheet)

  #Start to read in ####
  ##Order according to `as_sed` list format ####
  list = list(openxlsx::read.xlsx(xlsxFile = file, sheet = "factor"))
  for(i in 1:length(sheet_data)){
    sel_sheet_data = sheet_data[i]
    ###Find the item and data sheet ####
    spesheet_item = paste0("list", stringr::str_sub(sel_sheet_data, start = 5L, end = 1L))
    if(spesheet_item %in% sheet_item){
      list[[i*2]] = openxlsx::read.xlsx(xlsxFile = file, sheet = spesheet_item,
                                        fillMergedCells = fillMergedCells)
      list[[i*2+1]] = openxlsx::read.xlsx(xlsxFile = file, sheet = sel_sheet_data,
                                          fillMergedCells = fillMergedCells)
    } else {
      list[[i*2]] = openxlsx::read.xlsx(xlsxFile = file, sheet = "item",
                                        fillMergedCells = fillMergedCells)
      list[[i*2+1]] = openxlsx::read.xlsx(xlsxFile = file, sheet = sel_sheet_data,
                                          fillMergedCells = fillMergedCells)
    }
  }

  #Convert to SED format
  list = seal::as_sed(list = list, func = "read_sed_xlsx")

  #Convert to a named SED
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
