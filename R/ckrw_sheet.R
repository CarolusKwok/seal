#' Pre-check for r/w: Check the sheet structure of `xlsx` file.
#'
#' @param file The file address of the `xlsx` file.
#' @param sheet The `data-sheets` to be read in. By default (`NULL`), all `data-sheets` will be read in.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name causing the error in `character`. By default `NULL`, this will not be displayed.
#'
#' @return a logical value, with `TRUE` representing an error occurred; and a standardized message if `silent` is `FALSE`.
#' @keywords internal
#' @noRd
#'
#' @examples ckrw_sheet(file = DIR)
ckrw_sheet = function(file, sheet, silent = F, func = NULL){
  #Prepare ####
  sheets = openxlsx::getSheetNames(file)
  sheet_data = seal:::sys_grab_sheetdata(sheet = sheets)
  sheet_item = seal:::sys_grab_sheetitem(sheet = sheets)

  #Check if sheet `factor` is present ####
  if(!("factor" %in% sheets)){
    return(invisible(seal:::sys_msg_error(title = "`factor` is missing in xlsx file",
                                          error = "Please create the `factor` sheet.",
                                          silent = silent, func = func)))
  }

  #Check if arg `sheet` are present in file as data sheets####
  if(hasArg(sheet)){
    if(length(sheet) != length(seal:::sys_grab_sheetdata(sheet))){
      return(invisible(seal:::sys_msg_error(title = "Some `sheet` are not data sheet",
                                            error = "Please check the argument `sheet`",
                                            silent = silent, func = func)))
    }
    if(sum(!(sheet %in% sheet_data))){
      failed_sheet = sheet[!(sheet %in% sheet_data)]
      error = paste0("Please check the ",
                     stringr::str_flatten(string = failed_sheet, collapse = ", ", last = " & "))
      return(invisible(seal:::sys_msg_error(title = "Some `sheet` are missing in xlsx file",
                                            error = error, silent = silent, func = func)))
    }
  } else {
    sheet = sheet_data
  }

  #Check if the data sheets have a corresponding item sheet####
  if(!("item" %in% sheet_item)){
    expect_item_sheet = stringr::str_sub(string = sheet, start = 5L, end = -1L)
    expect_item_sheet = paste0("item", expect_item_sheet)
    if(sum(!(expect_item_sheet %in% sheet_item))){
      failed_sheet = expect_item_sheet[!(expect_item_sheet %in% sheet_item)]
      error = paste0("Please check ",
                     stringr::str_flatten(string = paste0("`", failed_sheet, "`"),
                                          collapse = ", ", last = " & "))
      return(invisible(seal:::sys_msg_error(title = "Some `data` are not described in xlsx.",
                                            error = error, silent = silent, func = func)))
    }
  }

  #Return FALSE when OK
  return(invisible(FALSE))
}
