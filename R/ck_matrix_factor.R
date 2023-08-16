#' SED: Check `factor` matrix fulfills requirement
#'
#' This function checks if data in the subclass `sed_factor` fulfills the SED requirements.
#' This check ignores cooperative nature of items in `sed`, and only focuses if the factor matrix makes sense.
#'
#' @param factor the `factor` matrix in dataframe or tibble.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name that called this function, which is to be passed to `sys_msg_error` or `sys_msgwarning`. Default as `NULL`, the function name will not display.
#'
#' @return a logical value, with `TRUE` representing an error occurred; and a standardized message if `silent` is `FALSE`.
#' @keywords internal
#' @noRd
#'
#' @examples ck_factor(factor)
ck_factor = function(factor, silent = FALSE, func = NULL){
  #Check about the class ####
  if((!seal:::is_sed_factor(factor))){
    return(invisible(seal:::sys_msg_error(title = "Inputing `factor` with the wrong class",
                                          error = c("Please check if `factor` is `sed_factor`"),
                                          func = func, silent = silent)))}
  if((!is.data.frame(factor))){
    return(invisible(seal:::sys_msg_error(title = "Inputing `factor` with the wrong class",
                                          error = c("Please check if `factor` is `data.frame`"),
                                          func = func, silent = silent)))}

  #Remove the sed_factor class for easier process ####
  class_sed = class(factor)
  class(factor) = class_sed[class_sed != "sed_factor"]

  #Check if the dataframe contains `factor`, `label`, `abbr` ####
  columns = colnames(factor)

  if(!("factor" %in% columns)){
    return(invisible(seal:::sys_msg_error(title = "`factor` is not present.",
                                          error = "Please check `factor` and include the column",
                                          func = func, silent = silent)))
  }
  if(!("label" %in% columns)){
    return(invisible(seal:::sys_msg_error(title = "`label` is not present.",
                                          error = "Please check `factor` and include the column",
                                          func = func, silent = silent)))
  }
  if(!("abbr" %in% columns)){
    return(invisible(seal:::sys_msg_error(title = "`abbr` is not present.",
                                          error = "Please check `factor` and include the column",
                                          func = func, silent = silent)))
  }

  #Check if the dataframe columns are unique ####
  if(sum(duplicated(columns))){
    col = unique(columns[duplicated(columns)])
    return(invisible(seal:::sys_msg_error(title = "`factor` has duplicate columns",
                                          error = c("Please check if `factor` is all unique",
                                                    "Duplicated columns include...",
                                                    col),
                                          func = func, silent = silent)))
  }

  #Check if all columns are "characters" ####
  col_class = unlist(lapply(factor, class)) %>% unique

  if(length(col_class) != 1 | col_class[1] != "character"){
    return(invisible(seal:::sys_msg_error(title = "All columns in `factor` must be `character`",
                                          error = 'Please make sure all columns in `factor` are "character".',
                                          silent = silent, func = func)))
  }

  #Check if all columns are filled ####
  if(anyNA(factor)){
    return(invisible(seal:::sys_msg_error(title = "`factor` must be all filled.",
                                          error = "Please check `factor` and remove any blanks.")))
  }

  #Check if all item in the factor column has a "#" prefix ####
  list_factor = factor$factor
  list_factor_prefix = stringr::str_sub(string = list_factor, start = 1L, end = 1L) %>%
    stringr::str_detect(pattern = "#", negate = T)
  if(sum(list_factor_prefix) > 0){
    return(seal:::sys_msg_error(title = "Not all in `factor` are factors",
                                error = c('Please make sure all items in {.code factor} have a "#" prefix.'),
                                func = func, silent = silent) %>% invisible)
  }

  #Check if items in the label and abbr column is unique and check if "###" is present with other factors ####
  list_factor = unique(factor$factor)
  for(i in list_factor){
    factor_sel = dplyr::filter(factor, factor == i)
    ##Check if "###" is present with other factors ####
    if((length(factor_sel$label) > 1) & ("###" %in% factor_sel$label)){
      return(seal:::sys_msg_error(title = '"###" must not be present with other labels in factor',
                                  error = 'Please make sure the labels makes sense.',
                                  func = func, silent = silent) %>% invisible)
    }

    ##Check if `label` is unique ####
    if(sum(duplicated(factor_sel$label))){
      title = paste0("Some labels in factor ", i ," is not unique.")
      return(invisible(seal:::sys_msg_error(title = title,
                                            error = "Please check `factor`",
                                            func = func, silent = silent)))
    }

    ##Check if `abbr` is unique n length the same ####
    if(sum(duplicated(factor_sel$abbr))){
      title = paste0("Some abbr in factor ", i ," is not unique.")
      return(invisible(seal:::sys_msg_error(title = title,
                                            error = "Please check `factor`",
                                            func = func, silent = silent)))
    }
    if(length(unique(nchar(factor_sel$abbr))) > 1){
      return(seal:::sys_msg_error(title = "`abbr` in the same factor must have the same length",
                                  error = c('Please make sure all abbr in the factor has the same length'),
                                  func = func, silent = silent) %>% invisible)
    }
  }
  #Check if the respective abbr of "###" is "#"####
  factor_hashed = dplyr::filter(factor, label == "###" & abbr != "#")
    if(nrow(factor_hashed) != 0){
      return(seal:::sys_msg_error(title = 'abbr of "###" must be "#"',
                                  error = c('Please make the abbr of "###" as "#"'),
                                  func = func, silent = silent) %>% invisible)
  }
  #Return F if all ok ####
  return(invisible(FALSE))
}
