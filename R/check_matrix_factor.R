#' SED: Check `data` matrix fulfills requirement
#'
#' @param factor the `factor` matrix in dataframe or tibble.
#' @param silent Will this function return an error message? Default as `FALSE`, which will display a text message.
#' @param func The function name that called this function, which is to be passed to `sys_msgerror` or `sys_msgwarning`. Default as `"ck_factor"`. If `NULL`, the function name will not display.
#'
#' @return a logical value, with `TRUE` representing an error occurred; and a standardized message if `silent` is `FALSE`.
#' @keywords internal
#' @noRd
#'
#' @examples ck_factor(factor)
ck_factor = function(factor, silent = FALSE, func = "ckrw_factor"){
  #Check if there is an input ####
  if(!hasArg(factor)){return(invisible(seal:::sys_msgs_noinput(arg = "factor",
                                                               expect = c("data.frame", "tibble"))))}

  #Check about the class ####
  if((!seal::is_sed_factor(factor))){
    return(invisible(seal:::sys_msgs_wrongclass(arg = "factor",
                                                expect = c("sed_factor"),
                                                silent = silent, func = func)))}

  if((!is.data.frame(factor))){return(invisible(seal:::sys_msgs_wrongclass(arg = "factor",
                                                                           expect = c("data.frame", "tibble"))))}

  #Temporarily remove the sed_factor class for easier process ####
  class_sed = class(factor)
  class(factor) = class_sed[class_sed != "sed_factor"]

  #Check if column names include `factor`, `label`, `abbr` ####
  columns = colnames(factor)
  if(!("factor" %in% columns)){return(invisible(seal:::sys_msgsdf_nocolumn(col = "factor", dataframe = "factor")))}
  if(!("label" %in% columns)){return(invisible(seal:::sys_msgsdf_nocolumn(col = "label", dataframe = "factor")))}
  if(!("label" %in% columns)){return(invisible(seal:::sys_msgsdf_nocolumn(col = "abbr", dataframe = "factor")))}

  #Check if the column names are unique ####
  if(sum(duplicated(columns))){
    col = unique(columns[duplicated(columns)])
    return(invisible(seal:::sys_msgsdf_notuniquecolname(col = col,
                                                        dataframe = "factor")))}

  #Check if all columns are "characters"
  colclass = factor %>%
    lapply(class) %>%
    unclass() %>%
    unique()

  if(length(colclass) != 1 | colclass[1] != "character"){
    return(invisible(seal:::sys_msgerror(title = "All columns in `factor` must be `character`",
                                         error = 'Please make sure all columns in {.code factor} are "character".',
                                         silent = silent, func = func)))
  }

  #Check if all item in the factor column has a "#" prefix ####
  list_factor = factor$factor
  list_factor_prefix = stringr::str_sub(string = list_factor, start = 1L, end = 1L) %>%
    stringr::str_detect(pattern = "#", negate = T)
  if(sum(list_factor_prefix) > 0){
    return(seal:::sys_msgerror(title = "Not all in `factor` are factors",
                               error = c("x" = 'Please make sure all items in {.code factor} have a "#" prefix.'),
                               func = func, silent = silent) %>% invisible)
  }
  #Check if all items in the label column is filled ####
  list_label_NA = is.na(factor$label)
  if(sum(list_label_NA) > 0){return(invisible(seal:::sys_msgsdf_columnnotfull(col = "label", dataframe = "label")))}

  #Check if items in the label and abbr column is unique and check if "###" is present with other factors ####
  list_factor = unique(factor$factor)
  for(i in list_factor){
    factor_sel = dplyr::filter(factor, factor == i)
    ##Check if "###" is present with other factors ####
    if((length(factor_sel$label) > 1) & ("###" %in% factor_sel$label)){
      return(seal:::sys_msgerror(title = '"###" must not be present with other labels in factor',
                                 error = c("x" = 'Please make sure the labels makes sense.'),
                                 func = func, silent = silent) %>% invisible)
    }

    ##Check if `label` is unique ####
    if(sum(duplicated(factor_sel$label))){
      return(invisible(seal:::sys_msgsdf_itemnotunique(col = "label",
                                                       dataframe = paste0("label-",i),
                                                       func = func, silent = silent)))
    }

    ##Check if `abbr` is unique n length the same ####
    if(sum(!is.na(factor_sel$abbr))){
      if(sum(duplicated(factor_sel$abbr))){return(invisible(seal:::sys_msgsdf_itemnotunique(col = "abbr",
                                                                                            dataframe = paste0("label-",i),
                                                                                            func = func, silent = silent)))}
      if(length(unique(nchar(factor_sel$abbr))) > 1){
        return(seal:::sys_msgerror(title = "`abbr` in factor must have the same length in character",
                                   error = c("x" = 'Please make sure all abbr in the factor has the same length'),
                                   func = func, silent = silent) %>% invisible)
      }
    }
  }
  #Check if all items in the abbr column is all filled or all not filled ####
  list_abbr_NA = is.na(factor$abbr)
  list_abbr_length = length(factor$abbr)
  if(list_abbr_length != sum(list_abbr_NA) & list_abbr_length != sum(!list_abbr_NA)){
    return(invisible(seal:::sys_msgsdf_colnotfullempty(col = "abbr", dataframe = "label",
                                                       func = func, silent = silent)))
  }
  #Check if the respective abbr of "###" is "#"####
  if(sum(!list_abbr_NA) > 0){
    factor_hashed = dplyr::filter(factor, label == "###" & abbr != "#")
    if(nrow(factor_hashed) != 0){
      return(seal:::sys_msgerror(title = 'abbr of "###" must be "#"',
                                 error = c("x" = 'Please make the abbr of "###" as "#"'),
                                 func = func, silent = silent) %>% invisible)
    }
  }

  #Return `F` if nothing wrong ####
  return(invisible(FALSE))
}
