#' System tools: Automatically detect datatype from a vector
#'
#' @param vec A vector
#'
#' @return A character representing the datatype
#' @keywords internal
#' @noRd
#'
#' @examples sys_tld_autodatatype(vec)
sys_tld_autodatatype = function(vec){
  class_vec = class(vec)
  content_vec = unique(vec)
  datatype = NA
  if(class_vec == "character"){
    content_uni = unique(vec)
    if(length(content_uni) == 2){
      if(sum(content_uni %in% c("p", NA)) == 2){
        datatype = "p"
      } else {
        datatype = "c"
      }
    } else {
      datatype = "c"
    }
  }
  if(class_vec == "integer"){datatype = "i"}
  if(class_vec == "numeric"){datatype = "n"}
  return(datatype)
}
