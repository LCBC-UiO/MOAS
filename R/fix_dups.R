#' NA replacement in duplicated columns
#'
#' \code{fix_dups} creates a data.frame where duplicated columns
#' with a specific suffix are row-wise gone through, and where there
#' is NA in the original column, the suffixed column's data is
#' placed there instead.
#'
#' @param data A data.frame or tibble.
#' @param suffix A single string with column suffix to look for.
#' @param remove logical. If column with suffix are to be removed.
#'
#' @return a data frame with added/replaced age and timepoint variables.

#' @examples
#' \dontrun{
#' fix_dups(data, suffix="YY")
#' }
#'
#' @importFrom dplyr mutate_ select matches
#' @importFrom purrr is_empty
#' @importFrom magrittr "%>%"
#'
#' @export
fix_dups = function(data, suffix, remove = T){

  suff = paste0(suffix,"$")

  # Loop though duplicated columns, and coalesce them into single (NA's replaced with values from either)
  for(i in grep(suff,names(data))){
    nm = gsub(suff, "", names(data)[i])
    nmSuff = names(data)[i]
    #print(nm);print(nmSuff)

    if(!grep(paste0("^",nm,"$"), names(data)) %>% purrr::is_empty()){

      idx = which(is.na(data[,nm]))
      
      data[idx,nm] = data[idx,nmSuff]

      if(grepl("_Date", nm) & !class(data[,nm] %>% unlist()) %in% "character"){
        data[,nm] = as.Date(data[,nm], origin="1970-01-01") %>%
          as.character()
      }
    }else{
      names(data)[i] = gsub(suff, "", names(data)[i] )
    }
  }

  if(remove){
    data %>%
      dplyr::select(-dplyr::matches(suff))
  }else{
    data
  }
}