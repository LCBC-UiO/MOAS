#' Get epigenetic data
#'
#' @param file_path path to excel file containing epigenetic
#' data
#'
#' @param match_path path to file that matches genetic ID to
#' CrossProject_ID
#' @param debug logical, if debugging informaition should be added.
#' Defaults to FALSE
#'
#' @family epigen-functions
#' @family MOAS get-functions
#'
#' @importFrom dplyr starts_with rename_at rename mutate
#' @importFrom dplyr as_tibble select contains everything left_join
#' @importFrom rio import
#' @export
epigen_get <- function(file_path,
                       match_path = "path/to/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
                       debug = FALSE){

  if(!file.exists(file_path))
    stop(paste(file_path, "does not exist or is not a path. Please check the path and file name carefully."),
         call. = FALSE)

  if(!file.exists(match_path))
    stop(paste(match_path, "does not exist or is not a path. Please check the path and file name carefully."),
         call. = FALSE)

  epigen <- import(file_path) %>%
    mutate(SampleID = gsub("^X", "", SampleID )) %>%
    rename(FID = SampleID) %>%
    rename_at(vars(-FID), function(x) paste0("EpiGen_", x))

  match <- read_tsv(match_path) %>%
    filter(for_ewas == 1) %>%
    rename_at(vars(-FID, -Genetic_ID, -CrossProject_ID, -Project_Name, -Project_Wave), function(x) paste0("EpiGen_debug_", x))

  ret_dt <- left_join(epigen, match, by="FID") %>%
    as_tibble() %>%
    select(CrossProject_ID, contains("Project"), everything())

  if(debug){
    ret_dt
  } else {
    ret_dt %>%
      select(-FID, -starts_with("EpiGen_debug_"))
  }
}


#' Add epigenetic data to MOAS like data
#'
#' @param MOAS A MOAS-derived data frame you want data added to
#'
#' @inheritParams epigen_get
#'
#' @family epigen-functions
#' @family MOAS add-functions
#'
#' @export
epigen_add <- function(MOAS,
                       file_path,
                       match_path = "path/to/MOAS/data-raw/DNA/gID_MOAS_match.tsv",
                       debug = FALSE){

  if(is.null(MOAS)) stop("MOAS-type data is missing, please provide it. ")
  if(!any("data.frame" %in% class(MOAS))) stop("You need to provide the MOAS as an already loaded data.frame.",
                                               call.=FALSE)
  if(any(!c("CrossProject_ID", "Project_Name", "Project_Wave") %in% names(MOAS)))
    stop("One of 'CrossProject_ID', 'Project_Name', 'Project_Wave' is missing from the MOAS-like data. These are needed for merging.",
     call.=FALSE)

  MOAS <- mutate(MOAS, CrossProject_ID = as.numeric(as.character(CrossProject_ID)))
  epigen_data <- epigen_get(file_path, match_path, debug)

  left_join(MOAS, epigen_data, by=c("CrossProject_ID", "Project_Name", "Project_Wave")) %>%
    mutate(CrossProject_ID = as.factor(CrossProject_ID))
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  globalVariables(c("for_ewas", "Genetic_ID",
                           "SampleID"
  ))
}