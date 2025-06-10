#' Create Cancer Study Meta File
#'
#' @param study_folder The study folder.
#' @param type_of_cancer The cancer type abbreviation. This should be the same cancer type as specified in the meta_cancer_type.txt file, if available. The type can be "mixed" for studies with multiple cancer types.
#' @param cancer_study_identifier A string used to uniquely identify this cancer study within the database.
#' @param name The name of the cancer study.
#' @param description A description of the cancer study. This description may contain one or more URLs to relevant information.
#' @param citation (Optional) A relevant citation.
#' @param pmid (Optional) One or more relevant pubmed ids (comma separated without whitespace). If used, the field citation has to be filled, too.
#' @param groups (Optional) When using an authenticating cBioPortal, lists the user-groups that are allowed access to this study. Multiple groups are separated with a semicolon ";". The study will be invisible to users not in at least one of the listed groups, as if it wasn't loaded at all. See [User-Authorization](https://docs.cbioportal.org/deployment/authorization-and-authentication/user-authorization/) for more information on groups. Default to 'PUBLIC'
#' @param add_global_case_list (Optional) Set to 'true' if you would like the "All samples" case list to be generated automatically for you. See also [Case list](https://docs.cbioportal.org/file-formats/#case-lists)
#' @param tags_file (Optional) The file name containing custom study tags for the [study tags](https://docs.cbioportal.org/file-formats/#study-tags-file).
#' @param reference_genome (Optional) The study reference genome (e.g. hg19, hg38). Without specifying this property, the study will be assigned to the reference genome specified in application.properties (property ucsc.build).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' createCancerStudyMetafile(
#'   study_folder = tempdir(),
#'   cancer_study_identifier = "brca_joneslab_2013",
#'   name = "Breast Cancer (Jones Lab 2013)",
#'   description = "Comprehensive profiling of 103 breast cancer samples.",
#'   add_global_case_list = TRUE
#' )
#' }
createCancerStudyMetafile <- function(study_folder, type_of_cancer, cancer_study_identifier, name, description, citation = NULL, pmid = NULL, groups = "PUBLIC", add_global_case_list = NULL, tags_file = NULL, reference_genome = NULL) {
  # Create list with all fields
  fields_list <- mget(c("type_of_cancer", "cancer_study_identifier", "name", "description", "citation", "pmid", "groups", "add_global_case_list", "tags_file", "reference_genome"), envir = environment())

  # Remove NULL elements
  fields_list <- fields_list[!sapply(fields_list, is.null)]

  # Convert boolean values to lowercase string
  fields_list <- lapply(fields_list, function(x) {
    if (is.logical(x)) {
      return(tolower(as.character(x)))
    } else {
      return(x)
    }
  })

  # Convert list to array of strings
  lines <- paste(names(fields_list), fields_list, sep = ": ")

  # Write txt file to output folder
  writeLines(lines, file.path(study_folder, "meta_study.txt"))

  cat("File 'meta_study.txt' successfully created!\n")
}
