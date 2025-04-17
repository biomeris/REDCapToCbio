#' Create Cancer Type Meta File and Data File
#'
#' @param study_folder The study folder.
#' @param data_filename The datafile name. If not specified, default is 'cancer_type.txt'
#' @param type_of_cancer The cancer type abbreviation.
#' @param name The name of the cancer type.
#' @param dedicated_color CSS color name of the color associated with this cancer study. See [this list](https://www.w3.org/TR/css3-color/#svg-color) for supported names, and follow the [awareness ribbons](https://en.wikipedia.org/wiki/List_of_awareness_ribbons) color schema. This color is associated with the cancer study on various web pages within the cBioPortal.
#' @param parent_type_of_cancer The type_of_cancer field of the cancer type of which this is a subtype. ℹ️ : you can set parent to tissue, which is the reserved word to place the given cancer type at "root" level in the "studies oncotree" that will be generated in the homepage (aka query page) of the portal.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' createCancerTypeFiles(
#'   study_folder = tempdir(),
#'   type_of_cancer = "brca",
#'   name = "Breast Invasive Carcinoma",
#'   dedicated_color = "HotPink",
#'   parent_type_of_cancer = "Breast"
#' )
#' }
createCancerTypeFiles <- function(study_folder, data_filename = "cancer_type.txt", type_of_cancer, name, dedicated_color, parent_type_of_cancer) {
  # Write meta data file
  lines <- c("genetic_alteration_type: CANCER_TYPE", "datatype: CANCER_TYPE", paste("data_filename", data_filename, sep = ": "))
  writeLines(lines, file.path(study_folder, "meta_cancer_type.txt"))
  cat("File 'meta_cancer_type.txt' successfully created!\n")

  # Write data file
  col_list <- mget(c("type_of_cancer", "name", "dedicated_color", "parent_type_of_cancer"), envir = environment())
  line <- paste(col_list, collapse = "\t")
  writeLines(line, file.path(study_folder, data_filename))
  cat(sprintf("File '%s' successfully created!\n", data_filename))
}
