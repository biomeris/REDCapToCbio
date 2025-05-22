#' Zip a study folder
#'
#' This function compresses the contents of a given study folder into a ZIP archive,
#' saving the output in a specified directory.
#'
#' @param zipfile Name of the resulting ZIP file (e.g., `"study.zip"`).
#' @param study_folder Path to the folder whose contents should be zipped.
#' @param output_path Directory where the ZIP file will be saved. Defaults to the current working directory.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' zipStudyFolder(
#'   zipfile = "study.zip",
#'   study_folder = "path/to/study",
#'   output_path = "path/to/output")
#' }
zipStudyFolder <- function(zipfile, study_folder, output_path = getwd()) {
  # Zip destination
  zip_dest <- file.path(output_path, zipfile)
  # Obtain files names (only relative paths)
  files_to_zip <- list.files(study_folder, recursive = TRUE, full.names = FALSE)

  # Execute the zip from within the source folder (avoids including parent directories)
  old_wd <- setwd(study_folder)
  zip::zip(zipfile = zip_dest, files = files_to_zip)
  setwd(old_wd)
}
