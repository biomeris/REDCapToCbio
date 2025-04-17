#' Create study folder
#'
#' @param study_folder The folder name.
#' @param output_path The path where the folder will be created.
#'
#' @return The folder path.
#' @export
#'
#' @examples
#' output_folder <- createStudyFolder(study_folder = "brca", output_path = tempdir())
createStudyFolder <- function(study_folder, output_path = getwd()) {
  # Create folder if does not exists
  if (!file.exists(file.path(output_path, study_folder))) {
    dir.create(file.path(output_path, study_folder), recursive = TRUE)
    cat(sprintf("The directory '%s' has been successfully created!", study_folder))
  } else {
    cat(sprintf("The directory '%s' already exists at the specified path and will not be created.", study_folder))
  }
  # Return the folder path
  return(file.path(output_path, study_folder))
}
