#' Create Clinical Data Meta file and Data file
#'
#' @param study_folder The study folder.
#' @param cancer_study_identifier A string used to uniquely identify this cancer study within the database. Must be the same value specified in meta_study.txt
#' @param datatype 'PATIENT_ATTRIBUTES' or 'SAMPLE_ATTRIBUTES'
#' @param data_filename The datafile name. Defaults to 'data_clinical_patient.txt' or 'data_clinical_sample.txt', depending on the datatype.
#' @param mapping_file A JSON file that provides field-level mapping rules to convert REDCap data into a structure compatible with cBioPortal.
#' @inheritParams .redcapToCbio
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' createClinicalDataFiles(
#'   study_folder = tempdir(),
#'   cancer_study_identifier = "brca_joneslab_2013",
#'   datatype = "PATIENT_ATTRIBUTES",
#'   redcap_uri = Sys.getenv("RC_URI"),
#'   token = Sys.getenv("TOKEN")
#' )
createClinicalDataFiles <- function(study_folder, cancer_study_identifier, datatype = c("PATIENT_ATTRIBUTES", "SAMPLE_ATTRIBUTES"), data_filename = NULL, mapping_file = NULL, redcap_uri, token, redcap_template = NULL) {
  # Check if datatype matches allowed values
  datatype <- match.arg(datatype)

  # If data_filename is NULL, assign default value
  if (is.null(data_filename) & datatype == "PATIENT_ATTRIBUTES") {
    data_filename <- "data_clinical_patient.txt"
  } else if (is.null(data_filename) & datatype == "SAMPLE_ATTRIBUTES") {
    data_filename <- "data_clinical_sample.txt"
  }

  # Write meta data file ----
  lines <- c(paste("cancer_study_identifier", cancer_study_identifier, sep = ": "), "genetic_alteration_type: CLINICAL", paste("datatype", datatype, sep = ": "), paste("data_filename", data_filename, sep = ": "))

  if (datatype == "PATIENT_ATTRIBUTES") {
    metadata_filename <- "meta_clinical_patient.txt"
  } else if (datatype == "SAMPLE_ATTRIBUTES") {
    metadata_filename <- "meta_clinical_sample.txt"
  }

  writeLines(lines, file.path(study_folder, metadata_filename))

  cat(sprintf("File '%s' successfully created!\n", metadata_filename))

  # Extract and transform data from REDCap ----
  # Read mapping file and filter by attribute type (patient or sample)
  if (is.null(mapping_file)) {
    mapping_file <- "./inst/minimal_dataset/clinical_data_attributes.json"
  }

  redcap_cbio_mapping <- jsonlite::fromJSON(mapping_file)

  display_names <- lapply(redcap_cbio_mapping, function(x) x[["Display name"]]) %>%
    unlist() %>%
    paste(., collapse = "\t")

  descriptions <- lapply(redcap_cbio_mapping, function(x) x[["Description"]]) %>%
    unlist() %>%
    paste(., collapse = "\t")

  datatypes <- lapply(redcap_cbio_mapping, function(x) x[["Datatype"]]) %>%
    unlist() %>%
    paste(., collapse = "\t")

  priorities <- lapply(redcap_cbio_mapping, function(x) x[["Priority"]]) %>%
    unlist() %>%
    paste(., collapse = "\t")

  attr_names <- names(redcap_cbio_mapping) %>% paste(., collapse = "\t")

  lines <- paste0("#", display_names) %>%
    c(., paste0("#", descriptions)) %>%
    c(., paste0("#", datatypes)) %>%
    c(., paste0("#", priorities)) %>%
    c(., paste0("#", attr_names))

  cat("Exporting data from REDCap and converting to cBioPortal format...")
  clinical_data <- .redcapToCbio(datatype = datatype, redcap_uri = redcap_uri, token = token, redcap_cbio_mapping = redcap_cbio_mapping)

  clinical_data_lines <- apply(clinical_data, 1, function(row) paste(row, collapse = "\t"))

  lines <- c(lines, clinical_data_lines)

  writeLines(lines, file.path(study_folder, data_filename))

  cat(sprintf("File '%s' successfully created!\n", data_filename))
}





#' Convert REDCap data to cBioPortal format
#'
#' @param datatype 'PATIENT_ATTRIBUTES' or 'SAMPLE_ATTRIBUTES'
#' @inheritParams REDCapR::redcap_read
#' @param redcap_cbio_mapping A list that contains field-level mapping rules to convert REDCap data into a structure compatible with cBioPortal.
#' @param redcap_template A csv file with the REDCap data dictionary.
#'
#' @importFrom magrittr %>%
#'
#' @return A dataframe with data in cBioPortal format
.redcapToCbio <- function(datatype = c("PATIENT_ATTRIBUTES", "SAMPLE_ATTRIBUTES"), redcap_uri, token, redcap_cbio_mapping, redcap_template = NULL) {
  if (datatype == "PATIENT_ATTRIBUTES") {
    redcap_cbio_mapping <- purrr::keep(redcap_cbio_mapping, ~ .x$`Attribute type` %in% c("CLINICAL", "PATIENT"))
  } else if (datatype == "SAMPLE_ATTRIBUTES") {
    redcap_cbio_mapping <- purrr::keep(redcap_cbio_mapping, ~ .x$`Attribute type` %in% c("CLINICAL", "SAMPLE"))
  }

  if (is.null(redcap_template)) {
    redcap_template <- "./inst/minimal_dataset/StandardTemplateCRO_DataDictionary.csv"
  }

  redcap_fields <- read.csv(file = redcap_template) %>% dplyr::pull(1)

  # Extract data from REDCap
  redcap_data <- REDCapR::redcap_read(
    redcap_uri = redcap_uri,
    token = token,
    fields = redcap_fields
  )$data

  # Convert data from REDCap to cBioPortal format
  cbio_data <- redcap_data

  for (i in names(redcap_cbio_mapping)) {
    redcap_field <- redcap_cbio_mapping[[i]][["REDCap Field name"]]
    values_map <- redcap_cbio_mapping[[i]][["Values"]]
    date_map <- redcap_cbio_mapping[[i]][["Datediff"]]

    # Check that redcap_field exists in REDCap data
    if (!(redcap_field %in% names(cbio_data))) next

    # Case 1: map from list of values
    if (!is.null(values_map)) {
      cbio_data[[i]] <- sapply(
        cbio_data[[redcap_field]],
        function(x) values_map[[as.character(x)]] %||% NA_character_
      )
      # Case 2: difference between dates
    } else if (!is.null(date_map) &&
      all(c(date_map$start, date_map$end) %in% names(cbio_data))) {
      start_date <- date_map$start
      end_date <- date_map$end
      unit <- date_map$unit %||% "days"

      # difference in ("auto", "secs", "mins", "hours", "days", "weeks")
      if (unit %in% c("auto", "secs", "mins", "hours", "days", "weeks")) {
        cbio_data[[i]] <- as.numeric(difftime(
          cbio_data[[end_date]],
          cbio_data[[start_date]],
          units = unit
        ))
        # diffenrence in months
      } else if (unit == "months") {
        cbio_data[[i]] <- as.numeric(difftime(
          cbio_data[[end_date]],
          cbio_data[[start_date]],
          units = "days"
        )) / (365.25 / 12)
        # difference in years
      } else if (unit == "years") {
        cbio_data[[i]] <- as.numeric(difftime(
          cbio_data[[end_date]],
          cbio_data[[start_date]],
          units = "days"
        )) / (365.25)
      }
      # Case 3: copy the original value from REDCap
    } else {
      cbio_data[[i]] <- cbio_data[[redcap_field]]
    }
  }

  cbio_data <- cbio_data %>%
    dplyr::select(all_of(names(redcap_cbio_mapping)))

  return(cbio_data)
}
