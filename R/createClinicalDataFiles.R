#' Create Clinical Data Meta file and Data file
#'
#' @param study_folder The study folder.
#' @param cancer_study_identifier A string used to uniquely identify this cancer study within the database. Must be the same value specified in meta_study.txt
#' @param datatype 'PATIENT_ATTRIBUTES' or 'SAMPLE_ATTRIBUTES'
#' @param clinical_data A dataframe containing the clinical data
#' @param data_filename The datafile name. Defaults to 'data_clinical_patient.txt' or 'data_clinical_sample.txt', depending on the datatype.
#' @param mapping_file A JSON file that provides field-level mapping rules to convert source data into a structure compatible with cBioPortal.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Return data from REDCap
#' redcap_data <- redcapToCbio(
#'   redcap_uri = "https://redcap-dev-2.ouhsc.edu/redcap/api/"
#'   token = "9A068C425B1341D69E83064A2D273A70"
#' )
#'
#' # Create clinical data files
#' createClinicalDataFiles(
#'   study_folder = tempdir(),
#'   cancer_study_identifier = "brca_joneslab_2013",
#'   datatype = "PATIENT_ATTRIBUTES",
#'   clinical_data = redcap_data
#' )
#' }
createClinicalDataFiles <- function(study_folder, cancer_study_identifier, datatype = c("PATIENT_ATTRIBUTES", "SAMPLE_ATTRIBUTES"), clinical_data, data_filename = NULL, mapping_file = NULL) {
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

  # Read mapping file and filter by attribute type (patient or sample)
  if (is.null(mapping_file)) {
    mapping_file <- system.file("minimal_dataset", "clinical_data_attributes.json", package = "REDCapToCbio")
  }

  source_cbio_mapping <- jsonlite::fromJSON(mapping_file)

  if (datatype == "PATIENT_ATTRIBUTES") {
    source_cbio_mapping <- purrr::keep(source_cbio_mapping, ~ .x$`Attribute type` %in% c("CLINICAL", "PATIENT"))
  } else if (datatype == "SAMPLE_ATTRIBUTES") {
    source_cbio_mapping <- purrr::keep(source_cbio_mapping, ~ .x$`Attribute type` %in% c("CLINICAL", "SAMPLE"))
  }

  display_names <- lapply(source_cbio_mapping, function(x) x[["Display name"]]) %>%
    unlist() %>%
    paste(., collapse = "\t")

  descriptions <- lapply(source_cbio_mapping, function(x) x[["Description"]]) %>%
    unlist() %>%
    paste(., collapse = "\t")

  datatypes <- lapply(source_cbio_mapping, function(x) x[["Datatype"]]) %>%
    unlist() %>%
    paste(., collapse = "\t")

  priorities <- lapply(source_cbio_mapping, function(x) x[["Priority"]]) %>%
    unlist() %>%
    paste(., collapse = "\t")

  attr_names <- names(source_cbio_mapping) %>% paste(., collapse = "\t")

  lines <- paste0("#", display_names) %>%
    c(., paste0("#", descriptions)) %>%
    c(., paste0("#", datatypes)) %>%
    c(., paste0("#", priorities)) %>%
    c(., paste0("#", attr_names))

  clinical_data_lines <- apply(clinical_data, 1, function(row) {
    row[is.na(row)] <- ""
    paste(row, collapse = "\t")
  })

  lines <- c(lines, clinical_data_lines)

  writeLines(lines, file.path(study_folder, data_filename))

  cat(sprintf("File '%s' successfully created!\n", data_filename))
}





#' Convert REDCap data to cBioPortal format
#'
#' @inheritParams REDCapR::redcap_read
#' @param mapping_file A list that contains field-level mapping rules to convert source data into a structure compatible with cBioPortal.
#' @param redcap_template A csv file with the REDCap data dictionary.
#'
#' @return A dataframe with data in cBioPortal format
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' redcap_data <- redcapToCbio(
#'   redcap_uri = "https://redcap-dev-2.ouhsc.edu/redcap/api/"
#'   token = "9A068C425B1341D69E83064A2D273A70"
#' )
#' }
redcapToCbio <- function(redcap_uri, token, mapping_file = NULL, redcap_template = NULL) {

  # Read mapping file and filter by attribute type (patient or sample)
  if (is.null(mapping_file)) {
    mapping_file <- system.file("minimal_dataset", "clinical_data_attributes.json", package = "REDCapToCbio")
  }

  source_cbio_mapping <- jsonlite::fromJSON(mapping_file) %>%
    purrr::keep(~ .x$`Attribute type` %in% c("CLINICAL", "PATIENT"))

  if (is.null(redcap_template)) {
    redcap_template <- system.file("minimal_dataset", "StandardTemplateCRO_DataDictionary.csv", package = "REDCapToCbio")
  }

  redcap_fields <- read.csv(file = redcap_template)

  # Extract data from REDCap
  redcap_data <- REDCapR::redcap_read(
    redcap_uri = redcap_uri,
    token = token,
    fields = redcap_fields %>% dplyr::pull(1)
  )$data

  # Convert data from REDCap to cBioPortal format
  cbio_data <- redcap_data %>%
    dplyr::filter(is.na(.data$redcap_repeat_instrument) | .data$redcap_repeat_instrument %in% c("follow_up")) %>%
    dplyr::group_by(.data$record_id) %>%
    dplyr::summarise(
      dplyr::across(redcap_fields %>% dplyr::filter(.data$Form.Name == "paziente_e_diagnosi") %>% dplyr::pull(1) %>% .[-1], dplyr::first),
      dplyr::across(redcap_fields %>% dplyr::filter(.data$Form.Name == "follow_up") %>% dplyr::pull(1), dplyr::last)
    )

  for (i in names(source_cbio_mapping)) {
    redcap_field <- source_cbio_mapping[[i]][["REDCap Field name"]]
    values_map <- source_cbio_mapping[[i]][["Values"]]
    date_map <- source_cbio_mapping[[i]][["Datediff"]]
    condition_logic <- source_cbio_mapping[[i]][["Condition Logic"]]

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
      unit <- date_map$unit %||% "days"

      if (!is.null(date_map$end_date_logic)) {
        cbio_data$.__temp_end <- cbio_data %>%
          dplyr::rowwise() %>%
          dplyr::mutate(.temp = eval(parse(text = date_map$end_date_logic))) %>%
          dplyr::ungroup() %>%
          dplyr::pull(.data$.temp)
        end_date <- ".__temp_end"
      } else if (!is.null(date_map$end) && date_map$end %in% names(cbio_data)) {
        end_date <- date_map$end
      } else {
        next
      }

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

    # Apply Condition Logic
    if (!is.null(condition_logic) && nzchar(condition_logic)) {
      condition_mask <- with(cbio_data, eval(parse(text = condition_logic)))
      cbio_data[[i]][!condition_mask] <- NA
    }
  }

  cbio_data <- cbio_data %>%
    dplyr::select(dplyr::all_of(names(source_cbio_mapping)))

  return(cbio_data)
}





#' Request an Access Token from an OAuth2 API
#'
#' Sends a POST request to an OAuth2-compatible API to retrieve an access token using
#' the client credentials grant type.
#'
#' @param client_id The client ID provided by the API authorization server
#' @param client_secret The client secret associated with the client ID
#' @param token_url The URL of the token endpoint where the request should be sent
#' @param ssl_verifypeer Whether to verify SSL certificates. Default is TRUE
#'
#' @return Character string containing the access token if the request is successful,
#' or \code{NULL} if the request fails.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- requestAPIToken(
#'   client_id = "your-client-id",
#'   client_secret = "your-client-secret",
#'   token_url = "https://example.com/oauth/token"
#' )
#' }
requestAPIToken <- function(client_id, client_secret, token_url, ssl_verifypeer = TRUE) {
  # Send POST request
  response <- httr::POST(
    url = token_url,
    httr::authenticate(client_id, client_secret),
    body = list(grant_type = "client_credentials"),
    encode = "form",
    config(ssl_verifypeer = ssl_verifypeer)
  )

  # Check response
  if (httr::status_code(response) == 200) {
    access_token <- httr::content(response)$access_token
    cat("Authentication successful: access token retrieved.")
  } else {
    access_token <- NULL
    cat(httr::content(response))
  }

  return(access_token)
}
