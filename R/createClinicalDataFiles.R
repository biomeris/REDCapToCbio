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
    source_cbio_mapping <- purrr::keep(source_cbio_mapping, ~ .x$`Attribute type` == "PATIENT")
  } else if (datatype == "SAMPLE_ATTRIBUTES") {
    source_cbio_mapping <- purrr::keep(source_cbio_mapping, ~ .x$`Attribute type` == "SAMPLE")
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
    c(., attr_names)

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
#' @param mapping_file A JSON file that provides field-level mapping rules to convert source data into a structure compatible with cBioPortal.
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
    purrr::keep(~ .x$`Attribute type` == "PATIENT")

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
  redcap_data <- redcap_data %>%
    dplyr::filter(is.na(.data$redcap_repeat_instrument) | .data$redcap_repeat_instrument %in% c("follow_up")) %>%
    dplyr::group_by(.data$record_id) %>%
    dplyr::summarise(
      dplyr::across(redcap_fields %>% dplyr::filter(.data$Form.Name == "paziente_e_diagnosi") %>% dplyr::pull(1) %>% .[-1], dplyr::first),
      dplyr::across(redcap_fields %>% dplyr::filter(.data$Form.Name == "follow_up") %>% dplyr::pull(1), dplyr::last)
    )

  cbio_data <- .source2CbioMap(source_data = redcap_data, source_cbio_mapping = source_cbio_mapping)

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
    httr::config(ssl_verifypeer = ssl_verifypeer)
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





#' Search study by REDCap URL and PID
#'
#' Searches and retrieves study metadata from a REDCap instance using the REDCap URL and project identifier (PID).
#'
#' @param studylink_url The base URL of the StudyLink application
#' @param access_token A valid access token used for authentication with the StudyLink API
#' @param redcap_url The URL of the REDCap instance from which to retrieve the study
#' @param pid The project ID (PID) of the REDCap project whose study metadata should be retrieved
#'
#' @returns The study ID corresponding to the specified REDCap study. Returns only one study or \code{NULL} if the request fails.
#' @export
#'
#' @examples
#' \dontrun{
#' study_id <- getStudyID(
#'   studylink_url = "https://studylink.example.org",
#'   access_token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
#'   redcap_url = "https://redcap.example.org",
#'   pid = "1234"
#' )
#' }
getStudyID <- function(studylink_url, access_token, redcap_url, pid) {
  headers = c('Authorization' = paste("Bearer", access_token))
  api_url <- paste(studylink_url, "api/v1/study/getByRedcap", sep = "/")

  # Define parameters
  params <- list(
    url = redcap_url,
    pid = pid
  )

  # Send GET request
  res <- httr::GET(url = api_url, httr::add_headers(headers), query = params)
  res_text <- httr::content(res, as = 'text', encoding = 'UTF-8')
  res_list <- jsonlite::fromJSON(res_text, simplifyVector = FALSE)

  # Extract and return study ID
  study_id <- res_list[["id"]]
  return(study_id)
}





#' Get all vials with details and attributes by study id
#'
#' @inheritParams getStudyID
#' @param study_id The study ID
#' @param mapping_file A JSON file that provides field-level mapping rules to convert source data into a structure compatible with cBioPortal.
#'
#' @returns A dataframe with data in cBioPortal format
#' @export
#'
#' @examples
#' \dontrun{
#' study_id <- getStudyID(
#'   studylink_url = "https://studylink.example.org",
#'   access_token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
#'   redcap_url = "https://redcap.example.org",
#'   pid = "1234"
#' )
#' }
getVialsAttributes <- function(studylink_url, access_token, study_id, mapping_file = NULL) {
  # Read mapping file and filter by attribute type (patient or sample)
  if (is.null(mapping_file)) {
    mapping_file <- system.file("minimal_dataset", "clinical_data_attributes.json", package = "REDCapToCbio")
  }

  source_cbio_mapping <- jsonlite::fromJSON(mapping_file) %>%
    purrr::keep(~ .x$`Attribute type` == "SAMPLE")

  # Get all vials with details and attributes by study id
  headers = c('Authorization' = paste("Bearer", access_token))
  api_url <- paste(studylink_url, "/api/v1/vial/detailsAndAttributes", study_id, sep = "/")

  params <- list(
    studyId = study_id
  )

  res <- httr::GET(url = api_url, httr::add_headers(headers), query = params)
  res_text <- httr::content(res, as = 'text', encoding = 'UTF-8')
  vial_list <- jsonlite::fromJSON(res_text, simplifyVector = FALSE)

  # Extract vials details from list and create a dataframe
  vial_df <- purrr::map_dfr(vial_list, function(x) {
    vial <- x$vial
    sampleDTO <- vial$sampleDetail$sampleDTO
    diagnosis <- sampleDTO$diagnosis
    attributes <- x$attributes

    # Common fields (same across all attributes)
    base_info <- tibble::tibble(
      code = vial$code,
      date_of_freezing = vial$dateOfFreezing,
      material = vial$material,
      status = vial$status,
      partialThawingDateTime = vial$partialThawingDateTime,
      thawingDateTime = vial$thawingDateTime,

      # sampleDetail fields
      enrollment_id = vial$sampleDetail$enrollmentPseudoId,

      # sampleDTO fields
      materialType = sampleDTO$materialType,
      storageType = sampleDTO$storageType,

      # diagnosis fields
      diagnosis_type = diagnosis$type,
      diagnosis_site = diagnosis$site,
      diagnosis_site_lab = diagnosis$siteTerm,
      diagnosis_morphology = diagnosis$morphology,
      diagnosis_morpho_lab = diagnosis$morphologyTerm,
      diagnosis_grade = diagnosis$grade,
      diagnosis_date = diagnosis$date,
    )

    # Return base_info with NA if attributes is empty
    if (length(attributes) == 0) {
      return(base_info %>% mutate(attribute_name = NA, attribute_value = NA))
    }

    # Espandi ogni attributo
    attr_df <- purrr::map_dfr(attributes, function(attr) {
      tibble::tibble(
        attribute_name = attr$name,
        attribute_value = attr$value
      )
    })

    # Aggiungi base_info a ogni attributo
    dplyr::bind_cols(
      base_info[rep(1, nrow(attr_df)), ],
      attr_df
    )
  })

  # Returns one row per `vial`, with each attribute represented as a separate column.
  vial_wide <- vial_df %>%
    tidyr::pivot_wider(names_from = attribute_name, values_from = attribute_value)

  # Convert data to cBioPortal format
  cbio_data <- .source2CbioMap(source_data = vial_wide, source_cbio_mapping = source_cbio_mapping)

  return(cbio_data)

}





#' Convert source-like Data to cBioPortal Format
#'
#' This function takes a source data frame (e.g., extracted from REDCap), applies a predefined field mapping,
#' and returns a data frame formatted according to the structure expected by cBioPortal.
#'
#' @param source_data A data frame containing the source data to be mapped
#' @param source_cbio_mapping A named list defining how fields from the source data should be mapped to cBioPortal fields
#'
#' @returns A data frame compatible with the cBioPortal data structure.
#'
#' @examples
#' \dontrun{
#' cbio_df <- .source2CbioMap(
#'   source_data = redcap_data,
#'   source_cbio_mapping = redcap_cbio_mapping
#' )
#' }
.source2CbioMap <- function(source_data, source_cbio_mapping) {
  # Map each source field to the corresponding cBioPortal clinical data field
  for (i in names(source_cbio_mapping)) {
    source_field <- source_cbio_mapping[[i]][["Source Field name"]]
    values_map <- source_cbio_mapping[[i]][["Values"]]
    date_map <- source_cbio_mapping[[i]][["Datediff"]]
    condition_logic <- source_cbio_mapping[[i]][["Condition Logic"]]

    # Check that source_field exists in source data
    if (!(source_field %in% names(source_data))) next

    # Case 1: map from list of values
    if (!is.null(values_map)) {
      source_data[[i]] <- sapply(
        source_data[[source_field]],
        function(x) values_map[[as.character(x)]] %||% NA_character_
      )
      # Case 2: difference between dates
    } else if (!is.null(date_map) &&
               date_map$start %in% names(source_data)) {
      start_date <- date_map$start
      unit <- date_map$unit %||% "days"

      if (!is.null(date_map$end_date_logic)) {
        source_data$.__temp_end <- source_data %>%
          dplyr::rowwise() %>%
          dplyr::mutate(.temp = eval(parse(text = date_map$end_date_logic))) %>%
          dplyr::ungroup() %>%
          dplyr::pull(.data$.temp)
        end_date <- ".__temp_end"
      } else if (!is.null(date_map$end) && date_map$end %in% names(source_data)) {
        end_date <- date_map$end
      } else {
        next
      }

      # difference in ("auto", "secs", "mins", "hours", "days", "weeks")
      if (unit %in% c("auto", "secs", "mins", "hours", "days", "weeks")) {
        source_data[[i]] <- as.numeric(difftime(
          source_data[[end_date]],
          source_data[[start_date]],
          units = unit
        ))
        # diffenrence in months
      } else if (unit == "months") {
        source_data[[i]] <- as.numeric(difftime(
          source_data[[end_date]],
          source_data[[start_date]],
          units = "days"
        )) / (365.25 / 12)
        # difference in years
      } else if (unit == "years") {
        source_data[[i]] <- as.numeric(difftime(
          source_data[[end_date]],
          source_data[[start_date]],
          units = "days"
        )) / (365.25)
      }
      # Case 3: copy the original value from REDCap
    } else {
      source_data[[i]] <- source_data[[source_field]]
    }

    # Apply Condition Logic
    if (!is.null(condition_logic) && nzchar(condition_logic)) {
      condition_mask <- with(source_data, eval(parse(text = condition_logic)))
      source_data[[i]][!condition_mask] <- NA
    }
  }

  cbio_data <- source_data %>%
    dplyr::select(dplyr::all_of(names(source_cbio_mapping)))

  return(cbio_data)
}
