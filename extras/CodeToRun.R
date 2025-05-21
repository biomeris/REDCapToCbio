library(REDCapToCbio)

# Create study folder
# output_folder <- createStudyFolder(study_folder = "brca", output_path = "/path/to/folder")
output_folder <- createStudyFolder(study_folder = "brca", output_path = "/Users/vramella/tmp")

# Define global variables
cancer_study_identifier <- "brca_joneslab_2013"

# Write Cancer Study metafile
createCancerStudyMetafile(study_folder = output_folder,
                          type_of_cancer = "brca",
                          cancer_study_identifier = cancer_study_identifier,
                          name = "Breast Cancer (Jones Lab 2013)",
                          description = "Comprehensive profiling of 103 breast cancer samples.",
                          add_global_case_list = TRUE)

# Write Cancer Type meta file and data file
createCancerTypeFiles(study_folder = output_folder,
                      type_of_cancer = "brca",
                      name = "Invasive Breast Carcinoma",
                      dedicated_color = "HotPink",
                      parent_type_of_cancer = "Breast")

# Write Clinical Data File - PATIENT_ATTRIBUTES
# Export data from REDCap
redcap_data <- redcapToCbio(
  redcap_uri = Sys.getenv("RC_URI"),
  token = Sys.getenv("TOKEN_913")
)

# Write files
createClinicalDataFiles(
  study_folder = output_folder,
  cancer_study_identifier = cancer_study_identifier,
  datatype = "PATIENT_ATTRIBUTES",
  clinical_data = redcap_data
)

# Write Clinical Data File - SAMPLE_ATTRIBUTES
# Get access token
access_token <- requestAPIToken(
  client_id = Sys.getenv("CLIENT_ID"),
  client_secret = Sys.getenv("CLIENT_SECRET"),
  token_url = Sys.getenv("TOKEN_URL")
)

# Get study ID
study_id <- getStudyID(
  studylink_url = Sys.getenv("STUDYLINK_URL"),
  access_token = access_token,
  redcap_url = Sys.getenv("RC_URL"),
  pid = 913
)

# Get vials details
vials_details <- getVialsAttributes(
  studylink_url = Sys.getenv("STUDYLINK_URL"),
  access_token = access_token,
  study_id = study_id
)

# Write files
createClinicalDataFiles(
  study_folder = output_folder,
  cancer_study_identifier = cancer_study_identifier,
  datatype = "SAMPLE_ATTRIBUTES",
  clinical_data = vials_details
)
