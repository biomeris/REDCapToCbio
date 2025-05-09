library(REDCapToCbio)

# Create study folder
output_folder <- createStudyFolder(study_folder = "brca", output_path = "/path/to/folder")

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
                      name = "Breast Invasive Carcinoma",
                      dedicated_color = "HotPink",
                      parent_type_of_cancer = "Breast")

# Write Clinical Data File - PATIENT_ATTRIBUTES
# Export data from REDCap
redcap_data <- redcapToCbio(
  redcap_uri = Sys.getenv("RC_URL"),
  token = Sys.getenv("TOKEN_897")
)

# Write files
createClinicalDataFiles(
  study_folder = output_folder,
  cancer_study_identifier = cancer_study_identifier,
  datatype = "PATIENT_ATTRIBUTES",
  clinical_data = redcap_data
)
