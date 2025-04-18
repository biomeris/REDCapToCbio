test_that("Create a new study folder", {
  temp_path <- tempdir()
  studyFolder <- paste0("study_", as.integer(Sys.time()))
  full_path <- file.path(temp_path, studyFolder)

  on.exit(unlink(full_path, recursive = TRUE))

  createStudyFolder(study_folder = studyFolder, output_path = temp_path)

  expect_true(dir.exists(full_path))
})
