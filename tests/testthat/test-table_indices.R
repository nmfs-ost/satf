test_that("table_indices generates plots without errors", {

  # read in sample dataset
  dat <- utils::read.csv(
    system.file("tests", "testthat", "fixtures", "sample_data", "petrale_sole-after_2020.csv", package = "satf")
  )

  # expect error-free plot with minimal arguments
  expect_no_error(
    satf::table_indices(dat)
  )

  # expect error-free plot with many arguments
  expect_no_error(
    satf::table_indices(
      dat,
      make_rda = FALSE,
      rda_dir = getwd()
    )
  )


  # expect flextable object is returned
  expect_s3_class(
    satf::table_indices(
      dat,
      make_rda = FALSE,
      rda_dir = getwd()
    ),
    "flextable"
  )

})

test_that("rda file made when indicated",{

  # read in sample dataset
  dat <- utils::read.csv(
    system.file("tests", "testthat", "fixtures", "sample_data", "petrale_sole-after_2020.csv", package = "satf")
  )

  # export rda
  table_indices(
    dat,
    make_rda = TRUE,
    rda_dir = getwd()
  )

  # expect that both rda_files dir and the indices.abundance_table.rda file exist
  expect_true(dir.exists(file.path(here::here(getwd(), "rda_files"))))
  expect_true(file.exists(file.path(here::here(getwd(), "rda_files", "indices.abundance_table.rda"))))

  # erase files placed in here::here()
  on.exit(unlink(file.path(here::here(getwd(), "captions_alt_text.csv"))))
  on.exit(unlink(file.path(here::here(getwd(), "rda_files"), recursive = TRUE)))


})
