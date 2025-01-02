test_that("plot_spawn_recruitment generates plots without errors", {

  # read in sample dataset
  dat <- utils::read.csv(
    system.file("tests", "testthat", "fixtures", "sample_data", "petrale_sole-after_2020.csv", package = "satf")
  )

  # expect error-free plot with minimal arguments
  expect_no_error(
    satf::plot_spawn_recruitment(dat)
  )

  # expect error-free plot with many arguments
  expect_no_error(
    satf::plot_spawn_recruitment(
      dat,
      spawning_biomass_label = "mt",
      recruitment_label = "mt",
      end_year = 2026,
      make_rda = FALSE,
      rda_dir = getwd()
    )
  )


  # expect ggplot object is returned
  expect_s3_class(
    satf::plot_spawn_recruitment(
      dat,
      spawning_biomass_label = "mt",
      recruitment_label = "mt",
      end_year = 2026,
      make_rda = FALSE,
      rda_dir = getwd()
    ),
    "gg"
  )

})

test_that("rda file made when indicated",{

  # read in sample dataset
  dat <- utils::read.csv(
    system.file("tests", "testthat", "fixtures", "sample_data", "petrale_sole-after_2020.csv", package = "satf")
  )

  # export rda
  plot_spawn_recruitment(
    dat,
    spawning_biomass_label = "mt",
    recruitment_label = "mt",
    end_year = 2026,
    make_rda = TRUE,
    rda_dir = getwd()
  )

  # expect that both rda_files dir and the sr_figure.rda file exist
  expect_true(dir.exists(file.path(here::here(getwd(), "rda_files"))))
  expect_true(file.exists(file.path(here::here(getwd(), "rda_files", "sr_figure.rda"))))

  # erase files placed in here::here()
  on.exit(unlink(file.path(here::here(getwd(), "captions_alt_text.csv"))))
  on.exit(unlink(file.path(here::here(getwd(), "rda_files"), recursive = TRUE)))



})
