test_that("write_captions() function imports alt text/captions template
          as a data frame", {

            # import pre-written captions and alt text template
            caps_alttext <- utils::read.csv(
              system.file("resources", "captions_alt_text_template.csv", package = "satf")
            )

            expect_s3_class(caps_alttext, "data.frame")

            expect_no_error(caps_alttext)

          })

test_that("write_captions() function extracts some key quantities", {

  # read in sample dataset
  dat <- utils::read.csv(
    system.file("tests", "testthat", "fixtures", "sample_data", "petrale_sole-after_2020.csv", package = "satf")
  )

  # test biomass start year (B.start.year) extracted without error
  expect_no_error(
    dat |>
      dplyr::filter(label == "biomass",
                    module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
                    is.na(fleet),
                    is.na(age)) |>
      dplyr::mutate(estimate = as.numeric(estimate)) |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()
  )

  # test R0 extracted without error
  expect_no_error(
    dat |>
      dplyr::filter(grepl('R0', label) | grepl('recruitment_virgin', label)) |>
      dplyr::pull(estimate) |>
      as.numeric() |>
      round(digits = 2)
  )


  # test minimum recruitment deviation (recruit.dev.min) extracted without error
  expect_no_error(
    dat |>
      dplyr::filter(label == "recruitment_deviations" | label == "log_recruitment_deviations",
                    module_name == "SPAWN_RECRUIT" | module_name == "t.series",
                    !is.na(year),
                    is.na(fleet) | length(unique(fleet)) <= 1,
                    is.na(sex) | length(unique(sex)) <= 1,
                    is.na(area) | length(unique(area)) <= 1,
                    is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                    !year %in% year_exclusions
      ) |> # SS3 and BAM target module names
      dplyr::mutate(estimate = as.numeric(estimate),
                    year = as.numeric(year)) |>
      dplyr::slice(which.min(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)
  )

})
