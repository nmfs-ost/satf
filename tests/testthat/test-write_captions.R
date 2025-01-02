test_that("write_captions() function imports alt text/captions template as a data frame",
          {
            # import pre-written captions and alt text template
            caps_alttext <- utils::read.csv(system.file("resources", "captions_alt_text_template.csv", package = "satf"))

            expect_s3_class(caps_alttext, "data.frame")

            expect_no_error(caps_alttext)

          })


# test_that("write_captions() function replaces placeholder text with key quantities as expected",
#           {
#             dat <- utils::read.csv(
#               system.file(
#                 "tests",
#                 "testthat",
#                 "fixtures",
#                 "sample_data",
#                 "petrale_sole-after_2020.csv",
#                 package = "satf"
#               )
#             )
#
#             write_captions(dat, dir = here::here(), year = 2022)
#
#             caps <- read.csv(here::here(tempdir, "captions_alt_text.csv"))
#
#             caps[2,2]
#           })
