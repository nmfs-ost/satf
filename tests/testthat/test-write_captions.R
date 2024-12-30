test_that("write_captions() function imports alt text/captions template
          as a data frame", {

  # import pre-written captions and alt text template
  caps_alttext <- utils::read.csv(
    system.file("resources", "captions_alt_text_template.csv", package = "satf")
  )

  expect_s3_class(caps_alttext, "data.frame")

  expect_no_error(caps_alttext)

  })
