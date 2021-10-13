test_that("parse_pipeline works for Rmds", {
  skip_on_cran()

  temp_file <- tempfile(fileext = ".md")
  test_rmd <- system.file("test", "test.Rmd", package = "mario")
  test_md <- system.file("test", "test.md", package = "mario")

  rmarkdown::render(input = test_rmd, output_file = temp_file)

  expect_equal(readLines(test_md), readLines(temp_file))
})
