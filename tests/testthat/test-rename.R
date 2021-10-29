test_that("rename works", {
  verb <- "^rename"
  temp_dir <- tempdir()

  code_path <- system.file("test", "code", package = "mario") %>%
    list.files(full.names = TRUE) %>%
    purrr::keep(~grepl(verb, basename(.x)))

  correct_path <- system.file("test", "correct", package = "mario") %>%
    list.files(full.names = TRUE) %>%
    purrr::keep(~grepl(verb, basename(.x)))

  json_path <- file.path(temp_dir, basename(code_path)) %>%
    tools::file_path_sans_ext() %>%
    paste0(".json")

  purrr::map2(json_path, code_path, ~writeLines(file_to_json(.y), .x))
  correct <- purrr::map2_lgl(correct_path, json_path, ~all.equal(readLines(.x), readLines(.y)))

  # To debug:
  (wrong <- which(!correct))
  json_path[wrong]

  expect_true(all(correct))
})
