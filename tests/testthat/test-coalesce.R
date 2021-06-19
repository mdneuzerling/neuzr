test_that("%||% coalesces NULLs", {
  expect_equal("red panda" %||% NULL, "red panda")
  expect_equal(NULL %||% "red panda", "red panda")
  expect_equal(NULL %||% NULL %||% "red panda", "red panda")
  expect_null(NULL %||% NULL)
})
