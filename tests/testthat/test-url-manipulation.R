test_that("logicals are converted before going in URLs", {
  expect_equal(make_url_friendly(TRUE), "true")
  expect_equal(make_url_friendly(FALSE), "false")
})

test_that("spaces are converted before going in URLs", {
  expect_equal(make_url_friendly("red panda"), "red%20panda")
})

test_that("Can correctly suffix an individual parameter", {
  expect_equal(
    add_parameter("www.example.com", "animal1", "echidna"),
    "www.example.com?animal1=echidna"
  )
  expect_equal(
    add_parameter("www.example.com?animal1=echidna", "animal2", "ostrich"),
    "www.example.com?animal1=echidna&animal2=ostrich"
  )
})

test_that("Boolean parameters properly suffixed", {
  expect_equal(
    add_parameter("www.example.com", "pineapple", T),
    "www.example.com?pineapple=true"
  )
  expect_equal(
    add_parameter("www.example.com", "pineapple", F),
    "www.example.com?pineapple=false"
  )
})

# The rest of the tests will use the add_parameters function, which calls on the
# add_parameter function.

test_that("Can add parameters either directly or with variables", {
  fat_goose <- "penguin"
  expect_equal(
    add_parameters("www.example.com", animal = fat_goose, food = "peanut"),
    "www.example.com?animal=penguin&food=peanut"
  )
})

test_that("Adding a NULL parameter will return request unaltered", {
  expect_equal(
    add_parameters("www.example.com", parameter = NULL),
    "www.example.com"
  )
  expect_equal(
    add_parameters("www.example.com", parameter = NULL, animal = "emu"),
    "www.example.com?animal=emu"
  )
  expect_equal(
    add_parameters("www.example.com", animal = "emu", parameter = NULL),
    "www.example.com?animal=emu"
  )
})

test_that("Adding a length 0 parameter will return request unaltered", {
  expect_equal(
    add_parameters("www.example.com", parameter = character(0)),
    "www.example.com"
  )
  expect_equal(
    add_parameters("www.example.com", parameter = character(0), animal = "emu"),
    "www.example.com?animal=emu"
  )
  expect_equal(
    add_parameters("www.example.com", animal = "emu", parameter = character(0)),
    "www.example.com?animal=emu"
  )
})

test_that("Multi-valued parameters are combined according to the strategy", {
  expect_equal(
    add_parameters(
      "www.example.com",
      animal = "crocodile",
      eats = c("fish", "humans"),
      .combine = "repeat_name"
    ),
    "www.example.com?animal=crocodile&eats=fish&eats=humans"
  )
  expect_equal(
    add_parameters(
      "www.example.com",
      animal = "crocodile",
      eats = c("fish", "humans"),
      .combine = "with_commas"
    ),
    "www.example.com?animal=crocodile&eats=fish,humans"
  )
  expect_equal(
    add_parameters(
      "www.example.com",
      animal = "crocodile",
      eats = c("fish", "humans"),
      .combine = "with_hex_commas"
    ),
    "www.example.com?animal=crocodile&eats=fish%2Chumans"
  )
  expect_equal(
    add_parameters(
      "www.example.com",
      animal = "crocodile",
      eats = c("fish", "humans"),
      .combine = "|"
    ),
    "www.example.com?animal=crocodile&eats=fish|humans"
  )
})

test_that("Adding an unnamed parameter will error", {
  expect_error(
    add_parameters("www.example.com", "giraffe"),
    "Parameters must be named"
  )
  expect_error(
    add_parameters("www.example.com", food = "peanut", "giraffe"),
    "Parameters must be named"
  )
})

test_that("Adding no parameters leaves request unchanges", {
  expect_equal(
    add_parameters("www.example.com"),
    "www.example.com"
  )
})

test_that("Can suffix multiple parameters at once", {
  expect_equal(
    add_parameters("example.com", para1 = "fish", para2 = "cow"),
    "example.com?para1=fish&para2=cow"
  )
})
