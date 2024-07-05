test_that("validation checks works", {

  expect_equal(is_valid_hcpcs("11646"), TRUE)
  expect_error(is_valid_hcpcs("1164"))

  expect_equal(is_hcpcs_level_I("11646"), TRUE)
  expect_equal(is_hcpcs_level_I("E8015"), FALSE)

  expect_equal(is_hcpcs_level_II("11646"), FALSE)
  expect_equal(is_hcpcs_level_II("E8015"), TRUE)

  expect_equal(is_cpt_category_I("11646"), TRUE)
  expect_equal(is_cpt_category_I("1164F"), FALSE)

  expect_equal(is_cpt_category_II("11646"), FALSE)
  expect_equal(is_cpt_category_II("1164F"), TRUE)

  expect_equal(is_cpt_category_III("0074T"), TRUE)
  expect_equal(is_cpt_category_III("1164F"), FALSE)

})
