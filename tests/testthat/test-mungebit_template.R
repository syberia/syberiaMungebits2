context("mungebit_template")

test_that("it can create a new mungebit from a simple template", {
  generator <- mungebit_template({
    train   <- identity
    predict <- identity
  })
  expect_is(generator(), "mungebit") 
})

test_that("it can create a new mungebit from a simple template with column transformation set", {
  generator <- mungebit_template({
    column_transformation <- TRUE
    train   <- identity
    predict <- identity
  })
  expect_is(generator(), "mungebit") 
  expect_is(generator()$train_function(), "column_transformation")
})

