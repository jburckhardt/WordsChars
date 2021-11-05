test_that("redundant test for count_chr_words", {
  expect_equal(count_chr_words("", count_spaces = TRUE), c(0,0)) # test empty string
  expect_equal(count_chr_words("hello", count_spaces = TRUE), c(1,5)) # test string with one word
  expect_equal(count_chr_words("hello", count_spaces = FALSE), c(1,5)) # string with one word but with count_spaces = FALSE should return same output
  expect_equal(count_chr_words("hello world", count_spaces = TRUE), c(2,11)) # string with two words
  expect_equal(count_chr_words("hello world", count_spaces = FALSE), c(2,10)) # string two words but not counting spaces as characters
  expect_equal(count_chr_words("This is a big string", count_spaces = TRUE), c(5, 20)) # text with more than two words
})

test_that("type of output is a interger vector",{
  expect_type(count_chr_words("hello world", count_spaces = FALSE), "integer") # expect return type -> base R type() function
})


test_that("Output is a vector",{
  expect_vector(count_chr_words("hello world", count_spaces = FALSE)) # expect return to be a vector
})

test_that("Length of output vector is 2",{
  expect_length(count_chr_words("hello world", count_spaces = FALSE), 2)
})

test_that("Return errors when incorrect input types",{
  expect_error(count_chr_words(2, count_spaces = TRUE)) # non character input given
  expect_error(count_chr_words("", count_spaces = 2)) # non logical input given
  expect_error(count_chr_words(c(), count_spaces = FALSE))  # vector given
})
