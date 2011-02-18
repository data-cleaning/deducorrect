library(testthat)

test_that("Comparing equal strings works",{
   expect_equal(damerauLevenshteinDistance(1340, 1340), 0)
})

test_that("Deletions works",{
   expect_equal(damerauLevenshteinDistance(130, 1340), 1)
})

test_that("Insertion works",{
   expect_equal(damerauLevenshteinDistance(1340, 13040), 1)
})

test_that("Substitution works",{
   expect_equal(damerauLevenshteinDistance(1340, 1380), 1)
})

test_that("Transposition",{
   expect_equal(damerauLevenshteinDistance(1340, 1430), 1)
})

test_that("Transposition",{
   expect_equal(damerauLevenshteinDistance(1340, 14300), 2)
})