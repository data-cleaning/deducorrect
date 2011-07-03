library(testthat)
library(editrules)


test_that("correctSigns",{
    expect_identical(
        correctSigns(
            editmatrix(c("x>0","y>0","z>0","x+y==z")),
            data.frame(x=-1,y=1,z=2))$corrected,
        data.frame(x=1,y=1,z=2)
    )
    expect_identical(
        correctSigns(
            editmatrix(c("x>0","y>0","z>0","x+y==z")),
            data.frame(x=1,y=2,z=1))$corrected,
       data.frame(x=1,y=2,z=1)
    )
}) 



