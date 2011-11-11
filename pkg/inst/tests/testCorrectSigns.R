library(testthat)
library(editrules)

context("Correctoion of sign errors")

test_that("correctSigns",{
    expect_identical(
        correctSigns(
            editmatrix(c("x>0","y>0","z>0","x+y==z")),
            data.frame(x=-1,y=1,z=2))$corrected,
        data.frame(x=1,y=1,z=2)
    )
    expect_identical(
        correctSigns( # nothing can be done without specifying swap
            editmatrix(c("x>0","y>0","z>0","x+y==z")),
            data.frame(x=1,y=2,z=1))$corrected,
       data.frame(x=1,y=2,z=1)
    )
    expect_identical(
        correctSigns( # specify swap
            editmatrix(c("x>0","y>0","z>0","x+y==z")),
            data.frame(x=1,y=2,z=1),
            swap=list("z","y"))$corrected,
       data.frame(x=1,y=1,z=2)
    )
    expect_identical( # flip has variable not in E
        correctSigns(
            editmatrix(c("y>0","z>0","x+y==z")),
            data.frame(x=1,y=2,z=1,u=3),
            flip=c("x","u"))$corrected,
       data.frame(x=-1,y=2,z=1,u=3)
    )
    expect_identical( # swap has variable not in E
        correctSigns(
            editmatrix(c("x>0","y>0","z>0","x-y==z")),
            data.frame(x=1,y=2,z=1,u=3),
            flip=c(),swap=list(c("x","y"),c("u","z")))$corrected,
       data.frame(x=2,y=1,z=1,u=3)
    )
    
}) 



