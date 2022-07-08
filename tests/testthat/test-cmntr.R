library(testthat)
library(cmntr)

descTest <- "Macro generates source code from a specified macro. Macro in the source code will be resolved so that the resulting code file can run stand-alone"
assumpTest <- "Macro must have mprint statements to flag code generation. And another line to test wrapping"
inputsTest <- c("A","B","C","D")
dependTest <- list(
  Packages = "",
  Functions = "",
  Formats = "",
  Metadata = ""
)
paramsTest <- list(
  in_loc = "The location of the input file",
  in_flnm = "The name of the input file",
  out_loc = "The location of the output file",
  out_flnm = "The name of the output file"
)


test_that("Test Comment Constructor", {

  testComment <- comment("test-cmntr.R", "0.1", descTest, "Aheer Alvi",
                         "2022-07-06", assumpTest, paramsTest, inputsTest,
                         inputsTest, dependTest)

  expect_equal(attributes(testComment)$class, "comment")
})

