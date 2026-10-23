context("rwl tidiers")

library(dplR)
data('ca533')
crn <- chron(ca533)

test_that("conversion followed by backconversion equals initial object", {
  # dplR >= 1.8.0 stores how a file was read in attr(, "dplR.provenance");
  # the tidy form does not carry it, so compare without it.
  ref <- ca533
  attr(ref, "dplR.provenance") <- NULL
  expect_that(untidy_rwl(tidy_rwl(ca533)), equals(ref))

  expect_that(untidy_crn(tidy_crn(crn)), equals(crn))

})
