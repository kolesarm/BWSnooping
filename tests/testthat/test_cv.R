context("Test Monte Carlo calculation of critical values")

test_that("Sanity check for SnoopingCV", {

    diff1 <- SnoopingCV(100, "triangular", TRUE, 2)-
        SnoopingCV(99, "triangular", TRUE, 2)
    diff2 <- SnoopingCV(10, "uniform", TRUE, 2, alpha=0.06) -
        SnoopingCV(10, "uniform", TRUE, 2)

    expect_true(diff1 > 0 & diff1<0.02)
    expect_true(diff2 < 0 & diff2>-0.02)
})
