imgs <- tibble::tribble(
  ~site_label, ~tree_label, ~woodpiece_label, ~slide_label, ~image_label, ~org_img_name, ~software,
  "S1", "S1_T1", "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG1", "S1_T1_WP1_SL1_IMG1", "roxas",
  "S1", "S1_T1", "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG2", "S1_T1_WP1_SL1_IMG2", "roxas",
)

test_that("manual columns are kept and typed if present", {
  x <- suppressMessages(
    QWAimages(dplyr::mutate(imgs, comment_handled = c("TRUE", NA)))
  )

  expect_equal(x$comment_handled, c(TRUE, NA))
})

test_that("complete_QWAimages does not add manual columns", {
  x <- suppressMessages(QWAimages(imgs))
  x <- suppressWarnings(suppressMessages(complete_QWAimages(x)))

  expect_true("spatial_resolution" %in% names(x))
  expect_false(any(c("only_ew", "img_comment", "comment_handled") %in% names(x)))
})

test_that("check_missing_opt ignores manual columns", {
  x <- suppressWarnings(suppressMessages(complete_QWAimages(QWAimages(imgs))))

  expect_no_warning(suppressMessages(check_QWAimages(x)))
})
