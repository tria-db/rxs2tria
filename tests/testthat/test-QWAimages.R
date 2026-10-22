imgs <- tibble::tribble(
  ~site_label, ~tree_label, ~woodpiece_label, ~slide_label, ~image_label, ~org_img_name, ~software,
  "S1", "S1_T1", "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG1", "S1_T1_WP1_SL1_IMG1", "roxas",
  "S1", "S1_T1", "S1_T1_WP1", "S1_T1_WP1_SL1", "S1_T1_WP1_SL1_IMG2", "S1_T1_WP1_SL1_IMG2", "roxas",
)

tbl_props_roxas <- function() {
  schema_path <- system.file(schema_rel_path("roxas"), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(schema_path, engine = "ajv")
  get_tbl_props(resolve_schema(schema_obj, schema_path))
}

test_that("validate_QWAimages aborts on invalid objects", {
  x <- suppressMessages(QWAimages(imgs))
  expect_identical(validate_QWAimages(x), x)

  na_lbl <- new_QWAimages(dplyr::mutate(imgs, slide_label = c(NA, "S1_T1_WP1_SL1")), "roxas")
  expect_error(validate_QWAimages(na_lbl), "missing values")
  no_lbl <- new_QWAimages(dplyr::select(imgs, -"tree_label"), "roxas")
  expect_error(validate_QWAimages(no_lbl), "missing key column")
  dupl <- new_QWAimages(dplyr::mutate(imgs, image_label = image_label[1]), "roxas")
  expect_error(validate_QWAimages(dupl), "duplicate")
  sw <- new_QWAimages(imgs, "roxas_ai")
  expect_error(validate_QWAimages(sw), "software")
})

test_that("QWAimages sets software to roxas_version", {
  expect_no_warning(
    x <- suppressMessages(QWAimages(dplyr::mutate(imgs, software = c("ROXAS", NA))))
  )
  expect_equal(x$software, c("roxas", "roxas"))

  mixed <- dplyr::mutate(imgs, software = c("roxas", "roxas_ai"))
  expect_warning(
    expect_warning(x <- suppressMessages(QWAimages(mixed, "roxas")), "set to"),
    "Mixed")
  expect_equal(x$software, c("roxas", "roxas"))
})

test_that("qc_input columns are kept and typed if present", {
  x <- suppressMessages(
    QWAimages(dplyr::mutate(imgs, comment_handled = c("TRUE", NA)))
  )

  expect_equal(x$comment_handled, c(TRUE, NA))
})

test_that("complete_QWAimages adds meta_input but not qc_input columns", {
  x <- suppressMessages(QWAimages(imgs))
  x <- suppressWarnings(suppressMessages(complete_QWAimages(x)))

  expect_true(all(c("spatial_resolution", "only_ew", "img_comment") %in% names(x)))
  expect_false("comment_handled" %in% names(x))
})

test_that("check_missing_opt reports missing optional columns except qc_input", {
  tbl_props <- tbl_props_roxas()
  x <- suppressWarnings(suppressMessages(complete_QWAimages(QWAimages(imgs))))
  expect_no_message(check_missing_opt(x, tbl_props, "roxas"))

  x$img_comment <- NULL
  expect_message(check_missing_opt(x, tbl_props, "roxas"), "img_comment")
})
