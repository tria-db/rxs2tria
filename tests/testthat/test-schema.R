# keys of schema_rel_path() that map to a table schema, per stage (path checks the lookup)
tbl_keys <- c("dataset", "authors", "funding", "supplementary", "sites",
              "trees", "woodpieces", "slides", "roxas", "roxas_ai", "cells", "rings")
stage_keys <- list(submission = c(tbl_keys, "related"), tria = c(tbl_keys, "datafiles"))

# validate a list of rows against the table schema of a stage
validate_rows <- function(rows, key, submission = TRUE) {
  path <- system.file(schema_rel_path(key, submission), package = "rxs2tria")
  schema_obj <- jsonvalidate::json_schema$new(path, engine = "ajv")
  schema_obj$validate(jsonlite::toJSON(rows, auto_unbox = TRUE, null = "null"))
}

ds_shared <- list(ds_name = "test", ds_title = "Test dataset", description = "A test",
                  ds_license = "CC BY 4.0")
ds_sub <- c(ds_shared, list(ds_access = "public", embargoed_until = NULL))
ds_tria <- c(ds_shared, list(ds_label = "TRIA_test", url = "https://tria.example/TRIA_test",
                             doi = NULL, citation = "Test (2026)", dataset_version = 1L))

res_shared <- list(resource_name = "images/img1.jpg", resource_type = "image_original",
                   linked_level = "image", linked_label = "img1")
res_sub <- c(res_shared, list(fname_resource = "/data/images/img1.jpg", status = "ok"))
res_tria <- c(res_shared, list(size_bytes = 1024L, checksum = strrep("a", 32)))

img_shared <- list(image_label = "S_T1_W1_S1_I1", slide_label = "S_T1_W1_S1",
                   woodpiece_label = "S_T1_W1", tree_label = "S_T1", site_label = "S",
                   org_img_name = "img1", software = "roxas")

test_that("table schemas yield usable column definitions at both stages", {
  for (stage in names(stage_keys)) {
    for (key in stage_keys[[stage]]) {
      path <- system.file(schema_rel_path(key, stage == "submission"), package = "rxs2tria")
      schema_obj <- jsonvalidate::json_schema$new(path, engine = "ajv")
      props <- get_tbl_props(resolve_schema(schema_obj, path))
      label <- paste(stage, key)
      expect_gt(length(props$properties), 0, label = label)
      expect_true(all(props$required %in% names(props$properties)), label = label)
      expect_named(create_empty_df(props), names(props$properties), label = label)
    }
  }
})

test_that("full metadata and app schemas resolve all refs", {
  for (submission in c(TRUE, FALSE)) {
    path <- system.file(schema_rel_path("QWAmetadata", submission), package = "rxs2tria")
    expect_no_error(resolve_schema(jsonvalidate::json_schema$new(path, engine = "ajv"), path))
  }
  path <- system.file(schema_rel_path("shiny_meta"), package = "rxs2tria")
  expect_no_error(resolve_schema(jsonvalidate::json_schema$new(path, engine = "ajv"), path))
})

test_that("stage-only schemas are not found at the other stage", {
  expect_error(schema_rel_path("related", submission = FALSE), "No schema")
  expect_error(schema_rel_path("datafiles", submission = TRUE), "No schema")
})

test_that("dataset: each stage accepts its own rows and rejects the other's", {
  expect_true(validate_rows(list(ds_sub), "dataset", TRUE))
  expect_true(validate_rows(list(ds_tria), "dataset", FALSE))
  expect_false(validate_rows(list(ds_tria), "dataset", TRUE))
  expect_false(validate_rows(list(ds_sub), "dataset", FALSE))
})

test_that("dataset: 'All rights reserved' is only allowed in TRIA dataset files", {
  expect_false(validate_rows(list(utils::modifyList(ds_sub, list(ds_license = "All rights reserved"))),
                             "dataset", TRUE))
  expect_true(validate_rows(list(utils::modifyList(ds_tria, list(ds_license = "All rights reserved"))),
                            "dataset", FALSE))
})

test_that("dataset: 'other' is only allowed in submission files", {
  expect_true(validate_rows(list(utils::modifyList(ds_sub, list(ds_license = "other"))),
                            "dataset", TRUE))
  expect_false(validate_rows(list(utils::modifyList(ds_tria, list(ds_license = "other"))),
                             "dataset", FALSE))
})

test_that("dataset: license may only be empty for restricted submissions", {
  restricted <- utils::modifyList(ds_sub, list(ds_access = "restricted", embargoed_until = "2027-01-01"))
  for (empty in list("", NULL)) {
    expect_false(validate_rows(list(c(ds_sub[names(ds_sub) != "ds_license"], list(ds_license = empty))),
                               "dataset", TRUE))
    expect_true(validate_rows(list(c(restricted[names(restricted) != "ds_license"], list(ds_license = empty))),
                              "dataset", TRUE))
    expect_false(validate_rows(list(c(ds_tria[names(ds_tria) != "ds_license"], list(ds_license = empty))),
                               "dataset", FALSE))
  }
  # absent field
  expect_false(validate_rows(list(ds_sub[names(ds_sub) != "ds_license"]), "dataset", TRUE))
  expect_true(validate_rows(list(restricted[names(restricted) != "ds_license"]), "dataset", TRUE))
  expect_false(validate_rows(list(ds_tria[names(ds_tria) != "ds_license"]), "dataset", FALSE))
})

test_that("dataset: embargo date may be absent for public submissions only", {
  restricted <- utils::modifyList(ds_sub, list(ds_access = "restricted", embargoed_until = "2027-01-01"))
  expect_true(validate_rows(list(ds_sub[names(ds_sub) != "embargoed_until"]), "dataset", TRUE))
  expect_false(validate_rows(list(restricted[names(restricted) != "embargoed_until"]), "dataset", TRUE))
})

test_that("conditionally required fields are not extracted as required", {
  path <- system.file(schema_rel_path("dataset", TRUE), package = "rxs2tria")
  props <- get_tbl_props(resolve_schema(jsonvalidate::json_schema$new(path, engine = "ajv"), path))
  expect_setequal(props$required, c("ds_name", "ds_title", "description", "ds_access"))

  path <- system.file(schema_rel_path("dataset", FALSE), package = "rxs2tria")
  props <- get_tbl_props(resolve_schema(jsonvalidate::json_schema$new(path, engine = "ajv"), path))
  expect_true("ds_license" %in% props$required)
})

test_that("dataset: restricted access requires an embargo date", {
  restricted <- utils::modifyList(ds_sub, list(ds_access = "restricted"))
  expect_false(validate_rows(list(restricted), "dataset", TRUE))
  expect_false(validate_rows(list(utils::modifyList(restricted, list(embargoed_until = ""))),
                             "dataset", TRUE))
  expect_true(validate_rows(list(utils::modifyList(restricted, list(embargoed_until = "2027-01-01"))),
                            "dataset", TRUE))
})

test_that("supplementary: each stage accepts its own rows and rejects the other's", {
  expect_true(validate_rows(list(res_sub), "supplementary", TRUE))
  expect_true(validate_rows(list(res_tria), "supplementary", FALSE))
  expect_false(validate_rows(list(res_tria), "supplementary", TRUE))
  expect_false(validate_rows(list(res_sub), "supplementary", FALSE))
})

test_that("datafiles require size, checksum and S3 key", {
  expect_false(validate_rows(list(res_tria), "datafiles", FALSE))
  expect_true(validate_rows(list(c(res_tria, list(s3_key = "TRIA_test/data/cells.csv.gz"))),
                            "datafiles", FALSE))
})

test_that("images: fname_* only in submission files", {
  img_sub <- c(img_shared, list(fname_image = "img1.jpg"))
  for (rv in c("roxas", "roxas_ai")) {
    img <- utils::modifyList(img_sub, list(software = rv))
    expect_true(validate_rows(list(img), rv, TRUE), label = rv)
    expect_false(validate_rows(list(img), rv, FALSE), label = rv)
    expect_true(validate_rows(list(img[names(img) != "fname_image"]), rv, FALSE), label = rv)
  }
})

test_that("example QWAmetadata files read and check against schemas", {
  for (f in c("TRIA_example_QWAmetadata.json", "TRIA_example_AI_QWAmetadata.json")) {
    x <- suppressWarnings(suppressMessages(
      read_QWAmetadata(system.file("extdata", f, package = "rxs2tria"))))
    expect_s3_class(x, "QWAmetadata")
    expect_snapshot(check_QWAmetadata(x))
  }
})
