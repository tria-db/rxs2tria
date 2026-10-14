# keys of schema_rel_path() that map to a table schema (path checks the lookup table)
tbl_keys <- c("dataset", "authors", "funding", "related", "supplementary", "sites",
              "trees", "woodpieces", "slides", "roxas", "roxas_ai", "cells", "rings")

test_that("table schemas yield usable column definitions", {
  for (key in tbl_keys) {
    path <- system.file(schema_rel_path(key), package = "rxs2tria")
    schema_obj <- jsonvalidate::json_schema$new(path, engine = "ajv")
    props <- get_tbl_props(resolve_schema(schema_obj, path))
    expect_gt(length(props$properties), 0, label = key)
    expect_true(all(props$required %in% names(props$properties)), label = key)
    expect_named(create_empty_df(props), names(props$properties), label = key)
  }
})

test_that("full metadata and app schemas resolve all refs", {
  for (key in c("QWAmetadata", "shiny_meta")) {
    path <- system.file(schema_rel_path(key), package = "rxs2tria")
    expect_no_error(
      resolve_schema(jsonvalidate::json_schema$new(path, engine = "ajv"), path)
    )
  }
})

test_that("example QWAmetadata files read and check against schemas without error", {
  for (f in c("TRIA_example_QWAmetadata.json", "TRIA_example_AI_QWAmetadata.json")) {
    x <- suppressWarnings(suppressMessages(
      read_QWAmetadata(system.file("extdata", f, package = "rxs2tria"))))
    expect_s3_class(x, "QWAmetadata")
    expect_no_error(suppressWarnings(suppressMessages(check_QWAmetadata(x))))
  }
})
