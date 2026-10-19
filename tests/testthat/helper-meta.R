# synthetic TRIA dataset files version of an example QWAmetadata JSON, as a raw list
tria_example_raw <- function(f = "TRIA_example_QWAmetadata.json") {
  raw <- jsonlite::read_json(system.file("extdata", f, package = "rxs2tria"), simplifyVector = TRUE)
  raw$related <- NULL
  raw$dataset <- raw$dataset |>
    dplyr::select(-dplyr::any_of(c("ds_access", "embargoed_until"))) |>
    dplyr::mutate(ds_label = "TRIA_test", url = "https://tria.example/TRIA_test",
                  doi = NA_character_, citation = "Test (2026). https://tria.example/TRIA_test",
                  dataset_version = 1L)
  raw$images <- dplyr::select(raw$images, -dplyr::starts_with("fname_"))
  raw$datafiles <- data.frame(
    resource_name = c("data/TRIA_test_QWAcells.csv.gz", "data/TRIA_test_QWArings.csv.gz"),
    resource_type = c("QWAdata_cells", "QWAdata_rings"),
    linked_level = "dataset",
    linked_label = "TRIA_test",
    size_bytes = c(1024L, 256L),
    checksum = strrep(c("a", "b"), 32),
    s3_key = c("TRIA_test/data/TRIA_test_QWAcells.csv.gz", "TRIA_test/data/TRIA_test_QWArings.csv.gz")
  )
  raw
}

# write a raw list to a temp JSON file in the layout read_QWAmetadata() expects
write_raw_json <- function(raw) {
  file <- tempfile(fileext = ".json")
  jsonlite::write_json(raw, file, na = "null", digits = NA)
  file
}
