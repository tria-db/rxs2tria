#' Read already processed QWA data from CSV files
#' @export
read_QWAdata <- function(file_path, dataset_name = NULL, file_rings = NULL, file_cells = NULL) {

  # create filepaths from path and dataset name
  if (!is.null(dataset_name)) {
    file_rings <- file.path(file_path, glue::glue("{dataset_name}_rings.csv"))
    if (!file.exists(file_rings)) {
      stop("Rings file not found under: ", file_rings)
    }
    file_cells <- file.path(file_path, glue::glue("{dataset_name}_cells.csv"))
    if (!file.exists(file_rings)) {
      stop("Cells file not found under: ", file_rings)
    }
  }

  # TODO: implement for providing file names directly

  QWA_data <- list()

  # TODO: fix with final columns
  QWA_data$rings <- vroom::vroom(
    file_rings,
    col_types = c(.default = "d",
                  tree_code = "c", woodpiece_code = "c", slide_code = "c", image_code = "c",
                  year = "i", cno = "i", innermost_ring = "l", outermost_ring = "l",
                  incomplete_inner= "l", incomplete_innerv2= "l", incomplete_ring= "l", incomplete_fct_check = "l",
                  missing_ring= "l", no_MRW_other= "l", missing_ringV2= "l", duplicate_ring= "l", duplicate_rank= "i"))

  # TODO: final cols, EW_LW as logical?
  QWA_data$cells <- vroom::vroom(
    file_cells,
    col_types = c(.default = "d",
                  image_code = "c", year = "i", xpix = "i", ypix = "i", nbrno = "i", nbrid = "i", sector100 = "i", ew_lw = "c")
    )
  # TODO: check for problems?

  # TODO: validate that its after all processing steps?


  return(QWA_data)
}
