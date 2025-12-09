# define color range
prim_col <- "#006268"
sec_col <- "#69004F"
tert_col <- "#00206E"
prim_col_grad <- c("#338585", "#66A3A3", "#99C2C2", "#CCE0E0", "#E6F0F0", "#F2F7F7")
sec_col_grad <- c("#853270", "#A36794", "#C299B8", "#E0CCDB", "#F0E6ED", "#F7F2F6")
tert_col_grad <- c("#324a85", "#6778a3", "#99a5c2", "#ccd2e0", "#e6e9f0", "#f2f4f7")

options(shiny.maxRequestSize = 100 * 1024^2)  # Set limit to 500 MB
#base_palette <- RColorBrewer::brewer.pal(11, "Spectral")
base_palette <- scales::pal_hue()(9)

# helper function to extend palette to desired length, reordering for better contrast if desired
extend_palette <- function(palette, n, contrasting = TRUE) {
  if (n>length(palette)){
    full_palette <- grDevices::colorRampPalette(palette)(n)
  } else {
    full_palette <- palette[1:n]
  }

  if (contrasting){
    k <- n %/% length(palette) + 1
    indices <- integer(0)
    for (j in 1:k){
      indices <- c(indices,seq(from = j, to = n, by = k))
    }
    full_palette <- full_palette[indices]
  }

  full_palette
}

shinyInput_CB_DT <- function(id, num, values, disabled = FALSE){
  inputs <- character(num)
  for (i in seq_len(num)) {
    if (!is.na(values[i])){
      if (disabled){
        inputs[i] <- as.character(shinyjs::disabled(shiny::checkboxInput(
          paste0(id, i), label = NULL, value = values[i], width = NULL)))
      } else {
        inputs[i] <- as.character(shiny::checkboxInput(
          paste0(id, i), label = NULL, value = values[i], width = NULL))
      }
    } else {
      inputs[i] <- ""
    }
  }
  inputs
}

discrete_features <- c(
  "Blue ring" = "blue_ring",
  "Frost ring" = "frost_ring",
  "Light ring" = "light_ring",
  "Intra-annual density fluctuations" = "iadf",
  "Traumatic resin ducts" = "traum_resin_ducts",
  "Other" = "other_discrete"
)

disqual_issues <- c(
  "Duplicate ring" = "duplicate_ring",
  "Radially incomplete ring" = "incomplete_ring",
  "Wedging/missing ring" = "missing_ring",
  "Crossdating" = "x_dating",
  "Compression wood" = "compression_wood",
  "Orientation" = "orientation",
  "Tyloses" = "tyloses",
  "Decay" = "decay",
  "Technical issues" = "technical_issues",
  "Other" = "other_disqual"
)

technical_issues <- c(
  "Out of focus" = "out_of_focus",
  "Crack(s)" = "cracks",
  "Paraffin" = "paraffin",
  "Compressed cells" = "compressed_cells",
  "Overlapping cells" = "overlapping_cells",
  "Broken cells" = "broken_cells",
  "Tangentially incomplete" = "tang_incomplete"
)

flag_cols <- c(
  "duplicate_rank" = "Duplicate Rank",
  "incomplete_ring" = "Incomplete Ring",
  "missing_cells" = "Missing Cells",
  "abnormal_cells" = "Abnormal Cells",
  "abnormal_profile" = "Abnormal Profile",
  "visual_quality" = "Visual Quality"
)
