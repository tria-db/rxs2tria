# GLOBAL OPTIONS -----------
options(shiny.maxRequestSize = 100 * 1024^2)  # Set limit to 500 MB

# COLORS ------------------
# define color range
prim_col <- "#006268"
sec_col <- "#69004F"
tert_col <- "#00206E"
prim_col_grad <- c("#338585", "#66A3A3", "#99C2C2", "#CCE0E0", "#E6F0F0", "#F2F7F7")
sec_col_grad <- c("#853270", "#A36794", "#C299B8", "#E0CCDB", "#F0E6ED", "#F7F2F6")
tert_col_grad <- c("#324a85", "#6778a3", "#99a5c2", "#ccd2e0", "#e6e9f0", "#f2f4f7")

# create palette for plots
#base_palette <- RColorBrewer::brewer.pal(8, "Dark2")
#base_palette <- scales::pal_hue()(9)
#base_palette <- c("#338585", "#324a85","#853270")
base_palette <- c("#1B9E77FF", "#D95F02FF", "#7570B3FF", "#E7298AFF", "#66A61EFF", "#338585", "#324a85","#853270", "#A6761DFF", "#666666FF")
# dark2 with added lighter base colors

# helper function to extend palette to desired length,
# reordering for better contrast if desired
extend_palette <- function(palette, n, contrasting = TRUE) {
  if (n > length(palette)) {
    full_palette <- grDevices::colorRampPalette(palette)(n)
  } else {
    full_palette <- palette[1:n]
  }

  if (contrasting) {
    k <- n %/% length(palette) + 1
    indices <- integer(0)
    for (j in 1:k) {
      indices <- c(indices, seq(from = j, to = n, by = k))
    }
    full_palette <- full_palette[indices]
  }

  full_palette
}

sample_depth_color <- tert_col
coverage_colorscale <- list(c(0, "#FED976"), c(1, prim_col))

default_bg_color <- tert_col_grad[4]
hot_color_dupl <- prim_col_grad[3]
hot_color_excl <- tert_col_grad[3]
hot_color_iss1 <- tert_col_grad[4]
hot_color_iss2 <- tert_col_grad[5]

sel_row_color <- "#ed4c4c"
chosen_dupl_color <- prim_col_grad[2]
warn_inv_color <- 'pink'

# BSLIB THEME ------------------
# helper function to create the theme
add_gradient_vars <- function(theme, prefix, gradient) {
  vars <- stats::setNames(
    as.list(gradient),
    paste0(prefix, "-", seq_along(gradient))
  )

  # Build the full argument list: theme first, then the variables
  args <- c(list(theme), vars)
  do.call(bslib::bs_add_variables, args)
}
create_theme <- function(
  prim_col, sec_col, tert_col,
  prim_col_grad, sec_col_grad, tert_col_grad
) {
  theme <- bslib::bs_theme(
    version = 5,
    primary   = prim_col,
    secondary = sec_col,
    info      = tert_col,
    font_scale = 0.8,
    preset = "zephyr"
  ) |>
  bslib::bs_add_variables(
    "body-bg"         = prim_col_grad[5],
    "focus-ring-color" = sec_col_grad[4]
  ) |>
  add_gradient_vars("prim-col-grad", prim_col_grad) |>
  add_gradient_vars("sec-col-grad", sec_col_grad) |>
  add_gradient_vars("tert-col-grad", tert_col_grad) |>
  bslib::bs_add_rules(
    list(
      sass::sass_file("www/css/custom-theme.scss")   # ← the big CSS block lives here
    )
  )

  theme
}

theme <- create_theme(
  prim_col, sec_col, tert_col,
  prim_col_grad, sec_col_grad, tert_col_grad
)


# GLOBAL VARS ------------
discrete_features <- c(
  "Blue ring" = "blue_ring",
  "Frost ring" = "frost_ring",
  "Light ring" = "light_ring",
  "Intra-annual density fluctuations" = "iadf",
  "Traumatic resin ducts" = "traum_resin_ducts",
  "Trabeculae" = "trabeculae",
  "Other features" = "other_discrete"
)

disqual_issues <- c(
  "Radially incomplete ring" = "incomplete_ring",
  "Wedging/missing ring" = "missing_ring",
  "Crossdating" = "x_dating",
  "Compression wood" = "compression_wood",
  "Orientation" = "orientation"
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

other_issues <- c(
  "Tyloses" = "tyloses",
  "Decay" = "decay",
  "Other issues" = "other_disqual"
)

all_flags <- unname(c(discrete_features, disqual_issues,
                      technical_issues, other_issues))

input_specs <- list(
  prf_data = list(
    req_cols = c(image_label = "c", year = "i", sector_n = "i")
    # opt_cols: any numeric are considered as measurements for the plot
    # (except for counts -> see below)
  ),
  rings_data = list(
    req_cols = c(woodpiece_label = "c", slide_label = "c", image_label = "c",
                 year = "i", incomplete_ring = "l", missing_ring = "l",
                 duplicate_ring = "l", exclude_dupl = "l", exclude_issues = "l"),
    opt_cols = c(
      # any of the quality flag columns (e.g. if data was previously edited in app)
      setNames(rep("l", length(all_flags)), all_flags),
      affected_tissue = "c", comment = "c")
    # plus any numeric columns to considered as measurements -> see below
  ),
  rxsmeta_data = list(
    req_cols = c(image_label = "c"),
    opt_cols = c(woodpiece_label = "c",
                 species_code = "c", site_label = "c", comment = "c", comment_handled = "l",
                 fname_image = "c", fname_annotated = "c", fname_annotated_twin = "c")
  )
)

# GENERAL HELPERS ------------------
label_with_tt <- function(label_text, tooltip_text,
                           icon_name = "info-circle", icon_lib = "font-awesome",
                           tooltip_title = NULL){
  shiny::span(
    label_text,
    bslib::tooltip(
      shiny::icon(icon_name, lib = icon_lib),
      title = tooltip_title,
      tooltip_text
    )
  )
}

# wrapper for an error message
show_error_modal <- function(title, message) {
  if (shiny::isRunning()) {
    shiny::showModal(shiny::modalDialog(
      title = title,
      message,
      easyClose = TRUE,
      footer = NULL
    ))
  } else {
    message(sprintf("[ERROR] %s: %s", title, message))
  }
}

# wrapper for a warning notification
show_warning_notification <- function(message) {
  if (shiny::isRunning()) {
    shiny::showNotification(message, type = "warning")
  } else {
    message(sprintf("[WARNING] %s", message))
  }
}

# Safely try to run a given expression without crashing the app
# run the expr in the given block
# and catch errors and warnings, showing them in a modal dialog or notification
# in case of a warning, the rest of the block is still executed
# in case of an error, the block is exited, returning NULL, and the error is shown in a modal dialog
# if propagate_err = FALSE, the parent context will not receive an error and execution continues with the NULL return from the current block
# modal dialog is shown only if the error is not already a modal_shown class, so only for the lowest level
safe_block <- function(expr,
                       err_title = "Error", err_message = "An error occurred:",
                       propagate_err = TRUE,
                       warn_message = "A warning occurred:") {
  tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        show_warning_notification(paste(warn_message, w$message))
        rlang::cnd_muffle(w) # muffle warnings so they don't propagate up
      }
    ),
    error = function(e) {
      if (!inherits(e, "modal_shown")) {
        show_error_modal(err_title, paste(err_message, e$message))
        e <- structure(e, class = c("modal_shown", class(e)))
      }
      if (propagate_err) {
        stop(e)  # re-throw error in parent context
      }
      return(NULL)
    }
  )
}


