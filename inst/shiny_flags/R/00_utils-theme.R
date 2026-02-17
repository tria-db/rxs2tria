# COLORS ------------------
# define color range
prim_col <- "#006268"
sec_col <- "#69004F"
tert_col <- "#00206E"
prim_col_grad <- c("#338585", "#66A3A3", "#99C2C2", "#CCE0E0", "#E6F0F0", "#F2F7F7")
sec_col_grad <- c("#853270", "#A36794", "#C299B8", "#E0CCDB", "#F0E6ED", "#F7F2F6")
tert_col_grad <- c("#324a85", "#6778a3", "#99a5c2", "#ccd2e0", "#e6e9f0", "#f2f4f7")

# create palette for plots
base_palette <- RColorBrewer::brewer.pal(11, "Spectral")
#base_palette <- scales::pal_hue()(9)
#base_palette <- c("#338585", "#324a85","#853270")

# helper function to extend palette to desired length,
# reordering for better contrast if desired
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
