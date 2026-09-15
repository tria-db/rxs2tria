# Base df_rings fixture: 
df_rings_multi <- tibble::tribble(
  ~woodpiece_label, ~slide_label, ~image_label, ~year, ~mrw, ~exclude_dupl, ~exclude_issues,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2001, 100, FALSE, FALSE,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2002, 110, FALSE, FALSE,
  "WP1", "WP1_SL1", "WP1_SL1_IMG1", 2003, 120, FALSE, FALSE,
  "WP1", "WP1_SL1", "WP1_SL1_IMG2", 2003, 999, TRUE,  FALSE,
  "WP1", "WP1_SL1", "WP1_SL1_IMG2", 2004, 130, FALSE, FALSE,
  "WP1", "WP1_SL1", "WP1_SL1_IMG2", 2005, 140, FALSE, TRUE,
  "WP2", "WP2_SL1", "WP2_SL1_IMG1", 2003, 200, FALSE, FALSE,
  "WP2", "WP2_SL1", "WP2_SL1_IMG1", 2004, 210, FALSE, FALSE,
  "WP2", "WP2_SL1", "WP2_SL1_IMG2", 2005, 220, FALSE, FALSE,
  "WP2", "WP2_SL1", "WP2_SL1_IMG2", 2006, 230, FALSE, FALSE,
)

# Matching prf_data fixture:
prf_data_multi <- tibble::tribble(
  ~image_label, ~year, ~sector_n, ~la_mean,
  "WP1_SL1_IMG1", 2001, 1, 1100,
  "WP1_SL1_IMG1", 2002, 1, 1110,
  "WP1_SL1_IMG1", 2003, 1, 1120,
  "WP1_SL1_IMG2", 2003, 1, 9999, # matches the excluded duplicate ring; must not appear
  "WP1_SL1_IMG2", 2004, 1, 1130,
  "WP1_SL1_IMG2", 2005, 1, 1140, # matches the excluded issue ring; must not appear
  "WP2_SL1_IMG1", 2003, 1, 2200,
  "WP2_SL1_IMG1", 2004, 1, 2210,
  "WP2_SL1_IMG2", 2005, 1, 2220,
  "WP2_SL1_IMG2", 2006, 1, 2230,
  "WP1_SL1_IMG1", 2001, 2, 9999, # other sector, must be excluded
  "WP2_SL1_IMG1", 2003, 2, 9999, # other sector, must be excluded
)

test_that("ring-level export aggregates images per woodpiece, drops excluded rows, and fills year gaps", {
  df_rwl <- create_rwl(df_rings = df_rings_multi, param = "mrw")

  expect_s3_class(df_rwl, "rwl")
  expect_equal(rownames(df_rwl), as.character(2001:2006))
  expect_equal(unname(df_rwl[["WP1"]]), c(100, 110, 120, 130, NA, NA))
  expect_equal(unname(df_rwl[["WP2"]]), c(NA, NA, 200, 210, 220, 230))
})

test_that("profile-level export filters by sector, joins by image_label/year, and respects exclusions", {
  df_rwl <- create_rwl(
    df_rings = df_rings_multi, param = "la_mean",
    prf_data = prf_data_multi, sector = 1
  )

  expect_equal(unname(df_rwl[["WP1"]]), c(1100, 1110, 1120, 1130, NA, NA))
  expect_equal(unname(df_rwl[["WP2"]]), c(NA, NA, 2200, 2210, 2220, 2230))
})

test_that("missing flag columns abort with a hint", {
  df_rings <- df_rings_multi |> dplyr::select(-exclude_issues)

  expect_error(
    create_rwl(df_rings = df_rings, param = "mrw"),
    regexp = "Missing required column"
  )
})

test_that("non-existent / non-measurement param aborts", {
  expect_error(
    create_rwl(df_rings = df_rings_multi, param = "nonexistent_param"),
    regexp = "not a measurements column"
  )

  expect_error(
    create_rwl(df_rings = df_rings_multi, param = "image_label", 
      prf_data = prf_data_multi, sector = 1),
    regexp = "is not a measurements column"
  )
})

test_that("scale_for_tucson auto-scales to use the available digit range", {
  df_rwl <- create_rwl(df_rings = df_rings_multi, param = "mrw")

  # max_val = 230, max_representable = (10^5 - 1) * 0.001 = 99.999
  # -> optimal_scale = 99.999 / 230 = 0.43478... -> scaling = 10^floor(log10(.)) = 0.1
  scaled <- suppressMessages(scale_for_tucson(df_rwl))

  expect_equal(scaled$scaling, 0.1)
  expect_equal(unname(scaled$rwl[["WP2"]]), c(NA, NA, 20, 21, 22, 23))

  # max_representable = (10^5 - 1) * 0.01 = 999.99
  # -> optimal_scale = 999.99 / 230 = 4.3478... -> scaling = 10^floor(log10(.)) = 1
  scaled <- scale_for_tucson(df_rwl, prec = 0.01)

  expect_equal(scaled$scaling, 1)
})

test_that("scale_for_tucson warns and falls back to scaling 1 when no positive values are available", {
  df_rings <- df_rings_multi |> dplyr::mutate(mrw = NA_real_)
  df_rwl <- create_rwl(df_rings = df_rings, param = "mrw")

  expect_warning(
    scaled <- scale_for_tucson(df_rwl),
    regexp = "auto-scaling factor"
  )
  expect_equal(scaled$scaling, 1)
})
