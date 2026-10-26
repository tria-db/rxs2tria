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

test_that("ring-level export creates series per woodpiece, drops excluded and fills year gaps", {
  df_rwl <- extract_rwl(df_rings = df_rings_multi, param = "mrw")

  expect_s3_class(df_rwl, "rwl")
  expect_equal(rownames(df_rwl), as.character(2001:2006))
  expect_equal(unname(df_rwl[["WP1"]]), c(100, 110, 120, 130, NA, NA))
  expect_equal(unname(df_rwl[["WP2"]]), c(NA, NA, 200, 210, 220, 230))
})

test_that("ring-level export accepts estimate columns (eww, lww)", {
  df_rwl <- extract_rwl(df_rings = dplyr::mutate(df_rings_multi, eww = mrw / 2), param = "eww")

  expect_equal(unname(df_rwl[["WP2"]]), c(NA, NA, 100, 105, 110, 115))
})

test_that("profile-level export filters by sector, joins by image_label/year, and respects exclusions", {
  df_rwl <- extract_rwl(
    df_rings = df_rings_multi, param = "la_mean",
    prf_data = prf_data_multi, sector = 1
  )

  expect_equal(unname(df_rwl[["WP1"]]), c(1100, 1110, 1120, 1130, NA, NA))
  expect_equal(unname(df_rwl[["WP2"]]), c(NA, NA, 2200, 2210, 2220, 2230))
})

test_that("profile-level export aborts on profile rows without a rings row", {
  prf_data <- dplyr::bind_rows(prf_data_multi, tibble::tibble(
    image_label = "WP1_SL1_IMG1", year = 2010, sector_n = 1, la_mean = 1))

  expect_error(extract_rwl(df_rings_multi, "la_mean", prf_data = prf_data, sector = 1),
               "no row for 1 year")
})

test_that("profile-level export accepts quantile params as named by the profile functions", {
  prf_data <- prf_data_multi |> dplyr::mutate(la_q25.7 = la_mean, la_q100 = la_mean)

  for (param in c("la_q25.7", "la_q100")) {
    df_rwl <- extract_rwl(df_rings = df_rings_multi, param = param,
                          prf_data = prf_data, sector = 1)
    expect_equal(unname(df_rwl[["WP1"]]), c(1100, 1110, 1120, 1130, NA, NA))
  }
})

test_that("tissue set to 'affected' keeps issue rings with only ew/lw affected tissue", {
  df_rings <- df_rings_multi |>
    dplyr::mutate(affected_tissue = dplyr::case_when(
      woodpiece_label == "WP1" & year == 2005 ~ "ew", # issue ring: kept
      mrw == 999 ~ "lw"                               # duplicate: still dropped
    ))
  # issue ring with NA affected_tissue: still dropped
  df_rings$exclude_issues[df_rings$woodpiece_label == "WP2" & df_rings$year == 2006] <- TRUE

  df_rwl <- extract_rwl(df_rings = df_rings, param = "la_mean",
                        prf_data = prf_data_multi, sector = 1, tissue = "affected")

  expect_equal(rownames(df_rwl), as.character(2001:2005))
  expect_equal(unname(df_rwl[["WP1"]]), c(1100, 1110, 1120, 1130, 1140))
  expect_equal(unname(df_rwl[["WP2"]]), c(NA, NA, 2200, 2210, 2220))
})

test_that("extract_ring_measures drops excluded rings and keeps id and measure columns", {
  df_rings <- df_rings_multi |>
    dplyr::mutate(cno = 10, eww = 50, missing_ring = FALSE, comment = NA_character_)

  df <- extract_ring_measures(df_rings)

  expect_named(df, c("woodpiece_label", "slide_label", "image_label", "year", "cno", "mrw", "eww"))
  expect_equal(nrow(df), 8)
  expect_false(any(df$mrw %in% c(999, 140)))
})

test_that("extract_ring_measures aborts on several rings per woodpiece and year", {
  df_rings <- df_rings_multi
  df_rings$exclude_dupl[df_rings$mrw == 999] <- FALSE

  expect_error(extract_ring_measures(df_rings), regexp = "Multiple rings per woodpiece and year")
})

test_that("scale_for_tucson auto-scales to use the available digit range", {
  df_rwl <- extract_rwl(df_rings = df_rings_multi, param = "mrw")

  # max_val = 230, max_representable = (10^5 - 1) * 0.001 = 99.999
  # -> optimal_scale = 99.999 / 230 = 0.43478... -> scaling = 10^floor(log10(.)) = 0.1
  scaled <- suppressMessages(scale_for_tucson(df_rwl))

  expect_equal(attr(scaled, "scaling"), 0.1)
  expect_equal(unname(scaled[["WP2"]]), c(NA, NA, 20, 21, 22, 23))

  # max_representable = (10^5 - 1) * 0.01 = 999.99
  # -> optimal_scale = 999.99 / 230 = 4.3478... -> scaling = 10^floor(log10(.)) = 1
  scaled <- suppressMessages(scale_for_tucson(df_rwl, prec = 0.01))

  expect_equal(attr(scaled, "scaling"), 1)
})

test_that("scale_for_tucson warns and falls back to scaling 1 when no positive values are available", {
  df_rings <- df_rings_multi |> dplyr::mutate(mrw = NA_real_)
  df_rwl <- extract_rwl(df_rings = df_rings, param = "mrw")

  expect_warning(
    scaled <- scale_for_tucson(df_rwl),
    regexp = "auto-scaling factor"
  )
  expect_equal(attr(scaled, "scaling"), 1)
})

test_that("scale_for_tucson applies a manual scaling as given, without messages", {
  df_rwl <- extract_rwl(df_rings = df_rings_multi, param = "mrw")

  expect_silent(scaled <- scale_for_tucson(df_rwl, scaling = 0.001))
  expect_equal(attr(scaled, "scaling"), 0.001)
  expect_equal(unname(scaled[["WP2"]]), c(NA, NA, 0.2, 0.21, 0.22, 0.23))
})

test_that("scale_for_tucson aborts when a manual scaling exceeds the Tucson range", {
  df_rwl <- extract_rwl(df_rings = df_rings_multi, param = "mrw")

  # max_val = 230 > max_representable = 99.999 at prec = 0.001; auto factor is 0.1
  expect_error(
    scale_for_tucson(df_rwl, scaling = 1),
    regexp = "exceeds the Tucson range"
  )
  expect_error(
    scale_for_tucson(df_rwl, scaling = 1),
    regexp = "scaling = 0.1"
  )
})

test_that("scale_for_tucson and rename_for_tucson keep each other's attributes in either order", {
  df_rwl <- extract_rwl(df_rings = df_rings_multi, param = "mrw")
  df_structure <- tibble::tibble(woodpiece_label = c("WP1", "WP2"), site_label = "SITE")

  out1 <- df_rwl |>
    scale_for_tucson(scaling = 0.001) |>
    rename_for_tucson(df_structure, long.names = TRUE)
  out2 <- df_rwl |>
    rename_for_tucson(df_structure, long.names = TRUE) |>
    scale_for_tucson(scaling = 0.001)

  for (out in list(out1, out2)) {
    expect_s3_class(out, "rwl")
    expect_equal(attr(out, "scaling"), 0.001)
    expect_equal(attr(out, "mapping")$woodpiece_label, c("WP1", "WP2"))
    expect_equal(attr(out, "mapping")$series_id, names(out))
  }
})

test_that("make_short_series_ids picks the first variant that is short and unique", {
  # repeated rows as in QWAimages: one row per woodpiece
  df_one <- tibble::tibble(woodpiece_label = "YAM_LASI_122_a", site_label = "YAM",
                           species_code = "LASI")[c(1, 1), ]

  # site + base
  expect_equal(make_short_series_ids(df_one)$series_id, "YAM122a")
  # base only, as site + base is too long
  expect_equal(make_short_series_ids(df_one, max_chars = 6)$series_id, "122a")
  # species + base, as site + base and base are not unique
  df_two <- tibble::tibble(woodpiece_label = c("YAM_LASI_1", "YAM_PISY_1"),
                           site_label = "YAM", species_code = c("LASI", "PISY"))
  expect_equal(make_short_series_ids(df_two)$series_id, c("LASI1", "PISY1"))
  # no variant fits
  expect_error(make_short_series_ids(df_one, max_chars = 2), "Could not derive")
})

test_that("rename_for_tucson limits series IDs to the long.names width", {
  rwl <- dplR::as.rwl(data.frame(YAM_LASI_122_a = c(1, 2), row.names = c("2001", "2002")))
  df_structure <- tibble::tibble(woodpiece_label = "YAM_LASI_122_a", site_label = "YAM",
                                 species_code = "LASI")

  expect_named(rename_for_tucson(rwl, df_structure, long.names = TRUE), "YAM122a")
  expect_named(rename_for_tucson(rwl, df_structure, long.names = FALSE), "122a")
})
