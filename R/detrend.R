# detrending


library(data.table)
library(zoo)
library(dplR)
library(forecast)

summary_det_sector <- function(
    df,
    spline_years = c(50, 30, 10),
    do_zscore = TRUE,
    do_ar = TRUE,
    do_diff = TRUE,
    min_year_cor = 1850
) {
  library(data.table)
  library(zoo)
  library(dplR)
  library(forecast)

  dt <- as.data.table(df)
  setorder(dt, tree_id, param, stat, sector_n, year)

  # -----------------------------
  # 1. Interpolate per TREE/param/stat
  # -----------------------------
  dt[, value_interp := zoo::na.approx(value, na.rm = FALSE),
     by = .(tree_id, param, stat)]

  detrended_cols <- character()

  # -----------------------------
  # 2. Z-score detrending per TREE × SECTOR × PARAM × STAT
  # -----------------------------
  if (do_zscore) {
    dt[, detrended_zscore := {
      mu  <- mean(value_interp, na.rm = TRUE)
      sdv <- sd(value_interp, na.rm = TRUE)
      if (is.na(sdv) || sdv == 0) NA_real_ else (value_interp - mu) / sdv
    }, by = .(tree_id, sector_n, param, stat)]

    dt[is.na(value), detrended_zscore := NA]
    detrended_cols <- c(detrended_cols, "detrended_zscore")
  }

  # -----------------------------
  # 3. Spline detrending per TREE × SECTOR × PARAM × STAT
  # -----------------------------
  for (ny in spline_years) {
    cname <- paste0("detrended_spline_", ny)

    # Aggregate per tree/sector/param/stat/year and apply spline
    agg <- dt[, .(val = mean(value_interp, na.rm = TRUE)),
              by = .(tree_id, sector_n, param, stat, year)]

    agg[, (cname) := {
      x <- val
      yrs <- year
      if (sum(!is.na(x)) < 10 || length(unique(yrs)) != length(yrs)) {
        rep(NA_real_, .N)
      } else {
        tryCatch({
          rwl <- data.frame(series = x)
          rownames(rwl) <- yrs
          as.numeric(dplR::detrend(rwl, method = "Spline", nyrs = ny)[, 1])
        }, error = function(e) rep(NA_real_, .N))
      }
    }, by = .(tree_id, sector_n, param, stat)]

    # Merge back into main dt — column already has correct name
    agg_subset <- agg[, .(tree_id, sector_n, param, stat, year, val_spline = get(cname))]
    setnames(agg_subset, "val_spline", cname)
    dt <- merge(
      dt,
      agg_subset,
      by = c("tree_id", "sector_n", "param", "stat", "year"),
      all.x = TRUE
    )


    detrended_cols <- c(detrended_cols, cname)
  }

  # -----------------------------
  # 4. AR(1) residuals per TREE × SECTOR × PARAM × STAT
  # -----------------------------
  if (do_ar) {
    dt[, detrended_ar1 := {
      x <- value_interp
      if (sum(!is.na(x)) < 10 || length(unique(na.omit(x))) < 2) {
        rep(NA_real_, .N)
      } else {
        tryCatch(
          as.numeric(resid(forecast::Arima(x, order = c(1,0,0), include.mean = TRUE))),
          error = function(e) rep(NA_real_, .N)
        )
      }
    }, by = .(tree_id, sector_n, param, stat)]

    dt[is.na(value), detrended_ar1 := NA]
    detrended_cols <- c(detrended_cols, "detrended_ar1")
  }

  # -----------------------------
  # 5. First difference per TREE × SECTOR × PARAM × STAT
  # -----------------------------
  if (do_diff) {
    dt[, detrended_diff := c(NA_real_, diff(value_interp)),
       by = .(tree_id, sector_n, param, stat)]

    dt[is.na(value), detrended_diff := NA]
    detrended_cols <- c(detrended_cols, "detrended_diff")
  }

  # -----------------------------
  # 6. Sector-level chronologies
  # -----------------------------
  chronologies <- rbindlist(lapply(detrended_cols, function(col) {
    dt[, .(
      value   = dplR::tbrm(get(col)),
      n_trees = sum(!is.na(get(col)))
    ), by = .(sector_n, param, stat, year)][
      , det_method := col]
  }))

  # -----------------------------
  # 7. Rbar + EPS diagnostics per SECTOR × PARAM × STAT
  # -----------------------------
  diagnostics <- rbindlist(lapply(detrended_cols, function(col) {
    rbindlist(lapply(unique(dt$sector_n), function(sector) {
      rbindlist(lapply(unique(dt$param), function(p) {
        rbindlist(lapply(unique(dt$stat), function(s) {
          tmp <- dt[sector_n == sector & param == p & stat == s & year >= min_year_cor]
          tmp_agg <- tmp[, .(val = mean(get(col), na.rm = TRUE)), by = .(year, tree_id)]
          tree_ids <- unique(tmp_agg$tree_id)
          n_trees <- length(tree_ids)

          Rbar <- NA_real_
          EPS  <- NA_real_

          if (n_trees >= 2) {
            mat_df <- dcast(tmp_agg, year ~ tree_id, value.var = "val", fill = NA)
            tree_cols <- setdiff(names(mat_df), "year")
            tree_cols <- tree_cols[sapply(mat_df[, tree_cols, with = FALSE],
                                          function(x) length(unique(na.omit(x))) >= 2)]
            if (length(tree_cols) >= 2) {
              cor_mat <- suppressWarnings(cor(mat_df[, ..tree_cols], use = "pairwise.complete.obs"))
              rbar <- mean(cor_mat[upper.tri(cor_mat)], na.rm = TRUE)
              eps <- (length(tree_cols) * rbar) / (length(tree_cols) * rbar + (1 - rbar))

              Rbar <- rbar
              EPS  <- eps
            }
          }

          data.table(
            sector_n  = sector,
            param     = p,
            stat      = s,
            Rbar      = Rbar,
            EPS       = EPS,
            n_trees   = n_trees,
            det_method = col
          )
        }))
      }))
    }))
  }))

  # -----------------------------
  # 8. Return
  # -----------------------------
  list(
    det.timeseries  = dt,
    det.chronology  = chronologies,
    det.diagnostics = diagnostics
  )
}


summary_det_band <- function(
    df,
    spline_years = c(50, 30, 10),
    do_zscore = TRUE,
    do_ar = TRUE,
    do_diff = TRUE,
    min_year_cor = 1850
) {
  library(data.table)
  library(zoo)
  library(dplR)
  library(forecast)

  dt <- as.data.table(df)
  setorder(dt, tree_id, ew_lw, param, stat, year)

  # ---- 1️⃣ interpolate missing values ----
  dt[, value_interp := zoo::na.approx(value, na.rm = FALSE),
     by = .(tree_id, ew_lw, param, stat)]

  detrended_cols <- character()

  # ---- 2️⃣ z-score detrending ----
  if(do_zscore){
    dt[, detrended_zscore := {
      mu <- mean(value_interp, na.rm = TRUE)
      sdv <- sd(value_interp, na.rm = TRUE)
      if(is.na(sdv) || sdv == 0) NA_real_ else (value_interp - mu)/sdv
    }, by = .(tree_id, ew_lw, param, stat)]
    dt[is.na(value), detrended_zscore := NA]
    detrended_cols <- c(detrended_cols, "detrended_zscore")
  }

  # ---- 3️⃣ spline detrending ----
  for(ny in spline_years){
    cname <- paste0("detrended_spline_", ny)
    dt[, (cname) := {
      x <- value_interp
      valid_len <- sum(!is.na(x))
      if(valid_len < 5) {
        rep(NA_real_, .N)
      } else {
        tryCatch({
          nyrs_use <- min(ny, valid_len)
          # create rwl data.frame with years as rownames
          rwl <- data.frame(series = x)
          rownames(rwl) <- year
          spline_vec <- dplR::detrend(rwl, method="Spline", nyrs=nyrs_use)[,1]
          as.numeric(spline_vec)
        }, error=function(e) rep(NA_real_, .N))
      }
    }, by = .(tree_id, ew_lw, param, stat)]
    dt[is.na(value), (cname) := NA]
    detrended_cols <- c(detrended_cols, cname)
  }

  # ---- 4️⃣ AR(1) residuals ----
  if(do_ar){
    dt[, detrended_ar1 := {
      x <- value_interp
      if(sum(!is.na(x)) < 10 || length(unique(na.omit(x))) < 2) rep(NA_real_, .N) else {
        tryCatch(as.numeric(resid(Arima(x, order = c(1,0,0), include.mean = TRUE))),
                 error=function(e) rep(NA_real_, .N))
      }
    }, by = .(tree_id, ew_lw, param, stat)]
    dt[is.na(value), detrended_ar1 := NA]
    detrended_cols <- c(detrended_cols, "detrended_ar1")
  }

  # ---- 5️⃣ first difference ----
  if(do_diff){
    dt[, detrended_diff := c(NA_real_, diff(value_interp)), by = .(tree_id, ew_lw, param, stat)]
    dt[is.na(value), detrended_diff := NA]
    detrended_cols <- c(detrended_cols, "detrended_diff")
  }

  # ---- 6️⃣ Chronologies per detrending method ----
  chronologies <- rbindlist(lapply(detrended_cols, function(col){
    dt[, .(value = dplR::tbrm(get(col))), by = .(ew_lw, param, stat, year)][, det_method := col]
  }))

  # ---- 7️⃣ Diagnostics: Rbar, EPS, n_trees ----
  # Get min year for correlation window
  detrended_cols <- grep("^detrended", names(dt), value = TRUE)

  diagnostics <- rbindlist(lapply(detrended_cols, function(col) {
    rbindlist(lapply(c("all", "EW", "LW"), function(ew_type) {
      rbindlist(lapply(unique(dt$param), function(p) {        # loop param
        rbindlist(lapply(unique(dt$stat), function(s) {      # loop stat

          tmp_s <- copy(dt)
          if(ew_type != "all") tmp_s <- tmp_s[ew_lw == ew_type]
          tmp_s <- tmp_s[param == p & stat == s & year >= min_year_cor]

          # Aggregate duplicates per tree/year
          tmp_s_agg <- tmp_s[, .(val = mean(get(col), na.rm=TRUE)), by = .(year, tree_id)]

          tree_ids <- unique(tmp_s_agg$tree_id)
          n_trees <- length(tree_ids)

          Rbar <- NA_real_
          EPS  <- NA_real_

          if(n_trees >= 2){
            mat_df <- dcast(tmp_s_agg, year ~ tree_id, value.var = "val", fill = NA)
            tree_cols <- setdiff(names(mat_df), "year")
            tree_cols <- tree_cols[sapply(mat_df[, tree_cols, with = FALSE],
                                          function(x) length(unique(na.omit(x))) >= 2)]
            if(length(tree_cols) >= 2){
              cor_mat <- suppressWarnings(cor(mat_df[, ..tree_cols], use="pairwise.complete.obs"))
              Rbar <- mean(cor_mat[upper.tri(cor_mat)], na.rm=TRUE)
              EPS  <- (length(tree_cols) * Rbar) / (length(tree_cols) * Rbar + (1-Rbar))
            }
          }

          data.table(
            ew_lw = ew_type,
            param  = p,
            stat   = s,
            Rbar   = Rbar,
            EPS    = EPS,
            n_trees = n_trees,
            det_method = col
          )

        })) # stat
      })) # param
    })) # ew_type
  })) # detrended_cols




  # ---- 8️⃣ Return results ----
  list(
    det.timeseries  = dt,
    det.chronology  = chronologies,
    det.diagnostics = diagnostics
  )
}




