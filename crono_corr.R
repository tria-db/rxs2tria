library(dplyr)
library(collapse)
library(purrr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)


prf_data <- prf_data %>%
  left_join(QWA_data$rings %>% select(image_label, year, mrw), by = c("image_label", "year")) %>%
  mutate(tree_id = str_c(word(image_label, 1, 3, sep = "_"), collapse = NULL))

create_chronology <- function(prf_data,
                              QWA_rings = NULL,
                              PARAM,
                              SECTOR = NULL,
                              include_mrw = FALSE,
                              fname_out = "chronology") {

  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)

  # 1️⃣ Create tree_id
  prf_data <- prf_data %>%
    arrange(image_label, year, sector_n) %>%
    mutate(tree_id = str_c(word(image_label, 1, 3, sep = "_"), collapse = NULL))

  # 2️⃣ Merge MRW if requested
  if (include_mrw && !is.null(QWA_rings)) {
    prf_data <- prf_data %>%
      left_join(
        QWA_rings %>% select(image_label, year, mrw),
        by = c("image_label", "year")
      )
  }

  # 3️⃣ Exclude double rings
  if (!is.null(QWA_rings)) {
    exclude_pairs <- QWA_rings %>%
      filter(exclude_dupl == TRUE) %>%
      select(image_label, year) %>%
      distinct()

    prf_data <- prf_data %>%
      anti_join(exclude_pairs, by = c("image_label", "year"))
  }

  # 4️⃣ Filter sector if specified
  if (!is.null(SECTOR)) {
    prf_data <- prf_data %>%
      filter(sector_n == SECTOR)
  }

  # 5️⃣ Aggregate to tree × year using dplyr
  tree_year <- prf_data %>%
    group_by(tree_id, year) %>%
    summarise(
      mean_value = mean(.data[[PARAM]], na.rm = TRUE),
      .groups = "drop"
    )

  # 6️⃣ Yearly mean chronology
  yearly <- tree_year %>%
    group_by(year) %>%
    summarise(
      mean_value_yearly = mean(mean_value, na.rm = TRUE),
      .groups = "drop"
    )

  write_csv(
    yearly,
    paste0(fname_out, "_", PARAM,
           ifelse(is.null(SECTOR), "", paste0("_sector", SECTOR)),
           "_yearly.csv")
  )

  # 7️⃣ Pivot wide for inter-tree correlation
  wide <- tree_year %>%
    pivot_wider(
      names_from = tree_id,
      values_from = mean_value
    ) %>%
    arrange(year) %>%
    filter(year >= 1900)

  cor_mat <- cor(
    select(wide, -year),
    use = "pairwise.complete.obs"
  )

  mean_cor <- mean(cor_mat[upper.tri(cor_mat)], na.rm = TRUE)
  cat("Mean inter-tree correlation (raw):", mean_cor, "\n")

  # Return results
  return(list(
    tree_year = tree_year,
    yearly = yearly,
    wide = wide,
    cor_mat = cor_mat,
    mean_cor = mean_cor
  ))
}



# cwtrad_mean for sector 5
res_cwtrad <- create_chronology(
  prf_data = prf_data,
  QWA_rings = QWA_data$rings,
  PARAM = "cwtrad_mean",
  SECTOR = 5,
  include_mrw = FALSE,
  fname_out = "cwtrad"
)

res_la <- create_chronology(
  prf_data = prf_data,
  QWA_rings = QWA_data$rings,
  PARAM = "la_mean",
  SECTOR = NULL,
  include_mrw = FALSE,
  fname_out = "la"
)


# mrw (all sectors)
res_mrw <- create_chronology(
  prf_data = prf_data %>% filter(tree_id %in% c("TAZ_CDAT_09E2", "TAZ_CDAT_09E")),
  QWA_rings = QWA_data$rings,
  PARAM = "mrw",
  SECTOR = NULL,
  include_mrw = TRUE,
  fname_out = "mrw"
)


create_chronologies_fast <- function(prf_data,
         QWA_rings = NULL,
         quant_probs = c(0.1, 0.5, 0.75, 0.9),
         sel_cell_params = c("la", "cwttan", "cwtrad",
                             "cwtall", "drad", "dtan", "cwa"),
         SECTOR = NULL,
         fname_out = "chronology") {



  # 1️⃣ Prepare data -----------------------------------------------------------
  prf_data <- prf_data %>%
    arrange(image_label, year, sector_n) %>%
    mutate(tree_id = str_c(word(image_label, 1, 3, sep = "_"), collapse = NULL))

  # Exclude double rings
  if (!is.null(QWA_rings)) {
    prf_data <- prf_data %>%
      anti_join(
        QWA_rings %>%
          filter(exclude_dupl) %>%
          distinct(image_label, year),
        by = c("image_label", "year")
      )
  }

  # 2️⃣ Sector filtering (NOW FLEXIBLE) ---------------------------------------
  sector_label <- "all"
  if (!is.null(SECTOR)) {
    prf_data <- prf_data %>%
      filter(sector_n %in% SECTOR)

    sector_label <- paste0("sector", paste(SECTOR, collapse = "_"))
  }

  # 3️⃣ Build parameter × quantile grid ---------------------------------------
  grid <- cross_df(list(
    param = sel_cell_params,
    prob  = quant_probs
  )) %>%
    mutate(
      q_lab = paste0("q", sprintf("%02d", round(prob * 100))),
      col   = paste0(param, "_", q_lab)
    ) %>%
    filter(col %in% names(prf_data))

  # 4️⃣ Core computation -------------------------------------------------------
  results <- grid %>%
    split(seq_len(nrow(.))) %>%
    map(function(g) {

      col <- g$col
      param <- g$param
      q_lab <- g$q_lab

      # Tree × year mean
      tree_year <- fmean(
        prf_data[[col]],
        g = interaction(prf_data$tree_id, prf_data$year, drop = TRUE),
        na.rm = TRUE
      ) %>%
        data.frame(mean_value = .,
                   tree_year = names(.)) %>%
        separate(tree_year, into = c("tree_id", "year"),
                 sep = "\\.", convert = TRUE)

      # Yearly chronology
      yearly <- fmean(
        tree_year$mean_value,
        g = tree_year$year,
        na.rm = TRUE
      ) %>%
        data.frame(
          year = as.numeric(names(.)),
          mean_value_yearly = .
        )

      # Write CSV
      write_csv(
        yearly,
        paste0(fname_out, "_", col, "_", sector_label, "_yearly.csv")
      )

      # Inter-tree correlation
      wide <- tree_year %>%
        pivot_wider(
          names_from = tree_id,
          values_from = mean_value
        ) %>%
        arrange(year) %>%
        filter(year >= 1900)

      cor_mat <- cor(
        select(wide, -year),
        use = "pairwise.complete.obs"
      )

      mean_cor <- mean(cor_mat[upper.tri(cor_mat)], na.rm = TRUE)

      list(
        tree_year = tree_year,
        yearly = yearly,
        wide = wide,
        cor_mat = cor_mat,
        mean_cor = mean_cor
      )
    })

  # 5️⃣ Name output ------------------------------------------------------------
  results_named <- results %>%
    set_names(map_chr(grid$col, ~ paste0(.x, "_", sector_label)))

  return(results_named)
}


res_fast <- create_chronologies_fast(
  prf_data = prf_data,
  QWA_rings = QWA_data$rings,
  quant_probs = c(0.75),
  sel_cell_params = c("la", "cwttan", "cwtrad",
                      "cwtall", "drad", "dtan", "cwa", "mrw"),
  SECTOR = 5,
  fname_out = "QWA"
)

res_s1_5 <- create_chronologies_fast(prf_data,
                                              QWA_data$rings,
                                              quant_probs = c(0.75),
                                              sel_cell_params = c("la", "cwttan", "cwtrad","cwa", "mrw"),
                                              SECTOR = 1:5,
                                              fname_out = "QWA")

###################
create_chronologies_by_sector <- function(
    prf_data,
    QWA_rings = NULL,
    quant_probs = c(0.1, 0.5, 0.75, 0.9),
    sel_cell_params = c("la", "cwttan", "cwtrad",
                        "cwtall", "drad", "dtan", "cwa"),
    SECTOR = NULL,
    fname_out = "chronology"
) {


  # 1️⃣ Prepare data -----------------------------------------------------------
  prf_data <- prf_data %>%
    arrange(image_label, year, sector_n) %>%
    mutate(tree_id = str_c(word(image_label, 1, 3, sep = "_"), collapse = NULL))

  # --- Auto-create quantile columns for parameters with no quantiles ---
  sel_cell_params <- sel_cell_params %>% unique()

  for (param in sel_cell_params) {

    # Detect if parameter already has any _qXX columns
    has_quant <- any(str_detect(names(prf_data), paste0("^", param, "_q\\d+$")))

    if (!has_quant && param %in% names(prf_data)) {

      # Create all requested quantile columns
      for (p in quant_probs) {
        q_label <- paste0("q", sprintf("%02d", round(p*100)))
        new_col <- paste0(param, "_", q_label)
        prf_data[[new_col]] <- prf_data[[param]]
      }

      message("Parameter '", param, "' has no quantiles. Created columns: ",
              paste0(param, "_q", round(quant_probs*100), collapse = ", "))
    }
  }


  # Exclude double rings
  if (!is.null(QWA_rings)) {
    prf_data <- prf_data %>%
      anti_join(
        QWA_rings %>%
          filter(exclude_dupl) %>%
          distinct(image_label, year),
        by = c("image_label", "year")
      )
  }

  # 2️⃣ Define sector list -----------------------------------------------------
  if (is.null(SECTOR)) {
    sector_list <- list(all = prf_data)
  } else {
    sector_list <- prf_data %>%
      filter(sector_n %in% SECTOR) %>%
      split(.$sector_n)
  }

  # 3️⃣ Parameter × quantile grid ---------------------------------------------
  grid <- cross_df(list(
    param = sel_cell_params,
    prob  = quant_probs
  )) %>%
    mutate(
      q_lab = paste0("q", sprintf("%02d", round(prob * 100))),
      col   = paste0(param, "_", q_lab)
    )

  # 4️⃣ Sector-wise computation -----------------------------------------------
  results <- imap(sector_list, function(dat, sec) {

    message("Processing sector: ", sec)

    grid %>%
      split(seq_len(nrow(.))) %>%
      map(function(g) {

        col <- g$col

        if (!col %in% names(dat)) return(NULL)

        # Tree × year mean
        tree_year <- fmean(
          dat[[col]],
          g = interaction(dat$tree_id, dat$year, drop = TRUE),
          na.rm = TRUE
        ) %>%
          data.frame(mean_value = ., tree_year = names(.)) %>%
          separate(tree_year, into = c("tree_id", "year"),
                   sep = "\\.", convert = TRUE)

        # Yearly chronology
        yearly <- fmean(
          tree_year$mean_value,
          g = tree_year$year,
          na.rm = TRUE
        ) %>%
          data.frame(
            year = as.numeric(names(.)),
            mean_value_yearly = .
          )

        # Write CSV
        write_csv(
          yearly,
          paste0(fname_out, "_", col, "_sector", sec, "_yearly.csv")
        )

        # Inter-tree correlation
        wide <- tree_year %>%
          pivot_wider(
            names_from = tree_id,
            values_from = mean_value
          ) %>%
          arrange(year) %>%
          filter(year >= 1900)

        cor_mat <- cor(
          select(wide, -year),
          use = "pairwise.complete.obs"
        )

        mean_cor <- mean(cor_mat[upper.tri(cor_mat)], na.rm = TRUE)

        list(
          tree_year = tree_year,
          yearly = yearly,
          wide = wide,
          cor_mat = cor_mat,
          mean_cor = mean_cor
        )
      }) %>%
      set_names(grid$col)
  })

  # 5️⃣ Name sectors -----------------------------------------------------------
  names(results) <- paste0("sector_", names(results))

  return(results)
}

res <- create_chronologies_by_sector(
  prf_data, # %>% filter(tree_id %in% c("TAZ_CDAT_09E2", "TAZ_CDAT_03NW", "TAZ_CDAT_09E", "TAZ_CDAT_10N", "TAZ_CDAT_10W",  "TAZ_CDAT_11N")),
  QWA_data$rings,
  quant_probs = c(0.75),
  sel_cell_params = c("la", "cwttan", "cwtrad","cwa", "mrw"),
  SECTOR = 1:5,
  fname_out = "QWA"
)


res$sector_3$la_q75
res$sector_5$cwtrad_q90$yearly
res$sector_3$mrw_q75$yearly



######
# import climate KNMI format
# Load temp
df_temp <- read_table2(paste(path_out, "cru_ts4.08.1901.2023.temp.dat", sep = "/"), comment = "#", col_names = FALSE)
df_temp <- df_temp[,1:13]
colnames(df_temp) <- c("year", month.abb)
df_temp <- df_temp %>%
  pivot_longer(cols = Jan:Dec, names_to = "month", values_to = "temp") %>%
  mutate(month = match(month, month.abb))

# Load precip
df_precip <- read_table2(paste(path_out, "cru_ts4.08.1901.2023.precip.dat", sep = "/"), comment = "#", col_names = FALSE)
colnames(df_precip) <- c("year", month.abb)
df_precip <- df_precip[,1:13]
df_precip <- df_precip %>%
  pivot_longer(cols = Jan:Dec, names_to = "month", values_to = "precip") %>%
  mutate(month = match(month, month.abb))

# Load drought
df_drought <- read_table2(paste(path_out, "cru_ts4.08.1901.2023.drought.dat", sep = "/"), comment = "#", col_names = FALSE)
colnames(df_drought) <- c("year", month.abb)
df_drought <- df_drought[,1:13]
df_drought <- df_drought %>%
  pivot_longer(cols = Jan:Dec, names_to = "month", values_to = "drought") %>%
  mutate(month = match(month, month.abb))

# Merge all
clim <- df_precip %>%
  left_join(df_temp, by = c("year", "month")) %>%
  left_join(df_drought, by = c("year", "month"))

head(clim)



make_seasonal_climate <- function(clim,
                                  var,
                                  months,
                                  fun = fmean) {

  clim %>%
    filter(month %in% months) %>%
    group_by(year) %>%
    summarise(
      clim_value = fun(.data[[var]], na.rm = TRUE),
      .groups = "drop"
    )
}


climate_correlation_loop <- function(
    chronos,
    clim,
    clim_vars = c("temp", "precip", "drought"),
    seasons = list(
      DJF = c(12, 1, 2),
      MAM = 3:5,
      JJA = 6:8,
      SON = 9:11
    ),
    year_range = c(1901, 2023)
) {

  # Extend clim by 1 year before start to allow negative months
  clim <- clim %>% filter(year >= (year_range[1]-1), year <= year_range[2])

  # Precompute seasonal averages for all climate variables and all seasons
  clim_season_list <- map(clim_vars, function(clim_var) {

    map(seasons, function(months_sel) {

      # Create month-year offsets for lag handling
      clim_expanded <- clim %>%
        mutate(year_for = year)

      # For negative months, adjust year
      months_info <- tibble(month_sel = months_sel) %>%
        mutate(
          month_adj = if_else(month_sel < 0, 12 + month_sel + 1, month_sel),
          year_offset = if_else(month_sel < 0, -1L, 0L)
        )

      # Join to clim data to get all months at once
      clim_join <- expand_grid(year = year_range[1]:year_range[2], months_info) %>%
        mutate(year_clim = year + year_offset)

      # Join with clim and compute seasonal mean
      clim_avg <- clim_join %>%
        left_join(clim, by = c("year_clim" = "year", "month_adj" = "month")) %>%
        group_by(year) %>%
        summarize(clim_value = mean(.data[[clim_var]], na.rm = TRUE), .groups = "drop")

      return(clim_avg)

    })

  })
  names(clim_season_list) <- clim_vars

  # Loop over sectors/parameters
  imap_dfr(chronos, function(sec_data, sec_name) {

    imap_dfr(sec_data, function(param_data, param_name) {

      if (is.null(param_data$yearly)) return(NULL)

      growth <- param_data$yearly %>%
        rename(growth = mean_value_yearly) %>%
        filter(year >= year_range[1], year <= year_range[2])

      map_dfr(names(seasons), function(sea) {
        map_dfr(clim_vars, function(clim_var) {

          clim_season <- clim_season_list[[clim_var]][[sea]]
          dat <- inner_join(growth, clim_season, by = "year")

          r_val <- if (nrow(dat) == 0 || all(is.na(dat$clim_value))) NA_real_ else {
            cor(dat$growth, dat$clim_value, use = "pairwise.complete.obs")
          }

          tibble(
            sector = sec_name,
            parameter = param_name,
            season = sea,
            clim_var = clim_var,
            n = nrow(dat),
            r = r_val
          )

        })
      })

    })

  })

}




corr <- climate_correlation_loop(
  chronos = res,
  clim = clim,
  clim_var = c("temp", "precip", "drought"),
  seasons = list(
    pJJA = -6:-8,
    pMJJAS = -5:-9,
#    pMJJA = -5:-8,
#    pJJAS = -6:-9,
#    pJJASO = -6:-10,
    # Jan = 1,
    # Feb = 2,
    # Mar = 3,
    # Apr = 4,
    # May = 5,
    # Jun = 6,
    # Jul = 7,
    # Aug = 8,
    # Sep = 9,
    # Oct = 10,
    # Nov = 11,
    # Dec = 12,
    DJF = c(12, 1, 2),
    AMJ = 4:6,
    JJA = 6:8,
    MJJAS = 5:9,
    MJJASO = 5:10,
    ASO = 8:10
  ),
  year_range = c(1901, 2023)
)

corr[which(corr$r == max(corr$r)),]
corr[which(corr$r == min(corr$r)),]

# moving_corr <- function(growth, clim, win = 30) {
#
#   yrs <- seq(min(growth$year), max(growth$year) - win)
#
#   map_dfr(yrs, function(y0) {
#
#     g <- growth %>% filter(year >= y0, year < y0 + win)
#     c <- clim   %>% filter(year >= y0, year < y0 + win)
#
#     tibble(
#       start_year = y0,
#       r = cor(g$growth, c$clim_value,
#               use = "pairwise.complete.obs")
#     )
#   })
# }
# mcorr <- moving_corr(
#   growth = res$sector_5$la_q75$yearly %>%
#     rename(growth = mean_value_yearly),
#   clim = make_seasonal_climate(
#     clim = clim,
#     var = "temp",
#     months = c(6, 7, 8)
#   ),
#   win = 30
# )


######## plotting
ggplot(corr, aes(x = season, y = clim_var, fill = r)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1,1)) +
  facet_grid(parameter ~ sector) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Seasonal Climate Correlations",
    x = "Season",
    y = "Climate Variable",
    fill = "Correlation"
  )


###### Plot correlation
library(ncdf4)
library(terra)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)


nc_file <- paste(path_out, "cru_ts4.08.1901.2023.tmp.dat_0-140E_20-80N_1901-2023.ncc", sep = "/")

# Read as SpatRaster
tmp <- rast(nc_file)

tmp
years <- rep(1901:2023, each = 12)
months <- rep(1:12, times = 123)

tmp_df_long <- as.data.frame(tmp, xy = TRUE) %>%   # do NOT use time=TRUE
  pivot_longer(
    cols = starts_with("tmp_"),   # all layers
    names_to = "layer",
    values_to = "temp"
  ) %>%
  mutate(
    # assign correct year/month based on layer index
    layer_index = as.integer(gsub("tmp_", "", layer)),
    year = years[layer_index],
    month = months[layer_index]
  ) %>%
  select(lon = x, lat = y, year, month, temp)


tmp_pMJJAS <- tmp_df_long %>%
  filter(month %in% 5:9) %>%
  mutate(year = year + 1) %>%        # previous-year effect
  group_by(lon, lat, year) %>%
  summarize(
    tmp_pMJJAS = mean(temp, na.rm = TRUE),
    .groups = "drop"
  )

growth <- res$sector_5$cwttan_q75$yearly %>%
  rename(growth = mean_value_yearly) %>%
  filter(year >= 1901, year <= 2023)

corr_map <- tmp_pMJJAS %>%
  inner_join(growth, by = "year") %>%
  group_by(lon, lat) %>%
  summarize(
    n = sum(complete.cases(growth, tmp_pMJJAS)),
    r = ifelse(
      n >= 20,
      cor(growth, tmp_pMJJAS, use = "pairwise.complete.obs"),
      NA_real_
    ),
    .groups = "drop"
  )


ggplot() +
  # correlation raster
  geom_raster(
    data = corr_map,
    aes(lon, lat, fill = r)
  ) +

  # coastlines / country borders
  geom_sf(
    data = world,
    fill = NA,
    color = "grey",
    linewidth = 0.3
  ) +

  coord_sf(
    xlim = range(corr_map$lon, na.rm = TRUE),
    ylim = range(corr_map$lat, na.rm = TRUE),
    expand = FALSE
  ) +

  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-.6, .6),
    na.value = "grey80",
    name = "r"
  ) +

  labs(
    title = "Sector 5 – cwttan_q75 vs pMJJAS temperature",
    subtitle = "CRU TS 4.08 (1901–2011)",
    x = "Longitude",
    y = "Latitude"
  ) +

  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank()
  )



# ---------------------------
#  Function: correlation_map
# ---------------------------

library(dplyr)
library(tidyr)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(sf)

clim_correlation_map <- function(
    chronos,          # output of create_quantile_chronologies_by_sector
    clim_df_long,     # long-format climate: lon, lat, year, month, temp/precip/drought
    clim_vars = c("temp"),
    sectors = NULL,
    params = NULL,
    seasons = list(pJJA=-6:-8, JJA=6:8, DJF=c(12,1,2)),
    year_range = c(1901, 2023),
    plot_map = TRUE
) {

  results <- list()

  # World boundaries for plotting
  world <- ne_countries(scale="medium", returnclass="sf")

  sector_names <- if(is.null(sectors)) names(chronos) else sectors

  for(sec in sector_names) {
    sec_data <- chronos[[sec]]
    param_names <- if(is.null(params)) names(sec_data) else params

    for(param in param_names) {
      # Check yearly growth exists
      if(is.null(sec_data[[param]]$yearly)) next

      growth_df <- sec_data[[param]]$yearly %>%
        filter(year >= year_range[1], year <= year_range[2]) %>%
        arrange(year)

      growth_years <- growth_df$year
      growth_vals  <- growth_df$mean_value_yearly

      # Loop climate variables
      for(clim_var in clim_vars) {
        for(season_name in names(seasons)) {
          months <- seasons[[season_name]]

          # Determine lag: negative months refer to previous year
          lag <- if(any(months < 0)) -1 else 0

          # Prepare seasonal climate
          season_df <- clim_df_long %>%
            filter(year >= year_range[1] + lag, year <= year_range[2] + lag) %>%
            filter(month %in% ifelse(months>0, months, 12 + months)) %>%
            group_by(lon, lat, year) %>%
            summarize(clim_value = mean(.data[[clim_var]], na.rm=TRUE), .groups="drop") %>%
            mutate(year = year - ifelse(any(months<0), 1, 0)) %>% # adjust year for lag
            filter(year %in% growth_years)  # align years exactly

          if(nrow(season_df) == 0) {
            warning("No overlapping climate data for ", sec, "/", param, "/", clim_var, "/", season_name)
            next
          }

          # Create raster for correlation
          lon_vals <- sort(unique(season_df$lon))
          lat_vals <- sort(unique(season_df$lat))

          r_out <- rast(nrows=length(lat_vals), ncols=length(lon_vals),
                        xmin=min(lon_vals), xmax=max(lon_vals),
                        ymin=min(lat_vals), ymax=max(lat_vals))

          # Correlation per grid cell
          cor_mat <- matrix(NA, nrow=length(lat_vals), ncol=length(lon_vals))

          for(i in seq_along(lat_vals)) {
            for(j in seq_along(lon_vals)) {
              tmp_vals <- season_df %>%
                filter(lon==lon_vals[j], lat==lat_vals[i]) %>%
                arrange(year) %>%
                pull(clim_value)

              if(length(tmp_vals) == length(growth_vals)) {
                cor_mat[i,j] <- cor(growth_vals, tmp_vals, use="pairwise.complete.obs")
              } else {
                cor_mat[i,j] <- NA
              }
            }
          }

          # Fill raster
          values(r_out) <- as.vector(t(cor_mat[nrow(cor_mat):1,])) # flip lat for raster
          names(r_out) <- paste(sec, param, clim_var, season_name, sep="_")

          results[[names(r_out)]] <- r_out

          # Optional plotting
          if(plot_map) {
            df_plot <- as.data.frame(r_out, xy=TRUE)
            names(df_plot)[3] <- "r"

            p <- ggplot() +
              geom_raster(data=df_plot, aes(x=x, y=y, fill=r)) +
              geom_sf(data=world, fill=NA, color="black", linewidth=0.3) +
              coord_sf(xlim=range(df_plot$x), ylim=range(df_plot$y), expand=FALSE) +
              scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0,
                                   limits=c(-1,1), na.value="grey80", name="r") +
              labs(title=paste(sec, param, clim_var, season_name),
                   x="Longitude", y="Latitude") +
              theme_minimal() +
              theme(panel.grid.major=element_line(color="grey90", linewidth=0.2),
                    panel.grid.minor=element_blank())

            print(p)
          }
        }
      }
    }
  }

  return(results)
}



# Example usage
# Named list of climate rasters
clim_vars <- c("temp")  # extend to "precip", "drought" if available

results <- clim_correlation_map(
  chronos = res,
  clim_df_long = tmp_df_long,
  clim_vars = clim_vars,
  sectors = c("sector_5"),
  params = c("cwttan_q75", "mrw_q75"),
  seasons = list(pMJJAS = -5:-9),
  year_range = c(1901, 2023),
  plot_map = TRUE
)
