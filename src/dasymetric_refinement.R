#!/usr/bin/env Rscript

################################################################################
# MODULE: Dasymetric refinement of LAU (Local Area Unit) human population for a 
# chosen year estimated finer to 1 km2 raster cells based on urban Corine classes
# and weights as given in inputted weight table as well as building footprint
# for cells with building count passing an optimised threshold. If simple is 
# chosen as refinement type, a binary distribution of human population to 
# urbanised Corine classes 111 and 112 is carried out.
#
# When pop_year == "2021" and refinement_type != "simple", a building-count
# threshold search is run internally: candidate thresholds are scored against
# the census grid's observed population (same classification used by
# add_evaluations_to_censusgrid()), and the best threshold found is returned
# so it can be reused for other years via best_building_threshold.
################################################################################

# --- 1. DEPENDENCIES ---
library(terra)
library(dplyr)
library(sf)

# --- 2. GLOBAL SETTINGS ---
options(scipen = 100, digits = 4)

# --- 3. FUNCTION DEFINITION (Original Code) ---
dasymetric_refinement_raster <- function(cor_rast_geom,
                                         cor_code_raster_columnname,
                                         lau_in_catchment,
                                         source_id,
                                         source_value_col,
                                         pop_year,
                                         catchment,
                                         weight_table_final,
                                         refinement_type,
                                         buildings_vect,
                                         census_grid_geom_cropped,
                                         census_grid_value_col,
                                         best_building_threshold = NA
) {
  
  # Ensure the spatial extent is a SpatVector
  lau_vect <- terra::vect(lau_in_catchment)
  
  # make numeric LAU ID column
  lau_ids <- terra::values(lau_vect)[[source_id]]
  lau_vect$LAU_ID_num <- as.integer(lau_ids)
  
  # rasterize LAU IDs
  lau_raster <- terra::rasterize(
    lau_vect,
    cor_rast_geom,
    field = "LAU_ID_num",
    background = NA
  )
  
  # Mask LAU to valid corine classes
  lau_raster <- terra::mask(lau_raster, cor_rast_geom)
  
  # Filter buildings to only those constructed in or before pop_year and buildings without construction year
  buildings_vect_filtered <- buildings_vect[
    is.na(buildings_vect$construction_year) |
      buildings_vect$construction_year <= pop_year,
  ]
  
  buildings_spatvect <- terra::vect(buildings_vect_filtered)
  buildings_centroids  <- terra::centroids(buildings_spatvect)
  
  # Count number of buildings per cell
  building_count <- terra::rasterize(
    buildings_centroids,
    cor_rast_geom,
    field = 1,
    fun = "length"     # counts how many points fall in each cell
  )
  
  # Treat "no buildings" (NA from rasterize) as a true 0, not a missing value —
  # otherwise NA > bt stays NA all the way through and these cells silently
  # drop out of the error checks below instead of counting as "fails threshold"
  building_count <- terra::ifel(is.na(building_count), 0, building_count)
  
  # Keep only very urban CLC
  cor_urban_only <- cor_rast_geom
  cor_urban_only[!cor_urban_only %in% c(111, 112)] <- NA
  
  # join lau with normalised weights (independent of building_mask, computed once)
  lau_with_pop <- terra::as.data.frame(lau_vect)[, c("LAU_ID_num", source_value_col)]
  
  # Runs the full weighted apportionment for a given building_mask and returns the
  # resulting population raster (cropped to the catchment) plus the combined
  # CLC+buildings raster. Shared by both the threshold search and the final run so
  # the estimation logic only lives in one place.
  run_weighted_estimation <- function(building_mask, write_cell_counts = FALSE) {
    
    # Convert the boolean mask into actual CLC values where buildings overlap
    building_values <- terra::mask(cor_rast_geom, building_mask, maskvalues = c(NA, FALSE))
    
    # Combine the two rasters
    cor_artificial_plus_buildings <- terra::cover(cor_urban_only, building_values)
    
    # encode both IDs in one raster
    combo_raster <- lau_raster * 1000 + cor_artificial_plus_buildings
    
    # frequencies
    freq_table <- terra::freq(combo_raster)
    
    # Count cells per LAU-CORINE class
    cell_counts <- freq_table |>
      dplyr::as_tibble() |>
      dplyr::mutate(
        LAU_ID = value %/% 1000,
        corine = value %% 1000,
        n_cells = count
      ) |>
      dplyr::select(LAU_ID, corine, n_cells)
    
    # for later statistics visualisation
    lau_cell_counts <- cell_counts
    
    # Join CORINE weights
    cell_counts <- cell_counts |>
      dplyr::left_join(
        weight_table_final,
        by = c("corine" = cor_code_raster_columnname)
      )
    
    # compute weighted area
    cell_counts <- cell_counts |>
      dplyr::mutate(
        weight = (percent/100) * n_cells
      )
    
    # normalize weights per LAU
    cell_counts <- cell_counts |>
      dplyr::group_by(LAU_ID) |>
      dplyr::mutate(
        weight_norm = weight / sum(weight, na.rm = TRUE)
      ) |>
      dplyr::ungroup()
    
    cell_counts <- cell_counts |>
      dplyr::left_join(
        lau_with_pop,
        by = c("LAU_ID" = "LAU_ID_num")
      )
    
    # Estimate population per CORINE class
    cell_counts <- cell_counts |>
      dplyr::mutate(
        pop_corine = .data[[source_value_col]] * weight_norm
      )
    
    # Build lookup table: population per cell and combo value of combined LAU and corine IDs
    cell_counts <- cell_counts %>%
      mutate(
        pop_per_cell = pop_corine / n_cells,
        combo_val = LAU_ID * 1000 + corine
      ) %>%
      select(combo_val, pop_per_cell)
    
    # Map population to raster
    raster_vals <- terra::values(combo_raster)
    pop_vals <- cell_counts$pop_per_cell[match(raster_vals, cell_counts$combo_val)]
    
    # make a pop raster in the first step based on combo IDs
    pop_raster <- combo_raster
    # and in the second step based on estimated population in raster cells
    terra::values(pop_raster) <- pop_vals
    
    # Replace NAs and zeros with NA
    pop_raster <- terra::ifel(is.na(pop_raster) | pop_raster == 0, NA, pop_raster)
    names(pop_raster) <- "pop_est"
    
    # crop raster to extent
    refinement_cropped <- pop_raster %>%
      terra::crop(catchment) %>%
      terra::mask(catchment)
    
    refinement_cropped_1dec <- terra::round(refinement_cropped, digits = 1)
    refinement_cropped_1dec[refinement_cropped_1dec == 0] <- NA
    
    list(pop_raster = pop_raster,
         refinement = refinement_cropped_1dec,
         cor_artificial_plus_buildings = cor_artificial_plus_buildings,
         lau_cell_counts = lau_cell_counts)
  }
  
  # Scores a candidate building_mask the same way add_evaluations_to_censusgrid() scores
  # the final result: count census-grid cells that are grossly wrong — population guessed
  # where none was observed (number_of_wrong_cells_included, dif_perc == 999, i.e. observed
  # value of 0) plus population observed but fully missed by the estimate
  # (number_of_correct_cells_excluded, dif_perc == 100, i.e. estimate of 0). A plain
  # population-error-sum proxy is not the same objective: it keeps favouring ever-lower
  # thresholds (more building-covered cells shrinks the "missed" side of the error) instead
  # of settling on the threshold that best matches which cells were observed to have
  # population at all — the same classification add_evaluations_to_censusgrid() reports.
  score_building_mask <- function(building_mask) {
    run <- run_weighted_estimation(building_mask, write_cell_counts = FALSE)
    
    census_grid_eval <- census_grid_geom_cropped
    pop_census <- terra::extract(run$refinement, census_grid_eval, fun = sum, na.rm = TRUE)
    census_grid_eval$pop_est_cell <- pop_census[, 2]
    
    # same row-dropping steps as add_evaluations_to_censusgrid(), so the classification
    # below is computed over the identical set of cells
    census_grid_eval <- census_grid_eval[
      !(is.na(census_grid_eval$pop_est_cell) & is.na(census_grid_eval[[census_grid_value_col]])),
    ]
    census_grid_eval <- census_grid_eval[
      !((census_grid_eval$pop_est_cell == 0) & is.na(census_grid_eval[[census_grid_value_col]])),
    ]
    census_grid_eval <- census_grid_eval[
      !((census_grid_eval$pop_est_cell == 0) & (census_grid_eval[[census_grid_value_col]] == 0)),
    ]
    census_grid_eval <- census_grid_eval[
      !(is.na(census_grid_eval$pop_est_cell) & (census_grid_eval[[census_grid_value_col]] == 0)),
    ]
    
    census_grid_eval$pop_est_cell[is.na(census_grid_eval$pop_est_cell)] <- 0
    census_grid_eval[[census_grid_value_col]][is.na(census_grid_eval[[census_grid_value_col]])] <- 0
    
    dif <- census_grid_eval$pop_est_cell - census_grid_eval[[census_grid_value_col]]
    dif_perc <- abs((dif / census_grid_eval[[census_grid_value_col]]) * 100)
    dif_perc[is.na(dif_perc)] <- 0
    dif_perc[is.infinite(dif_perc)] <- 999
    
    number_of_wrong_cells_included <- sum(dif_perc == 999.0)
    number_of_correct_cells_excluded <- sum(dif_perc == 100.0)
    
    number_of_wrong_cells_included + number_of_correct_cells_excluded
  }
  
  if (refinement_type != "simple" && pop_year == "2021") {
    
    building_threshold_candidates <- c(1, 2, 3, 5, 7, 10, 15, 20, 30)
    
    best_error_sum <- Inf
    best_building_mask <- NULL
    prev_error_sum <- NA  # tracks previous iteration's sum, to detect an increase
    
    results <- data.frame(
      building_threshold   = building_threshold_candidates,
      n_misclassified_cells = NA_integer_
    )
    
    for (i in seq_along(building_threshold_candidates)) {
      
      bt <- building_threshold_candidates[i]
      
      # Keep only cells with MORE than bt buildings
      building_mask <- building_count > bt   # returns TRUE/1 where count > bt, NA/FALSE elsewhere
      
      error_sum <- score_building_mask(building_mask)
      results$n_misclassified_cells[i] <- error_sum
      
      if (error_sum < best_error_sum) {
        best_error_sum  <- error_sum
        best_building_threshold <- bt
        best_building_mask <- building_mask
      }
      
      if (!is.na(prev_error_sum) && error_sum > prev_error_sum) {
        message("Misclassified cell count increased at bt = ", bt, " — stopping search.")
        break
      }
      
      prev_error_sum <- error_sum
      
    }
    
    print(results[!is.na(results$n_misclassified_cells), ])
    cat("\nBest threshold:", best_building_threshold, "\n")
    cat("Best misclassified cell count:", best_error_sum, "\n")
    
    # unify variable name used downstream regardless of which branch ran
    building_mask <- best_building_mask
    
  } else {
    
    # Keep only cells with MORE than best_building_threshold buildings
    building_mask <- building_count > best_building_threshold
    
    message("best building threshold is used")
  }
  
  final_run <- run_weighted_estimation(building_mask, write_cell_counts = TRUE)
  cor_artificial_plus_buildings <- final_run$cor_artificial_plus_buildings
  refinement_cropped_1dec <- final_run$refinement
  
  print("count totals:")
  print(terra::global(cor_artificial_plus_buildings, fun = "notNA"))
  
  message("Non-111/112 cells added by building filter: ",
          sum(terra::values(cor_artificial_plus_buildings) %in%
                setdiff(unique(terra::values(cor_rast_geom)), c(111,112)), na.rm = TRUE))
  
  # print sanity check
  total_pop <- terra::global(final_run$pop_raster, fun = "sum", na.rm = TRUE)[1,1]
  print(paste0("Number of estimated population: ", total_pop))
  print(paste0("Number of source population: ", sum(lau_vect[[source_value_col]])))
  
  return(list(refinement_cropped_1dec = refinement_cropped_1dec,
              lau_cell_counts = final_run$lau_cell_counts,
              cor_artificial_plus_buildings = cor_artificial_plus_buildings,
              best_building_threshold = best_building_threshold))
  
}

################################################################################
# D2K WRAPPER
################################################################################

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 15) {
  stop("Usage: Rscript src/dasymetric_refinement.R <refinement_type> <corineCLC_rds_path> <corine_year_rds_path> <lau_in_catchment_rds_path> <pop_focus_year_rds_path> <catchment_gpkg_path> <weight_table_rds_path> <buildings_rds_path> <census_grid_rds_path> <best_threshold_if_existing_rds_path> <output_refinement_rds_path> <output_refinement_tif_path> <output_cell_statistics_rds_path> <output_corine_final_rds_path> <output_best_threshold_rds_path>", call. = FALSE)
}

refinement_type <- args[1]
if (!(refinement_type %in% c("simple", "weighted"))) {
  stop(
    paste0(
      "Invalid refinement type: ", 
      refinement_type,
      ". Allowed types are: 'simple' and 'weighted'"
    ),
    call. = FALSE
  )
}

corineCLC_rds_path <- args[2]
corine_year_rds_path <- args[3]
corine_year <- readRDS(corine_year_rds_path)
corine_year <- as.character(corine_year)
if (!(corine_year == "2018")) {
  stop(
    paste0(
      "Invalid corine year: ", corine_year,
      ". Allowed year is only year 2018", 
      collapse = ", "
    ),
    call. = FALSE
  )
}

lau_in_catchment_rds_path <- args[4]
pop_focus_year_rds_path <- args[5]
pop_focus_year <- readRDS(pop_focus_year_rds_path)
pop_focus_year <- as.character(pop_focus_year)

catchment_gpkg_path <- args[6]
weight_table_rds_path <- args[7]
buildings_rds_path <- args[8]
census_grid_rds_path <- args[9]
best_threshold_if_existing_rds_path <- args[10]
if (!file.exists(best_threshold_if_existing_rds_path)) {
  saveRDS(NA, file = best_threshold_if_existing_rds_path)
}  
best_threshold_if_existing <- readRDS(best_threshold_if_existing_rds_path)
best_threshold_if_existing <- as.numeric(best_threshold_if_existing) # if (refinement_type != "simple" && pop_year == "2021") {best_threshold_if_existing <- NA}

output_refinement_rds_path <- args[11]
output_refinement_tif_path <- args[12]
output_cell_statistics_rds_path <- args[13]
output_corine_final_rds_path <- args[14]
output_best_threshold_rds_path <- args[15]

message("D2K Wrapper Started for dasymetric refinement.")

tryCatch({
  
  # Read spatial focus object
  corCLC <- readRDS(corineCLC_rds_path)
  
  cor_code_raster_columnname <- paste0("CODE_", 
                                       substr(corine_year, 
                                              3, 4)) # e.g. "CODE_18"
  
  weight_table_final <- readRDS(weight_table_rds_path)
  
  # if simple refinement all weights should be equal 
  # and only class 111 and 112 are accepted
  if (refinement_type == "simple") {
    weight_table_final$percent <- 1.
    weight_table_final <- weight_table_final[
      weight_table_final$CODE_18 %in% c(111, 112),
    ]
  }
  
  lau_in_catchment <- readRDS(lau_in_catchment_rds_path)
  
  lau_value_col_focus <- paste0("POP_", 
                                pop_focus_year) #"values"
  
  # Read spatial focus object
  catchment_gpkg <- sf::st_read(catchment_gpkg_path,
                                quiet = TRUE)
  
  buildings_vect <- readRDS(buildings_rds_path)
  
  census_grid_geom_cropped <- readRDS(census_grid_rds_path)
  
  # Ground-truth population column always refers to the 2021 census reference
  # year, regardless of which year is being estimated (pop_focus_year) — this
  # is what the internal threshold search calibrates against.
  census_grid_value_col <- "TOT_P_2021"
  
  outputs_dasymetric_refinement <- dasymetric_refinement_raster(cor_rast_geom = corCLC,
                                                                cor_code_raster_columnname = cor_code_raster_columnname,
                                                                lau_in_catchment = lau_in_catchment,
                                                                source_id = "LAU_ID",
                                                                source_value_col = lau_value_col_focus,
                                                                pop_year = pop_focus_year,
                                                                catchment = catchment_gpkg,
                                                                weight_table_final = weight_table_final,
                                                                refinement_type = refinement_type,
                                                                buildings_vect = buildings_vect,
                                                                census_grid_geom_cropped = census_grid_geom_cropped,
                                                                census_grid_value_col = census_grid_value_col,
                                                                best_building_threshold = best_threshold_if_existing)
  refinement_cropped_1dec <- outputs_dasymetric_refinement$refinement_cropped_1dec
  lau_cell_counts <- outputs_dasymetric_refinement$lau_cell_counts
  cor_artificial_plus_buildings <- outputs_dasymetric_refinement$cor_artificial_plus_buildings
  best_building_threshold <- outputs_dasymetric_refinement$best_building_threshold
  
  # Save as .rds for machine/subsequent steps
  saveRDS(refinement_cropped_1dec, 
          file = output_refinement_rds_path)

  # Save as tif file
  terra::writeRaster(
    refinement_cropped_1dec,
    output_refinement_tif_path,
    overwrite = TRUE
  )
  
  # Save as .rds for machine/subsequent steps
  saveRDS(lau_cell_counts, 
          file = output_cell_statistics_rds_path)
  
  # Save as .rds for machine/subsequent steps
  saveRDS(cor_artificial_plus_buildings, 
          file = output_corine_final_rds_path)
  
  # Save as .rds for machine/subsequent steps
  saveRDS(best_building_threshold, 
          file = output_best_threshold_rds_path)
  
  message(paste("D2K Wrapper Finished. Dasymetric refinement raster saved to", 
                output_refinement_tif_path))
  
}, error = function(e) {
  stop(paste("Error during script execution:", e$message))
})
