#!/usr/bin/env Rscript

################################################################################
# MODULE: Dasymetric refinement of LAU (Local Area Unit) human population for a 
# chosen year estimated finer to 1 km2 raster cells based on urban Corine classes
# and weights as given in inputted weight table. If simple is chosen as refinement 
# type, a binary distribution of human population to urbanised Corine classes 
# 111 and 112 is carried out. 
#
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
                                         weight_table_final) 
  {
  
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
  
  # encode both IDs in one raster
  combo_raster <- lau_raster * 1000 + cor_rast_geom
  
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
  
  # join lau with normalised weights   
  lau_with_pop <- terra::as.data.frame(lau_vect)[, c("LAU_ID_num", source_value_col)]
  cell_counts <- cell_counts |>
    left_join(
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
  
  # print sanity check 
  total_pop <- terra::global(pop_raster, fun = "sum", na.rm = TRUE)[1,1]
  message(paste0("Number of estimated population: ", total_pop))
  message(paste0("Number of source population: ", sum(lau_vect[[source_value_col]])))
  
  # replace name of raster values to be pop_est 
  names(pop_raster) <- "pop_est"
  
  # crop raster to extent
  refinement_cropped <- pop_raster %>%
    terra::crop(catchment) %>%
    terra::mask(catchment)
  
  refinement_cropped_1dec <- terra::round(refinement_cropped, digits = 1)
  
  refinement_cropped_1dec[refinement_cropped_1dec == 0] <- NA
  
  return(list(refinement_cropped_1dec = refinement_cropped_1dec, 
              lau_cell_counts = lau_cell_counts))
}

################################################################################
# D2K WRAPPER
################################################################################

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 10) {
  stop("Usage: Rscript src/dasymetric_refinement.R <refinement_type> <corineCLC_rds_path> <corine_year_rds_path> <lau_in_catchment_rds_path> <pop_focus_year_rds_path> <catchment_gpkg_path> <weight_table_rds_path> <output_refinement_rds_path> <output_refinement_tif_path> <output_cell_statistics_rds_path>", call. = FALSE) 
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
output_refinement_rds_path <- args[8]
output_refinement_tif_path <- args[9]
output_cell_statistics_rds_path <- args[10]

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
  
  outputs_dasymetric_refinement <- dasymetric_refinement_raster(cor_rast_geom = corCLC,
                                                               cor_code_raster_columnname = cor_code_raster_columnname,
                                                               lau_in_catchment = lau_in_catchment,
                                                               source_id = "LAU_ID",
                                                               source_value_col = lau_value_col_focus, 
                                                               pop_year = pop_focus_year,
                                                               catchment = catchment_gpkg,
                                                               weight_table_final = weight_table_final)
  output_dasymetric_refinement_cropped_1dec <- outputs_dasymetric_refinement$refinement_cropped_1dec
  lau_cell_counts <- outputs_dasymetric_refinement$lau_cell_counts
  
  # Save as .rds for machine/subsequent steps
  saveRDS(output_dasymetric_refinement_cropped_1dec, 
          file = output_refinement_rds_path)
  
  # Save as tif file
  terra::writeRaster(
    output_dasymetric_refinement_cropped_1dec,
    output_refinement_tif_path,
    overwrite = TRUE
  )

  # Save as .rds for machine/subsequent steps
  saveRDS(lau_cell_counts, 
          file = output_cell_statistics_rds_path)
  
  message(paste("D2K Wrapper Finished. Dasymetric refinement raster saved to", 
                output_refinement_tif_path))
  
}, error = function(e) {
  stop(paste("Error during script execution:", e$message))
})
