#!/usr/bin/env Rscript

################################################################################
# MODULE: Get LAU human population data from Eurostat for inputted focus year. 
# Fetches LAU (Local Areal Unit) data from Eurostat for focus year.
################################################################################

# --- 1. DEPENDENCIES ---
library(giscoR)

# --- 2. GLOBAL SETTINGS ---
options(scipen = 100, digits = 4)

# --- 3. FUNCTION DEFINITION (Original Code) ---
get_lau_population <- function(pop_focus_year, 
                               countrylist,
                               target_crs = "3035") {
  
  # Get data #LAU_ID #POP_2021
  lau_pop <- giscoR::gisco_get_lau(year = pop_focus_year,
                                   epsg = target_crs,
                                   country = countrylist) 
  
  return(lau_pop)
}  

################################################################################
# D2K WRAPPER
################################################################################

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 4) {
  stop("Usage: Rscript src/get_lau_data.R <countries_for_catchment> <focus_year> <output_lau_focus_rds_path> <output_focusyear_rds_path>", call. = FALSE)
}

countries_rds_path <- args[1]
focus_year <- args[2]
# validation (clean version)
focus_year <- as.character(focus_year)
valid_years <- c(
  "2024", "2023", "2022", "2021",
  "2020", "2019", "2018", "2017",
  "2016", "2015", "2014", "2013",
  "2012", "2011"
)
if (!(focus_year %in% valid_years)) {
  stop(
    paste0(
      "Invalid focus year: ", focus_year,
      ". Allowed years are: ",
      paste(valid_years, collapse = ", ")
    ),
    call. = FALSE
  )
}

output_lau_focus_rds_path <- args[3]
output_focusyear_rds_path <- args[4]

message(paste("D2K Wrapper Started. Output will be saved to:", 
              output_lau_focus_rds_path))

tryCatch({
  
  countries_for_catchment <- readRDS(countries_rds_path)
  
  LAU_focus <- get_lau_population(
    pop_focus_year = focus_year,
    countrylist = countries_for_catchment
  )
  
  # Save as .rds for machine/subsequent steps
  saveRDS(LAU_focus, 
          file = output_lau_focus_rds_path)

  # Save as .rds for machine/subsequent steps
  saveRDS(focus_year, 
          file = output_focusyear_rds_path)

  message(paste("D2K Wrapper Finished. LAU population data saved to", 
                output_lau_focus_rds_path))
  
}, error = function(e) {
  stop(paste("Error during script execution:", e$message))
})
