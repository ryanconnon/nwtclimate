##############################################################################################

# Dependency data
# locations
# station_ids

##############################################################################################

# Arguments for testing:

# filepath = "C:/Users/Ryan_Connon/Documents/NT_Hydrology/Climate/ECCC_station_data/"
# input_file = "updated_data/ECCC_Climate_Data_Updated.rds"
# output_file = "filtered_data/ECCC_Climate_Data_Filtered.rds"

##############################################################################################

# Function to filter data

filter <- function(
    filepath,
    input_file,
    output_file
  )
  
{
  
  # Read in current data
  
  data <- readRDS(paste0(filepath, input_file))
  
  # Create list of data to filter
  
  # Remove bad quality snow data at 51058 (Yellowknife A)
  # These are erroneously low data from Lambrecht heated tipping bucket
  
  snow_start_51058 <- "2019-09-01"
  snow_stop_51058 <- "2022-04-30"
  
  df <- data %>%
    dplyr::mutate(total_precip = ifelse(station_id == 51058 & 
                                          date >= snow_start_51058 & 
                                          date <= snow_stop_51058 &
                                          mean_temp < 0, 
                                        NA, 
                                        total_precip))

  
  saveRDS(df, file = paste0(filepath, output_file))
  print(paste0("Congratulations! Climate datafile has been succesfully filtered to ", max(df$date)))
  
}

