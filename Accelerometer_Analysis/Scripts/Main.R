# Main Script for Sensitivity Analysis Data Exploration --------------------------
# Written by Oakleigh Wilson, Nov 2024
# Abridged by Ryley Delaney, Nov 2024

# Main Script for Sensitivity Analysis ------------------------------------

# Install packages and Source Functions -------------------------------------------------------
library(pacman)
p_load(data.table, tidyverse, kableExtra)

# Hardcoded variables -----------------------------------------------------

# Input the dataset name from the list of dictionaries in Species Settings
dataset_name <- "Studd_Squirrel"
# Initialise the folder structure later for when creating directories.
folder_structure <- c("Data/Data", "Data/Test_Data", "Data/Training_Data", "Outputs")

# Set the base path to our directory we are working from.
# I've chosen to do this via the working directory but you can set it manually as well.
# base_path <- "C:/Users/user/Desktop/Oakleigh_Project/AccelerometerData/Accelerometer_Analysis"
base_path <- getwd()

# Source all functions we are using, I've decided to source them all at once instead of as used because atm every function within the script is used at some point. 
source(file.path("Scripts/Functions.R")) # Source all of the functions we are using

  # Here we created a dictionary that's a list of variables for each species: sample rate, overlap and windows percentage.
  species_settings <- list(Studd_Squirrel = list(sample_rate = 1, overlap_percent = 20, window_length = 10),
                           Seal = list(sample_rate = 100, overlap_percent = 20, window_length = 10))
  
  # Here we are going to set the sample rate based on the name from the dictionary and pull the values of that.
  sample_rate <- species_settings[[dataset_name]]$sample_rate
  # Doing the same for overlap variable within our dictionary.
  overlap_percent <- species_settings[[dataset_name]]$overlap_percent
  # Same for window length
  window_length <- species_settings[[dataset_name]]$window_length
  # Assign feature settings to a global variable.
  feature_settings <- c(sample_rate, overlap_percent, window_length)
  # Assign available axes
  available_axes <- c("Accelerometer.X", "Accelerometer.Y", "Accelerometer.Z")


# Create Directories ----------------------------------------------------------
  # Check the current folder structure and create any folders that are missing.
  createDirectories(base_path, folder_structure)
  
# Split test data out and load other data ---------------------------------
  splitTestData(paste0(dataset_name, "_Modified.csv"))

# Create Box-Plot Graph --------------------------------------------------------
  # Check if the file already exists
  if (file.exists(paste0(base_path,"/Outputs/", dataset_name, "_Behaviour_Duration_Report.html"))) {
    # Print that the file already exists
    cat(paste0("Behaviour duration report already exists for ", dataset_name,".csv", " in:\n\n", base_path, "/Outputs/"))
    cat("\n\n")
    # Ask if they want to overwrite the file
    overwrite <- menu(choices = c("Yes", "No"), title = "Would you like to overwrite it?")
      # If yes then generate a new report
      if (overwrite == 1)
      {
        tryCatch(
          {
            inc_outlier_choice = !(as.logical((menu(c("Include","Exclude"), title = "Include outliers in behaviour duration plot?"))-1))
            generateBD_Report(base_path, dataset_name, sample_rate)
          }, error = function(e) {
            message("Error in making the data exploration report: ", e$message)
            stop()
          })
      }
      # If not then quit the program.
      else {
        cat("Quitting Program...")
      }
    } else {
    # If no report exists, generate one.
      tryCatch(
        {
          inc_outlier_choice = as.logical((menu(c("Include","Exclude"), title = "Include outliers in behaviour duration plot?"))-1)
          generateBD_Report(base_path, dataset_name, sample_rate)
        }, error = function(e) {
          message("Error in making the data exploration report: ", e$message)
          stop()
        })
    }

# Open Behaviour Report in browser.
  browseURL(paste0("Outputs/", dataset_name, "_Behaviour_Duration_Report.html"))




