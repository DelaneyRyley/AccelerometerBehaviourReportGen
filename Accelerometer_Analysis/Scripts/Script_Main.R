# Main Script for Sensitivity Analysis Data Exploration --------------------------
# Written by Oakleigh Wilson, Nov 2024
# Abridged by Ryley Delaney, Nov 2024

# Main Script for Sensitivity Analysis ------------------------------------

# Install packages and Source Functions -------------------------------------------------------
library(data.table)
library(tidyverse)
library(tsfeatures)
library(umap)
library(caret)
library(ggpubr) # for retrieving the legend in one of my plots

# Hardcoded variables -----------------------------------------------------

  # Input the dataset name from the list of dictionaries in Species Settings
  dataset_name <- "Studd_Squirrel"
  
  # Set the base path to our directory we are working from.
    # base_path <- "C:/Users/user/Desktop/Oakleigh_Project/AccelerometerData/Accelerometer_Analysis"
  base_path <- getwd()
  #Source Report Generator Functions
  source(file.path(base_path, "Scripts", "Report_Generators.R"))
  
  # Here we created a dictionary that that's a list of variables for each species: sample rate and overlap
  species_settings <- list(Studd_Squirrel = list(sample_rate = 1, overlap_percent = 20, window_length = 10),
                           Seal = list(sample_rate = 100, overlap_percent = 20, window_length = 10))
  
  # Here we are going to set the sample rate based on the name from the dictionary and pull the values of that.
  sample_rate <- species_settings[[dataset_name]]$sample_rate
  # Doing the same for overlap variable within our dictionary.
  overlap_percent <- species_settings[[dataset_name]]$overlap_percent
  # Same for window length
  window_length <- species_settings[[dataset_name]]$window_length
  # Assign available axes
  available_axes <- c("Accelerometer.X", "Accelerometer.Y", "Accelerometer.Z")


# Create Directories ----------------------------------------------------------
  createDirectories()
 # In a separate script titled "ModifyData" we have edited the data and rewritten it.

  # Split test data out and load other data ---------------------------------
  source(file.path(base_path, "Scripts", "SplitTestData.R"))


# Create Box-Plot Graph --------------------------------------------------------
  # After splitting the data create a box-plot of the average behaviour times per individual.
  
  # Call for the Rmd to generate into an html file: 
  
  # Create a check that sees if the file has already been created, or created recently.
  
  # Check if the file already exists
  if (file.exists(paste0(base_path,"/Plots/", dataset_name, "_Behaviour_Duration_Report.html"))) {
    # Print that the file already exists
    cat(paste0("Behaviour duration report already exists for ", dataset_name,".csv", " in:\n\n", base_path, "/Plots/"))
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
    

# Generate features for training data -------------------------------------

# currently set to only process a very small number of windows
source(file.path(base_path, "Scripts", "GeneratingFeatures.R"))



