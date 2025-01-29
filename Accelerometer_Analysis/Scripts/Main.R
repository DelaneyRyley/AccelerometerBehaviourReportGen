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
library(data.table)
library(purrr)
library(tidyverse)
library(kableExtra)
library(ggpubr) # for retrieving the legend in one of my plots
# Source all of our functions
#' Edit: Can be done using p_load() for more concise code

# Hardcoded variables -----------------------------------------------------

# Input the dataset name from the list of dictionaries in Species Settings
dataset_name <- "Studd_Squirrel"

# Set the base path to our directory we are working from.
# I've chosen to do this via the working directory but you can set it manually as well.
# base_path <- "C:/Users/user/Desktop/Oakleigh_Project/AccelerometerData/Accelerometer_Analysis"
base_path <- getwd()

# Source all functions we are using, I've decided to source them all at once because every function within the script is used at some point. 
source(file.path("Scripts/Functions.R")) # Source all of the functions we are using
#' Resolved: Edit: This line only works if using the script as a part of the R Projects
#' Resolved: Maybe just bump it down a little until the basepath has been defined


  #Source Report Generator Functions
  source(file.path(base_path, "Scripts", "Report_Generators.R"))
  #' Edit: Might be nice to move this a little further down until after the hardcodes
  
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
  #' Edit: format for documentation written in function folder
  createDirectories()
  #' Edit: This is a scary function because I don't know what it will do
  #' Might be good to add something more explicit to the call - like the structure you are going to generate
  #' Also note that this particular call doesn't generate all the folders we need. Which is hard to discern because we don't have an overall view
  #' When all the functionality is in the function, the function is not reusable / generalisable
  
  folder_structure <- list("Data" = c("Raw_Data" = list("Raw_Data", 
                                                        "Modified_Data"), 
                                      "Feature_Data" = list("Training_Data", 
                                                            "Test_Data")),
                           "Outputs" = c("Reports", 
                                         "Plots"),
                           "Results" = c("Analysed_Data", "Performance_Results")
                          )
  #' I know it's messy and ugly compared to your nice work... not sure best way to do it
  
  # Split test data out and load other data ---------------------------------
  source(file.path(base_path, "Scripts", "SplitTestData.R"))
  #' Edit: added file = to file.path to account for strange paths (one-drive is weird)
  #' Edit: folder 'Other_Data' didn't exist for me
  #' Edit: Note that is it saving the training data to the 'Other_Data' folder
  

# Create Box-Plot Graph --------------------------------------------------------
  # After splitting the data create a box-plot of the average behaviour times per individual.
  
  # Call for the Rmd to generate into an html file: 
  
  # Create a check that sees if the file has already been created, or created recently.
  
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
    #' Edit: is trying to pull the training data from 'Training_Data' folder (line 39) but it isn't there - it's in 'Other_Data'
    #' Edit: I manually copied the data over

  
# Open Behaviour Report 
  browseURL(paste0("Outputs/", dataset_name, "_Behaviour_Duration_Report.html"))
#' LOVE this!! Didn't even know it was possible and will integrate into all of my code now!

# Generate features for training data -------------------------------------
  # Gets the inputs for the sample rate, overlap percentage and window length.
  # feature_settings <- getFeatureInputs()
  
  #' this is completely perfect! Great flow through and logical usability
  
  
# currently set to only process a very small number of windows
generateFeatureData()



