### Header --------------------------------------------------------------------
# A script for functions used for generating Reports
# Created by Ryley Delaney
# Nov 2024


# Basic Functionality ---------------------------------------------------------
createDirectories <- function(wrkdir = base_path, structure = folder_structure) # Defaults as working directory if not specified
{
  # Checks if the 'Data' and 'Outputs' folders needed for the main script to
  # run exist and if they don't creates them.
  #' @param wrkdir This is the root directory that the script checks from
  #' @return Tells the user that the folders have been created or creates them 
  #' and then prompts the user to add their data to the 'data' folder.
  
  for (n in structure)
  {
    if (!dir.exists(n)) {
      dir.create(n, recursive = TRUE)
    }
  }
}


# Plots -------------------------------------------------------------------

  # Plot the behaviour duration (i.e. sleep for 6 hours). Uses modified data
  plotBehaviourDuration <- function(data, sample_rate = sample_rate, inc_outliers = FALSE)
  {
    #' Plots a box and whisker plot of  behaviour duration by behaviour
    #' -i.e. Sleep 6 hours- using modified data.
    #' @param data The data used, should be the modified data.
    #' @param sample_rate The sample rate of accelerometer, by default uses
    #' the sample rate of the data set.
    #' @param inc_outliers Determines whether or not to include outliers
    #' within the plot.
    #' @return Returns a ggPlot of the behaviour duration times. 

    
     
    summary <- data %>%
      arrange(ID) %>%            # Sort by ID (not time because multiple trials in dog data)
      group_by(ID) %>%      
      mutate(
        behavior_change = lag(Activity) != Activity,  # Detect changes in Activity
        behavior_change = ifelse(is.na(behavior_change), TRUE, behavior_change)  # Handle the first row
      ) %>%
      mutate(
        behavior_id = cumsum(behavior_change)  # Create an identifier for each continuous behavior segment
      ) %>%
      group_by(ID, behavior_id) %>%            # Group by ID and behavior_id
      mutate(
        row_count = row_number()                # Count rows within each behavior segment
      ) %>%
      ungroup() %>%
      select(ID, Time, Activity, row_count, behavior_id) %>%   # Select relevant columns
      group_by(ID, Activity, behavior_id) %>%
      summarise(duration_sec = max(row_count)/sample_rate)
    
    # Create a DF of stats about the behaviour durations
    duration_stats <- summary %>%
      group_by(Activity) %>%
      summarise(
        dur_mean = mean(duration_sec, na.rm = TRUE),
        dur_minimum = min(duration_sec, na.rm = TRUE),
        dur_median = median(duration_sec, na.rm = TRUE),
        dur_maximum = max(duration_sec, na.rm = TRUE),
        dur_lwr_quantile = quantile(duration_sec, probs = 0.25, na.rm = TRUE)
      )
    
    # plot that
    duration_plot <<- ggplot(summary,
                            aes(x = Activity,
                                y = as.numeric(duration_sec))) +
      geom_boxplot(aes(color = Activity), # Use color to distinguish activities
                   outliers = inc_outliers)  # Remove outliers from boxplot based on input
    # Creating a variable of the box-plot data
    duration_plot_data <<- ggplot_build(duration_plot)$data[[1]]
    
    # Create variables for the upper and lower scales with and without outliers.
    # With outliers
    inc_ymax <- max(summary$duration_sec, na.rm = TRUE)
    inc_ymin <- min(summary$duration_sec, na.rm = TRUE)
    # Without outliers
    exc_ymax <- max(duration_plot_data$ymax, na.rm = TRUE)
    exc_ymin <- min(duration_plot_data$ymin, na.rm = TRUE)
    
    # Check to see whether to include outliers or not and adjust y-scale accordingly
    if (inc_outlier_choice == TRUE) {
      duration_plot <- duration_plot +
      scale_y_continuous(
        limits = c(inc_ymin, inc_ymax),  # Set y-axis limits to min/max duration
        breaks = seq(0, inc_ymax, by = 60),  # Adjust the step size
      )
    } else {
      duration_plot <- duration_plot +
        scale_y_continuous(
          limits = c(exc_ymin,exc_ymax), # Set limits to the end ranges of Q1 and Q3
          breaks = seq(0, exc_ymax, by = 60) # Adjust step size based on Q3
        )
    }
    
    duration_plot <- duration_plot +
      theme(
        legend.position = "none",             # Remove legend
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis labels 90 degrees
        panel.grid = element_blank(),         # Remove grid lines
        panel.border = element_rect(color = "black", fill = NA)  # Add black border around the plot
      ) +
      labs(
        y = "Duration (seconds)",
        x = "Activities"
      ) 
    
    # Gets the behaviour names for all rows that contain the minimum behaviour duration.
    min_activity_duration <- duration_stats$Activity[which((duration_stats$dur_minimum) == min(duration_stats$dur_minimum))]
    # Does the same for the behaviours with the smallest medians.
    min_median_activity_duration <- duration_stats$Activity[which((duration_stats$dur_median == min(duration_stats$dur_median)))]
    # Does the same for the behaviours with the smallest medians.
    min_lwrquantile_activity_duration <- duration_stats$Activity[which((duration_stats$dur_lwr_quantile == min(duration_stats$dur_lwr_quantile)))]
    # Create a list of all the objects we want to print in the R Markdown file
    duration_report_combined <- list(
      # [1] Names of behaviours that share shortest duration.
      "Smallest Minimum Behaviours" = min_activity_duration,
      # [2] The shortest behaviour time in seconds
      "Smallest Minimum Behaviour Duration" = min(duration_stats$dur_minimum),
      # [3] The behaviours with the smallest median duration
      "Smallest Median Behaviours" = min_median_activity_duration,
      # [4] The shortest median duration time in seconds.
      "Smallest Median Behaviour Duration" = min(duration_stats$dur_median),
      # [5] The behaviours with the smallest lower quantiles (25%) duration.
      "Smallest Lower Quantiles Behaviours" = min_lwrquantile_activity_duration,
      # [6] The shortest lower quantile duration in seconds.
      "Smallest Lower Quantile Behaviour Durations" = min(duration_stats$dur_lwr_quantile),
      # [7] The GGplot of our behaviours
      "Boxplot" = duration_plot,
      # [8] A DF of all the stats: Activities, Median, Maximum, Minimum.
      "Duration Stats" = duration_stats %>% 
        setNames(c("Behaviour","Mean", "Minimum", "Median",
                   "Maximum", "Lower Quantile")) ,
      # [9] Number of individuals 
      "Individual_total" = length(unique(data$ID)),
      # [10] Number of behaviours
      "Behaviour_total" = length(unique(data$Activity))
      )
    return(duration_report_combined)
  }
  
  
  generate_random_colors <- function(n) {
    #' Generates a random number of colours for use by different plots
    #' @param n input for the number of colours requested.
    #' @return returns a list of character strings of different hexcodes.
 
    
    # Generate random red, green and yellow values.
    colors <- rgb(runif(n), runif(n), runif(n))
    return(colors)
  }
  
  # Data Manipulation --------------
  
  loadTrainingData <- function(data = dataset_name) 
  {
    #' Attempts to load in the training data as a dataframe
    #' @param data the dataset name used. By default, uses the dataset from Main.R
    #' @return Loads the training data from the 'Training_Data' folder and returns it to the user.
    #'  \training_data Data frame of the training data.
    
    # Sets that raw_data exists to false in case the user runs the script twice without clearing the environment.
    raw_data_exists <- FALSE
    
    # Check that data isn't blank.
    if (data == "") {
      print("Dataset defined as blank.")
    } else
    {
      # Check that the file exists, it not, notify the user.
      if(!file.exists(file.path(base_path, "Data", "Training_Data", paste0(data, "_Training.csv"))))
      {
        raw_data_exists <- FALSE
        print(paste0("Data set ", data, "_Training.csv", " Couldn't be found. Check your base path."))
      }
      # Else, return training data as dataframe.
      else
      {
        raw_data_exists <- TRUE
        
        training_data <- fread(file.path(base_path, "Data", "Training_Data", paste0(data, "_Training.csv")))
        return(training_data)
      }
    }
  }
  splitTestData <- function(modified_data)
  {
    #' Checks to see if test data exists and if not, creates a split of training and test data from the original study dataset.
    #' The split is done by splitting 20% of the individuals into a test_data set and putting the rest into a training_data set.
    #' @param modified_data is the post processed study data.
    #' @return The function will write to files, 'dataset_name_test.csv' and 'dataset_name_training.csv'
    #'  \test.csv a 20% split of the study data, written into the Test_Data directory
    #'  \training.csv an 80% split of the study data, written into the Training_Data directory
    
    # Check if the file exists.
    if
    (file.exists(file.path(base_path, "Data", "Test_Data", paste0(dataset_name, "_Test.csv"))))
    {
      print(paste(dataset_name, "test data already exists"))
      # fread(file.path(base_path, "Data", "Test_Data", paste0(dataset_name, "_Test.csv")))
    } else
      
    {
      
      # Read in the raw data
      modified_data <- fread(file.path(base_path, "Data", "Data", paste0(dataset_name, "_Modified.csv")))
      
      # How many individuals are in our data
      individuals <- unique(modified_data$ID)
      
      # Select 20% of them to be in the test set.
      # Calculate number of individuals within the test set
      # Calculated by randomly taking 20% of the values from individuals rounded.
      test_individuals <- sample(individuals, round(length(individuals)*0.2, 0))
      # We are going to create a new dataframe out of just our test individuals
      test_data <- modified_data %>% 
        filter(ID %in% test_individuals)
      
      # The remainder will be in the training set, used by calculating the difference between the test individuals and the remaining individuals
      training_data <- modified_data %>% 
        filter(!ID %in% test_individuals)
      
      # Save both of these to the test data folder.
      # Save the test data to the test_data folder
      fwrite(test_data, file.path(base_path, "Data", "Test_Data", paste0(dataset_name, "_Test.csv")))
      # Save the training/ validation data to the training_data folder
      fwrite(training_data, file.path(base_path, "Data", "Other_Data", paste0(dataset_name, "_Training.csv")))
    }
    
  }
  
  # Generate Behaviour Duration Report from Markdown ------------------------
  
  # Runs the Generate Behaviour Duration Report
  generateBD_Report <- function(base_path, dataset_name, sample_rate) {
    ### Might have to get the program to check if this folder exists and if not get it to make it?
    # Define what we will be calling
    output_file <- paste0(dataset_name, "_Behaviour_Duration_Report.html")
    
    # Knit the GenerateBehaviourDurationReport.Rmd file as an HTML report
    # start <- Sys.time()
    rmarkdown::render(
      # Input file that we are going to render
      input = file.path(base_path, "Scripts", "GenerateBehaviourDurationReport.Rmd"),
      # Output as a html document.
      output_format = "html_document",
      # Define the output file based on the previous output file variable
      output_file = output_file,  # File name only
      # May need to check if "Outputs" folder exists and create it, possibly in another script.
      output_dir = file.path(base_path, "Outputs"),   # Directory for saving the file
      params = list(
        base_path = base_path,
        dataset_name = dataset_name,
        sample_rate = sample_rate
      )
    )
    # print(paste("render: ", Sys.time() - start))
    # Write success message w/ path
    message(paste0("Exploration report saved to: ",base_path, "Outputs"))
  }