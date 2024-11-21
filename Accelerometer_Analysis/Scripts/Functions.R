# Function for plotting the volume (in minutes) of behaviour per individual
  plotBehaviourDuration <- function(data, frequency) 
  {
    my_colours <- generate_random_colors(length(unique(data$ID)))
    # summarise into a table
    labelledDataSummary <- data %>%
      #filter(!Activity %in% ignore_behaviours) %>%
      count(ID, Activity)
    
    # account for the HZ, convert to minutes
    labelledDataSummaryplot <- labelledDataSummary %>%
      mutate(minutes = (n/frequency)/60)
    
    # Plot the stacked bar graph
    plot_activity_by_ID <- ggplot(
      labelledDataSummaryplot,
      aes(x = Activity,
          y = minutes,
          fill = as.factor(ID))) +
      geom_bar(stat = "identity") +
      labs(x = "Activity",
           y = "minutes") +
      theme_minimal() +
      scale_fill_manual(values = my_colours) +
      theme(axis.line = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border = element_rect(color = "black", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    return(plot_activity_by_ID)
  }
  
  # load in the raw data and cleaned feature data (e.g., remove redundant and NA features)
  loadDataTables <- function(dataset_name)
  {
  if (dataset_name == "") {
    stop("Error: 'dataset_name' parameter is empty. Data set not found.")
  } else {
    tryCatch({
      # Load the raw data
      raw_data <- fread(file.path(base_path, "Data", "Other_data", paste0(dataset_name, "_Training.csv")))
      
      # Load the feature data
      feature_data <- fread(file.path(base_path, "Data", "Feature_data", paste0(dataset_name, "_other_feature_data.csv")))
    }, error = function(e) {
      message("Error: Can't find the nominated dataset. :")
      # message("Error: Can't find the nominated dataset. :", e$message)
      stop(e)
    })
        }
  }
  
  # Function for generating random colours
  generate_random_colors <- function(n) {
    # Generate random red, green and yellow values.
    colors <- rgb(runif(n), runif(n), runif(n))
    return(colors)
  }
  

# All Generate Feature Functions ------------------------------------------

  
  # Main function that calls the others
  generateFeatures <- function(window_length, sample_rate, overlap_percent, raw_data, features_type)
  {
    
    # multiprocessing   
    #plan(multisession, workers = availableCores())  # Use parallel processing 
    
    # Split raw_data by 'ID' # was by = "ID" before
    raw_data_by_id <- split(raw_data, raw_data$ID)
    
    # Process each ID's raw_data
    features_by_id <- list()
    for (id in unique(raw_data$ID)) {
      print(id)
      # I changed the way this was subsetted. Was previously raw_data_by_id[[as.character(id)]]
      features_by_id[[id]] <- processDataPerID(
        id_raw_data = raw_data_by_id[[id]],
        features_type,
        window_length,
        sample_rate,
        overlap_percent
      )
    }
    all_features <- do.call(rbind, features_by_id)
    
    #plan(sequential)  # Return to sequential execution
    
    all_features <- rbindlist(features_by_id)
    
    return(all_features)
  }
  
  # Function to process data for each ID
  processDataPerID <- function(id_raw_data, features_type, window_length, sample_rate, overlap_percent) {
    
    # Calculate window length and overlap
    samples_per_window <- window_length * sample_rate
    overlap_samples <- if (overlap_percent > 0) ((overlap_percent / 100) * samples_per_window) else 0
    num_windows <- ceiling((nrow(id_raw_data) - overlap_samples) / (samples_per_window - overlap_samples))
    
    # Function to process each window for this specific ID
    process_window <- function(i) {
      print(i)
      start_index <- max(1, round((i - 1) * (samples_per_window - overlap_samples) + 1))
      end_index <- min(start_index + samples_per_window - 1, nrow(id_raw_data))
      window_chunk <- id_raw_data[start_index:end_index, ]
      
      # Initialize output features
      window_info <- tibble(Time = NA, ID = NA, Activity = NA, GeneralisedActivity = NA, OtherActivity = NA)
      statistical_features <- tibble() 
      single_row_features <- tibble()  
      
      # Extract statistical features
      if ("statistical" %in% features_type) {
        statistical_features <- generateStatisticalFeatures(window_chunk = window_chunk, down_Hz = sample_rate)
      }
      
      # Extract timeseries features and flatten
      if ("timeseries" %in% features_type) {
        time_series_features <- tryCatch({
          generateTsFeatures(data = window_chunk)
        }, error = function(e) {
          message("Error in tsfeatures: ", e$message)
          return(tibble())  # Return an empty tibble on error
        })
        
        if (nrow(time_series_features) > 0) {
          single_row_features <- time_series_features %>%
            # NOTE: changed this here to add the gyro but didn't test it
          mutate(axis = rep(c("Accel.X", "Accel.Y", "Accel.Z", "Gyro.X", "Gyro.Y", "Gyro.Z"), length.out = n())) %>%
            pivot_longer(cols = -axis, names_to = "feature", values_to = "value") %>%
            unite("feature_name", axis, feature, sep = "_") %>%
            pivot_wider(names_from = feature_name, values_from = value)
        } else {
          message("No rows in time_series_features. Returning empty tibble.")
          single_row_features <- tibble(matrix(NA, nrow = 1, ncol = length(unique(paste0(rep(c("X", "Y", "Z"), each = length(time_series_features)), "_", names(time_series_features))))))  # Fill with NAs
          colnames(single_row_features) <- unique(paste0(rep(c("X", "Y", "Z"), each = length(time_series_features)), "_", names(time_series_features)))  # Match the column names
        }
      }
      
      # Extract window identifying info
      if (nrow(window_chunk) > 0) {
        window_info <- window_chunk %>% 
          summarise(
            Time = Time[1],
            ID = ID[1],
            Activity = as.character(
              names(sort(table(Activity), decreasing = TRUE))[1]),
            GeneralisedActivity = as.character(
              names(sort(table(Activity), decreasing = TRUE))[1]), 
            OtherActivity = as.character(
              names(sort(table(Activity), decreasing = TRUE))[1])
          ) %>% ungroup()
      }
      
      # Combine the window info, time series, and statistical features
      combined_features <- cbind(window_info, single_row_features, statistical_features) %>% 
        mutate(across(everything(), ~replace_na(., NA)))  # Ensure all columns are present
      
      return(combined_features)
    }
    
    # Use lapply to process each window for the current ID
    window_features_list <- lapply(1:num_windows, process_window)
    
    # Combine all the windows for this ID into a single data frame
    features <- bind_rows(window_features_list)
    return(features)
  }
  
  # generate time series features 
  generateTsFeatures <- function(data) {
    ts_list <- list(
      X = data[["Accelerometer.X"]],
      Y = data[["Accelerometer.Y"]],
      Z = data[["Accelerometer.Z"]]
    )
    
    # List of features to calculate
    features_to_calculate <- c(
      "acf_features", "arch_stat", "autocorr_features", "crossing_points", "dist_features",
      "entropy", "firstzero_ac", "flat_spots", "heterogeneity", "hw_parameters", "hurst",
      "lumpiness", "stability", "max_level_shift", "max_var_shift", "max_kl_shift", 
      "nonlinearity", "pacf_features", "pred_features", "scal_features", "station_features", 
      "stl_features", "unitroot_kpss", "zero_proportion"
    )
    
    # Initialise an empty list to store features
    time_series_features <- list()
    
    # Loop through each feature and calculate it
    for (feature in features_to_calculate) {
      tryCatch({
        feature_values <- tsfeatures(
          tslist = ts_list,
          features = feature,
          scale = FALSE,
          multiprocess = TRUE
        )
        time_series_features[[feature]] <- feature_values
      }, error = function(e) {
        message("Skipping feature ", feature, " due to error: ", e$message)
      })
    }
    
    # Combine all features into a single tibble
    if (length(time_series_features) > 0) {
      time_series_features <- bind_cols(time_series_features)
    } else {
      time_series_features <- tibble()
    }
    
    return(time_series_features)
  }
  
  # generate statistical features
  # Fast Fourier Transformation based features
  extractFftFeatures <- function(window_data, down_Hz) {
    n <- length(window_data)
    
    # Compute FFT
    fft_result <- fft(window_data)
    
    # Compute frequencies
    freq <- (0:(n/2 - 1)) * (down_Hz / n)
    
    # Compute magnitude
    magnitude <- abs(fft_result[1:(n/2)])
    
    # Calculate features
    mean_magnitude <- mean(magnitude)
    max_magnitude <- max(magnitude)
    total_power <- sum(magnitude^2)
    peak_frequency <- freq[which.max(magnitude)]
    
    # Return features
    return(list(Mean_Magnitude = mean_magnitude,
                Max_Magnitude = max_magnitude,
                Total_Power = total_power,
                Peak_Frequency = peak_frequency))
  }
  
  # making this faster using := which modifies in place rather than copying and modifying
  generateStatisticalFeatures <- function(window_chunk, down_Hz) {
    
    result <- data.table()
    
    window_chunk <- setDT(window_chunk)
    
    for (axis in available_axes) {
      axis_data <- window_chunk[[axis]]  # Extract the data for the window
      
      # Compute stats
      stats <- lapply(list(mean = mean, max = max, min = min, sd = sd), 
                      function(f) f(axis_data, na.rm = TRUE))
      
      # Assign stats to result
      result[, paste0(c("mean_", "max_", "min_", "sd_"), axis) := stats]
      
      # Calculate skewness
      result[, paste0("sk_", axis) := e1071::skewness(axis_data, na.rm = TRUE)]
      
      # Extract FFT features
      fft_features <- extractFftFeatures(axis_data, down_Hz)
      
      # Add FFT features to result as well
      result[, paste0(c("mean_mag_", "max_mag_", "total_power_", "peak_freq_"), axis) := 
               list(fft_features$Mean_Magnitude, fft_features$Max_Magnitude, 
                    fft_features$Total_Power, fft_features$Peak_Frequency)]
    }
    
    # calculate SMA, ODBA, and VDBA
    result[, SMA := sum(rowSums(abs(window_chunk[, ..available_axes]))) / nrow(window_chunk)]
    ODBA <- rowSums(abs(window_chunk[, ..available_axes]))
    result[, `:=`(
      minODBA = min(ODBA, na.rm = TRUE),
      maxODBA = max(ODBA, na.rm = TRUE)
    )]
    VDBA <- sqrt(rowSums(window_chunk[, ..available_axes]^2))
    result[, `:=`(
      minVDBA = min(VDBA, na.rm = TRUE),
      maxVDBA = max(VDBA, na.rm = TRUE)
    )]
    
    return(result)
  }