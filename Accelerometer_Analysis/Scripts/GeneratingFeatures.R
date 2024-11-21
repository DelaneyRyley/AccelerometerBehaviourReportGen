# Generate Features -------------------------------------------------------
# taken directly from my AnomalyDetection OCC work
# functions first and then the code

# Code here ---------------------------------------------------------------

if (file.exists(file.path(base_path, "Data", "Feature_Data", paste0(dataset_name, "_Other_Feature_Data.csv")))) {
  feature_data_other <- fread(file.path(base_path, "Data", "Feature_Data", paste0(dataset_name, "_Other_Feature_Data.csv")))
  } else {
  
    # Read the data from our training/validation data set.
  data1 <- fread(file.path(base_path, "Data", "Other_Data", paste0(dataset_name, "_Training.csv")))
    # Subset the data into a smaller amount, "toy data"
  data <- data1 %>% 
    group_by(ID,Activity) %>% 
    slice(1:5) %>% 
    as.data.table()
    
    feature_data_other <- generateFeatures(window_length, sample_rate, overlap_percent, 
                                           raw_data = data, 
                                           features_type = c("statistical", "timeseries"))
    
    fwrite(feature_data_other, file.path(base_path, "Data", "Feature_Data", paste0(dataset_name, "_Other_Feature_DataSlice.csv")))
    
  }
