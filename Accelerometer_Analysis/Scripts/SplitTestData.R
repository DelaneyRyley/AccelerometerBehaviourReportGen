# Split out test data -----------------------------------------------------

if
  (file.exists(file.path(base_path, "Data", "Test_Data", paste0(dataset_name, "_Test.csv"))))
  {
    fread(file.path(base_path, "Data", "Test_Data", paste0(dataset_name, "_Test.csv")))
   } else
  {
    # Read in the raw data
    modified_data <- fread(file.path(base_path, "Data", "Modified_Data", paste0(dataset_name, "_Modified.csv")))
    
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
      # Save the test data to the test data folder
      fwrite(test_data, file.path(base_path, "Data", "Test_Data", paste0(dataset_name, "_Test.csv")))
      # Save the training/ validation data to the "other" data folder
      fwrite(training_data, file.path(base_path, "Data", "Other_Data", paste0(dataset_name, "_Training.csv")))
  }
