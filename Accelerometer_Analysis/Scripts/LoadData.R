
# Attempt to load in the data ---------------------------------------------
# Set as default that the raw data doesn't exist, in case the script is used twice without clearing the environment.
raw_data_exists <- FALSE

if (dataset_name == "") {
  print("Dataset defined as blank.")
  } else
  {
    if(!file.exists(file.path(base_path, "Data", "Other_Data", paste0(dataset_name, "_Training.csv"))))
    {
      raw_data_exists <- FALSE
      print(paste0("Data set ", dataset_name, "_Training.csv", " Couldn't be found. Check your base path."))
    }
    else
    {
      raw_data_exists <- TRUE
    }
  }