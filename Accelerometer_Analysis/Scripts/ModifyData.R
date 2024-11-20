
raw_data <- fread(file.path(base_path, "Data", "Raw_Data", paste0(dataset_name, "_Labelled.csv")))
  modify_data <- raw_data %>% 
    rename(Time = time) %>% 
    select(-Ind_ID)
  
  # Writing over old data with new data.
  fwrite(modify_data, file.path(base_path, "Data", "Modified_Data", paste0(dataset_name, "_Modified.csv")))
  