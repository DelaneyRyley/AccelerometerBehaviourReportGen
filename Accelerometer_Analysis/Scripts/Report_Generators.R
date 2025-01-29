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