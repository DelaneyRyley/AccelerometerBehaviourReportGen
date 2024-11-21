# Basic input function

  # A State machine that receives input from the user and adds it to the given variable.
  recieveUserInput <- function(varName){
    # Getting the input for the window length
    # Include information on what the minimum window length should be and include catch cases.
    if(firstOrLast == "window_length"){
      x <- readline(prompt = "Enter length of window (in sec): ")
    }
    # Input for sample rate
    else if(firstOrLast == "sample_rate"){
      x <- readline(prompt = "Enter accelerometer sampling frequency (in Hz): ")
    }
    # Return the value
    return(x)
  }
