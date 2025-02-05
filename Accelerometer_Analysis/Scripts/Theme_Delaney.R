# Custom Theme for Behaviour Duration Plot -------------------
# by Ryley Delaney
# Created February 2025

Theme_delaney <- function(){
  theme(
  # Background
    panel.background = element_rect(fill = "grey90"),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),  # Add black border around the plot
    
    
  # Gridlines
    # panel.grid = element_blank(),
  panel.grid = element_line(color = "grey99"),
    panel.grid.major.y = element_line(size = 0.75),
    panel.grid.major.x = element_line(size = 0.75),
  
  # Axis
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(size = 12, vjust = 1, hjust = 0.5, angle = -20,),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 10), size = 15),
    axis.title.y = element_text(margin = margin(r = 15), size = 15),
  
  # Legend
    legend.position = "none" # Remove legend
  )
}