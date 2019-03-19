#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

matrix_create <- function(seq_1, seq_2) {
  seq_1_split <- strsplit(seq_1, "")
  seq_2_split <- strsplit(seq_2, "")
  
  #Forming Matrix
  align_matrix <- matrix(ncol = (length(seq_1_split[[1]]) + 1), nrow = (length(seq_2_split[[1]])) + 1)
  column_names <- c( "-", seq_1_split[[1]])
  row_names <- c("-", seq_2_split[[1]])
  colnames(align_matrix) <- column_names
  rownames(align_matrix) <- row_names
  align_matrix[,1] <- 0
  align_matrix[1,] <- 0
  dimensions <- dim(align_matrix)
  for(i in 2:dimensions[2]){
    for(n in 2:dimensions[1]){
      if(column_names[i] == row_names[n]){
        score = 5
      }else{
        score = -3
      }
      align_matrix[n,i] <- max(c((score + align_matrix[(n-1),(i-1)])), align_matrix[n,(i-1)] + -4, align_matrix[(n-1),i] + -4, 0)
    }
  }
  
  results <- list(align_matrix, row_names, column_names)
  return(results)
}

data_frame_create <- function(list_in) {
  #Create Data-frame
  number_of_scores <- dim(list_in[[1]])
  df_form_matrix <- matrix(ncol = 3, nrow = (number_of_scores[1] * number_of_scores[2]))
  index <- 1
  for (i in 1:length(list_in[[3]])) {
    for (s in 1:length(list_in[[2]])) {
      df_form_matrix[index, 1] <- i
      df_form_matrix[index, 2] <- s
      df_form_matrix[index, 3] <- list_in[[1]][s, i]
      index <- index + 1
    }
  }
  align_df <- data.frame(seq1 = df_form_matrix[,1], seq2 = df_form_matrix[,2], scores = df_form_matrix[,3])
  align_df$scores <- as.character(align_df$scores)
  return(align_df)
}



# Define reactive components and rendering of plot
shinyServer(function(input, output) {
  
  output$map <- renderPlot({
    
    input$do

    # Sequences
    mat_data <- isolate({
      validate(
        need(input$seq1 != "", "Please input a sequence for Sequence 1"),
        need(input$seq2 != "", "Please input a sequence for Sequence 2")
      )
      matrix_create(input$seq1, input$seq2)
    })
    
    
    plot_data <- isolate(({
      data_frame_create(mat_data)
    }))
    
    ggplot(plot_data, aes(x = plot_data$seq1, y = plot_data$seq2, fill = as.double(plot_data$scores))) +
      geom_raster() +
      scale_y_reverse(name="Sequence 2", expand=c(0,0), minor_breaks=NULL, breaks=c(1:length(mat_data[[2]])), labels=mat_data[[2]]) +
      scale_x_discrete(name="Sequence 1", position="top", expand=c(0,0), limits=c(1:length(mat_data[[3]])), breaks=c(1:length(mat_data[[3]])), labels=mat_data[[3]]) +
      scale_fill_distiller(name="Score") +
      geom_tile(colour="white", size=0.25) +
      theme_classic(base_size = 12) +
      theme(
        legend.text=element_text(face = "bold"),
        legend.key.size = unit(30, "pt"),
        axis.ticks = element_line(size=0.4),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        axis.title.x.top = element_text()
      )
  })
})
