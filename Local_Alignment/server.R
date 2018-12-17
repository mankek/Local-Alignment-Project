#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

matrix_create <- function(in1, in2) {
  seq_1 <- in1
  seq_2 <- in2
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
  
  #Differentiate Nucleotide names
  column_names_unique <- vector()
  x_axis <- vector()
  index <- 1
  for (i in column_names) {
    x_axis[index] <- i
    n <- index
    column_names_unique[index] <- n
    index <- index+1
  }
  row_names_unique <- vector()
  y_axis <- vector()
  index_2 <- 1
  for (i in row_names) {
    y_axis[index_2] <- i
    m <- index_2
    row_names_unique[index_2] <- m
    index_2 <- index_2+1
  }
  results <- list(align_matrix, column_names_unique, row_names_unique, x_axis, y_axis)
  return(results)
}

data_frame_create <- function(list_in) {
  #Create Data-frame
  number_of_scores <- dim(list_in[[1]])
  df_form_matrix <- matrix(ncol = 3, nrow = (number_of_scores[1] * number_of_scores[2]))
  pair_ids <- vector()
  index_3 <- 1
  for (i in 1:length(list_in[[2]])) {
    for (s in 1:length(list_in[[3]])) {
      df_form_matrix[index_3, 1] <- list_in[[2]][i]
      df_form_matrix[index_3, 2] <- list_in[[3]][s]
      df_form_matrix[index_3, 3] <- list_in[[1]][s, i]
      pair_ids[index_3] <- paste(list_in[[4]][i],list_in[[5]][s],sep = "-")
      index_3 <- index_3 + 1
    }
  }
  align_df <- data.frame(seq1 = df_form_matrix[,1], seq2 = df_form_matrix[,2], scores = df_form_matrix[,3])
  align_df$scores <- as.character(align_df$scores)
  align_df$scores <- as.double(align_df$scores)
  test_list <- list(align_df, pair_ids)
  return(align_df)
}


pair_ids_get <- function(list_in) {
  #Return only pair-IDs
  number_of_scores <- dim(list_in[[1]])
  df_form_matrix <- matrix(ncol = 3, nrow = (number_of_scores[1] * number_of_scores[2]))
  pair_ids <- vector()
  index_3 <- 1
  for (i in 1:length(list_in[[2]])) {
    for (s in 1:length(list_in[[3]])) {
      df_form_matrix[index_3, 1] <- list_in[[2]][i]
      df_form_matrix[index_3, 2] <- list_in[[3]][s]
      df_form_matrix[index_3, 3] <- list_in[[1]][s, i]
      pair_ids[index_3] <- paste(list_in[[4]][i],list_in[[5]][s],sep = "-")
      index_3 <- index_3 + 1
    }
  }
  return(pair_ids)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

    # Sequences
    mat_data <- reactive({
      matrix_create(input$seq1, input$seq2)
    })
    
    pair_id_v <- reactive({
      pair_ids_get(mat_data())
    })
    
    plot_data <- reactive(({
      data_frame_create(mat_data())
    }))

    output$p <- renderPlotly({
      plot_ly(plot_data(), x = ~seq1, y = ~seq2, z = ~scores, type = "heatmap", text = ~paste('X-Y: ',pair_id_v())) %>%
      layout(yaxis = list(autorange = "reversed"))
    })
})
