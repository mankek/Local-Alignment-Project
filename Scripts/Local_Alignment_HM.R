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

#Create Data-frame
number_of_scores <- dim(align_matrix)
df_form_matrix <- matrix(ncol = 3, nrow = (number_of_scores[1] * number_of_scores[2]))
pair_ids <- vector()
index_3 <- 1
for (i in 1:length(column_names_unique)) {
  for (s in 1:length(row_names_unique)) {
    df_form_matrix[index_3, 1] <- column_names_unique[i]
    df_form_matrix[index_3, 2] <- row_names_unique[s]
    df_form_matrix[index_3, 3] <- align_matrix[s, i]
    pair_ids[index_3] <- paste(x_axis[i],y_axis[s],sep = "-")
    index_3 <- index_3 + 1
  }
}
align_df <- data.frame(seq1 = df_form_matrix[,1], seq2 = df_form_matrix[,2], scores = df_form_matrix[,3])
library(plotly)
align_df$scores <- as.character(align_df$scores)
align_df$scores <- as.double(align_df$scores)
p <- plot_ly(x = align_df$seq1, y = align_df$seq2, z = align_df$scores, type = "heatmap", text = ~paste('X-Y: ',pair_ids)) %>%
  layout(yaxis = list(autorange = "reversed"))
p

