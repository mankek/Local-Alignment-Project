# Trace Back

inds <- which(align_matrix == max(align_matrix), arr.ind = TRUE)
first_max <- unname(inds[1,])
Sequence_1 <- vector()
Sequence_1 <- c(Sequence_1, row.names(align_matrix)[first_max[1]])
Sequence_2 <- vector()
Sequence_2 <- c(Sequence_2, colnames(align_matrix)[first_max[2]])


traceback_inds <- list(c(first_max[1]-1, first_max[2]), c(first_max[1]-1, first_max[2]-1), c(first_max[1], first_max[2]-1))
matrix_vals <- vector()
for (i in 1:length(traceback_inds)){
  matrix_vals <- c(matrix_vals, align_matrix[traceback_inds[[i]][1], traceback_inds[[i]][2]])
}

max_val <- traceback_inds[[which.max(matrix_vals)]]
Sequence_1 <- c(Sequence_1, row.names(align_matrix)[max_val][1])
Sequence_2 <- c(Sequence_2, colnames(align_matrix)[max_val][2])


# Process: Start at highest score and re-calculate the scores of the [k-1,j-1],[k,j-1], & [k-1,j] cells and re-find max
  # starting indices: max_scores
  #current_score = max_scores
  # while align_matrix[k,j] != 0
    #