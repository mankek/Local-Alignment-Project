# Sequences
seq_1 <- c("ATG")
seq_2 <- c("ATG")
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

align_matrix

