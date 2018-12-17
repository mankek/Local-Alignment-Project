# Trace Back

# Process: Start at highest score and re-calculate the scores of the [k-1,j-1],[k,j-1], & [k-1,j] cells and re-find max
  # starting indices: max_scores
  #current_score = max_scores
  # while align_matrix[k,j] != 0
    #