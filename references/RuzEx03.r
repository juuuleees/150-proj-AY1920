# RUZ, Julianne Marie
# 2014-04280
# CMSC 150 B-1L

AugCoeffMatrix <- function(a_list) {
  
  vars = vector()
  rownames = vector()
  colnames = vector()
  values_vector = vector()
  eqn = vector()
  
  # get the rownames
  c = 1
  while (c <= length(a_list)) {
    rownames = c(rownames, c)
    c = c + 1
  }
  
  i = 1
  r = 1
  for (i in a_list) {
    # convert the functions into strings
    deparsed = deparse(i)
    eqn = strsplit(deparsed[2], " ")
    
    # get the colnames
    if (r == 1) {
      j = 3
      while (j <= length(eqn[[1]])) {
        colnames = c(colnames, eqn[[1]][j])
        j = j + 4
      }
      vars = colnames
      # r here stops the loop from running for every equation in the list
      r = r + 1
    }
    
    
    # get the numerical values for the matrix
    j = 1
    while (j <= length(eqn[[1]])) {
      value = as.numeric(eqn[[1]][j])
      values_vector = c(values_vector, value)
      j = j + 4
    }

  }
  
  # add RHS to colnames
  colnames = c(colnames, "RHS")
  
  matrix_result = matrix(values_vector, nrow = length(rownames), ncol = length(colnames), dimnames = list(rownames, colnames), byrow = TRUE)
  
  results = list(variables = vars, augcoeffmatrix = matrix_result)
  
}