# RUZ, Julianne Marie
# 2014-04280
# CMSC 150 B-1L

# edited to fit exercise requirements
GaussianElimination <- function(acm) {

  # trans_acm = a_list$augcoeffmatrix

  row_count = nrow(acm)
  matrix_sols = integer(row_count)

  rows = c(1:(row_count-1))

  # forward elimination
  for (i in rows) {

    sorted_col = sort(acm[,i], decreasing = TRUE)

    # for if you need to swap
    if (!isTRUE(all.equal(acm[,i], sorted_col))) {
      # swaps rows
      pivot_row = which(acm[,i] == max(abs(acm[i:row_count, i])), arr.ind = TRUE)
      
      if (length(pivot_row) == 1) {          
        if (acm[pivot_row,i] != 0) {
  
          acm[c(pivot_row,i),] <- acm[c(i, pivot_row),]
          j = i + 1
          remaining_rows = c(i:row_count)
        
          repeat {
      
            pivot_element = acm[i,i]      
            row_multiplier = acm[j,i] / pivot_element      
            normalized_row = acm[i,] * row_multiplier  
            acm[j,] = acm[j,] - normalized_row

            j = j + 1
            if (j > length(remaining_rows)) { break }
          }

        } else {
          matrix_sols = 'NA'
        }

      }

    } else {
      # for when you don't need to swap

        # j = 2 so you skip the first row
        j = 2
        remaining_rows = c(i:row_count)
        
        repeat {

          pivot_element = acm[i,i]
          row_multiplier = acm[j,i] / pivot_element
          normalized_row = acm[i,] * row_multiplier
          acm[j,] = acm[j,] - normalized_row

          j = j + 1
          if (j > length(remaining_rows)) { break }

        }
      
    }

  }

  # backwards substitution
  rhs = acm[,ncol(acm)]
  j = row_count
  repeat {

    matrix_sols[j] = (rhs[j] - sum(acm[j, j:row_count] * matrix_sols[j:length(matrix_sols)])) / acm[j,j]
    j = j - 1

    if (j == 0) { break }

  }

  results = list(augcoeffmatrix = acm, solution = matrix_sols)

}

GaussJordanMethod <- function(a_list) {

  trans_acm = a_list$augcoeffmatrix
  row_count = nrow(trans_acm)
  col_count = ncol(trans_acm)
  rows = row_count

  matrix_sols = integer(row_count)

  i = 1
  repeat {

    sorted_col = sort(trans_acm[,i], decreasing = TRUE)

    
    if (!isTRUE(all.equal(trans_acm[,i], sorted_col))) {
      pivot_row = which(trans_acm[,i] == max(abs(trans_acm[i:row_count, i])), arr.ind = TRUE)      
      if (length(pivot_row) == 1) {          
        if (trans_acm[pivot_row,i] != 0) {  
          trans_acm[c(pivot_row,i),] <- trans_acm[c(i, pivot_row),]

          trans_acm[i,] = trans_acm[i,] / trans_acm[i,i]
                  
          remaining_rows = c(i:row_count)

          j = 1
          repeat {

            if (j != i) {
              normalized_row = trans_acm[j,i] * trans_acm[i,]
              
              
              trans_acm[j,] = trans_acm[j,] - normalized_row
            } 

            j = j+1
            if (j > row_count) { break }

          }

        } else {
          matrix_sols = 'NA'
        }
      }
    } else {
      if (trans_acm[pivot_row, i] != 0) {
        trans_acm[i,] = trans_acm[i,] / trans_acm[i,i]
             
        remaining_rows = c(i:row_count)
        j = 1
        repeat {
          
          if (j != i) {
            normalized_row = trans_acm[j,i] * trans_acm[i,]
            
            
            trans_acm[j,] = trans_acm[j,] - normalized_row
          } 
          j = j+1
          if (j > row_count) { break }
             
        }
      
      }
      
    }

    i = i + 1
    if (i > row_count) { break }

  }

  # to make one last iteration bc ugh
  last_i = row_count
  trans_acm[last_i,] = trans_acm[last_i,] / trans_acm[last_i,row_count]
  
  j = 1
  repeat {

    if (j != last_i) {
      normalized_row = trans_acm[j,last_i] * trans_acm[last_i,]
      trans_acm[j,] = trans_acm[j,] - normalized_row
    } 

    j = j+1
    if (j == row_count) { break }

  }

  matrix_sols = trans_acm[,col_count]

  results = list(variables = a_list$variables, augcoeffmatrix = trans_acm, solution = matrix_sols)
  
}

# makes the augmented coefficient matrix needed for linear regression
RegressionACM <- function(degree, inputs) {
  i = 0;
  LHS_values = vector();
  RHS_values = vector();
  for_final_matrix = vector();
  eqns = degree + 1;


  # get LHS values
  for (i in length(inputs)) {
    # inputs[[1]] = x, inputs[[2]] = y
    j = 1; k = 0;

    for (j in inputs[[1]]) {
  
      x_j = sum(x ^ k);
      y_j = sum((x ^ k) * y);


      LHS_values = c(LHS_values, x_j);
      RHS_values = c(RHS_values, y_j);


      k = k + 1;
      j = j + 1
  
    }

    i = i + 1;

  }

  a = 1;
  repeat {

    for_final_matrix = c(for_final_matrix, LHS_values[a:(a+degree)], RHS_values[a]);

    if (a > degree) { break; }
    a = a + 1;

  }

  results = matrix(for_final_matrix, nrow = eqns, ncol = (eqns + 1), byrow = TRUE);

}

PolynomialRegression <- function(degree, list_elements) {
  
  degrees_x = vector();
  coeff_strs = vector();

  reg_acm = RegressionACM(degree, list_elements);

  gauss_res = GaussianElimination(reg_acm);

  d = 1;
  # generates degrees of x for function string
  repeat {
    temp = paste("x^", d);
    degrees_x = c(degrees_x, temp);

    d = d + 1;
    if (d > degree) { break; }

  }

  i = 2; j = 1;
  # attach coefficients to correct x^d string
  repeat {

    temp = paste("+", gauss_res$solution[i], "*", degrees_x[j], sep = " ");
    coeff_strs = c(coeff_strs, temp);

    i = i + 1;
    j = j + 1;

    if (i > length(gauss_res$solution)) { break; }

  }

  final_func_str = paste("function (x)", gauss_res$solution[1], sep = " ");

  i = 1;
  # paste contents of coeff_strs into final function string
  repeat {
    final_func_str = paste(final_func_str, coeff_strs[i], sep = " ");

    i = i + 1;

    if (i > length(coeff_strs)) { break; }
  }

  results = list(augcoeffmatrix = reg_acm, unknowns = gauss_res$solution, polynomial_string = final_func_str, 
                 polynomial_function = eval(parse(text = final_func_str)));

}


# References:
#   https://stackoverflow.com/questions/20782218/how-to-find-row-number-of-a-value-in-r-code/20782675
#   https://www.r-bloggers.com/r-tips-swapping-columns-in-a-matrix/
#   https://stackoverflow.com/questions/10374932/comparing-two-vectors-in-an-if-statement