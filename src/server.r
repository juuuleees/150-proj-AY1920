library(shiny)

shinyServer(function(input, output) {
	
	# outputs have to be in different variables so you can see the 
	# original data in each tab's mainPanel

	# General functions
	table_data <- reactive({

		in_file <- input$user_file
		if (is.null(in_file)) { return(NULL) }

		table_data <- read.csv(in_file$datapath, header = FALSE)
		table_data = as.list(table_data)

		return(table_data)

	})

	get_x_in <- renderText({input$pr_x_value})
	get_func_degree <- renderText({input$pr_func_degree})

	gauss_jordan <- function(given_acm) {

		rows = nrow(given_acm)
		temp_acm = given_acm

		i = 1
		j = 1
		repeat {

			# put the largest element in the column on the main diagonal
			sorted_col = sort(temp_acm[,i], decreasing = TRUE)

			if (!isTRUE(all.equal(temp_acm[,i], sorted_col))) {
				if (i != 1) {
					
					if (i == (rows-1)) {
			
						sub_acm = temp_acm[i:rows,]
						sub_acm = sub_acm[order(abs(sub_acm[,i]), decreasing = TRUE),]
						temp_acm[i:rows,] = sub_acm
			
					} else if (i < rows) {

						sub_acm = temp_acm[(i+1):rows,]
						sub_acm = sub_acm[order(sub_acm[,i], decreasing = TRUE),]
						temp_acm[(i+1):rows,] = sub_acm
			
					}

				} else {
					temp_acm = temp_acm[order(temp_acm[i:rows], decreasing = TRUE),]
				}
			}


			# divide the row by the value in the main diagonal
			temp_acm[i,] = temp_acm[i,] / temp_acm[i,i]

			repeat {
			
				if (j != i) {
					temp_acm[j,] = temp_acm[j,] - (temp_acm[i,] * temp_acm[j,i])
				}
				
				if (j == rows) { break }
				j = j + 1

			}

			j = 1
			if (i == rows) { break }
			i = i + 1

		}

		solution_vector = temp_acm[,ncol(temp_acm)]
		return(solution_vector)

	}

	# Functions for polynomial regression
	pr_eqn <- reactiveValues(data = NULL)
	pr_x_estimate <- reactiveValues(data = NULL)

	poly_reg_acm <- function(data_list, degree) {

		rows = degree + 1
		cols = degree + 2
		# assumes that the length of the vectors in the data list are equal
		data_points = length(data_list$V1)
		pr_matrix_LHS_values = vector()
		pr_matrix_RHS_values = vector()
		pr_final_matrix_values = vector()

		# separate the vectors in the list for later
		x_vals = data_list$V1
		y_vals = data_list$V2

		# iterators
		n = 0
		curr_n = 0
		max_n = degree

		repeat {

			repeat {
				
				pr_mtrx_element = sum((x_vals ^ n))
				pr_matrix_LHS_values = c(pr_matrix_LHS_values, pr_mtrx_element);
				
				if (n == max_n) { 
				
					pr_mtrx_element = sum(((x_vals ^ curr_n) * y_vals))
					
					# put the current row values into the final values vector
					pr_final_matrix_values = c(pr_final_matrix_values, pr_matrix_LHS_values, pr_mtrx_element)
					
					pr_matrix_LHS_values = pr_matrix_LHS_values[pr_matrix_LHS_values %in% 1:length(pr_matrix_LHS_values)]

					break 

				}

				n = n + 1

			}

			if (max_n == (2*degree)) { break }
			
			curr_n = curr_n + 1
			n = curr_n
			max_n = max_n + 1

		}

		pr_acm = matrix(pr_final_matrix_values, nrow = rows, ncol = cols, byrow = TRUE)
		
		return(pr_acm)

	}

	# new and improved!! (original source code from Ex06)
	pr_gauss_jordan <- function(pr_acm) {

		rows = nrow(pr_acm)
		temp_acm = pr_acm

		i = 1
		j = 1
		repeat {

			# put the largest element in the column on the main diagonal
			sorted_col = sort(temp_acm[,i], decreasing = TRUE)

			if (!isTRUE(all.equal(temp_acm[,i], sorted_col))) {
				if (i != 1) {
					
					if (i == (rows-1)) {
			
						sub_acm = temp_acm[i:rows,]
						sub_acm = sub_acm[order(abs(sub_acm[,i]), decreasing = TRUE),]
						temp_acm[i:rows,] = sub_acm
			
					} else if (i < rows) {

						sub_acm = temp_acm[(i+1):rows,]
						sub_acm = sub_acm[order(sub_acm[,i], decreasing = TRUE),]
						temp_acm[(i+1):rows,] = sub_acm
			
					}

				} else {
					temp_acm = temp_acm[order(temp_acm[i:rows], decreasing = TRUE),]
				}
			}


			# divide the row by the value in the main diagonal
			temp_acm[i,] = temp_acm[i,] / temp_acm[i,i]

			repeat {
			
				if (j != i) {
					temp_acm[j,] = temp_acm[j,] - (temp_acm[i,] * temp_acm[j,i])
				}
				
				if (j == rows) { break }
				j = j + 1

			}

			j = 1
			if (i == rows) { break }
			i = i + 1

		}

		solution_vector = temp_acm[,ncol(temp_acm)]
		return(solution_vector)

	}

	# new and improved!! (original source code from Ex06)
	pr_final_values <- function(sol_set) {

		degrees_x = vector()
		coeff_strs = vector()

		if (length(sol_set) == 1) {
			
			final_func_str = paste("function (x)", sol_set, sep = " ");
	  		
  			results = list(polynomial_str = final_func_str, polynomial_func = eval(parse(text = final_func_str)))

  			return(results)

		} else {
			d = 1;
  			# generates degrees of x for function string
  			repeat {
    			temp = paste("x^", d, sep = "");
    			degrees_x = c(degrees_x, temp);
			
    			d = d + 1;
    			if (d > (length(sol_set)-1)) { break; }
			
  			}
	
  			i = 2; j = 1;
  			# attach coefficients to correct x^d string
  			repeat {
			
    			temp = paste("+", sol_set[i], "*", degrees_x[j], sep = " ");
    			coeff_strs = c(coeff_strs, temp);
			
    			i = i + 1;
    			j = j + 1;
			
    			if (i > length(sol_set)) { break; }
			
  			}
			
  			final_func_str = paste("function (x)", sol_set[1], sep = " ");
  			i = 1;
  			# paste contents of coeff_strs into final function string
  			repeat {
    			final_func_str = paste(final_func_str, coeff_strs[i], sep = " ");
			
    			i = i + 1;
			
    			if (i > length(coeff_strs)) { break; }
  			}
	  		
  			results = list(polynomial_str = final_func_str, polynomial_func = eval(parse(text = final_func_str)))
  			return(results)
		}

	}

	# assigns the equation to the global(?) variable
	# to pr_eqn$data
	observeEvent(input$pr_get_equation, {
		x_value = get_x_in()
		degree = get_func_degree()
		in_data = table_data()

		x_value = as.integer(x_value)
		degree = as.integer(degree)

		if (degree > (length(in_data$V1) - 1)) {
			output_text = "Degree is too high!"
		} else {				
			pr_acm = poly_reg_acm(in_data, degree)
			solution_vector = pr_gauss_jordan(pr_acm)
			results = pr_final_values(solution_vector)			
	
			output_text = paste("Your function: ", results$polynomial_str)
		}
		
		pr_eqn$data <- output_text

	})

	# assigns the estimated x value to pr_x_estimate$data
	observeEvent(input$pr_x_estimate, {
		x_value = get_x_in()
		degree = get_func_degree()
		in_data = table_data()

		x_value = as.integer(x_value)
		degree = as.integer(degree)

		pr_acm = poly_reg_acm(in_data, degree)
		solution_vector = pr_gauss_jordan(pr_acm)
		results = pr_final_values(solution_vector)
		
		final_func = results$polynomial_func
		estimated_x = final_func(x_value)
		
		output_text = paste(results$polynomial_str, " when x = ", x_value, ": ", estimated_x)
		
		pr_x_estimate$data <- output_text
	})

	output$final_pr_eqn <- renderText({
		if (is.null(pr_eqn$data)) {
			return()
		} else { pr_eqn$data }
	})

	output$final_pr_estimated_x <- renderText({
		if (is.null(pr_x_estimate$data)) {
			return()
		} else { pr_x_estimate$data }
	})



	# Functions for quadratic spline

	# all qs_condition functions return the coefficients for 
	# the required 3n equations
	qs_get_x_in <- renderText({input$qs_x_value})

	qs_condition1 <- function(data) {

		limit = length(data$V1) - 1
		coefficients = list()
		x0 = 1		

		i = 2
		repeat {

			x2 = data$V1[i] ^ 2
			x1 = data$V1[i]

			eqn_coeffs = c(x2, x1, x0)

			eqn1_terms = c(eqn_coeffs, data$V2[i])
			eqn2_terms = c(eqn_coeffs, data$V2[i])
			# coefficients = c(coefficients, eqn_coeffs, eqn_coeffs)
			coefficients = append(coefficients, list(eqn1_terms, eqn2_terms))

			i = i + 1
			if (i > limit) { break }

		}

		return(coefficients)

	}

	qs_condition2 <- function(data) {

		coefficients = list()
		first = vector()
		last = vector()
		x2 = 0
		x1 = 0
		x0 = 1							

		i = 1
		repeat {

			if (i == 1 || i == length(data$V1)) {
				x2 = data$V1[i] ^ 2
				x1 = data$V1[i]

				eqn_coeffs = c(x2, x1, x0, data$V2[i])

				if (i == 1) {
					first = eqn_coeffs
				} else { last = eqn_coeffs }
			}

			if (i == length(data$V1)) { break }
			i = i + 1

		}

		coefficients = list(first, last)

		return(coefficients)

	}

	qs_condition3 <- function(data) {
		
		limit = length(data$V1) - 1
		coefficients = list()
		x1 = 0
		x0 = 1

		i = 2
		repeat {

			x1 = data$V1[i] * 2

			eqn_coeffs = c(x1, x0, -x1, -x0)

			# terms = list(LHS_variables, eqn_coeffs)
			# terms2 = list(RHS_variables, eqn_coeffs)

			# eqn_coeffs = c(x1, x0, x1, x0)
			coefficients = append(coefficients, list(eqn_coeffs))

			i = i + 1
			if (i > limit) { break }

		}

		return(coefficients)

	}

	quad_spline_acm <- function(data) {

		cond1 = qs_condition1(data)
		cond2 = qs_condition2(data)
		cond3 = qs_condition3(data)

		initial_n = length(data$V1)
		intervals = initial_n - 1
		matrix_rows = ((initial_n - 1) * 3) - 1
		matrix_cols = matrix_rows + 1
		cond1_limit = (2*intervals) - 2
		cond2_offset = (2 * (initial_n-1)) - 1
		cond3_offset = matrix_rows - (intervals - 1) + 1

		starting_mtrx = matrix(0, nrow = matrix_rows, ncol = matrix_cols, byrow = TRUE)

		# just pick any vector in cond1 bc they're all the same length anyway
		length_ref = length(cond1[[1]])
		i = 1
		row = 1 
		col = 3
		move_row = 1
		# for cond1
		repeat {

			if (i == 1) {

				starting_mtrx[i,i] = cond1[[i]][(i+1)]
				starting_mtrx[i,(i+1)] = cond1[[i]][(i+2)]
				starting_mtrx[i,(matrix_rows+1)] = cond1[[i]][length(cond1[[i]])]

			} else {

				starting_mtrx[row,col] = cond1[[i]][1]
				starting_mtrx[row,(col+1)] = cond1[[i]][2]
				starting_mtrx[row,(col+2)] = cond1[[i]][3]
				starting_mtrx[row,(matrix_rows+1)] = cond1[[i]][4]				

				if ((row %% 2)) { 
					move_row = move_row + 1 
					col = col + 3
				}

			}

			row = row + 1

			if (i == length(cond1)) { break }
			i = i + 1
		}

		# for cond2
		i = 1
		row = cond2_offset
		col = 1

		repeat {

			if (i == 1) {
				starting_mtrx[row,col] = cond2[[i]][(i+1)]
				starting_mtrx[row,(i+1)] = cond2[[i]][(i+2)]
				starting_mtrx[row,(matrix_rows+1)] = cond2[[i]][length(cond2[[i]])]
			} else {
				starting_mtrx[row,col] = cond2[[i]][1]
				starting_mtrx[row,(col+1)] = cond2[[i]][2]
				starting_mtrx[row,(col+2)] = cond2[[i]][3]
				starting_mtrx[row,(matrix_rows+1)] = cond2[[i]][4]
			}

			if (i == length(cond2)) { break }
			row = row + 1
			col = matrix_cols - 3
			i = i + 1

		}

		# for cond3
		i = 1
		row = cond3_offset
		col = 1

		repeat {

			if (i == 1) {
				starting_mtrx[row,col] = cond3[[i]][(i+1)]
				starting_mtrx[row,(col+2)] = cond3[[i]][(i+2)]
				starting_mtrx[row,(col+3)] = cond3[[i]][(i+3)]
			} else {
				starting_mtrx[row,col] = cond3[[i]][1]
				starting_mtrx[row,(col+1)] = cond3[[i]][2]
				starting_mtrx[row,(col+3)] = cond3[[i]][3]
				starting_mtrx[row,(col+4)] = cond3[[i]][4]
			}

			col = col + 2
			row = row + 1

			if (i == length(cond3)) { break }
			i = i + 1

		}

		return(starting_mtrx)

	}

	qs_final_eqns <- function(solution_set) {

		final_sol_set = c(0, solution_set)
		split_sol_set = vector()

		counter = 1
		index_offset = 1
		repeat {

			eqn_coeffs = final_sol_set[index_offset:(index_offset+2)]
			split_sol_set = append(split_sol_set, list(eqn_coeffs))

			index_offset = index_offset + 3

			if(counter == (length(final_sol_set)/3)) { break }
			counter = counter + 1

		}

		degrees_x = vector()
		coeff_strs = vector()
		function_strings = vector()
		parsed_functions = vector()

		d = 0;
  		# generates degrees of x for function string
  		counter = 1
  		repeat {

  			eqn_coeffs = split_sol_set[[counter]]

  			repeat {
    			temp = paste("x^", d, sep = "");
    			degrees_x = c(degrees_x, temp);			
    			d = d + 1;
    			if (d > (length(eqn_coeffs)-1)) { break; }			
  			}	

  			# quick fix, save sanity!
  			degrees_x = rev(degrees_x)
	
  			i = 1; j = 1;
  			# attach coefficients to correct x^d string
  			repeat {			
    			if (i != length(eqn_coeffs)) {
    				temp = paste(eqn_coeffs[i], "*", degrees_x[j], "+", sep = " ");
    			} else {
    				temp = paste(eqn_coeffs[i], "*", degrees_x[j], sep = " ");
    			}
    			
    			coeff_strs = c(coeff_strs, temp);			
    			i = i + 1;
    			j = j + 1;			
    			if (i > length(eqn_coeffs)) { break; }			
  			}
	
  			final_func_str = "function (x)"
  			i = 1;
  			# paste contents of coeff_strs into final function string
  			repeat {
    			final_func_str = paste(final_func_str, coeff_strs[i], sep = " ");			
    			i = i + 1;			
    			if (i > length(coeff_strs)) { break }
  			}

  			# parse then add to the function vector
  			function_strings = c(function_strings, final_func_str)
  			polynomial_func = eval(parse(text = final_func_str))
  			parsed_functions = c(parsed_functions, polynomial_func)

  			degrees_x = vector()
  			coeff_strs = vector()
  			d = 0

  			if (counter == length(split_sol_set)) { break }
  			counter = counter + 1

  		}

  		results = list(final_strs = function_strings, final_parsed = parsed_functions)
  		return(results)

	}

	qs_eqn <- reactiveValues(data = NULL)
	qs_x_estimate <- reactiveValues(data = NULL)

	observeEvent(input$qs_get_equation, {
		in_data = table_data()

		if (length(in_data$V1) <= 2) {
			output_text = "Not enough data points(?)"
			qs_eqn$data <- output_text
		} else {
			qs_acm = quad_spline_acm(in_data)
			sol_set = gauss_jordan(qs_acm)
			qs_results = qs_final_eqns(sol_set)

			output_vec = qs_results$final_strs
			# print(output_vec)
			qs_eqn$data <- output_vec
		}

	})

	qs_pick_interval <- function(x, data, parsed_funcs) {

		estimated_x = 0
		intervals = length(data$V1) - 1

		i = 1
		j = 1
		repeat {

			x1 = data$V1[i]
			x2 = data$V1[(i+1)]

			if(x > x1 && x < x2) {
				
				interval_func = parsed_funcs[[j]]
				estimated_x = interval_func(x)
			
			}

			if (j == intervals) { break }
			i = i + 1
			j = j + 1
		}

		return(estimated_x)
		
	}

	observeEvent(input$qs_x_estimate, {
		x_value = qs_get_x_in()
		in_data = table_data()

		x_value = as.integer(x_value)

		if (length(in_data$V1) <= 2) {
			output_text = "Not enough data points(?)"
			qs_x_estimate$data <- output_text
		} else if (x_value > (length(in_data$V1) - 1) || x_value <= 0) {
			print(x_value)
			output_text = "X value too small!"
			qs_x_estimate$data <- output_text
		} 

		qs_acm = quad_spline_acm(in_data)
		sol_set = gauss_jordan(qs_acm)
		qs_results = qs_final_eqns(sol_set)

		output_vec = qs_results$final_parsed
		estimated_x = qs_pick_interval(x_value, in_data, output_vec)

		str_components = c("x =", x_value, estimated_x)

		qs_x_estimate$data <- str_components

	})

	output$final_qs_eqn <- renderUI({
		if (is.null(qs_eqn$data)) {
			return()
		} else { 
			HTML(paste(qs_eqn$data[1], qs_eqn$data[2], qs_eqn$data[3], sep = "<br/>"))
		}
	})

	output$final_qs_estimated_x <- renderText({
		if (is.null(qs_x_estimate$data)) {
			return()
		} else if(is.character(qs_x_estimate$data) && length(qs_x_estimate$data) == 1) {
			qs_x_estimate$data
		} else { 
			HTML(paste(paste("Your estimated x value when", qs_x_estimate$data[1],
						 qs_x_estimate$data[2], "is:", sep = " "), qs_x_estimate$data[3], sep = "<br/>")) 
		}
	})

	# Functions for simplex

	# i tried but i dieded

})

