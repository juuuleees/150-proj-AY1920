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

		print("Given acm:")
		print(given_acm)

		rows = nrow(given_acm)
		temp_acm = given_acm

		i = 1
		j = 1
		print("Solving...")
		repeat {

			# put the largest element in the column on the main diagonal
			sorted_col = sort(temp_acm[,i], decreasing = TRUE)
			print("Sorting the column...")
			print(sorted_col)

			if (!isTRUE(all.equal(temp_acm[,i], sorted_col))) {
				if (i != 1) {
					
					if (i == (rows-1)) {

						print(paste("[i,rows,(rows-1)]:", i, rows, (rows-1)))
			
						sub_acm = temp_acm[i:rows,]
						sub_acm = sub_acm[order(abs(sub_acm[,i]), decreasing = TRUE),]
						temp_acm[i:rows,] = sub_acm


						print("Current acm: ")
						print(temp_acm)
			
					} else if (i < rows) {

						print(paste("[i,rows]:", i, rows))

						sub_acm = temp_acm[(i+1):rows,]
						sub_acm = sub_acm[order(sub_acm[,i], decreasing = TRUE),]
						temp_acm[(i+1):rows,] = sub_acm


						print("Current acm: ")
						print(temp_acm)
			
					}

				} else {
					temp_acm = temp_acm[order(temp_acm[i:rows], decreasing = TRUE),]

					print("Current acm: ")
					print(temp_acm)
				}

			}

			# divide the row by the value in the main diagonal
			if (temp_acm[i,i] != 0) {
				temp_acm[i,] = temp_acm[i,] / temp_acm[i,i]
			}

			repeat {
			
				if (j != i) {
					temp_acm[j,] = temp_acm[j,] - (temp_acm[i,] * temp_acm[j,i])
				}
				
				if (j == rows) { break }
				j = j + 1

			}

			j = 1

			# if (i == 5) { break }
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

	# improved code from Ex03
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

		# print(solution_set)

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

		in_data = table_data()
		x_value = qs_get_x_in()
		x_value = as.integer(x_value)

		if (is.na(x_value)) {
			qs_x_estimate$data <- "No input?"
		} else {

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

		}

	})

	output$final_qs_eqn <- renderUI({
		if (is.null(qs_eqn$data)) {
			return()
		} else {

			final_eqn_output = "" 
			i = 1
			repeat {
				final_eqn_output = paste(final_eqn_output, qs_eqn$data[i], sep = "<br/>")
		
				i = i + 1
				if (i > length(qs_eqn$data)) { break }

			}

			HTML(final_eqn_output)

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
	# time of awat: 0537H
	init_tab <- reactiveValues(data = NULL)
	iterated_tab <- reactiveValues(data = NULL)
	iterations <- reactiveValues(data = 0)
	last_iteration <- reactiveValues(data = FALSE)

	make_tableau <- function(data) {
		# print(data)
		# constraints
		z_vec = vector()
		supply1 = vector()
		supply2 = vector()
		supply3 = vector()
		demand1 = vector()
		demand2 = vector()
		demand3 = vector()
		demand4 = vector()
		demand5 = vector()

		print(data)

		# going to paste stuff into this
		b = matrix(0, nrow = 3, ncol = 10)
		e = matrix(0, nrow = 5, ncol = 12)

		# get Z coefficients
		i = 1
		j = 1
		repeat {

			repeat {

				z_vec = c(z_vec, data[[j]][i])
	

				i = i + 1
				if (i == length(data[[j]])) { break }
	
			}

			i = 1
			j = j + 1
			if (j == length(data)) { break }

		}
		z_vec = c(z_vec, 1)

		# get supply coefficients
		i = 1
		repeat {
			supply1 = c(supply1, data$V1[i])

			i = i + 1
			if (i == length(data$V1)) { break }

		}
		supply1 = c(supply1, integer(10), data$V1[6])

		i = 1
		repeat {
			supply2 = c(supply2, data$V2[i])

			i = i + 1
			if (i == length(data$V2)) { break }

		}
		supply2 = c(integer(5), supply2, integer(5), data$V2[6])

		i = 1
		repeat {
			supply3 = c(supply3, data$V3[i])

			i = i + 1
			if (i == length(data$V3)) { break }

		}
		supply3 = c(integer(10), supply3, data$V3[6])

		# get demand coefficients
		i = 1
		repeat {

			demand1 = c(demand1, data[[i]][1])

			if (i == length(data)) { break }
			i = i + 1

		}
		demand1 = c(demand1[1],integer(4), demand1[2], 
					integer(4), demand1[3], integer(4), demand1[4]) * -1
		i = 1
		repeat {

			demand2 = c(demand2, data[[i]][2])

			if (i == length(data)) { break }
			i = i + 1

		}
		demand2 = c(0,demand2[1],integer(4), demand2[2], 
					integer(4), demand2[3], integer(3), demand2[4]) * -1
		i = 1
		repeat {

			demand3 = c(demand3, data[[i]][3])

			if (i == length(data)) { break }
			i = i + 1

		}
		demand3 = c(integer(2),demand3[1],integer(4), demand3[2], 
					integer(4), demand3[3], integer(2), demand3[4]) * -1
		i = 1
		repeat {

			demand4 = c(demand4, data[[i]][4])

			if (i == length(data)) { break }
			i = i + 1

		}
		demand4 = c(integer(3),demand4[1],integer(4), demand4[2], 
					integer(4), demand4[3], integer(1), demand4[4]) * -1
		i = 1
		repeat {

			demand5 = c(demand5, data[[i]][5])

			if (i == length(data)) { break }
			i = i + 1

		}
		demand5 = c(integer(4),demand5[1],integer(4), demand5[2], 
					integer(4), demand5[3], demand5[4]) * -1

		# # transpose it bc we minimizing
		matrix_values = c(supply1, supply2, supply3, demand1, demand2, demand3,
							demand4, demand5, z_vec)
		simplex_acm = t(matrix(matrix_values, nrow = 9, ncol = 16, byrow = TRUE))
		simplex_acm[nrow(simplex_acm),ncol(simplex_acm)] = 0
		# print(simplex_acm)

		# then make a maxiization matrix ugh lordt andaming values
		slack_vars = diag(16)
		sols = simplex_acm[,9]
		non_slack = simplex_acm[1:16, 1:8]
		final_tableau = cbind(non_slack, slack_vars, sols)
		return(final_tableau)
		
	}

	iterate_gjm <- function(tableau, last_i) {

		if (all(tableau[,nrow(tableau)] > 0)) {
			last_i <- TRUE
		}
		# print(tableau)

		# pick a pivot column
		last_row = tableau[nrow(tableau),1:24]
		print("last row: ")
		print(last_row)

		# pivot_col is a vector if there are similar negative values
		# e.g. -4 and -4 are the smallest negatives
		pivot_col = which(-last_row == max(-last_row))
		print("negative value indexes: ")
		print(pivot_col)
		pivot_col_index = 0
		# if pivot_col is a vector get the first element
		if (length(pivot_col) > 1) {
			pivot_col_index = pivot_col[1]
		} else {
			pivot_col_index = pivot_col
		}
		# print(pivot_col_index)

		# get the temp values to pick a pivot row
		sols = tableau[,25]
		pivot_col = tableau[,pivot_col_index]
		temp_values = vector()

		print("sols: ")
		print(sols)
		print("pivot col: ")
		print(pivot_col)
		i = 1
		repeat {

			if (pivot_col[i] != 0) {
				temp_values = c(temp_values, (sols[i] / pivot_col[i]))
			} else {
				temp_values = c(temp_values, 0);
			}

			if (i == length(pivot_col)) { break }
			i = i + 1

		}

		# print("col")
		# print(pivot_col_index)
		# same choosing criteria for pivot row index
		# bug here!!!!
		print("temp values: ")
		# forgot to catch the instances where there are positive temp values
		print(temp_values)
		pivot_row = which(-temp_values == max(-temp_values))

		print("pivot row: ")
		print(pivot_row)
		pivot_row_index = 0
		if (length(pivot_row) > 1) {
			pivot_row_index = pivot_row[1]
		} else {
			pivot_row_index = pivot_row
		}
		# print("row")
		print(paste(pivot_row_index, pivot_col_index))

		pivot_element = tableau[pivot_row_index, pivot_col_index]

		print("pivot element")
		print(pivot_element)

		# gauss-jordan!!
		tableau[pivot_row_index,] = tableau[pivot_row_index,] / pivot_element
		normalized_row = tableau[pivot_row_index,]
		print(normalized_row)

		i = 1
		repeat {

			pcol_element = tableau[i,pivot_col_index]

			if (pcol_element != 0 && pcol_element != 1) {
				temp = normalized_row * pcol_element
				tableau[i,] = tableau[i,] - temp
			}

			i = i + 1
			if (i > nrow(tableau)) { break }

		}

		print(tableau)
		return(tableau)

	}

	observeEvent(input$smplx_tableau, {
		simplex_data = table_data()

		initial_tableau = make_tableau(simplex_data)
		init_tab$data <- initial_tableau
	})

	observeEvent(input$smplx_iteration, {
		
		if (iterations$data == 0) {
			iterated_tab$data <- iterate_gjm(init_tab$data, last_iteration$data)
		} else {
			print("dumadaan ka ba dito u hoe")
			iterated_tab$data <- iterate_gjm(iterated_tab$data, last_iteration$data)
		}
	
		if (last_iteration$data == FALSE) {
			iterations$data = iterations$data + 1
		}

	})

	output$smplx_init_tableau <- renderTable({init_tab$data})
	output$smplx_iterated_tab <- renderTable({iterated_tab$data})
	output$iteration_count <- renderText({

		is_initial_tableau = "(initial tableau)"

		# print(paste("Last iteration na talaga?: ", last_iteration$data))
		if (is.null(iterations$data)) {
			return()
		} else if (iterations$data == 0) {
			HTML(paste("Iterations: ", iterations$data, is_initial_tableau))
		} else if (last_iteration$data) {
			print(last_iteration$data)
			print(iterations$data)
			HTML(paste("Iterations: ", iterations$data, "No more negative values in Z row!", sep = "<br/>")) 
		} else {
			HTML(paste("Iterations: ", iterations$data))
		}

	})

})