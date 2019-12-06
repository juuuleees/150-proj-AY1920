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

	}

	observeEvent(input$pr_get_equation, {
		x_value = get_x_in()
		degree = get_func_degree()
		in_data = table_data()

		x_value = as.integer(x_value)
		degree = as.integer(degree)

		pr_acm = poly_reg_acm(in_data, degree)
		solution_vector = pr_gauss_jordan(pr_acm)
		results = pr_final_values(solution_vector)
		print("hoi")
		
		output_text = paste("Your function: ", results$polynomial_str)
		
		pr_eqn$data <- output_text

	})

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

	qs_eqn <- reactiveValues(data = NULL)
	qs_x_estimate <- reactiveValues(data = NULL)

	observeEvent(input$qs_get_equation, {
		x_value = get_x_in()
		in_data = table_data()

		qs_eqn$data <- "<qs_eqn>"

	})

	observeEvent(input$qs_x_estimate, {
		usable_qs_eqn = qs_eqn$data
		
		qs_x_estimate$data <- "<qs_x_estimate>"
	})

	output$final_qs_eqn <- renderText({
		if (is.null(qs_eqn$data)) {
			return()
		} else { qs_eqn$data }
	})

	output$final_qs_estimated_x <- renderText({
		if (is.null(qs_x_estimate$data)) {
			return()
		} else { qs_x_estimate$data }
	})

	# Functions for simplex

})

