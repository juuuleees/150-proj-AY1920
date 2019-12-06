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

	# Functions for polynomial regression
	pr_eqn <- reactiveValues(data = NULL)
	pr_x_estimate <- reactiveValues(data = NULL)

	# gaussian_for_poly_reg <- function(data) {}

	observeEvent(input$pr_get_equation, {
		x_value = get_x_in()
		in_data = table_data()

		x_value = as.integer(x_value)

		pr_acm = poly_reg_acm(in_data, x_value)
		print(pr_acm)
		
		pr_eqn$data <- "<pr_eqn>"

	})

	observeEvent(input$pr_x_estimate, {
		usable_pr_eqn = pr_eqn$data

		pr_x_estimate$data <- "<pr_x_estimate>"
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

