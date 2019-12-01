library(shiny)

shinyServer(function(input, output) {
	
	# outputs have to be in different variables so you can see the 
	# original data in each tab's mainPanel

	# main functions for the solvers
	polynomial_regression <- function(data, a) {
		
	}

	quadratic_spline <- function() {}

	simplex_solver <- function() {}

	# Functions for polynomail regression
	pr_get_table_data <- renderTable({

		inFile <- input$file1
		if (is.null(inFile)) { return(NULL) }

		table_data <- read.csv(inFile$datapath, header = FALSE)
		
		order_x = pr_get_x()
		polynomial_regression(table_data, order_x)	

	})

	pr_get_x <- renderText({input$pr_x_value})

	pr_order <- observeEvent(input$pr_x_estimate, {
		print("solb")
	})

	# Functions for quadratic spline
	output$quad_spline_contents <- renderTable({

		inFile <- input$file1
		if (is.null(inFile)) { return(NULL) }

		table_data <- read.csv(inFile$datapath, header = input$header)

	})

	# Functions for simplex

})

