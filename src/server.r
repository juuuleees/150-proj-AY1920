library(shiny)

shinyServer(function(input, output) {
	
	# outputs have to be in different variables so you can see the 
	# original data in each tab's mainPanel

	polynomial_regression <- function(a,b) {
		print("solb poly reg heer")
	}

	# Functions for polynomail regression
	output$poly_reg_contents <- renderTable({

		inFile <- input$file1
		if (is.null(inFile)) { return(NULL) }

		table_data <- read.csv(inFile$datapath, header = input$header)

	})

	pr_x_value <- renderPrint({input$pr_x_value})

	pr_order <- observeEvent(input$pr_solve, {
		print("solb")
	})

	polynomial_regression(pr_x_value, pr_order)

	# Functions for quadratic spline
	output$quad_spline_contents <- renderTable({

		inFile <- input$file1
		if (is.null(inFile)) { return(NULL) }

		table_data <- read.csv(inFile$datapath, header = input$header)

	})

})

