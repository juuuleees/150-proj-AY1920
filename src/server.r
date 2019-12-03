library(shiny)

shinyServer(function(input, output) {
	
	# outputs have to be in different variables so you can see the 
	# original data in each tab's mainPanel

	# General functions
	table_data <- reactive({

		in_file <- input$user_file
		if (is.null(in_file)) { return(NULL) }

		table_data <- read.csv(in_file$datapath, header = FALSE)
		return(table_data)

	})

	get_x_in <- renderText({input$pr_x_value})

	# Functions for polynomial regression
	pr_eqn <- reactiveValues(data = NULL)
	pr_x_estimate <- reactiveValues(data = NULL)

	observeEvent(input$pr_get_equation, {
		x_value = get_x_in()
		in_data = table_data()

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

