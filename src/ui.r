library(shiny)

fluidPage(
	fileInput(
		"file1",
		"Choose CSV file",
		accept = c('text/csv',
		'text/comma-separated-values, text/plain',
		'.csv')
	),
	tabsetPanel(
		tabPanel(
			title = "Polynomial Regression",
			sidebarLayout(
				sidebarPanel(
					checkboxInput('header', "Enable headers", TRUE),
					textInput(
						"pr_x_value",
						"Order value",
						"",
						placeholder = "x = ?"
					),
					actionButton(
						"pr_get_equation",
						"Display Equation"
					),
					actionButton(
						"pr_x_estimate",
						"Estimate X"
					)
				),
				mainPanel(
					h2("Results"),
					tableOutput("poly_reg_contents")
				)
			)
		),
		tabPanel(
			title = "Quadratic Spline",
			sidebarLayout(
				sidebarPanel(
					textInput(
						"qs_x_value",
						"Value of x",
						""
					),
					actionButton(
						"qs_get_equations",
						"Show Equations"
					),
					actionButton(
						"qs_x_estimate",
						"Estimate X"
					)
				),
				mainPanel(
					tableOutput("quad_spline_contents")
				)
			)	
		),
		tabPanel(
			title = "Simplex",
			sidebarLayout(
				sidebarPanel(
					actionButton(
						"smplx_add_text_in",
						"Add Input"
					),
					actionButton(
						"smplx_rmv_text_in",
						"Remove Input"
					),
					actionButton(
						"smplx_initial_iteration",
						"Show Final Result"
					),
					actionButton(
						"smplx_sol_by_iteration",
						"Show Results by Iteration"
					)
				),
				mainPanel(

				)
			)
		)
	)
)