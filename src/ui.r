library(shiny)

fluidPage(
	fileInput(
		"user_file",
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
					textInput(
						"pr_x_value",
						"Input x value:",
						"",
						placeholder = "x = ?"
					),
					# separate functions for both i guess?
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
					hr(),
					textOutput("final_pr_eqn"),
					textOutput("final_pr_estimated_x")
					# tableOutput("poly_reg_contents")
				)
			)
		),
		tabPanel(
			title = "Quadratic Spline",
			sidebarLayout(
				sidebarPanel(
					textInput(
						"qs_x_value",
						"Input x value:",
						"",
						placeholder = "x = ?"
					),
					actionButton(
						"qs_get_equation",
						"Display Equation"
					),
					actionButton(
						"qs_x_estimate",
						"Estimate X"
					)
				),
				mainPanel(
					h2("Results"),
					hr(),
					textOutput("final_qs_eqn"),
					textOutput("final_qs_estimated_x")
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