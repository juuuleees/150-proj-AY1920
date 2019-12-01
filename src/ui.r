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
						"pr_order",
						"Order value",
						"",
						placeholder = "x = ?"
					),
					textInput(
						"pr_x_value",
						"Value of x",
						""
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
			title = "Simplex"
			# this is either more complicated than I originally thought
			# or I'm overthinking it. Come back to this later.
		)
	)
)