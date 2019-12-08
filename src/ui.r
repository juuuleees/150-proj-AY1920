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
						"pr_func_degree",
						"Input value for function degree:",
						"",
						placeholder = "degree = ?"
					),
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
					h3("Results"),
					hr(),
					h4(textOutput("final_pr_eqn")),
					h4(textOutput("final_pr_estimated_x"))
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
					h2("Equations:"),
					h4(htmlOutput("final_qs_eqn")),
					hr(),
					h4(htmlOutput("final_qs_estimated_x"))
				)
			)	
		),
		tabPanel(
			title = "Simplex",
			sidebarLayout(
				sidebarPanel(
					
				),
				mainPanel(

				)
			)
		)
	)
)