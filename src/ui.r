library(shiny)

fluidPage(
	tabsetPanel(
		tabPanel(
			title = "Polynomial Regression",
			sidebarLayout(
				sidebarPanel(
					fileInput(
						"file1",
						"Choose CSV file",
						accept = c('text/csv',
						'text/comma-separated-values, text/plain',
						'.csv')
					),
					tags$hr(),
					checkboxInput('header', "Header", TRUE),
					textInput(
						"order",
						"Order value",
						"",
						placeholder = "x = ?"
					),
					textInput(
						"x_value",
						"Value of x",
						""
					),
					actionButton(
						"pr_solve",
						"Solve"
					)
				),
				mainPanel(
					tableOutput("file_contents")
				)
			)
		),
		tabPanel(
			title = "Quadratic Spline",
			sidebarLayout(
				sidebarPanel(
					fileInput(
						"file1",
						"Choose CSV file",
						accept = c('text/c', 
						'text/comma-separated-values, text/plain',
						'.csv')
					),
					tags$hr(),
					textInput(
						"x_value",
						"Value of x",
						""
					),
					actionButton(
						"qs_solve",
						"Solve"
					)
				),
				mainPanel(

				)
			)	
		),
		tabPanel(
			title = "Simplex"
		)
	)
)