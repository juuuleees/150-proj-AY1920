library(shiny)

shinyServer(function(input, output) {
	
	output$file_contents <- renderTable({

		inFile <- input$file1
		if (is.null(inFile)) { return(NULL) }

		table_data <- read.csv(inFile$datapath, header = input$header)

	})

})