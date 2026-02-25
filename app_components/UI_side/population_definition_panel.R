population_definition_panel <-
	nav_panel(
		"2. Define populations",
		fluidPage(
			div(
				style = "width: 70%;",
				# Heading + numeric input on the same line
				div(
					style = "display: flex; align-items: center; gap: 10px;",
					h5(style = "margin: 0;", "Population size (total)"),
					numericInput(
						inputId = "tot_pop_size",
						label = NULL,
						value = 1, 
						min = 1,
						max = 100000000000,
						step = 1,
						width = "200px"
					)
				),
				br(),
				uiOutput("req_strata")
			),
			br(),
			hidden(
				div(
					id = "req_state_dist_strata_container",
					uiOutput("req_state_dist_strata")
				)
			),
			uiOutput("error_starting_props")
		)
	)
