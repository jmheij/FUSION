msfit_upload_panel <-
	nav_panel(
		"1. Upload msfit",
		fluidPage(
			fluidRow(
				column(
					width = 12,
					card(
						card_header(tags$b("Important notes on file upload")),
						card_body(
							fluidRow(
								column(
									width = 8,
									tags$ul(class = "medium-text",
										p("The starting point for this tool is a user-provided msfit file or multiple msfit files."),
										tags$li("If you upload only 1 msfit object, it should correspond to the (only) population you wish to model scenarios for. This is also a good start to understand the tool."),
										br(),
										tags$b("If you upload multiple msfit files:"),
										tags$li("Each file should correspond to a specific stratum of the population of interest (e.g., sex = female, age group = >18)."),
										tags$li(
											"The msfit files must be Mutually Exclusive & Collectively Exhaustive (",
											tags$a(
												"MECE",
												`data-toggle` = "popover",
												`data-bs-content` = "Mutually exclusive (ME): This means that each group is completely distinct and there's no overlap between groups. Collectively exhaustive (CE): This means that the sum of all your groups covers all possible options.",
												style = "cursor: pointer;",
												href = "#"
											),
											tags$sup(
													"i"
												),
											") in terms of the strata they correspond to and the overall population of interest."
										),
										tags$li("The files should not correspond to conceptually different models (i.e., they must contain the same eligible states and transitions).")
									)
								),
								column(
									width = 4,
									card(  # Nested card to make it a separate box
										card_body(
											fileInput(
												inputId = "file",
												label = "Please upload your fitted multi-state model. The uploaded file should be '.RDS' format and must be an 'msfit' object (mstate package)",
												accept = ".rds",
												width = "100%",
												multiple = TRUE
											)
										)
									)
								)
							)
						)
					)
					),
				column(
					width = 4,
					card(
						card_header(tags$b("Model characteristics")),  
						card_body(
							textOutput("upload_feedback1"),
							uiOutput(outputId = "SO_basic_characteristics", container = pre)
						)
					)
				),
				
				# Second card for model diagram
				column(
					width = 8,
					card(
						full_screen = TRUE,
						card_header(tags$b("Model diagram")),
						card_body(
							grVizOutput("model_diag1", width = "100%"),
							p(class = "medium-text", "S: State (number)", style = "font-size: 11px; text-align: center;"),
							p(class = "medium-text", "Tr: Transition (number)", style = "font-size: 11px; text-align: center;")
						)
					)
				)
			)
		)
	)