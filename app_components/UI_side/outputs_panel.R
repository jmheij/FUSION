outputs_panel <-
	nav_panel(
		"4. Outputs",
		value = "outputs_panel",
		tabsetPanel(
			tabPanel("Descriptive Results",
							 br(),
							 card(
							 	card_header(strong("Accrued state and transition rewards per scenario")),
							 	fluidRow(
							 		input_switch("simple_detailed", "detailed/summary results", value = T),
							 		div(
							 			id = "simple_table_descriptive",
							 			reactableOutput("descriptive_table_simple")
							 		),
							 		
							 		# DETAILED view container (hidden by default)
							 		div(
							 			id = "detailed_tables_descriptive",
							 			style = "display: none;",
							 			uiOutput("descriptive_detailed_ui")
							 		))),
							 # textOutput("selected_strata"),
							 uiOutput("filter_card"),
							 br(),
							 h5(strong("Transition counts")),
							 # event counts
							 uiOutput("scenario_picker_counts"),
							 reactableOutput("transition_counts")	
							 
							 
							 # dataTableOutput("final_state_probs"),
							 
							 
			),
			tabPanel("Comparative results",
							 br(),
							 # toggle for summary vs detailed comparative
							 card(
							 	fluidRow(
							 		input_switch("simple_detailed_comp",
							 								 "detailed/summary comparative results",
							 								 value = FALSE),
							 		
							 		# SUMMARY (simple) comparative container
							 		div(
							 			id = "simple_tables_comp",
							 			uiOutput("simple_comp_nested_ui")
							 		),
							 		
							 		# DETAILED comparative container
							 		div(
							 			id = "detailed_tables_comp",
							 			style = "display: none;",
							 			uiOutput("detailed_comp_nested_ui")
							 		)
							 	)
							 )
							 
			),
			tabPanel(
				"Reports",
				br(),
				fluidRow(
					#Left col
					column(
						width = 4,
						card(
							card_header(strong("Reports (inputs and outputs)")),
							
							tagList(
								p(
									class = "medium-text",
									"We recommend that you download reports for each of your scenarios (below) for track keeping and reporting. ",
									"These reports contain all your inputs and scenario-specific results."
								),
								selectInput(
									"report_scenario",
									"Choose scenario to download report for:",
									choices = NULL  # filled from server
								),
								downloadButton(
									outputId = "download_scenario_report",
									label = "Download HTML report"
								)
							)
						)
					),
					
					# RIGHT COLUMN 
					column(
						width = 8,
						card(
							div(
								class = "report-thankyou",
								h4(tags$b("Thank you for using the FUSION Tool")),
								p(
									class = "medium-text",
									"Your feedback and use of this application help improve future versions ",
									"and support better decision making. Please send us your feedback to j.m.heijdra_suasnabar@lumc.nl."
								),
								br(),
								tags$b("MIT License"),
								p(
									class = "medium-text",
									"Copyright 2025, Leiden University Medical Center"
								),
								p(
									class = "medium-text",
									"Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:"
								),
								tags$ul(
									class = "medium-text",
									tags$li(
										"The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software."
									),
									tags$li(
										"All research outputs or reports that use the FUSION Tool are requested to reference the original manuscript as follows:"
									)
								),
								p(class = "medium-text", "pending"),
								tags$b("Disclaimer"),
								p(
									class = "medium-text",
									"THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, ",
									"EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF ",
									"MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ",
									"IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, ",
									"DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ",
									"ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS ",
									"IN THE SOFTWARE."
								)
							)
						)
					)
				)
			)
			
		)
	)