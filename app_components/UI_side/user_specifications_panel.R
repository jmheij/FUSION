user_specifications_panel <-
	nav_panel(
		"3. Define scenarios",
		value = "user_specifications_panel",
		tabsetPanel(
			id = "user_specifications_tabset",
			tabPanel("Steps",
							 br(),
							 div(style = "text-align: center;",
							 		actionButton("get_started_transitions", "Get started", icon = icon("arrow-right"), width = "50%", class = "btn-primary"),
							 ),
							 tags$h4("Scenario definition steps"),
							 div(style = "medium-text",
							 	p("In this tool, scenarios consist of complete and unique combinations of: A. Transition scaling factors and B. Rewards. By 'complete' is meant that all scaling factors and rewards are specified for each population stratum and scenario. Below is further information and examples to guide you in providing the required inputs."),
							 tags$b("Transition scaling factors"),
							 tags$li("Purpose: to increase/decrease the estimated transition hazards (from your fitted MSM) in such a way that they represent alternative/counterfactual scenarios."),
							 tags$li("Example: you know that the hazards in your MSM correspond to a population that started receiving treament A at t0 and that treatment A halves the mortality rate after 1 month (e.g., transition 'disease -> death'). You wish to calculate outcomes assuming that treatment A was provided 3 months later than t0. To acheive this, you scale the transition 'disease -> death' by 1.5 for the first month, then by 2 for 2 months, then by 1.5 for 1 month, and finally set it back to 1 for the reminder of the time."),
							 tags$li("For a mathematical explanation of how scaling factors are used to produce scaled transition hazards from the estimated hazards, please refer to the manuscript text and appendix."),
							 br(),
							 tags$b("Rewards for states and transitions"),
							 tags$li("Purpose: rewards enable the calculation of outcomes for each scenario. This tool can accomodate any quantifiable rewards of interest (e.g., costs, loneliness on a 1-5 scale, health-related quality of life measured using EQ-5D utilities, etc.)"),
							 tags$li("Example: you wish to consider vaccination-related costs. The vaccine you are interested in costs 20 euros per dose. In the tool, this would be inputted as a transition reward, which occurs on every transition to a vaccinated state."),
							 tags$li("Variants: another reasonable price estimate for a vaccine may be 30 euros in your setting. To obtain results assuming 30 instead of 20 euros for a vaccine, you may create a 'variant' for this transition reward. You will then get the estimated total vaccine costs assuming both values. In the user interface, you can specify variants using a semi-colon. Eg. '20;30'. Note that if you input '20;30' only for scenario 1 and strata 1, but not for the rest of the scenarios and strata, the software will carry forward the last value inputted in the respective cell for that scenario and strata, i.e., '20;20'. " ),
							 tags$li("Note: which transitions you scale can be scenario-specific, but reward types must apply to all scenarios to allow for comparisons between scenarios."),
							 tags$li("For a mathematical explanation of how the total transition and state rewards are calculated in this tool, please refer to the manuscript text and appendix."),
							 ),
							 ),
			tabPanel("A. Transitions",
							 br(),
							 fluidRow(
							 	column(width = 5,
							 				 card(
							 				 	full_screen = TRUE,
							 				 	card_header(tags$b("Model (for reference)")),
							 				 	card_body(
							 				 		grVizOutput("model_diag2", width = "100%", height = "200px")
							 				 	)
							 				 )
							 	),
							 	column(width = 7,
							 				 # Two buttons for adding/removing scenarios, plus a display for the current count
							 				 tags$b("Current scenarios: "),
							 				 textOutput("currentScenarioCount", inline = TRUE),
							 				 div(style = "width: 100%; display: flex; justify-content: space-between; align-items: center;",
							 				 		div(
							 				 			style = "margin-right: 10px;",
							 				 			actionButton("add_scenario", "Add scenario", icon = icon("plus")),
							 				 			actionButton("remove_scenario", "Remove last scenario", icon = icon("minus"))
							 				 			
							 				 		),
							 				 		actionButton("proceed_to_rewards", "Confirm inputs and proceed", icon=icon("check"), class= "btn-primary")
							 				 )
							 	)
							 ),
							 # Outer tabsetPanel for scenarios with an id and unique values for each tab.
							 tabsetPanel(
							 	type = "pills",
							 	id = "scenario_tabs",
							 	!!!lapply(1:10, function(i) {
							 		tabPanel(
							 			title = paste("Scenario", i),
							 			value = paste0("scenario", i),  # assign a unique value
							 			fluidRow(
							 				column(4,
							 							 textInput(
							 							 	inputId = paste0("scenario_label_", i),
							 							 	label = "Scenario Label",
							 							 	value = paste("Scenario", i)
							 							 )
							 				),
							 				column(4,
							 							 numericInput(
							 							 	inputId = paste0("n_time_ranges_", i),
							 							 	label = "Number of Time Windows",
							 							 	value = 1,
							 							 	min = 1,
							 							 	max = 10,
							 							 	step = 1
							 							 )
							 				)
							 			),
							 			uiOutput(paste0("noUiSlider_ui_", i)),
							 			# Nested time window tabs (each with an id and unique value)
							 			tabsetPanel(
							 				id = paste0("time_window_tabs_", i),
							 				!!!lapply(1:10, function(j) {
							 					tabPanel(
							 						title = paste("Time Window", j),
							 						value = paste0("timeWindow", j),  # unique value for the time window tab
							 						fluidRow(
							 							column(4, uiOutput(paste0("time_window_label_ui_", i, "_", j))),
							 							column(3, uiOutput(paste0("selected_transitions_ui_", i, "_", j)))
							 						),
							 						uiOutput(paste0("toggle_scaling_ui_", i, "_", j)),
							 						uiOutput(paste0("matrix_input_ui_", i, "_", j))
							 					)
							 				})
							 			)
							 		)
							 	})
							 )
			),
			tabPanel("B. Rewards",
							 br(),
							 div(style = "width: 100%; display: flex; justify-content: space-between; align-items: center;",
							 		div(
							 			style = "margin-right: 10px;",
							 			tags$span("Current Rewards: "),
							 			textOutput("currentRewardCount", inline = TRUE),
							 			actionButton("add_reward", "Add reward", icon = icon("plus")),
							 			actionButton("remove_reward", "Remove last reward", icon = icon("minus"))
							 		),
							 		actionButton("proceed_to_final_check", "Confirm inputs and proceed to final check", 
							 								 icon = icon("check"), class = "btn-primary")
							 ),
							 
							 tags$b("The below rewards are required to proceed:"),
							 br(),
							 tabsetPanel(
							 	type = "pills",
							 	id = "rewards_tabs",
							 	!!!lapply(1:10, function(reward_num) {
							 		tabPanel(
							 			title = paste("Reward", reward_num),
							 			value = paste0("Reward", reward_num),  # assign a unique value
							 			fluidRow(
							 				column(4,
							 							 textInput(
							 							 	inputId = paste0("Reward_label_", reward_num),
							 							 	label = "Reward Label",
							 							 	value = paste("Reward", reward_num)
							 							 )
							 				)
							 			),
							 			card(
							 				full_screen = TRUE,
							 				card_header("State values"),
							 				card_body(
							 					p("Note that state reward values must be inputted for each state, but may be generalized to all scenarios and strata if relevant"),
							 					radioButtons(
							 						inputId = paste0("sr_radioButtons_",reward_num),
							 						label = "Make a selection",
							 						choices = c("State reward values generalize accross scenarios and strata",
							 												"State reward values vary per scenario",
							 												"State reward values vary per strata",
							 												"State reward values vary per scenario and per strata"),
							 						width = "100%"
							 					),
							 					uiOutput(paste0("matrix_ui_sr", reward_num, "_1")),
							 					uiOutput(paste0("matrix_ui_sr", reward_num, "_2")),
							 					uiOutput(paste0("matrix_ui_sr", reward_num, "_3")),
							 					uiOutput(paste0("matrix_ui_sr", reward_num, "_4"))
							 				)
							 			),
							 			card(
							 				full_screen = TRUE,
							 				card_header("Transition values"),
							 				card_body(
							 					p("Note that transition reward values must be inputted for each transition, but may be generalized to all scenarios and strata if relevant"),
							 					radioButtons(
							 						inputId = paste0("tr_radioButtons_",reward_num),
							 						label = "Make a selection",
							 						choices = c("Transition reward values generalize accross scenarios and strata",
							 												"Transition reward values vary per scenario",
							 												"Transition reward values vary per strata",
							 												"Transition reward values vary per scenario and per strata"),
							 						width = "100%"
							 					),
							 					uiOutput(paste0("matrix_ui_tr", reward_num, "_1")),
							 					uiOutput(paste0("matrix_ui_tr", reward_num, "_2")),
							 					uiOutput(paste0("matrix_ui_tr", reward_num, "_3")),
							 					uiOutput(paste0("matrix_ui_tr", reward_num, "_4"))							 				
							 				)
							 			)
							 		)
							 	})
							 )
							 
							 
			),
			tabPanel("C. Review & Confirm",
							 br(),
							 h4(tags$b("Final checks")),
							 div(style = "text-align: center;",
							 		actionButton("calculate_scenario", label = "Calculate scenario", icon = icon("check") , class = "btn-primary", width = "50%")
							 ),
							 p("Make sure that your inputs are valid and as you intended. After confirming, you will no longer be able to modify your inputs without restarting the session")							 ),
		)
	)