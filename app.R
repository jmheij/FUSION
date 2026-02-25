
#IMPORTANT: Before first use, please run the below two lines (uncomment).

# install.packages("renv")  #if necessary, install "renv". This and the below line are to ensure that the same package versions are used as in development. 
# renv::restore()

rm(list = ls())

library(shiny)
library(mstate)
library(ggpubr)
library(ggplot2)
library(plotly)
library(reshape2)
library(DiagrammeR)
library(DiagrammeRsvg)
library(bslib)
library(DT)
library(shinycssloaders)
library(shinyMatrix)
library(shinyjs)
library(shinyWidgets)
library(data.table)
library(shinyTree)
library(shiny.fluent)
library(reactable)

source("functions/funs_jh.R") ; org_save = FALSE

source("app_components/UI_side/landing_panel.R")
source("app_components/UI_side/population_definition_panel.R")
source("app_components/UI_side/msfit_upload_panel.R")
source("app_components/UI_side/user_specifications_panel.R")
source("app_components/UI_side/outputs_panel.R")
source("html_appearance.R")



##	FRONT-END	##

ui <- page_navbar(
	useShinyjs(),
	
	header = tags$header(actionBttn("restart_session", " Restart session", 
																	style = "minimal", 
																	color = "warning", 
																	icon = icon("exclamation-circle"), 
																	size = "sm")),
	script_html,
	style_html,
	
	id = "main_nav",
	
	title = tags$span("FUSION Tool        ", style = "white-space: pre;"),
	
	theme = bs_theme(5, "yeti"),
	navbar_options = list(bg = "#0080BA"),
	
	
	landing_panel,
	msfit_upload_panel,
	population_definition_panel,
	user_specifications_panel,
	outputs_panel
)


## Back-end

server <- function(input, output, session) {
	useShinyjs()
	observe({
		if (!is.null(input$activeInnerTab)) {
			print(paste("Active inner tab:", input$activeInnerTab))
		}
	})
	
	#initiate empty reactive values
	SI_rvs <- reactiveValues()
	
	#enable restart of session if needed
	observeEvent(input$restart_session, {
		session$reload()  # Fully reloads the session
	})
	
	
	#--------------------------------------------------------------------------------#
	####	Upload file	####
	#--------------------------------------------------------------------------------#
	in_file_msfit <- reactive({
		validate(
			need(input$file, message = "Necessary msfit file(s) missing (please upload)")
		)
		
		msfit_list <- lapply(seq_along(input$file$datapath), function(i) {
			path <- input$file$datapath[i]
			# Attempt to read the RDS file
			obj <- tryCatch(readRDS(file = path), error = function(e) NULL)
			validate(
				need(inherits(obj, "msfit"), message = paste("The file", input$file$name[i], "is not of class 'msfit'. Please upload a valid file."))
			)
			# Assign the corresponding file name to the object
			obj$filename <- input$file$name[i]
			return(obj)
		})
		
		# Extract 'trans' values from each msfit object
		trans_values <- lapply(msfit_list, function(x) x$trans)
		
		# Ensure all trans values are identical if more than one file is uploaded
		if (length(unique(trans_values)) > 1) {
			validate(
				need(FALSE, message = "Cannot upload conceptually different models. If more than 1 msfit object is uploaded, these must have the same eligible states and transitions (i.e., they should only differ in terms of population/strata).")
			)
		}
		
		return(msfit_list)
	})
	
	#If file upload successful, pop-up for user to acknowledge
	observeEvent(in_file_msfit(), {
		msfit_list <- in_file_msfit()
		saveRDS(msfit_list, file = "msfit_list.rds")
		
		# Show the modal dialog
		showModal(modalDialog(
			title = "Important note",
			p(if (length(msfit_list) > 1) {
				paste0("You uploaded ", length(msfit_list), 
							 " msfit files. That means you wish to model scenarios in which certain policies, parameters, or assumptions vary according to the stratum. 
							 The uploaded msfit files must be Mutually Exclusive & Collectively Exhaustive (MECE) in terms of the strata they correspond to (e.g., if you want to model scenarios by sex and age group then upload msfit files with the predicted hazards for each combination of sex and age group). 
							 In the 'Define populations' tab, you will be requested to assign population weights to each strata (the sum of all must = 1).")
			} else {
				"You uploaded 1 msfit file. This means only you are only looking at 1 overall population and therefore cannot make strata-specific assumptions."
			}),
			
			footer = tagList(
				actionButton("acknowledge1", "Close")
			)
		))
	})
	# Handle close button
	observeEvent(input$acknowledge1, {
		removeModal()
	})
	
	
	output$upload_feedback1 <- renderText({
		msfit_list <- in_file_msfit()
		if(length(msfit_list) > 1){
			paste0(c("You uploaded ", length(msfit_list), " msfit files corresponding to a multi-state model with the below characteristics."))
		}else{
			c("You uploaded 1 msfit file corresponding to a multi-state model with the below characteristics..")
		}
	})
	
	
	
	#--------------------------------------------------------------------------------#
	#### Collect model characteristics ####	
	#--------------------------------------------------------------------------------#
	
	in_msfit_desc <- reactive({
		msfit_list <- in_file_msfit()
		first_msfit <- msfit_list[[1]]  # Use the first object since all trans values are identical
		
		s_names <- colnames(first_msfit$trans)
		
		s_names_short <- sapply(s_names, function(name) {
			paste(substr(strsplit(name, "[- ]")[[1]], 1, 3), collapse = " ")
		})
		
		trans_names <- to.trans2(first_msfit$trans)$transname
		
		trans_names_short <- sapply(trans_names, function(name) {
			paste(substr(strsplit(name, "[- ]")[[1]], 1, 3), collapse = " ")
		})
		
		list(
			n_states = ncol(first_msfit$trans),
			transit = to.trans2(first_msfit$trans),
			n_trans = max(to.trans2(first_msfit$trans)$transno),
			m_t_desc = first_msfit$trans,
			s_names = s_names,
			s_names_short = s_names_short,
			trans_names = to.trans2(first_msfit$trans)$transname,
			trans_names_short = trans_names_short,
			time_range = first_msfit$Haz$time[first_msfit$Haz$trans == 1]
		)
	})
	
	
	#--------------------------------------------------------------------------------#
	#### Return model descriptives and figure ####
	#--------------------------------------------------------------------------------#
	
	#Descriptives
	output$SO_basic_characteristics <- renderUI({
		paste(
			paste("Number of states:", in_msfit_desc()$n_states),
			"",
			paste("Number of transitions:", in_msfit_desc()$n_trans),
			sep = "\n"
		)
		
	})
	
	#Figure
	output$model_diag1 <- output$model_diag2 <- renderGrViz({
		graph <- gen_diagram(m_t_desc = in_msfit_desc()$m_t_desc)
	})
	
	#--------------------------------------------------------------------------------#
	#### Population definition by strata ####	
	#--------------------------------------------------------------------------------#
	
	output$req_strata <- renderUI({
		req(input$file, in_msfit_desc())
		num_files <- length(input$file$name)
		file_names <- input$file$name
		
		if (num_files == 1) {
			# Special case: single file means no strata, just "Population"
			tagList(
				h4("Population Label"),
				p("For convenience, you may re-label your population to a sensible name (e.g., Genpop)"),
				textInput(
					"strata_label_1",
					label = "Label for Population:",
					value = "Population"
				),
				actionButton("confirm_strata", "Proceed")
			)
		} else {
			strata_matrix <- matrix(
				c(paste("Strata", 1:num_files), rep(0, num_files)),
				nrow = num_files,
				ncol = 2,
				dimnames = list(c(paste0("Strata ", 1:num_files, " (corresponding msfit file: ", file_names, ")")), c("Label", "Weight"))
			)
			
			tagList(
				h5("Define Strata Labels and Proportions"),
				p("Provide labels and weights for each stratum. Weights must sum to 1."),
				matrixInput("strata_matrix", value = strata_matrix, class = "character", cols = list(names = TRUE), rows = list(names = TRUE)),
				actionButton("confirm_strata", "Confirm Strata")
			)
		}
	})
	
	observeEvent(input$confirm_strata, {
		req(input$file, in_msfit_desc())
		num_files <- length(input$file$name)
		strata_matrix <- input$strata_matrix
		weights <- as.numeric(strata_matrix[, "Weight"])
		labels <- strata_matrix[, "Label"]
		if (num_files == 1) {
			SI_rvs$strata_weights <- c(1)
			SI_rvs$strata_labels <- c(input$strata_label_1)
		}else{
			if (any(is.na(weights))) {
				showModal(modalDialog(title = "Strata Definition Error", "The strata weights must be numbers between 0-1.", easyClose = TRUE))
				return()  # Stop if there's an error
			}
			if (sum(weights) != 1) {
				showModal(modalDialog(title = "Strata Definition Error", "The sum of the strata weights must equal 1.", easyClose = TRUE))
				return()  # Stop if there's an error
			}
			SI_rvs$strata_weights <- weights
			SI_rvs$strata_labels <- labels
		}
		
		print("======   Latest strata labels:   ======")
		print(SI_rvs$strata_labels)
		print("======   Latest strata weights:   ======")
		print(SI_rvs$strata_weights)
		
		show("req_state_dist_strata_container")
	})
	
	
	#--------------------------------------------------------------------------------#
	#### Dynamically request starting state distributions for each specified population ####
	#--------------------------------------------------------------------------------#
	SI_rvs$df_in_state_dist_strata <- NULL
	
	output$req_state_dist_strata <- renderUI({
		req(in_file_msfit(), SI_rvs$strata_labels)
		tagList(
			h4("Starting state distributions"),
			checkboxInput("different_dist_per_strata", "The starting state distributions differ per strata", FALSE, width = "100%"),
			uiOutput("state_dist_matrix_ui")
		)
	})
	
	output$state_dist_matrix_ui <- renderUI({
		req(in_file_msfit(), SI_rvs$strata_labels)
		
		tryCatch({
			msfit_list <- in_file_msfit()
			num_files <- length(msfit_list)
			prev_values <- isolate(input$SI_state_dist_strata)
			
			new_nrows <- in_msfit_desc()$n_states  # Rows = states
			if (input$different_dist_per_strata) {
				new_ncols <- length(SI_rvs$strata_labels)  # Columns = strata
				row_labels <- in_msfit_desc()$s_names   
				col_labels <- SI_rvs$strata_labels             
			} else {
				new_ncols <- 1  # Only one distribution to be recycled across all strata
				row_labels <- in_msfit_desc()$s_names
				col_labels <- "All Strata"
			}
			
			new_matrix <- matrix(0, nrow = new_nrows, ncol = new_ncols,
													 dimnames = list(row_labels, col_labels))
			
			new_matrix[1, ] <- 1  # Set the first state (first row) to 1 for every column.
			
			# If any previous values exist, copy as many entries as possible.
			if (!is.null(prev_values)) {
				old_nrows <- nrow(prev_values)
				old_ncols <- ncol(prev_values)
				copy_rows <- min(old_nrows, new_nrows)
				copy_cols <- min(old_ncols, new_ncols)
				new_matrix[1:copy_rows, 1:copy_cols] <- prev_values[1:copy_rows, 1:copy_cols]
			}
			
			matrixInput("SI_state_dist_strata", value = new_matrix, class = "numeric")
		}, error = function(e) {
			NULL
		})
	})
	
	observe({
		req(input$SI_state_dist_strata)
		
		if (input$different_dist_per_strata) {
			# When per-strata distributions are enabled, use the matrix as entered.
			SI_rvs$df_in_state_dist_strata <- input$SI_state_dist_strata
		} else {
			# For a single state distribution, replicate the column over all strata.
			SI_rvs$df_in_state_dist_strata <- matrix(
				rep(input$SI_state_dist_strata[, 1], times = length(SI_rvs$strata_labels)),
				nrow = nrow(input$SI_state_dist_strata), 
				ncol = length(SI_rvs$strata_labels),
				byrow = FALSE,
				dimnames = list(rownames(input$SI_state_dist_strata), SI_rvs$strata_labels)
			)
		}
		
		print("======   Latest starting state distributions:   ======")
		print(SI_rvs$df_in_state_dist_strata)
	})
	
	output$error_starting_props <- renderUI({
		req(SI_rvs$df_in_state_dist_strata)
		df <- SI_rvs$df_in_state_dist_strata
		
		if (any(is.na(df))) {
			return(HTML('<span style="color:red; font-family: monospace; background-color:#f8d7da;">
                 Cannot proceed. All cells must contain a numeric value (no missing values or text).</span>'))
		} else if (any(df < 0)) {
			return(HTML('<span style="color:red; font-family: monospace; background-color:#f8d7da;">
                 Cannot proceed. All values must be non-negative.</span>'))
		} else if (sum(colSums(df)) != ncol(df)) {
			# Check that for each column (i.e. for each stratum) the state probabilities sum to 1.
			return(HTML('<span style="color:red; font-family: monospace; background-color:#f8d7da;">
                 Cannot proceed. The starting state proportions must sum to 1 for each stratum
                 (i.e., all column sums = 1).</span>'))
		} else {
			return(actionButton("confirm_state_dist", "Confirm State Distribution"))
		}
	})
	
	observeEvent(input$confirm_state_dist, {
		showModal(modalDialog(
			title = "State Distributions Saved Successfully",
			p("The state distributions have been successfully saved. You may now proceed to defining scenarios."),
			footer = tagList(
				actionButton("close_modal", "Close"),
				actionButton("proceed_to_scenarios", "Proceed to Defining Scenarios", class = "btn-primary")
			),
			easyClose = TRUE
		))
	})
	
	observeEvent(input$close_modal, {
		removeModal()
	})
	
	
	#--------------------------------------------------------------------------------#
	#### Dynamically Define Transition Scenarios with Time Windows ####
	#--------------------------------------------------------------------------------#
	
	observeEvent(input$proceed_to_scenarios, {
		removeModal()
		updateNavbarPage(inputId = "main_nav", selected = "user_specifications_panel")
	})
	
	observeEvent(input$get_started_transitions, {
		runjs('$("#user_specifications_tabset a:contains(\'A. Transitions\')").click();')
		
	})
	
	SI_rvs$mats <- list() #empty, for later
	
	currentScenarioCount <- reactiveVal(1) # Initialize reactive value for current scenario count
	
	
	# Display current number of scenarios
	output$currentScenarioCount <- renderText({
		currentScenarioCount()
	})
	
	# Increase scenario count (max 10)
	observeEvent(input$add_scenario, {
		if (currentScenarioCount() < 10) {
			currentScenarioCount(currentScenarioCount() + 1)
		}
	})
	
	# Decrease scenario count (min 1)
	observeEvent(input$remove_scenario, {
		if (currentScenarioCount() > 1) {
			currentScenarioCount(currentScenarioCount() - 1)
		}
	})
	
	# Observer to hide/show scenario tabs based on currentScenarioCount
	observe({
		n_scen <- currentScenarioCount()
		for (i in 1:10) {
			js_code <- sprintf("$('#scenario_tabs li a[data-value=\"scenario%d\"]').parent()", i)
			if (i <= n_scen) {
				runjs(sprintf("%s.show();", js_code))
			} else {
				runjs(sprintf("%s.hide();", js_code))
			}
		}
	})
	
	# For each scenario, observe its n_time_ranges input and hide/show time window tabs accordingly
	lapply(1:10, function(i) {
		observe({
			req(input[[paste0("n_time_ranges_", i)]])
			n_time <- input[[paste0("n_time_ranges_", i)]]
			for (j in 1:10) {
				js_code <- sprintf("$('#time_window_tabs_%d li a[data-value=\"timeWindow%d\"]').parent()", i, j)
				if (j <= n_time) {
					runjs(sprintf("%s.show();", js_code))
				} else {
					runjs(sprintf("%s.hide();", js_code))
				}
			}
		})
	})
	
	observe({
		req(in_msfit_desc(), SI_rvs$strata_labels)
		if(input$calculate_scenario == FALSE) {
			for(i in 1:10) {
				for(j in 1:10) {
					mat_name <- paste0("matrix_", i, "_", j)
					if (is.null(SI_rvs$mats[[mat_name]])) {
						SI_rvs$mats[[mat_name]] <- matrix(
							1, #todo: add toggle 0/1. Done.
							nrow = length(SI_rvs$strata_labels),
							ncol = length(in_msfit_desc()$trans_names),
							dimnames = list(SI_rvs$strata_labels, in_msfit_desc()$trans_names)
						)
					}
				}
			}
		}
	})
	
	
	# Render slider for scenario depending on n_time_ranges
	observe({
		req(in_msfit_desc())
		for(i in 1:10) {
			local({
				ii <- i 
				
				output[[paste0("noUiSlider_ui_", ii)]] <- renderUI({
					noUiSliderInput(
						inputId = paste0("time_ranges_", ii),
						label = "Time window breakpoints",
						min = 0,
						max = max(in_msfit_desc()$time_range),
						value = prep_t_range(in_msfit_desc()$time_range, input[[paste0("n_time_ranges_", ii)]]),
						connect = FALSE,
						range = prepare_step(in_msfit_desc()$time_range),
						# format = wNumbFormat(decimals = count_decimals(in_msfit_desc()$time_range)),
						# format = wNumbFormat(decimals = 7),
						behaviour = "snap",
						pips = list(mode="positions", values = c(0, 25, 50, 75, 100), density = 4),
						width = "100%"
					)
				})
			})
		}
		# print(prepare_step(in_msfit_desc()$time_range))
	})
	
	
	# Render UI for selectInput and sliderTextInput (time window)
	observe({
		req(in_msfit_desc())
		
		for(i in 1:10) {
			for(j in 1:10) {
				local({
					ii <- i
					jj <- j
					
					# Render the selectInput for selecting transitions
					output[[paste0("time_window_label_ui_", ii, "_", jj)]] <- renderUI({
						textInput(
							inputId = paste0("time_window_label", ii, "_", jj),
							label = "Label for time window",
							value = paste0("Time window S", ii, " T", jj)
						)
					})
					# Render the selectInput for selecting transitions
					output[[paste0("selected_transitions_ui_", ii, "_", jj)]] <- renderUI({
						Dropdown.shinyInput(
							inputId = paste0("selected_transitions_", ii, "_", jj),
							label = "Select transitions",
							multiSelect = TRUE,
							options = lapply(in_msfit_desc()$trans_names, function(x) list(key = x, text = x)),
							dropdownWidth = "auto",
							styles = list(
								dropdownOptionText = list(overflow = "visible", whiteSpace = "normal"),
								dropdownItem = list(height = "auto")
							)
						)
						
					})
					
					#Render toggle for simplified inputs
					output[[paste0("toggle_scaling_ui_", ii, "_", jj)]] <- renderUI({
						input_switch(
							id = paste0("toggle_scaling_", ii, "_", jj),
							label = "Generalize scaling factors accross strata? (No/Yes). If yes, fill ONLY TOP row.", value = F, width = "100%")
					})
				})
			}
		}
	})
	
	
	#Separate Observer to Render the matrixInput UI (when toggle == F)
	observe({
		req(in_msfit_desc())
		for (i in 1:10) {
			for (j in 1:10) {
				local({
					ii <- i; jj <- j
					output[[paste0("matrix_input_ui_", ii, "_", jj)]] <- renderUI({
						selected_trans <- input[[paste0("selected_transitions_", ii, "_", jj)]]
						
						if (min(input[[paste0("time_ranges_", ii)]]) != 0 ||
								max(input[[paste0("time_ranges_", ii)]]) != max(in_msfit_desc()$time_range)) {
							return(tags$div(HTML(
								'<span style="color:red; font-family: monospace; background-color:#f8d7da;">
               WARNING: invalid time window definition. The outermost values should always be 0 and the maximum time in the msfit file.
               </span>'
							)))
						}
						
						if (is.null(selected_trans) || length(selected_trans) == 0)
							return(tags$div("No transitions selected."))
						
						if (isTRUE(input$calculate_scenario))
							return(tags$div("Sorry, you already confirmed and calculated scenarios. You may no longer make changes here and must restart the session."))
						
						stored_matrix <- SI_rvs$mats[[paste0("matrix_", ii, "_", jj)]]
						if (is.null(stored_matrix))
							return(tags$div("Matrix not initialized yet."))
						
						valid_trans <- intersect(selected_trans, colnames(stored_matrix))
						if (length(valid_trans) == 0)
							return(tags$div("Selected transitions are not valid."))
						
						display_matrix <- stored_matrix[, valid_trans, drop = FALSE]
						
						matrixInput(
							inputId = paste0("matrix_input_", ii, "_", jj),
							value   = display_matrix
						)
					})
				})
			}
		}
	})
	
	#When user edits the matrix, pass to SI_rvs$mats
	for (i in 1:10) {
		for (j in 1:10) {
			local({
				ii <- i; jj <- j
				observeEvent(input[[paste0("matrix_input_", ii, "_", jj)]], {
					req(input[[paste0("matrix_input_", ii, "_", jj)]])
					new_values     <- input[[paste0("matrix_input_", ii, "_", jj)]]
					selected_trans <- input[[paste0("selected_transitions_", ii, "_", jj)]]
					stored_matrix  <- SI_rvs$mats[[paste0("matrix_", ii, "_", jj)]]
					valid_trans    <- intersect(selected_trans, colnames(stored_matrix))
					if (length(valid_trans) > 0) {
						stored_matrix[, valid_trans] <- new_values
						SI_rvs$mats[[paste0("matrix_", ii, "_", jj)]] <- stored_matrix
					}
				}, ignoreInit = TRUE)
			})
		}
	}
	
	#Propagate top row to all rows when toggle is TRUE, and update the UI
	for (i in 1:10) {
		for (j in 1:10) {
			local({
				ii <- i; jj <- j
				toggle_id <- paste0("toggle_scaling_", ii, "_", jj)
				mat_id    <- paste0("matrix_", ii, "_", jj)
				input_id  <- paste0("matrix_input_", ii, "_", jj)
				
				observeEvent(input[[toggle_id]], {
					req(SI_rvs$mats[[mat_id]])
					if (isTRUE(input[[toggle_id]])) {
						# First, merge any current on-screen edits back into the stored matrix 
						current_display <- input[[input_id]]
						if (!is.null(current_display)) {
							sel <- intersect(colnames(current_display), colnames(SI_rvs$mats[[mat_id]]))
							if (length(sel) > 0) {
								SI_rvs$mats[[mat_id]][, sel] <- current_display[, sel, drop = FALSE]
							}
						}
						
						# Propagate row 1 -> all rows in the stored matrix
						m <- SI_rvs$mats[[mat_id]]
						if (nrow(m) >= 1) {
							m[,] <- rep(m[1, , drop = TRUE], each = nrow(m))
							# keep dimnames intact
							dimnames(m) <- dimnames(SI_rvs$mats[[mat_id]])
							SI_rvs$mats[[mat_id]] <- m
						}
						
						# Reflect the change in the visible control 
						selected_trans <- input[[paste0("selected_transitions_", ii, "_", jj)]]
						valid_trans    <- intersect(selected_trans, colnames(SI_rvs$mats[[mat_id]]))
						if (length(valid_trans) > 0) {
							shinyMatrix::updateMatrixInput(
								session,
								inputId = input_id,
								value   = SI_rvs$mats[[mat_id]][, valid_trans, drop = FALSE]
							)
						}
					}
				}, ignoreInit = TRUE)
				
				# Keep rows in sync whenever the matrix changes while toggle is TRUE
				observeEvent(input[[input_id]], {
					req(SI_rvs$mats[[mat_id]])
					if (!isTRUE(input[[toggle_id]])) return()
					
					# pass edit
					new_values     <- input[[input_id]]
					selected_trans <- input[[paste0("selected_transitions_", ii, "_", jj)]]
					valid_trans    <- intersect(selected_trans, colnames(SI_rvs$mats[[mat_id]]))
					if (length(valid_trans) > 0) {
						SI_rvs$mats[[mat_id]][, valid_trans] <- new_values[, valid_trans, drop = FALSE]
					}
					
					# re-propagate from top row
					m <- SI_rvs$mats[[mat_id]]
					m[,] <- rep(m[1, , drop = TRUE], each = nrow(m))
					dimnames(m) <- dimnames(SI_rvs$mats[[mat_id]])
					SI_rvs$mats[[mat_id]] <- m
					
					# update visible subset
					if (length(valid_trans) > 0) {
						shinyMatrix::updateMatrixInput(
							session,
							inputId = input_id,
							value   = m[, valid_trans, drop = FALSE]
						)
					}
				}, ignoreInit = TRUE)
			})
		}
	}
	
	

##Once all transition-related inputs are provided, we confirm and proceed
	
	observeEvent(input$proceed_to_rewards, {
		req(in_msfit_desc()) 
		
		n_scen <- currentScenarioCount()  
		new_list_scenarios <- list()       
		
		# Vector to collect scenario/time window identifiers missing transitions
		warning_entries <- character(0)
		
		# Iterate over all scenarios
		for (i in 1:n_scen) {
			scenario_label    <- input[[paste0("scenario_label_", i)]]
			n_time_windows    <- input[[paste0("n_time_ranges_", i)]]
			
			#collecting the time window breakpoints to report to user.
			user_durations    <- input[[paste0("time_ranges_", i)]] # grab what the user actually typed
			allowed_durations <- c(0, in_msfit_desc()$time_range) # define the only-allowed values
			time_window_durations <- vapply(user_durations, function(x) { #map each user value to its nearest allowed value
				allowed_durations[ which.min(abs(allowed_durations - x)) ]
			}, numeric(1))
			adjusted <- which(user_durations != time_window_durations)
			if (length(adjusted) > 0) {
				warn_msg <- sprintf(
					"Scenario %d, breakpoints(s) %s: Snapped %s --->  TO  ---> %s",
					i,
					paste(adjusted, collapse = ", "),
					paste(user_durations[adjusted], collapse = ", "),
					paste(time_window_durations[adjusted], collapse = ", ")
				)
				warning(warn_msg, call. = FALSE)
			}
			
			# Initialize list of time windows for this scenario
			time_windows_list <- list()
			
			# Iterate over all time windows in the current scenario
			for (j in 1:n_time_windows) {
				local({
					ii <- i
					jj <- j
					
					# Collect inputs for this specific time window
					time_window_label <- input[[paste0("time_window_label", ii, "_", jj)]]
					time_window_matrix <- input[[paste0("matrix_input_", ii, "_", jj)]]
					
					# If no transitions have been selected, record a simple identifier.
					if (is.null(time_window_matrix)) {
						warning_entries <<- c(warning_entries, paste("Scenario", ii, "time window", jj))
					}
					
					# Append time window data to the list
					time_windows_list[[jj]] <<- list(
						label = time_window_label,
						matrix = time_window_matrix
					)
				})
			}
			names(time_windows_list) <- paste("time window", 1:n_time_windows)
			
			
			# Once all time windows are gathered, store the scenario
			new_list_scenarios[[i]] <- list(
				scenario_id = i,
				label = scenario_label,
				n_time_windows = n_time_windows,
				time_window_durations = time_window_durations,
				time_windows = time_windows_list
			)
		}
		
		# Store the full scenario list in reactiveValues
		SI_rvs$scenarios <- new_list_scenarios
		
		# Check if any time windows are missing transitions
		if (length(warning_entries) > 0) {
			# Construct the full warning message with HTML line breaks for formatting
			full_warning_message <- paste0(
				"No transitions have been selected for some time windows. Please review the following:<br><br>",
				paste(warning_entries, collapse = "<br>"),
				"<br><br>This should only be the case if you wish to not scale ANY transitions for these time windows (which is unlikely)."
			)
			
			showModal(
				modalDialog(
					title = "Warning",
					HTML(full_warning_message),
					footer = tagList(
						modalButton("Go back"),
						actionButton("proceed_anyway", "Proceed anyway")
					)
				)
			)
		} else {
			# If there are no warnings, proceed to switch the tab
			runjs('$("#user_specifications_tabset a:contains(\'B. Rewards\')").click();')
		}
	})
	
	# listens for the "Proceed anyway" button click.
	observeEvent(input$proceed_anyway, {
		removeModal()
		runjs('$("#user_specifications_tabset a:contains(\'B. Rewards\')").click();')
	})
	
	
	
	
	
	
	#--------------------------------------------------------------------------------#
	#### State and transition reward definition ####
	#--------------------------------------------------------------------------------#
	
	# Initialize reactive value for current scenario count
	currentRewardCount <- reactiveVal(1)
	
	
	# Display current number of Rewards
	output$currentRewardCount <- renderText({
		currentRewardCount()
	})
	
	# Increase Reward count (max 10)
	observeEvent(input$add_reward, {
		if (currentRewardCount() < 10) {
			currentRewardCount(currentRewardCount() + 1)
		}
	})
	
	# Decrease Reward count (min 1)
	observeEvent(input$remove_reward, {
		if (currentRewardCount() > 1) {
			currentRewardCount(currentRewardCount() - 1)
		}
	})
	
	# Observer to hide/show Reward tabs based on currentRewardCount
	observe({
		n_reward <- currentRewardCount()
		for (i in 1:10) {
			js_code <- sprintf("$('#rewards_tabs li a[data-value=\"Reward%d\"]').parent()", i)
			if (i <= n_reward) {
				runjs(sprintf("%s.show();", js_code))
			} else {
				runjs(sprintf("%s.hide();", js_code))
			}
		}
	})
	
	
	##
	# Helper function to create a matrixInput based on the selected option.
	createMatrixInput <- function(inputId, option, in_msfit_desc, currentScenarioCount, SI_rvs, sr_tr) {
		# Get the state names which will now become the row names.
		
		if (sr_tr == "sr") {
			rows <- in_msfit_desc$s_names
		}
		
		if (sr_tr == "tr"){
			rows <- in_msfit_desc$trans_names
		}
		
		if (option == 1) {
			# Option 1: Generalize across scenarios/strata (one column)
			ncol_val <- 1
			col_names <- "Reward value"
			default_matrix <- matrix(
				1,
				nrow = length(rows),
				ncol = ncol_val,
				dimnames = list(rows, col_names)
			)
			return(matrixInput(
				inputId = inputId,
				value = default_matrix
			))
			
		} else if (option == 3) {
			# Option 3: Vary per strata
			n <- length(SI_rvs$strata_labels)
			col_names <- SI_rvs$strata_labels
			default_matrix <- matrix(
				1,
				nrow = length(rows),
				ncol = n,
				dimnames = list(rows, col_names)
			)
			return(matrixInput(
				inputId = inputId,
				value = default_matrix
			))
			
		} else if (option == 2) {
			# Option 2: Vary per scenario with variable time windows per scenario.
			# Create a tabsetPanel with one matrixInput per scenario.
			tabs <- lapply(1:currentScenarioCount, function(scen) {
				tw_count <- SI_rvs$scenarios[[scen]]$n_time_windows
				# Create column names for the current scenario's time windows.
				col_names <- sapply(1:tw_count, function(tw) paste(SI_rvs$scenarios[[scen]]$label, ", Time window", tw))
				default_matrix <- matrix(
					1,
					nrow = length(rows),
					ncol = tw_count,
					dimnames = list(rows, col_names)
				)
				matrix_id <- paste0(inputId, "_scen", scen)
				tabPanel(
					title = SI_rvs$scenarios[[scen]]$label,
					matrixInput(
						inputId = matrix_id,
						value = default_matrix
					)
				)
			})
			return(do.call(tabsetPanel, tabs))
			
		} else if (option == 4) {
			# Option 4: Vary per scenario and per strata (and per time window).
			# Outer level: one tab per scenario.
			outer_tabs <- lapply(1:currentScenarioCount, function(scen) {
				tw_count <- SI_rvs$scenarios[[scen]]$n_time_windows
				# Inner level: one tab per strata within the scenario.
				inner_tabs <- lapply(1:length(SI_rvs$strata_labels), function(strat) {
					col_names <- sapply(1:tw_count, function(tw) paste("Time window", tw))
					default_matrix <- matrix(
						1,
						nrow = length(rows),
						ncol = tw_count,
						dimnames = list(rows, col_names)
					)
					matrix_id <- paste0(inputId, "_scen", scen, "_strat", strat)
					tabPanel(
						title = SI_rvs$strata_labels[strat],
						matrixInput(
							inputId = matrix_id,
							value = default_matrix
						)
					)
				})
				tabPanel(
					title = SI_rvs$scenarios[[scen]]$label,
					do.call(tabsetPanel, inner_tabs)
				)
			})
			return(do.call(tabsetPanel, outer_tabs))
			
		} else {
			stop("Invalid option")
		}
	}
	
	# Pre-render the matrixInputs for each reward and for each option (1:4).
	# Here we assume that we have 10 possible rewards.
	for (r in 1:10) {
		local({
			rn <- r
			
			# Option 1: Generalized state reward matrix
			output[[paste0("matrix_ui_sr", rn, "_1")]] <- renderUI({
				req(in_msfit_desc(), SI_rvs, currentScenarioCount())
				createMatrixInput(
					inputId = paste0("matrix_sr_input_rn", rn, "_1"),
					option = 1,
					in_msfit_desc = in_msfit_desc(),
					currentScenarioCount = currentScenarioCount(),
					SI_rvs = SI_rvs,
					sr_tr = "sr"
				)
			})
			
			# Option 2: Vary per scenario with variable time windows per scenario.
			output[[paste0("matrix_ui_sr", rn, "_2")]] <- renderUI({
				req(in_msfit_desc(), SI_rvs, currentScenarioCount())
				createMatrixInput(
					inputId = paste0("matrix_sr_input_rn", rn, "_2"),
					option = 2,
					in_msfit_desc = in_msfit_desc(),
					currentScenarioCount = currentScenarioCount(),
					SI_rvs = SI_rvs,
					sr_tr = "sr"
				)
			})
			
			# Option 3: Vary per strata
			output[[paste0("matrix_ui_sr", rn, "_3")]] <- renderUI({
				req(in_msfit_desc(), SI_rvs, currentScenarioCount())
				createMatrixInput(
					inputId = paste0("matrix_sr_input_rn", rn, "_3"),
					option = 3,
					in_msfit_desc = in_msfit_desc(),
					currentScenarioCount = currentScenarioCount(),
					SI_rvs = SI_rvs,
					sr_tr = "sr"
				)
			})
			
			# Option 4: Vary per scenario and per strata (and time window) with variable time windows per scenario.
			output[[paste0("matrix_ui_sr", rn, "_4")]] <- renderUI({
				req(in_msfit_desc(), SI_rvs, currentScenarioCount())
				createMatrixInput(
					inputId = paste0("matrix_sr_input_rn", rn, "_4"),
					option = 4,
					in_msfit_desc = in_msfit_desc(),
					currentScenarioCount = currentScenarioCount(),
					SI_rvs = SI_rvs,
					sr_tr = "sr"
				)
			})
			
			#transitions
			
			# Option 1: Generalized transition reward matrix
			output[[paste0("matrix_ui_tr", rn, "_1")]] <- renderUI({
				req(in_msfit_desc(), SI_rvs, currentScenarioCount())
				createMatrixInput(
					inputId = paste0("matrix_tr_input_rn", rn, "_1"),
					option = 1,
					in_msfit_desc = in_msfit_desc(),
					currentScenarioCount = currentScenarioCount(),
					SI_rvs = SI_rvs,
					sr_tr = "tr"
				)
			})
			
			# Option 2: Vary per scenario with variable time windows per scenario.
			output[[paste0("matrix_ui_tr", rn, "_2")]] <- renderUI({
				req(in_msfit_desc(), SI_rvs, currentScenarioCount())
				createMatrixInput(
					inputId = paste0("matrix_tr_input_rn", rn, "_2"),
					option = 2,
					in_msfit_desc = in_msfit_desc(),
					currentScenarioCount = currentScenarioCount(),
					SI_rvs = SI_rvs,
					sr_tr = "tr"
				)
			})
			
			# Option 3: Vary per strata
			output[[paste0("matrix_ui_tr", rn, "_3")]] <- renderUI({
				req(in_msfit_desc(), SI_rvs, currentScenarioCount())
				createMatrixInput(
					inputId = paste0("matrix_tr_input_rn", rn, "_3"),
					option = 3,
					in_msfit_desc = in_msfit_desc(),
					currentScenarioCount = currentScenarioCount(),
					SI_rvs = SI_rvs,
					sr_tr = "tr"
				)
			})
			
			# Option 4: Vary per scenario and per strata (and time window) with variable time windows per scenario.
			output[[paste0("matrix_ui_tr", rn, "_4")]] <- renderUI({
				req(in_msfit_desc(), SI_rvs, currentScenarioCount())
				createMatrixInput(
					inputId = paste0("matrix_tr_input_rn", rn, "_4"),
					option = 4,
					in_msfit_desc = in_msfit_desc(),
					currentScenarioCount = currentScenarioCount(),
					SI_rvs = SI_rvs,
					sr_tr = "tr"
				)
			})
		})
	}
	
	
	
	observe({
		for(r in 1:10) {
			radio_input <- input[[paste0("sr_radioButtons_", r)]]
			
			# Hide all pre-rendered UI containers for reward r.
			hide(paste0("matrix_ui_sr", r, "_1"))
			hide(paste0("matrix_ui_sr", r, "_2"))
			hide(paste0("matrix_ui_sr", r, "_3"))
			hide(paste0("matrix_ui_sr", r, "_4"))
			
			# If no option is selected, do nothing.
			if (is.null(radio_input)) next
			
			# Show the container corresponding to the selected radio button option.
			if (radio_input == "State reward values generalize accross scenarios and strata") {
				show(paste0("matrix_ui_sr", r, "_1"))
			} else if (radio_input == "State reward values vary per scenario") {
				show(paste0("matrix_ui_sr", r, "_2"))
			} else if (radio_input == "State reward values vary per strata") {
				show(paste0("matrix_ui_sr", r, "_3"))
			} else if (radio_input == "State reward values vary per scenario and per strata") {
				show(paste0("matrix_ui_sr", r, "_4"))
			}
		}
	})
	
	observe({
		for(r in 1:10) {
			radio_input <- input[[paste0("tr_radioButtons_", r)]]
			
			# Hide all pre-rendered UI containers for reward r.
			hide(paste0("matrix_ui_tr", r, "_1"))
			hide(paste0("matrix_ui_tr", r, "_2"))
			hide(paste0("matrix_ui_tr", r, "_3"))
			hide(paste0("matrix_ui_tr", r, "_4"))
			
			# If no option is selected, do nothing.
			if (is.null(radio_input)) next
			
			# Show the container corresponding to the selected radio button option.
			if (radio_input == "Transition reward values generalize accross scenarios and strata") {
				show(paste0("matrix_ui_tr", r, "_1"))
			} else if (radio_input == "Transition reward values vary per scenario") {
				show(paste0("matrix_ui_tr", r, "_2"))
			} else if (radio_input == "Transition reward values vary per strata") {
				show(paste0("matrix_ui_tr", r, "_3"))
			} else if (radio_input == "Transition reward values vary per scenario and per strata") {
				show(paste0("matrix_ui_tr", r, "_4"))
			}
		}
	})
	
	
	
	#--------------------------------------------------------------------------------#
	### Save inputs for rewards ###
	#--------------------------------------------------------------------------------#
	observeEvent(input$proceed_to_final_check, {
		n_rewards <- currentRewardCount()
		n_scen    <- currentScenarioCount()
		n_strata  <- length(SI_rvs$strata_labels)
		
		#Expander + small helpers
		expand_matrix_dt <- function(dt) {
			rn <- dt$rowname
			dt2 <- dt[, !("rowname"), with = FALSE]
			nr <- nrow(dt2); nc <- ncol(dt2)
			cell_alts <- matrix(vector("list", nr*nc), nr, nc)
			max_alts <- 0
			for (i in seq_len(nr)) for (j in seq_len(nc)) {
				cell_val <- as.character(dt2[[j]][i])
				alts     <- trimws(unlist(strsplit(cell_val, ";")))
				cell_alts[[i,j]] <- alts
				max_alts <- max(max_alts, length(alts))
			}
			result <- vector("list", max_alts)
			for (k in seq_len(max_alts)) {
				new_mat <- matrix(NA, nrow=nr, ncol=nc)
				for (i in seq_len(nr)) for (j in seq_len(nc)) {
					alts <- cell_alts[[i,j]]
					new_mat[i,j] <- if (length(alts)>=k) alts[k] else alts[1]
				}
				new_dt <- as.data.table(new_mat)
				setnames(new_dt, names(dt2))
				result[[k]] <- cbind(rowname=rn, new_dt)
			}
			result
		}
		
		replicate_common <- function(mat) {
			dt <- as.data.table(
				matrix(rep(mat[,1], n_strata),
							 nrow=nrow(mat), ncol=n_strata)
			)
			setnames(dt, SI_rvs$strata_labels)
			dt <- cbind(rowname=rownames(mat), dt)
			expand_matrix_dt(dt)
		}
		
		replicate_scenario <- function(scenario_mat, tw) {
			col_vec <- scenario_mat[, tw, drop=FALSE]
			dt <- as.data.table(
				matrix(rep(col_vec[,1], n_strata),
							 nrow=nrow(scenario_mat), ncol=n_strata)
			)
			setnames(dt, SI_rvs$strata_labels)
			dt <- cbind(rowname=rownames(scenario_mat), dt)
			expand_matrix_dt(dt)
		}
		
		common_by_strata <- function(mat) {
			dt <- as.data.table(mat, keep.rownames="rowname")
			expand_matrix_dt(dt)
		}
		
		merge_option4 <- function(matrices, tw) {
			cols_list <- lapply(matrices, function(mat) mat[, tw, drop=FALSE])
			combined <- do.call(cbind, cols_list)
			dt <- as.data.table(combined, keep.rownames="rowname")
			setnames(dt, c("rowname", SI_rvs$strata_labels))
			expand_matrix_dt(dt)
		}
		
		
		# helper: build_variants() with input IDs 
		build_variants <- function(prefix, r, s) {
			# prefix is "sr" or "tr"
			opt <- input[[paste0(prefix, "_radioButtons_", r)]]
			scenario_tw <- SI_rvs$scenarios[[s]]$n_time_windows
			tw_list <- vector("list", scenario_tw)
			if (is.null(opt)) return(tw_list)
			
			# The option strings:
			if (prefix=="sr") {
				case1 <- "State reward values generalize accross scenarios and strata"
				case2 <- "State reward values vary per scenario"
				case3 <- "State reward values vary per strata"
				case4 <- "State reward values vary per scenario and per strata"
			} else {
				case1 <- "Transition reward values generalize accross scenarios and strata"
				case2 <- "Transition reward values vary per scenario"
				case3 <- "Transition reward values vary per strata"
				case4 <- "Transition reward values vary per scenario and per strata"
			}
			
			# build the exact matrix-xxx prefix used in the UI:
			base_id <- paste0("matrix_", prefix, "_input_rn", r)
			
			for (tw in seq_len(scenario_tw)) {
				if (opt == case1) {
					mat <- input[[ paste0(base_id, "_1") ]]
					tw_list[[tw]] <- replicate_common(mat)
					
				} else if (opt == case2) {
					mat <- input[[ paste0(base_id, "_2_scen", s) ]]
					tw_list[[tw]] <- replicate_scenario(mat, tw)
					
				} else if (opt == case3) {
					mat <- input[[ paste0(base_id, "_3") ]]
					tw_list[[tw]] <- common_by_strata(mat)
					
				} else if (opt == case4) {
					mats <- lapply(seq_len(n_strata), function(st) {
						input[[ paste0(base_id, "_4_scen", s, "_strat", st) ]]
					})
					tw_list[[tw]] <- merge_option4(mats, tw)
				}
			}
			
			tw_list
		}
		
		
		#find global max per reward (for variants)
		global_max_variants <- integer(n_rewards)
		for (s in seq_len(n_scen)) {
			for (r in seq_len(n_rewards)) {
				sr_tw <- build_variants("sr", r, s)
				tr_tw <- build_variants("tr", r, s)
				max_sr <- if (length(sr_tw)>0) max(sapply(sr_tw, length)) else 0
				max_tr <- if (length(tr_tw)>0) max(sapply(tr_tw, length)) else 0
				global_max_variants[r] <- max(global_max_variants[r], max_sr, max_tr)
			}
		}
		
		
		#rebuild, pad both sr & tr to that global max, then save 
		for (s in seq_len(n_scen)) {
			scenario_tw      <- SI_rvs$scenarios[[s]]$n_time_windows
			rewards_list     <- setNames(vector("list", n_rewards),
																	 paste0("reward", seq_len(n_rewards)))
			
			for (r in seq_len(n_rewards)) {
				sr_tw <- build_variants("sr", r, s)
				tr_tw <- build_variants("tr", r, s)
				max_var <- global_max_variants[r]
				
				for (tw in seq_len(scenario_tw)) {
					# pad state
					st_list <- sr_tw[[tw]]
					if (length(st_list) < max_var && length(st_list)>0) {
						st_list <- c(st_list,
												 rep(list(st_list[[1]]),
												 		max_var - length(st_list)))
					}
					sr_tw[[tw]] <- setNames(st_list,
																	paste0("variant", seq_len(max_var)))
					
					# pad transition
					tr_list <- tr_tw[[tw]]
					if (length(tr_list) < max_var && length(tr_list)>0) {
						tr_list <- c(tr_list,
												 rep(list(tr_list[[1]]),
												 		max_var - length(tr_list)))
					}
					tr_tw[[tw]] <- setNames(tr_list,
																	paste0("variant", seq_len(max_var)))
				}
				
				rewards_list[[r]] <- list(
					reward_id    = r,
					reward_label = input[[paste0("Reward_label_", r)]],
					sr           = setNames(sr_tw,
																	paste("time window", seq_len(scenario_tw))),
					tr           = setNames(tr_tw,
																	paste("time window", seq_len(scenario_tw)))
				)
			}
			
			SI_rvs$scenarios[[s]]$rewards <- rewards_list
		}
		
		
		# Final rename + UI switch
		SI_rvs$scenarios <- setNames(SI_rvs$scenarios,
																 paste("scenario", seq_len(n_scen)))
		runjs('$("#user_specifications_tabset a:contains(\'C. Review & Confirm\')").click();')
	})
	
	
	
	
	#--------------------------------------------------------------------------------#
	#### Calculate outputs ####
	#--------------------------------------------------------------------------------#
	
	SO_rvs <- reactiveValues()
	
	observeEvent(input$calculate_scenario, {
		req(in_file_msfit())
		SI_rvs$mats <- NULL
		saved <- reactiveValuesToList(SI_rvs)
		saveRDS(saved, file = "inputs.rds")
		
		updateNavbarPage(inputId = "main_nav", selected = "outputs_panel")
		
		# pop up the modal with a fresh bar
		showModal(modalDialog(
			title    = "Running scenario…",
			tags$style(HTML("
      .progress-group .progress-number {
        display: none !important;
      }
    ")),
			progressBar(
				id          = "scenario_pb",
				value       = 0,
				total       = 100,
				display_pct = TRUE,
				title       = "Starting…"
			),
			footer   = NULL,
			easyClose = FALSE
		))
		
		
		# callback only updates the bar in the modal
		pr <- function(percent, detail = NULL) {
			updateProgressBar(session, "scenario_pb", value = percent, title = detail)
		}
		
		rescaled_msfits <- rescale_msfits(
			inputs     = reactiveValuesToList(SI_rvs),
			msfit_list = in_file_msfit(),
			progress_cb = pr
		)
		
		# run the main function, with updates
		state_trans_res <- state_trans_res(
			inputs          = reactiveValuesToList(SI_rvs),
			rescaled_msfits = rescaled_msfits,
			progress_cb     = pr
		)
		
		SO_rvs$state_trans_res <- state_trans_res
		
		if(org_save == TRUE){
			saveRDS(state_trans_res, file = "state_trans_res_org.rds")
		}
		if(org_save == FALSE){
			saveRDS(state_trans_res, file = "state_trans_res.rds")
		}
		
		# final 100% and close modal
		updateProgressBar(session, "scenario_pb", value = 100, title = "Done!")
		removeModal()
		
		# calculate rewards
		reward_results <- calculate_rewards(
			inputs       = reactiveValuesToList(SI_rvs),
			results_list = state_trans_res,
			pop_size = input$tot_pop_size
		)
		SO_rvs$reward_results <- reward_results
		
		tc <- compute_transition_counts(inputs       = reactiveValuesToList(SI_rvs),
																		results_list = state_trans_res,
																		pop_size = input$tot_pop_size)
		SO_rvs$tc <- tc
		
		# browser()
		saveRDS(reward_results, file = "reward_results.rds")
		saveRDS(tc, file = "transition_counts.rds")
	})
	
	
	
	
	
	#--------------------------------------------------------------------------------#
	#### Table form DESCRIPTIVE results of rewards per scenario
	#--------------------------------------------------------------------------------#
	
	# Build the full rewards table + Totals
	raw_reward_df <- reactive({
		req(SO_rvs$reward_results)
		rr <- SO_rvs$reward_results
		
		# flatten State + Transition into a single data.table
		df <- data.table::rbindlist(
			lapply(names(rr), function(scn) {
				data.table::rbindlist(
					lapply(names(rr[[scn]]), function(strata) {
						data.table::rbindlist(
							lapply(names(rr[[scn]][[strata]]), function(rew) {
								obj <- rr[[scn]][[strata]][[rew]]
								# helper to gather one reward type
								gather <- function(type_name, type_lbl) {
									data.table::rbindlist(
										lapply(names(obj[[type_name]]), function(win) {
											vals <- obj[[type_name]][[win]]
											data.table::data.table(
												Scenario = scn,
												Strata   = strata,
												Reward   = rew,
												Window   = win,
												Type     = type_lbl,
												Variant  = names(vals),
												Value    = round(unlist(vals), 3)
											)
										}), use.names = TRUE
									)
								}
								# bind State + Transition
								data.table::rbindlist(
									list(
										gather("state_rewards",      "State"),
										gather("transition_rewards", "Transition")
									),
									use.names = TRUE
								)
							}), use.names = TRUE
						)
					}), use.names = TRUE
				)
			}), use.names = TRUE
		)
		
		# compute Totals 
		totals <- df[, .(Value = sum(Value)),
								 by = .(Scenario, Strata, Reward, Window, Variant)]
		totals[, Type := "Total"]
		# browser()
		data.table::rbindlist(list(df, totals), use.names = TRUE)
	})
	
	# Simple vs. detailed filters
	simple_reward_df <- reactive({
		raw_reward_df()[Strata == "pooled" &
											Window == "all"   &
											Type   %in% c("State", "Transition", "Total")]
	})
	
	detailed_reward_df <- reactive({
		dt <- raw_reward_df()[Strata != "pooled" &
														Window != "all"   &
														Type   != "Total"]
		# drop duplicates that differ only by Variant
		# unique(dt, by = c("Scenario","Window","Value","Strata","Reward","Type"))
	})
	
	# Toggle view
	observeEvent(input$simple_detailed, {
		if (isTRUE(input$simple_detailed)) {
			shinyjs::show("simple_table_descriptive")
			shinyjs::hide("detailed_tables_descriptive")
		} else {
			shinyjs::hide("simple_table_descriptive")
			shinyjs::show("detailed_tables_descriptive")
		}
	})
	
	
	output$descriptive_table_simple <- renderReactable({
		# base long data
		dt <- as.data.table(simple_reward_df())[Type == "Total"]
		scenarios <- sort(unique(dt$Scenario))
		
		# build flat + indented nested table
		df_nested <- rbindlist(
			lapply(unique(dt$Reward), function(r) {
				subr <- dt[Reward == r]
				# pivot the variants for this reward to wide
				variant_wide <- dcast(subr, Variant ~ Scenario, value.var = "Value")
				
				# header row for the Reward
				header <- data.table(Metric = r)
				for (s in scenarios) header[[s]] <- "" 
				header[, is_header := TRUE]
				
				# the Variant rows, indented
				variants <- copy(variant_wide)
				variants[, Metric := paste0("\u00A0\u00A0", Variant)]
				variants[, Variant := NULL]  # drop original
				variants[, is_header := FALSE]
				# ensure column order: Metric, all scenarios, is_header
				setcolorder(variants, c("Metric", scenarios, "is_header"))
				
				# bind reward header + its variants
				rbindlist(list(header, variants), use.names = TRUE, fill = TRUE)
			}),
			use.names = TRUE, fill = TRUE
		)
		
		# render as reactable
		reactable(
			df_nested,
			columns = list(
				Metric    = colDef(
					name  = "Reward / Variant",
					style = function(value, index) {
						if (df_nested$is_header[index]) {
							list(fontWeight = "bold")
						}
					}
				),
				is_header = colDef(show = FALSE)
			),
			# format every other column (all scenarios) to 3 decimals
			defaultColDef = colDef(format = colFormat(digits = 3)),
			compact    = TRUE,
			striped    = TRUE,
			highlight  = TRUE,
			filterable = FALSE,
			pagination = TRUE
		)
	})
	
	
	
	
	
	
	
	output$descriptive_detailed_ui <- renderUI({
		df    <- detailed_reward_df()
		scns  <- unique(df$Scenario)
		if (length(scns) == 0) return(p("No data available"))
		tabs <- lapply(scns, function(scn) {
			id <- paste0("detailed_tab_", gsub("\\W+","_", scn))
			tabPanel(title = scn, DT::dataTableOutput(id))
		})
		do.call(tabsetPanel, c(list(id = "detailed_panels", type = "tabs"), tabs))
	})
	
	
	
	observe({
		df <- detailed_reward_df()
		for (scn in unique(df$Scenario)) {
			id <- paste0("detailed_tab_", gsub("\\W+","_", scn))
			local({
				this_scn <- scn; this_id <- id
				output[[this_id]] <- DT::renderDataTable({
					subdf <- df[Scenario == this_scn]
					DT::datatable(
						subdf,
						rownames  = FALSE,
						filter    = "top",
						selection = "multiple",
						options   = list(pageLength = 10, autoWidth = TRUE)
					)
				})
			})
		}
	})
	
	
	
	#--------------------------------------------------------------------------------#
	#### Table form COMPARATIVE results of rewards per scenario
	#--------------------------------------------------------------------------------#
	#simple 
	output$simple_comp_nested_ui <- renderUI({
		# build the pair names directly
		dt    <- as.data.table(simple_reward_df())[Type == "Total"]
		scen  <- unique(dt$Scenario)
		
		if (length(scen) < 2) {
			return(p("You only specified one scenario. Comparative results are NA."))
		}
		
		pairs <- combn(scen, 2, simplify = FALSE)
		
		tabs <- lapply(pairs, function(pb) {
			nm <- paste(pb, collapse = " vs. ")
			tabPanel(nm, reactableOutput(paste0("simple_cmp_nested_", nm)))
		})
		
		do.call(
			tabsetPanel,
			c(list(id = "simple_comp_nested_tabs", type = "tabs"), tabs)
		)
	})
	
	#detailed
	output$detailed_comp_nested_ui <- renderUI({
		# build the pair names directly
		dt_raw <- as.data.table(raw_reward_df())
		dt     <- dt_raw[Window == "all", .(Scenario, Strata, Reward, Variant, Type, Value)]
		scen   <- unique(dt$Scenario)
		if (length(scen) < 2) {
			return(p("You only specified one scenario. Comparative results are NA."))
		}
		pairs  <- combn(scen, 2, simplify = FALSE)
		if (length(pairs) == 0) return(p("No comparisons available"))
		
		tabs <- lapply(pairs, function(pb) {
			nm <- paste(pb, collapse = " vs. ")
			tabPanel(nm, reactableOutput(paste0("detailed_cmp_nested_", nm)))
		})
		
		do.call(
			tabsetPanel,
			c(list(id = "detailed_comp_nested_tabs", type = "tabs"), tabs)
		)
	})
	
	
	#simple 	
	observe({
		# build comparison “cmp” 
		dt <- as.data.table(simple_reward_df())[Type == "Total"]
		scen <- unique(dt$Scenario)
		
		if (length(scen) < 2) {
			output$simple_cmp_nested_placeholder <- renderText(
				"You only specified one scenario. Comparative results are NA."
			)
			return()
		}
		
		pairs <- combn(scen, 2, simplify = FALSE)
		cmp <- rbindlist(lapply(pairs, function(pb) {
			A <- pb[1]; B <- pb[2]
			a <- dt[Scenario == A, .(Reward, Variant, ValueA = Value)]
			b <- dt[Scenario == B, .(Reward, Variant, ValueB = Value)]
			m <- merge(a, b, by = c("Reward","Variant"))
			m[, `:=`(
				ScenarioA = A,
				ScenarioB = B,
				Diff      = round(ValueB - ValueA, 3),
				Pair      = paste(A, B, sep = " vs. ")
			)]
			m[, .(Pair, ScenarioA, ScenarioB, Reward, Variant, ValueA, ValueB, Diff)]
		}), use.names = TRUE, fill = TRUE)
		
		# for each pair, build & render flat+indented table
		for (p in unique(cmp$Pair)) {
			local({
				pair <- p
				df0  <- cmp[Pair == pair]
				out  <- paste0("simple_cmp_nested_", pair)
				
				# flatten + indent Reward to Variant
				df_nested <- rbindlist(
					lapply(unique(df0$Reward), function(r) {
						subr <- df0[Reward == r]
						header <- data.table(
							Metric    = r,
							ScenarioA = "", ScenarioB = "", ValueA = "", ValueB = "", Diff = "",
							is_header = TRUE
						)
						variants <- subr[, .(
							Metric    = paste0("\u00A0\u00A0", Variant),
							ScenarioA, ScenarioB, ValueA, ValueB, Diff,
							is_header = FALSE
						)]
						rbindlist(list(header, variants), use.names = TRUE, fill = TRUE)
					}),
					use.names = TRUE,
					fill      = TRUE
				)
				
				output[[out]] <- renderReactable({
					reactable(
						df_nested,
						columns = list(
							Metric    = colDef(
								name  = "Reward / Variant",
								style = function(value, index) {
									if (df_nested$is_header[index]) list(fontWeight = "bold")
								}
							),
							ScenarioA = colDef(name = "Scenario A"),
							ScenarioB = colDef(name = "Scenario B"),
							ValueA    = colDef(name = "Value A",    format = colFormat(digits = 3)),
							ValueB    = colDef(name = "Value B",    format = colFormat(digits = 3)),
							Diff      = colDef(name = "Difference", format = colFormat(digits = 3)),
							is_header = colDef(show = FALSE)
						),
						compact    = TRUE,
						striped    = TRUE,
						highlight  = TRUE,
						filterable = FALSE,
						pagination = FALSE
					)
				})
			})
		}
	})
	
	
	#detailed	
	observe({
		# build comparison “cmp” 
		dt_raw <- as.data.table(raw_reward_df())
		dt     <- dt_raw[Window == "all", .(Scenario, Strata, Reward, Variant, Type, Value)]
		scen   <- unique(dt$Scenario)
		
		if (length(scen) < 2) {
			output$detailed_cmp_nested_placeholder <- renderText(
				"You only specified one scenario. Comparative results are NA."
			)
			return()
		}
		
		pairs  <- combn(scen, 2, simplify = FALSE)
		cmp <- rbindlist(lapply(pairs, function(pb) {
			A <- pb[1]; B <- pb[2]
			a <- dt[Scenario == A, .(Strata, Reward, Variant, Type, ValueA = Value)]
			b <- dt[Scenario == B, .(Strata, Reward, Variant, Type, ValueB = Value)]
			m <- merge(a, b, by = c("Strata","Reward","Variant","Type"))
			m[, `:=`(
				ScenarioA = A,
				ScenarioB = B,
				Diff      = round(ValueB - ValueA, 3),
				Pair      = paste(A, B, sep = " vs. ")
			)]
			m[, .(
				Pair, ScenarioA, ScenarioB,
				Strata, Reward, Variant, Type,
				ValueA, ValueB, Diff
			)]
		}), use.names = TRUE, fill = TRUE)
		
		# for each pair, build & render flat+indented table
		for (p in unique(cmp$Pair)) {
			local({
				pair <- p
				df0  <- cmp[Pair == pair]
				out  <- paste0("detailed_cmp_nested_", pair)
				
				# flatten + indent Strata by Reward by Variant by Type
				df_nested <- rbindlist(
					lapply(unique(df0$Strata), function(s) {
						strata_df <- df0[Strata == s]
						header_s <- data.table(
							Metric    = s,
							ScenarioA = "", ScenarioB = "", ValueA = "", ValueB = "", Diff = "",
							is_header = TRUE
						)
						reward_block <- rbindlist(
							lapply(unique(strata_df$Reward), function(r) {
								r_df <- strata_df[Reward == r]
								header_r <- data.table(
									Metric    = paste0("\u00A0", r),
									ScenarioA = "", ScenarioB = "", ValueA = "", ValueB = "", Diff = "",
									is_header = TRUE
								)
								variant_block <- rbindlist(
									lapply(unique(r_df$Variant), function(v) {
										v_df <- r_df[Variant == v]
										header_v <- data.table(
											Metric    = paste0("\u00A0\u00A0", v),
											ScenarioA = "", ScenarioB = "", ValueA = "", ValueB = "", Diff = "",
											is_header = TRUE
										)
										type_rows <- v_df[, .(
											Metric    = paste0("\u00A0\u00A0\u00A0\u00A0", Type),
											ScenarioA, ScenarioB, ValueA, ValueB, Diff,
											is_header = FALSE
										)]
										rbindlist(list(header_v, type_rows),
															use.names = TRUE, fill = TRUE)
									}),
									use.names = TRUE, fill = TRUE
								)
								rbindlist(list(header_r, variant_block),
													use.names = TRUE, fill = TRUE)
							}),
							use.names = TRUE, fill = TRUE
						)
						rbindlist(list(header_s, reward_block),
											use.names = TRUE, fill = TRUE)
					}),
					use.names = TRUE, fill = TRUE
				)
				
				output[[out]] <- renderReactable({
					reactable(
						df_nested,
						columns = list(
							Metric    = colDef(
								name  = "Strata / Reward / Variant / Type",
								style = function(value, index) {
									if (df_nested$is_header[index]) list(fontWeight = "bold")
								}
							),
							ScenarioA = colDef(name = "Scenario A"),
							ScenarioB = colDef(name = "Scenario B"),
							ValueA    = colDef(name = "Value A",    format = colFormat(digits = 3)),
							ValueB    = colDef(name = "Value B",    format = colFormat(digits = 3)),
							Diff      = colDef(name = "Difference", format = colFormat(digits = 3)),
							is_header = colDef(show = FALSE)
						),
						compact    = TRUE,
						striped    = TRUE,
						highlight  = TRUE,
						filterable = FALSE,
						pagination = FALSE
					)
				})
			})
		}
	})
	
	# toggle summary vs detailed comparative
	observeEvent(input$simple_detailed_comp, {
		if (isTRUE(input$simple_detailed_comp)) {
			shinyjs::show("simple_tables_comp")
			shinyjs::hide("detailed_tables_comp")
		} else {
			shinyjs::hide("simple_tables_comp")
			shinyjs::show("detailed_tables_comp")
		}
	})
	
	
	
	#--------------------------------------------------------------------------------#
	#### UI for viewing each trace. 
	#--------------------------------------------------------------------------------#
	
	
	observeEvent(SO_rvs$state_trans_res, {
		scenario_labels <- unname(sapply(SI_rvs$scenarios, `[[`, "label"))
		strata_labels   <- SI_rvs$strata_labels
		
		output$filter_card <- renderUI({
			card(
				card_header(strong("Transition counts")),
				fluidRow(
					column(
						width = 3,
						selectInput(
							"scenario_plot", label = NULL,
							choices = setNames(seq_along(scenario_labels), scenario_labels)
						)
					),
					column(
						width = 3,
						selectInput(
							"strata_plot", label = NULL,
							choices  = setNames(seq_along(c(strata_labels, "pooled")), c(strata_labels, "pooled"))
						)
					)
				),
				# then the plot below
				plotlyOutput("trace_plot")
			)
		})
	})
	
	
	filtered_data <- reactive({
		req(input$scenario_plot, input$strata_plot, SO_rvs$state_trans_res)
		
		i_scn <- as.integer(input$scenario_plot)
		i_strata <- as.integer(input$strata_plot)
		
		res_scn <- SO_rvs$state_trans_res[[ i_scn ]]
		df <- res_scn[[ i_strata ]]$state_probabilities
		df
	})
	
	
	output$trace_plot <- renderPlotly({
		dt <- filtered_data()
		req(dt)
		
		data_long <- melt(
			dt,
			id.vars       = "time",
			variable.name = "State",
			value.name    = "Value"
		)
		
		p <- ggplot(data_long, aes(time, Value, colour = State)) +
			geom_line(size = 0.5) +
			scale_y_continuous(limits = range(data_long$Value)) +
			labs(
				title  = "State occupation over time",
				x      = "Time",
				y      = "Occupancy prop.",
				colour = "State"
			) +
			theme_minimal() +
			theme(
				plot.title   = element_text(size = 14),
				axis.title.x = element_text(size = 12),
				axis.title.y = element_text(size = 12),
				axis.text.x  = element_text(size = 12),
				axis.text.y  = element_text(size = 12),
				legend.text  = element_text(size = 10)
			)
		ggplotly(p) %>%
			layout(legend = list(
				orientation = "v",    # vertical — this re-enables scrolling
				x           = 1,   # push it just past the right edge
				y           = 1   # align with top of plotting area
				
			))
	})
	

	
	
	#--------------------------------------------------------------------------------#
	#### Render transition counts table 
	#--------------------------------------------------------------------------------#
	
	output$scenario_picker_counts <- renderUI({
		labs <- if (length(SI_rvs$scenarios)) unname(sapply(SI_rvs$scenarios, `[[`, "label")) else character(0)
		selectInput(
			"scenario_filter_counts", "Scenario",
			choices  = labs,
			selected = if (length(labs)) labs[[1]] else NULL
		)
	})
	
	output$transition_counts <- reactable::renderReactable({
		req(SO_rvs$tc$counts_table)
		
		df <- as.data.frame(SO_rvs$tc$counts_table, optional = TRUE, stringsAsFactors = FALSE)
		
		scen_prefix <- paste0(input$scenario_filter_counts, " | ")
		scen_cols   <- names(df)[startsWith(names(df), scen_prefix)]
		
		out <- df[, c("transition", scen_cols), drop = FALSE]
		names(out)[-1] <- sub("^.*\\|\\s*", "", names(out)[-1])
		
		all_cols <- setdiff(names(out), "transition")
		col_defs <- c(
			list(transition = reactable::colDef(name = "Transition", sticky = "left")),
			setNames(lapply(all_cols, function(col) {
				reactable::colDef(name = col, align = "right",
													format = reactable::colFormat(digits = 2, separators = TRUE))
			}), all_cols)
		)
		
		reactable::reactable(
			out,
			columns      = col_defs,
			columnGroups = list(reactable::colGroup(name = input$scenario_filter_counts, columns = all_cols)),
			searchable   = F,
			highlight    = TRUE,
			bordered     = TRUE,
			resizable    = TRUE,
			showPageSizeOptions = T,
			paginationType  = "simple",
			wrap         = FALSE
		)
	})
	
	#--------------------------------------------------------------------------------#
	#### temporary UI for viewing each trace table. 
	#--------------------------------------------------------------------------------#
	
	final_state_probs_df <- reactive({
		req(SO_rvs$state_trans_res)
		rows <- list()
		cnt  <- 1
		
		# loop over scenarios and strata
		for(scn in names(SO_rvs$state_trans_res)) {
			for(strata in names(SO_rvs$state_trans_res[[scn]])) {
				sp       <- as.data.frame(SO_rvs$state_trans_res[[scn]][[strata]]$state_probabilities)
				last_row <- tail(sp, 1)
				
				# drop the time column
				last_row$time <- NULL
				
				# add identifiers
				last_row$Scenario <- scn
				last_row$Strata   <- strata
				
				rows[[cnt]] <- last_row
				cnt <- cnt + 1
			}
		}
		
		df <- do.call(rbind, rows)
		
		# reorder so Scenario/Strata come first
		df[, c("Scenario","Strata", setdiff(names(df), c("Scenario","Strata")))]
	})
	
	## 2) Expose it as a DT
	output$final_state_probs <- renderDataTable({
		datatable(
			final_state_probs_df(),
			rownames  = FALSE,
			filter    = "top",
			options   = list(pageLength = 10, autoWidth = TRUE)
		)
	})
	
	
	#--------------------------------------------------------------------------------#
	#### Report downloads
	#--------------------------------------------------------------------------------#
	# Populate the dropdown once results exist 
	observeEvent(SO_rvs$reward_results, {
		updateSelectInput(
			session, "report_scenario",
			choices  = names(SI_rvs$scenarios),
			selected = names(SI_rvs$scenarios)[1]
		)
	})
	
	# The downloadHandler 
	output$download_scenario_report <- downloadHandler(
		filename = function() {
			paste0("Scenario_", input$report_scenario, ".html")
		},
		content = function(file) {
			# copy template into tempdir
			tempRmd <- file.path(tempdir(), "report_scenario.Rmd")
			file.copy("report_scenario.Rmd", tempRmd, overwrite = TRUE)
			
			# collect global inputs
			global_inputs <- list(
				strata_labels  = SI_rvs$strata_labels,
				strata_weights = SI_rvs$strata_weights,
				state_dist     = SI_rvs$df_in_state_dist_strata
			)
			
			# pick the scenario
			scen_name  <- input$report_scenario
			scenario   <- SI_rvs$scenarios[[scen_name]]
			reward_tbl <- raw_reward_df()  
			trace_list <- SO_rvs$state_trans_res[[scen_name]]
			
			# render HTML report
			rmarkdown::render(
				input         = tempRmd,
				output_file   = file,
				params        = list(
					global_inputs = global_inputs,
					scenario      = scenario,
					reward_df     = reward_tbl,
					trace_list    = trace_list
				),
				envir         = new.env(parent = globalenv()),
				output_format = "html_document"
			)
		},
		contentType = "text/html"
	)
	
	
	
}

# RUN APP
shinyApp(ui, server)			

