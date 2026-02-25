

# Prep options for noUiSlider
prep_t_range <- function(P, n) {
	
	# Compute the desired endpoints (ideal equally spaced values)
	desired_endpoints <- seq(0, max(P), length.out = n + 1)
	
	# For each desired endpoint, find the closest actual value in P
	actual_endpoints <- sapply(desired_endpoints, function(x) {
		# Identify the index in P that minimizes the difference with x
		index <- which.min(abs(P - x))
		return(P[index])
	})
	
	# Ensure that the lowest endpoint is exactly 0
	actual_endpoints[1] <- 0
	
	return(actual_endpoints)
}



#prep step param for noUiSlider
prepare_step <- function(P) {
	
	if (!isTRUE(all.equal(min(P), 0))) P <- c(0, P) #first must be 0 because need to start with t = 0 
	
	# Calculate the total range of P
	total_range <- max(P)
	
	# Compute the percentage position for each value in P
	percents <- (P - min(P)) / total_range * 100
	
	# Generate names: "min" for the smallest, "max" for the largest,
	# and the intermediate ones as rounded percentages with a "%" sign.
	names_val <- c("min", paste0(round(percents[-c(1, length(P))], 5), "%"), "max")
	
	
	# Calculate the differences between consecutive values
	diffs <- c(0, diff(P))
	
	# Create the list: each element is a vector of two values:
	# the value from P and its difference from the preceding value.
	result <- mapply(function(p, d) c(p, d), P, diffs, SIMPLIFY = FALSE)
	
	# Assign names to the list elements
	names(result) <- names_val
	
	return(result)
}








# Reward missing error
createRewardMissingUI <- function(SI_reactiveValues, input_keys) {
	renderUI({
		# Check that SI_reactiveValues exists and contains no missing values
		req(SI_reactiveValues)
		
		# Convert SI_reactiveValues to a list
		values_list <- reactiveValuesToList(SI_reactiveValues)
		
		# Subset the list to include only specified keys
		filtered_values <- values_list[input_keys]
		
		# Check for missing values across the filtered elements
		has_missing_values <- any(sapply(filtered_values, function(value) any(is.na(value))))
		
		if (has_missing_values) {
			HTML('<span style="color:red; font-family: monospace; background-color:#f8d7da;">One or more input values are missing, every cell must contain a value in order to proceed.</span>')
		} else {
			HTML("")
		}
	})
}



gen_diagram <- function(m_t_desc) {
	trans_matrix <- m_t_desc
	
	# Extract edges from the transition matrix
	edges <- which(trans_matrix >= 1, arr.ind = TRUE)
	edges <- data.frame(
		from = rownames(trans_matrix)[edges[, 1]],
		to = colnames(trans_matrix)[edges[, 2]],
		label = as.character(trans_matrix[edges])  # Extract transition numbers as labels
	)
	
	# Create unique node IDs by replacing spaces with underscores
	node_ids <- unique(c(edges$from, edges$to))
	node_aliases <- gsub("[ .\\-]", "_", node_ids)
	
	# Wrap node labels if they exceed 15 characters
	wrap_label <- function(label) {
		paste(strwrap(label, width = 20), collapse = "\\n")
	}
	wrapped_labels <- sapply(node_ids, wrap_label)
	
	# Map IDs to wrapped labels (original names)
	node_map <- data.frame(id = node_aliases, label = wrapped_labels, stringsAsFactors = FALSE)
	
	# Assign state numbers based on column positions in m_t_desc
	state_numbers <- setNames(seq_along(colnames(trans_matrix)), colnames(trans_matrix))
	
	for (i in seq_along(node_map$id)) {
		state_num <- state_numbers[names(state_numbers) == node_ids[i]]
		if (length(state_num) > 0) {
			node_map$label[i] <- paste0("S", state_num, ": ", node_map$label[i])
		}
	}
	
	# Identify the starting state (first column in trans_matrix)
	starting_state <- gsub("[ .\\-]", "_", colnames(trans_matrix)[1])  
	
	# Generate DOT code for grViz
	dot_code <- "digraph G {
  rankdir=LR;

  # several 'node' statements
  node [shape = box,
        style = 'rounded,filled',
        fontname = \"Calibri\",
        fontsize = 10,
        fillcolor = Honeydew,
        penwidth = 0.7
        ]
  
  edge [arrowsize = 0.5,
        penwidth = 0.7,
        fontname = \"Calibri\",
				fontsize = 10]
"
	
	# Force the starting state to the leftmost position
	dot_code <- paste0(dot_code, "  {rank=source; ", starting_state, ";}\n")
	
	# Add node definitions
	for (i in 1:nrow(node_map)) {
		dot_code <- paste0(dot_code, "  ", node_map$id[i], " [label=\"", node_map$label[i], "\"];\n")
	}
	
	# Add edges with transition numbers
	edges$from <- gsub("[ .\\-]", "_", edges$from)
	edges$to <- gsub("[ .\\-]", "_", edges$to)
	for (i in 1:nrow(edges)) {
		dot_code <- paste0(dot_code, "  ", edges$from[i], " -> ", edges$to[i], " [label=\"Tr. ", edges$label[i], "\", fontname=\"Calibri\"];\n")
	}
	
	# Close the DOT graph definition
	dot_code <- paste0(dot_code, "}\n")
	
	# Return the DOT code, to be used in grViz()
	return(grViz(dot_code))
}



.null_progress <- function(...) invisible(NULL)

#function to produce the rescaled msfit objects that will go into "probtrans_fusion"

rescale_msfits <- function(inputs, msfit_list, progress_cb = .null_progress){
	
	progress_cb(5,  "Applying scaling factors to msfit objects...")
	
	n_strata <- length(inputs$strata_weights) #number of strata
	strata_labels <- unname(inputs$strata_labels) #strata labels
	n_scen <- length(inputs$scenarios) #number of scenarios
	trans <- msfit_list[[1]]$trans #matrix of allowed transitions
	state_names <- colnames(trans)
	transit <- to.trans2(trans) #special object needed later (and in probtrans)
	numtrans <- nrow(transit) # n transitions
	trans_names <- transit$transname
	scaled_msfits <- vector("list", n_scen)	# Initialize the overall results list

	for(scen in 1:n_scen){
		scenario_msfits <- vector("list", n_strata) 		# Prepare a list to store outputs for each strata within the scenario
		scen_n_tws <- inputs$scenarios[[scen]]$n_time_windows
		scen_tws <- inputs$scenarios[[scen]]$time_window_durations
		
		for(strata in 1:n_strata){
			msfit_strat_tmp <- msfit_list[[strata]]
			stackhaz <- msfit_strat_tmp$Haz
			for (i in 1:numtrans) stackhaz$dhaz[stackhaz$trans == i] <- diff(c(0, stackhaz$Haz[stackhaz$trans == i])) #inc HAZARDS
			stackhaz$dhaz_scaled <- stackhaz$dhaz 
			stackhaz$factor <- 1
			stackhaz$tw <- findInterval(stackhaz$time, inputs$scenarios[[scen]]$time_window_durations, rightmost.closed = TRUE)
			for(tw in 1:scen_n_tws){
				incl_trans_nums <- which(transit$transname %in% colnames(inputs$scenarios[[scen]]$time_windows[[tw]]$matrix))
				for(i in incl_trans_nums){
					trans_label <- transit$transname[transit$transno == i]
					stackhaz$factor[stackhaz$trans == i & stackhaz$tw == tw] <- inputs$scenarios[[scen]]$time_windows[[tw]]$matrix[strata,trans_label]
				}
			}
			stackhaz$dhaz_scaled <- as.numeric(stackhaz$factor)*stackhaz$dhaz  
			stackhaz$scaled <- stackhaz$Haz
			
			for(i in 1:numtrans){
				stackhaz$scaled[stackhaz$trans == i] <- cumsum(stackhaz$dhaz_scaled[stackhaz$trans==i]) 
			}
			

			stackhaz <- stackhaz[,c("time", "trans", "scaled")]
			colnames(stackhaz) <- c("time", "trans", "Haz")

			
			msfit_strat_tmp$Haz <- stackhaz	
			
			scenario_msfits[[strata]] <- msfit_strat_tmp	
	
		}
		scaled_msfits[[scen]] <- scenario_msfits
	}
	names(scaled_msfits) <- vapply(inputs$scenarios, `[[`, character(1), "label")
	progress_cb(20, "Rescaling complete")
	return(scaled_msfits)
	}





###### Modified probtrans for FUSION Tool. It additionally returns the transition dynamics.
# scaled_msfits <- rescale_msfits(msfit_list = msfit_list, inputs = inputs)
# object <- scaled_msfits[[1]][[1]] 
# direction = "forward"
# predt = 0
# method_pt = "exp"
# method = "aalen"
# variance = F
# covariance = F
probtrans_fusion <- function (object, predt, direction = c("forward", "fixedhorizon"), 
															method_pt = c("prodlim", "exp"), method = c("aalen", "greenwood"), 
															variance = FALSE, covariance = FALSE) 
{
	# if (!inherits(object, "msfit")) 
	# 	stop("'object' must be a 'msfit' object")
	# method_pt <- match.arg(method_pt)
	# method <- match.arg(method)
	# direction <- match.arg(direction)
	trans <- object$trans
	transit <- to.trans2(trans)
	numtrans <- nrow(transit)
	# variance_boot <- FALSE
	# if ("Haz.boot" %in% names(object)) {
	# 	covariance <- FALSE
	# 	if (variance) {
	# 		variance <- FALSE
	# 		variance_boot <- TRUE
	# 	}
	# }
	stackhaz <- object$Haz
	stackvarhaz <- object$varHaz
	for (i in 1:numtrans) stackhaz$dhaz[stackhaz$trans == i] <- diff(c(0, 
																																		 stackhaz$Haz[stackhaz$trans == i]))
	if (direction == "forward") 
		stackhaz <- stackhaz[stackhaz$time > predt, ]
	# else stackhaz <- stackhaz[stackhaz$time <= predt, ]
	untimes <- sort(unique(stackhaz$time))
	TT <- length(untimes)
	S <- nrow(trans)
	# if (covariance & method_pt == "exp") {
	# 	warning("no covariances calculated for method_pt='exp'")
	# 	covariance <- FALSE
	# }
	# if (variance & method_pt == "exp") {
	# 	warning("no variances calculated for method_pt='exp'")
	# 	variance <- FALSE
	# }
	# if (covariance) 
	# 	variance <- TRUE
	if (direction == "forward") {
		if (variance == TRUE) 
			res <- array(0, c(TT + 1, 2 * S + 1, S))
		else res <- array(0, c(TT + 1, S + 1, S))
		res_trans <- array(0, c(TT + 1, numtrans + 1, S))
		res[1, 1, ] <- res_trans[1, 1, ] <- predt
		for (j in 1:S) res[1, 1 + j, ] <- rep(c(0, 1, 0), c(j - 
																													1, 1, S - j))
		# if (variance) 
		# 	res[1, (S + 2):(2 * S + 1), ] <- 0
	}
	# else {
	# 	if (predt %in% untimes) {
	# 		if (variance)
	# 			res <- array(0, c(TT + 1, 2 * S + 1, S))
	# 		else res <- array(0, c(TT + 1, S + 1, S))
	# 		res[TT + 1, 1, ] <- predt
	# 		for (j in 1:S) res[TT + 1, 1 + j, ] <- rep(c(0, 1,
	# 																								 0), c(j - 1, 1, S - j))
	# 		if (variance)
	# 			res[TT + 1, (S + 2):(2 * S + 1), ] <- 0
	# 	}
	# 	else {
	# 		if (variance)
	# 			res <- array(0, c(TT + 2, 2 * S + 1, S))
	# 		else res <- array(0, c(TT + 2, S + 1, S))
	# 		res[TT + 1, 1, ] <- max(untimes)
	# 		for (j in 1:S) res[TT + 1, 1 + j, ] <- rep(c(0, 1,
	# 																								 0), c(j - 1, 1, S - j))
	# 		if (variance)
	# 			res[TT + 1, (S + 2):(2 * S + 1), ] <- 0
	# 		res[TT + 2, 1, ] <- predt
	# 		for (j in 1:S) res[TT + 2, 1 + j, ] <- rep(c(0, 1,
	# 																								 0), c(j - 1, 1, S - j))
	# 		if (variance)
	# 			res[TT + 2, (S + 2):(2 * S + 1), ] <- 0
	# 	}
	# }
	P <- diag(S)
	# if (covariance) {
	# 	varParr <- array(0, c(S^2, S^2, TT + 1))
	# 	if ((direction == "fixedhorizon") & !(predt %in% untimes)) 
	# 		varParr <- array(0, c(S^2, S^2, TT + 2))
	# 	ffrom <- rep(1:S, S)
	# 	tto <- rep(1:S, rep(S, S))
	# 	fromto <- paste("from", ffrom, "to", tto, sep = "")
	# 	if (direction == "forward") 
	# 		dimnames(varParr) <- list(fromto, fromto, c(predt, 
	# 																								untimes))
	# 	else {
	# 		if (predt %in% untimes) 
	# 			dimnames(varParr) <- list(fromto, fromto, c(0, 
	# 																									untimes))
	# 		else dimnames(varParr) <- list(fromto, fromto, c(0, 
	# 																										 untimes, predt))
	# 	}
	# }
	# if (variance) {
	# 	varP <- matrix(0, S^2, S^2)
	# 	if (direction == "forward") {
	# 		varAnew <- array(0, c(S, S, S, S))
	# 		if (predt != 0) {
	# 			tmin <- max(stackvarhaz$time[stackvarhaz$time <= 
	# 																	 	predt])
	# 			varHaz <- stackvarhaz[stackvarhaz$time == tmin, 
	# 			]
	# 			lHaz <- nrow(varHaz)
	# 			for (j in 1:lHaz) {
	# 				from1 <- transit$from[transit$transno == varHaz$trans1[j]]
	# 				to1 <- transit$to[transit$transno == varHaz$trans1[j]]
	# 				from2 <- transit$from[transit$transno == varHaz$trans2[j]]
	# 				to2 <- transit$to[transit$transno == varHaz$trans2[j]]
	# 				varAnew[from1, to1, from2, to2] <- varAnew[from2, 
	# 																									 to2, from1, to1] <- varHaz$varHaz[j]
	# 			}
	# 		}
	# 	}
	# 	else {
	# 		varA <- array(0, c(S, S, S, S))
	# 		varHaz <- stackvarhaz[stackvarhaz$time == untimes[TT], 
	# 		]
	# 		lHaz <- nrow(varHaz)
	# 		for (j in 1:lHaz) {
	# 			from1 <- transit$from[transit$transno == varHaz$trans1[j]]
	# 			to1 <- transit$to[transit$transno == varHaz$trans1[j]]
	# 			from2 <- transit$from[transit$transno == varHaz$trans2[j]]
	# 			to2 <- transit$to[transit$transno == varHaz$trans2[j]]
	# 			varA[from1, to1, from2, to2] <- varA[from2, to2, 
	# 																					 from1, to1] <- varHaz$varHaz[j]
	# 		}
	# 	}
	# }
	for (i in 1:TT) {
		idx <- ifelse(direction == "forward", i, TT + 1 - i)
		tt <- untimes[idx]
		Haztt <- stackhaz[stackhaz$time == tt, ]
		lHaztt <- nrow(Haztt)
		dA <- matrix(0, S, S)
		IplusdA <- diag(S)
		for (j in 1:lHaztt) {
			from <- transit$from[transit$transno == Haztt$trans[j]]
			to <- transit$to[transit$transno == Haztt$trans[j]]
			dA[from, to] <- Haztt$dhaz[j]
			dA[from, from] <- dA[from, from] - Haztt$dhaz[j]
			IplusdA[from, to] <- Haztt$dhaz[j]
			IplusdA[from, from] <- IplusdA[from, from] - Haztt$dhaz[j]
		}
		if (any(diag(IplusdA) < 0) & method_pt == "prodlim") 
			warning("Warning! Negative diagonal elements of (I+dA); the estimate may not be meaningful.\n                    Set argument method_pt to 'exp'\n")
		
		if (method_pt == "exp") {
			# Build 2S x 2S block matrix [[dA, I], [0, 0]]
			B <- rbind(
				cbind(dA,           diag(S)),
				cbind(matrix(0, S, S), matrix(0, S, S))
			)
			EB <- expm::expm(B)  #important that this stays with expm and not survexpm.
			M_unit <- EB[1:S, (S + 1):(2 * S), drop = FALSE]  # M_unit = ∫_0^1 e^{s dA} ds
			# Note: For a slice of length Δ with generator Q (so dA = QΔ),
			# the occupancy-time integral is M(Δ) = Δ * M_unit, and Q = dA / Δ.
			# Expected edge counts matrix for start distribution π is:
			#   C = diag(π %*% M_unit) %*% dA
			# which yields C_{i->j} = (π M_unit)_i * dA_{ij}.
		}
		
		# if (variance) {
		# 	if (direction == "forward") {
		# 		varA <- varAnew
		# 		varAnew <- array(0, c(S, S, S, S))
		# 		varHaztt <- stackvarhaz[stackvarhaz$time == tt, 
		# 		]
		# 		lHaztt <- nrow(varHaztt)
		# 		for (j in 1:lHaztt) {
		# 			from1 <- transit$from[transit$transno == varHaztt$trans1[j]]
		# 			to1 <- transit$to[transit$transno == varHaztt$trans1[j]]
		# 			from2 <- transit$from[transit$transno == varHaztt$trans2[j]]
		# 			to2 <- transit$to[transit$transno == varHaztt$trans2[j]]
		# 			varAnew[from1, to1, from2, to2] <- varAnew[from2, 
		# 																								 to2, from1, to1] <- varHaztt$varHaz[j]
		# 		}
		# 		vardA <- varAnew - varA
		# 	}
		# 	else {
		# 		varAttmin <- array(0, c(S, S, S, S))
		# 		varHazttmin <- stackvarhaz[stackvarhaz$time == 
		# 															 	untimes[idx - 1], ]
		# 		lHazttmin <- nrow(varHazttmin)
		# 		for (j in 1:lHazttmin) {
		# 			from1 <- transit$from[transit$transno == varHazttmin$trans1[j]]
		# 			to1 <- transit$to[transit$transno == varHazttmin$trans1[j]]
		# 			from2 <- transit$from[transit$transno == varHazttmin$trans2[j]]
		# 			to2 <- transit$to[transit$transno == varHazttmin$trans2[j]]
		# 			varAttmin[from1, to1, from2, to2] <- varAttmin[from2, 
		# 																										 to2, from1, to1] <- varHazttmin$varHaz[j]
		# 		}
		# 		vardA <- varA - varAttmin
		# 		varA <- varAttmin
		# 	}
		# 	for (from in 1:S) {
		# 		for (from2 in 1:S) {
		# 			for (to2 in 1:S) {
		# 				if (to2 != from2) 
		# 					vardA[from, from, from2, to2] <- vardA[from2, 
		# 																								 to2, from, from] <- -sum(vardA[from, 
		# 																								 															 -from, from2, to2])
		# 			}
		# 		}
		# 	}
		# 	for (from in 1:S) {
		# 		for (from2 in 1:S) vardA[from, from, from2, from2] <- vardA[from2, 
		# 																																from2, from, from] <- -sum(vardA[from, from, 
		# 																																																 from2, -from2])
		# 	}
		# 	vardA <- matrix(vardA, S^2, S^2)
		# }
		if (method == "aalen") {
			if (direction == "forward") {
				if (method_pt == "prodlim") {
					P <- P %*% IplusdA
					trans_tmp <- IplusdA
				} else if (method_pt == "exp") {
					P <- P %*% survival:::survexpm(dA)
				}
				# if (variance) {
				# 	tmp1 <- kronecker(t(IplusdA), diag(S)) %*% 
				# 		varP %*% kronecker(IplusdA, diag(S))
				# 	tmp2 <- kronecker(diag(S), P) %*% vardA %*% 
				# 		kronecker(diag(S), t(P))
				# 	varP <- tmp1 + tmp2
				# }
			}
			# else {
			# 	if (variance) {
			# 		tmp1 <- kronecker(diag(S), IplusdA) %*% varP %*%
			# 			kronecker(diag(S), t(IplusdA))
			# 		tmp2 <- kronecker(t(P), IplusdA) %*% vardA %*%
			# 			kronecker(P, t(IplusdA))
			# 		varP <- tmp1 + tmp2
			# 	}
			# 	if (method_pt == "prodlim")
			# 		P <- IplusdA %*% P
			# 	else if (method_pt == "exp")
			# 		P <- survival:::survexpm(dA) %*% P
			# }
		}
		# if (method == "greenwood") {
		# 	if (direction == "forward") {
		# 		if (variance) {
		# 			tmp1 <- kronecker(t(IplusdA), diag(S)) %*% 
		# 				varP %*% kronecker(IplusdA, diag(S))
		# 			tmp2 <- kronecker(diag(S), P) %*% vardA %*% 
		# 				kronecker(diag(S), t(P))
		# 			varP <- tmp1 + tmp2
		# 		}
		# 		if (method_pt == "prodlim") 
		# 			P <- P %*% IplusdA
		# 		else if (method_pt == "exp") 
		# 			P <- P %*% survival:::survexpm(dA)
		# 	}
		# 	else {
		# 		if (variance) {
		# 			tmp1 <- kronecker(diag(S), IplusdA) %*% varP %*% 
		# 				kronecker(diag(S), t(IplusdA))
		# 			tmp2 <- kronecker(t(P), diag(S)) %*% vardA %*% 
		# 				kronecker(P, diag(S))
		# 			varP <- tmp1 + tmp2
		# 		}
		# 		if (method_pt == "prodlim") 
		# 			P <- IplusdA %*% P
		# 		else if (method_pt == "exp") 
		# 			P <- survival:::survexpm(dA) %*% P
		# 	}
		# }
		# if (variance) {
		# 	seP <- sqrt(diag(varP))
		# 	seP <- matrix(seP, S, S)
		# }
		# if (covariance) {
		# 	if (direction == "forward") 
		# 		varParr[, , i + 1] <- varP
		# 	else {
		# 		varParr[, , idx + 1] <- varP
		# 	}
		# }
		if (direction == "forward") {
			res[idx + 1, 1, ] <- res_trans[idx + 1, 1, ] <- tt
			res[idx + 1, 2:(S + 1), ] <- t(P)
			if (variance) 
				res[idx + 1, (S + 2):(2 * S + 1), ] <- t(seP)
		}
		# else {
		# 	res[idx, 1, ] <- ifelse(i == TT, 0, untimes[TT - 
		# 																								i])
		# 	res[idx, 2:(S + 1), ] <- t(P)
		# 	if (variance) 
		# 		res[idx, (S + 2):(2 * S + 1), ] <- t(seP)
		# }
		if (method_pt == "prodlim") {
			for (k in 1:numtrans) {                           
				from <- transit$from[k]
				to   <- transit$to[k]
				val_tmp  <- trans_tmp[from, to]                 # prob of i->j in this slice
				vals_tmp <- res[idx, from + 1, ] * val_tmp
				res_trans[idx + 1, k + 1, ] <- vals_tmp         # write to the k-th transition column
			}
		} else if (method_pt == "exp") {
			# multi-jump-consistent edge counts using M_unit and dA
			# Grab the S x S matrix Π of start-of-slice occupancies: rows = current state i, cols = start state s
			# Collapse to an S x S matrix: rows = current state i, cols = start state s
			Pi_mat <- res[idx, 2:(S + 1), ]         # this drops the first dim and gives an SxS matrix
			Pi_mat <- as.matrix(Pi_mat)             
			
			# Expected time-in-state per start state (columns): r_s = M_unit^T * pi_s
			r_mat <- t(M_unit) %*% Pi_mat           # S x S
			
			for (k in 1:numtrans) {                           
				from <- transit$from[k]
				to   <- transit$to[k]
				vals_tmp <- r_mat[from, ] * dA[from, to]        # expected edge counts
				res_trans[idx + 1, k + 1, ] <- vals_tmp
			}
		}
	}
	# if (covariance & (direction == "fixedhorizon")) 
	# 	varParr[, , 1] <- varParr[, , 2]
	res2 <- vector("list", S)
	for (s in 1:S) {
		tmp <- as.data.frame(res[, , s])
		tmp_res_trans <- as.data.frame(res_trans[, , s])
		if (min(dim(tmp)) == 1){ 
			tmp <- res[, , s]
			tmp_res_trans <- res_trans[, , s]
		}
		if (variance)
			names(tmp) <- c("time", paste("pstate", 1:S, sep = ""),
											paste("se", 1:S, sep = ""))
		else names(tmp) <- c("time", paste("pstate", 1:S, sep = ""))
		names(tmp_res_trans) <- c("time", paste("trans", 1:numtrans, sep = ""))
		res2[[s]][["state_probabilities"]] <- tmp
		res2[[s]][["transition_dynamics"]] <- tmp_res_trans
		
	}
	# if (covariance) 
	# 	res2$varMatrix <- varParr
	# if (variance_boot) {
	# 	is_absorbing <- rowSums(!is.na(trans)) == 0
	# 	absorbing_true <- which(is_absorbing == TRUE)
	# 	absorbing_false <- which(is_absorbing == FALSE)
	# 	se_df <- as.data.frame(matrix(0, nrow = 1, ncol = S))
	# 	names(se_df) <- paste("se", 1:S, sep = "")
	# 	for (s in absorbing_true) {
	# 		res2[[s]] <- cbind(res2[[s]], se_df)
	# 	}
	# 	B <- length(object$Haz.boot)
	# 	pt_list <- vector("list", S)
	# 	for (s in absorbing_false) {
	# 		pt_list[[s]] <- vector("list", B)
	# 	}
	# 	for (i in 1:B) {
	# 		haz_tmp <- object$Haz.boot[[i]]
	# 		haz_tmp$b <- NULL
	# 		msfit_tmp <- list(Haz = haz_tmp, trans = trans)
	# 		class(msfit_tmp) <- "msfit"
	# 		pt_tmp <- suppressWarnings(probtrans(msfit_tmp, predt = predt, 
	# 																				 direction = direction, variance = FALSE, covariance = FALSE))
	# 		for (s in absorbing_false) {
	# 			pt_list[[s]][[i]] <- pt_tmp[[s]]
	# 		}
	# 	}
	# 	pt_list_save <- pt_list
	# 	for (s in absorbing_false) {
	# 		pt_list[[s]] <- do.call(rbind.data.frame, pt_list[[s]])
	# 		pt_list[[s]] <- aggregate(. ~ time, data = pt_list[[s]], 
	# 															FUN = stats::sd)
	# 		unneeded_times <- which(!(pt_list[[s]]$time %in% 
	# 																res2[[s]]$time))
	# 		if (length(unneeded_times) > 0) 
	# 			pt_list[[s]] <- pt_list[[s]][-unneeded_times, 
	# 			]
	# 		pt_list[[s]] <- pt_list[[s]][, 2:ncol(pt_list[[s]])]
	# 		names(pt_list[[s]]) <- gsub("pstate", "se", names(pt_list[[s]]))
	# 		res2[[s]] <- cbind(res2[[s]], pt_list[[s]])
	# 	}
	# 	res2$pt.boot <- pt_list_save
	# }
	# res2$trans <- trans
	# res2$method <- method
	# res2$predt <- predt
	# res2$direction <- direction
	# class(res2) <- "probtrans"
	return(res2)
}




## Function that applies probtrans_fusion to each strata (in each scenario) and then builds an object that contains state occupancies and transition dynamics per scen and per strata (and pooled) taking into account the starting state distributions and strata weights, for later input into the "calculate_rewards" function.
state_trans_res <- function(inputs, rescaled_msfits, progress_cb = .null_progress, pb_range    = c(20, 95)){
	n_strata <- length(inputs$strata_weights)
	n_scen   <- length(inputs$scenarios)
	n_states <- nrow(inputs$df_in_state_dist_strata)
	state_names <- rownames(inputs$df_in_state_dist_strata)
	trans <- rescaled_msfits[[1]][[1]]$trans
	transit <- to.trans2(trans)
	transnames <- transit$transname
	strata_weights <- inputs$strata_weights # a numeric vector where the first item is the weight for strata 1, second for strata 2 etc.
	
	# progress helpers
	pb_min <- pb_range[1]; pb_max <- pb_range[2]
	total_units <- max(1L, n_scen * n_strata) # guard against divide-by-zero
	pct_for <- function(scen, strata) {
		# linear mapping of (scenario,strata) to the reserved percentage range
		idx <- (scen - 1L) * n_strata + strata
		pb_min + floor((pb_max - pb_min) * idx / total_units)
	}
	
	progress_cb(pb_min, "Preparing transition results…")   # PROGRESS
	#
	
	out <- vector("list", n_scen)
	
	for (scen in seq_len(n_scen)) {
		out[[scen]] <- vector("list", n_strata+1) # +1 for the pooled
		
		for (strata in seq_len(n_strata)) {
			
			progress_cb(max(pb_min, pct_for(scen, strata) - 1L),
									sprintf("Calculating: Scenario %d/%d · Stratum %d/%d...",
													scen, n_scen, strata, n_strata))    
			
			out[[scen]][[strata]] <- vector("list", 2) # 1 is state_probabilities and 2 will be transition dynamics.
			names(out[[scen]][[strata]]) <- c("state_probabilities", "transition_dynamics")
			
			pt <- probtrans_fusion(object = rescaled_msfits[[scen]][[strata]],
														 direction = "forward",
														 predt = 0,
														 method_pt = "exp",
														 method = "aalen",
														 variance = F,
														 covariance = F)
			
			
			
			# weights for this stratum’s starting-state distribution (length = n_states)
			w <- as.vector(inputs$df_in_state_dist_strata[, strata])
			# take only the first n_states list items (note these are still lists, which is different to the standard probtrans where they would be dfs)
			lists <- pt[seq_len(n_states)]
			dfs_state_probabilities <- lapply(lists, `[[`, 1)
			dfs_transition_dynamics <- lapply(lists, `[[`, 2)
			
			# weight each data frame (leave time column unchanged)
			dfs_w <- Map(function(df, k){
				df2 <- df
				if (ncol(df2) > 1) {
					num_idx <- which(sapply(df2, is.numeric))
					num_idx <- setdiff(num_idx, 1L)      # exclude the first (time) column
					df2[num_idx] <- lapply(df2[num_idx], `*`, k)
				}
				df2
			}, dfs_state_probabilities, w)
			
			dfs_w_trans <- Map(function(df, k){
				df2 <- df
				if (ncol(df2) > 1) {
					num_idx <- which(sapply(df2, is.numeric))
					num_idx <- setdiff(num_idx, 1L)      # exclude the first (time) column
					df2[num_idx] <- lapply(df2[num_idx], `*`, k)
				}
				df2
			}, dfs_transition_dynamics, w)
			
			# sum the weighted data frames into a single data frame
			agg_df <- Reduce(function(a, b){
				# ensure identical time grids; if they aren't, merge+interpolate instead
				if (!isTRUE(all.equal(a[[1]], b[[1]], check.attributes = FALSE))) {
					stop("Time grids differ across probtrans data frames; cannot sum safely.")
				}
				a[-1] <- Map(`+`, a[-1], b[-1])  # sum columns 2:n
				a
			}, dfs_w)
			
			agg_df_trans <- Reduce(function(a, b){
				# ensure identical time grids; if they aren't, merge+interpolate instead
				if (!isTRUE(all.equal(a[[1]], b[[1]], check.attributes = FALSE))) {
					stop("Time grids differ across probtrans data frames; cannot sum safely.")
				}
				a[-1] <- Map(`+`, a[-1], b[-1])  # sum columns 2:n
				a
			}, dfs_w_trans)
			
			colnames(agg_df) <- c("time", state_names)
			colnames(agg_df_trans) <- c("time", transnames)
			
			# store the single aggregated data frame for this scen × stratum
			out[[scen]][[strata]][["state_probabilities"]] <- as.data.table(agg_df)
			out[[scen]][[strata]][["transition_dynamics"]] <- as.data.table(agg_df_trans)
		}
		
		## pooled (state probabilities)
		strata_dfs <- lapply(out[[scen]][seq_len(n_strata)], `[[`, "state_probabilities")
		strata_dfs_trans <- lapply(out[[scen]][seq_len(n_strata)], `[[`, "transition_dynamics")
		
		stopifnot(all(vapply(strata_dfs, function(d)
			isTRUE(all.equal(d$time, strata_dfs[[1]]$time, check.attributes = FALSE)), TRUE)))
		stopifnot(all(vapply(strata_dfs_trans, function(d)
			isTRUE(all.equal(d$time, strata_dfs_trans[[1]]$time, check.attributes = FALSE)), TRUE)))
		pooled_num <- Reduce(`+`, Map(`*`,
																	lapply(strata_dfs, function(d) {
																		cols <- setdiff(names(d), "time")     
																		as.matrix(d[, ..cols])               
																	}),
																	strata_weights))
		
		## pooled (transition dynamics)
		pooled_num_trans <- Reduce(`+`, Map(`*`,
																				lapply(strata_dfs_trans, function(d) {
																					cols <- setdiff(names(d), "time")
																					as.matrix(d[, ..cols])
																				}),
																				strata_weights))
		
		
		# init the pooled slot as a 2-element named list
		# init pooled slot
		out[[scen]][[n_strata+1]] <- vector("list", 2)
		names(out[[scen]][[n_strata+1]]) <- c("state_probabilities", "transition_dynamics")
		
		# state probabilities
		pooled_dt <- data.table(time = strata_dfs[[1]]$time)
		pooled_dt[, (state_names) := as.data.frame(pooled_num)]
		setcolorder(pooled_dt, c("time", state_names))
		out[[scen]][[n_strata+1]][["state_probabilities"]] <- pooled_dt
		
		# transition dynamics
		pooled_dt_trans <- data.table(time = strata_dfs_trans[[1]]$time)
		pooled_dt_trans[, (transnames) := as.data.frame(pooled_num_trans)]
		setcolorder(pooled_dt_trans, c("time", transnames))
		out[[scen]][[n_strata+1]][["transition_dynamics"]] <- pooled_dt_trans
		
		
		names(out[[scen]]) <- c(paste("strata", 1:n_strata),"pooled")
	}
	
	names(out) <- paste("scenario", 1:n_scen)
	
	
	out
}



# Function to apply rewards to state occupancies and transtion events
calculate_rewards <- function(inputs, results_list, pop_size) {
	# Initialize output list for scenarios
	reward_results <- vector("list", length(inputs$scenarios))
	scen_labels   <- vapply(inputs$scenarios,
													FUN = function(s) s$label,
													FUN.VALUE = character(1))
	names(reward_results) <- scen_labels
	
	# Loop over scenarios
	for (scen in seq_along(inputs$scenarios)) {
		scen_name <- scen_labels[scen]
		reward_results[[scen_name]] <- list()
		
		## Precompute window bounds and labels
		tw_breaks    <- inputs$scenarios[[scen]]$time_window_durations
		n_windows    <- length(tw_breaks) - 1
		window_names <- vapply(inputs$scenarios[[scen]]$time_windows,
													 FUN = function(s) s$label,
													 FUN.VALUE = character(1))
		window_bounds <- setNames(
			lapply(seq_len(n_windows), function(k) {
				c(start = tw_breaks[k], end = tw_breaks[k+1])
			}),
			window_names
		)
		
	
		
		## Preextract reward labels (to reuse for pooling)
		reward_labels <- vapply(inputs$scenarios[[scen]]$rewards,
														FUN = function(r) r$reward_label,
														FUN.VALUE = character(1))
		
		# For each strata within scenario
		n_strata     <- length(inputs$strata_weights)
		strata_names <- inputs$strata_labels
		
		for (st in seq_len(n_strata)) {
			strata_name   <- strata_names[st]
			strata_weight <- inputs$strata_weights[st]*pop_size
			
			# pull in this strata’s state probs & transition dynamics
			state_dt <- results_list[[scen]][[st]]$state_probabilities
			trans_dt <- results_list[[scen]][[st]]$transition_dynamics
			times    <- state_dt[[1]]
			dts      <- diff(times)
			
			# build up this strata’s reward list
			strata_rewards <- vector("list", length(reward_labels))
			names(strata_rewards) <- reward_labels
			
			
			
			for (r_nm in seq_along(reward_labels)) {
				reward_label <- reward_labels[r_nm]
				reward_obj   <- inputs$scenarios[[scen]]$rewards[[r_nm]]
				
				# placeholders for per-window lists
				sr_out <- list()
				tr_out <- list()
				
				# names of variants
				sr_vars <- names(reward_obj$sr[[1]])
				tr_vars <- names(reward_obj$tr[[1]])
				
				# compute per-window
				for (tw in seq_len(n_windows)) {
					tw_label <- window_names[tw]
					bounds   <- window_bounds[[tw_label]]
					rows     <- which(times >= bounds["start"] & times < bounds["end"])
					
					# STATE rewards
					sr_list <- list()
					for (v in sr_vars) {
						sr_df           <- reward_obj$sr[[tw]][[v]]
						probs           <- as.matrix(state_dt[rows, sr_df$rowname, with = FALSE])
						rw_vec          <- as.numeric(sr_df[[strata_name]])
						reward_per_time <- as.numeric(probs %*% rw_vec)
						total_sr        <- sum(reward_per_time * dts[rows])
						sr_list[[v]]    <- total_sr * strata_weight
					}
					sr_out[[tw_label]] <- sr_list
					
					# TRANSITION rewards
					tr_list <- list()
					for (v in tr_vars) {
						tr_df    <- reward_obj$tr[[tw]][[v]]
						tr_mat   <- as.matrix(trans_dt[rows, tr_df$rowname, with = FALSE])
						rw_tr    <- as.numeric(tr_df[[strata_name]])
						total_tr <- sum(tr_mat %*% rw_tr)
						tr_list[[v]] <- total_tr * strata_weight
					}
					tr_out[[tw_label]] <- tr_list
				}
				
				# compute “all” = sum across windows for this strata 
				sr_out[["all"]] <- setNames(
					lapply(sr_vars, function(v) {
						sum(vapply(window_names, function(w) sr_out[[w]][[v]], numeric(1)))
					}),
					sr_vars
				)
				tr_out[["all"]] <- setNames(
					lapply(tr_vars, function(v) {
						sum(vapply(window_names, function(w) tr_out[[w]][[v]], numeric(1)))
					}),
					tr_vars
				)
				
				# store for this reward & strata
				strata_rewards[[reward_label]] <- list(
					state_rewards      = sr_out,
					transition_rewards = tr_out
				)
			}
			
			# attach strata
			reward_results[[scen_name]][[strata_name]] <- strata_rewards
		}
		
		#  compute pooled 
		pooled <- vector("list", length(reward_labels))
		names(pooled) <- reward_labels
		
		for (r_nm in seq_along(reward_labels)) {
			reward_label <- reward_labels[r_nm]
			pooled_sr    <- list()
			pooled_tr    <- list()
			reward_obj   <- inputs$scenarios[[scen]]$rewards[[r_nm]]
			sr_vars <- names(reward_obj$sr[[1]])
			tr_vars <- names(reward_obj$tr[[1]])
			
			# sum by time window
			for (tw in seq_len(n_windows)) {
				tw_label <- window_names[tw]
				pooled_sr[[tw_label]] <- setNames(
					lapply(sr_vars, function(v) {
						sum(sapply(strata_names, function(sn) {
							reward_results[[scen_name]][[sn]][[reward_label]]$state_rewards[[tw_label]][[v]]
						}))
					}),
					sr_vars
				)
				pooled_tr[[tw_label]] <- setNames(
					lapply(tr_vars, function(v) {
						sum(sapply(strata_names, function(sn) {
							reward_results[[scen_name]][[sn]][[reward_label]]$transition_rewards[[tw_label]][[v]]
						}))
					}),
					tr_vars
				)
			}
			
			# pooled “all”
			pooled_sr[["all"]] <- setNames(
				lapply(sr_vars, function(v) {
					sum(unlist(lapply(window_names, function(w)
						pooled_sr[[w]][[v]])))
				}),
				sr_vars
			)
			pooled_tr[["all"]] <- setNames(
				lapply(tr_vars, function(v) {
					sum(unlist(lapply(window_names, function(w)
						pooled_tr[[w]][[v]])))
				}),
				tr_vars
			)
			
			pooled[[reward_label]] <- list(
				state_rewards      = pooled_sr,
				transition_rewards = pooled_tr
			)
		}
		# attach pooled
		reward_results[[scen_name]][["pooled"]] <- pooled
		
		
	}
	
	return(reward_results)
}


# Standalone helper to compute transition counts from scenario_probtrans() output
compute_transition_counts <- function(inputs, results_list, pop_size) {
	# labels & sizes
	scen_labels   <- vapply(inputs$scenarios,
																					 FUN = function(s) s$label,
																					 FUN.VALUE = character(1))
	strata_names  <- inputs$strata_labels
	n_strata      <- length(inputs$strata_weights)
	
	# Union of transition names across all scenarios & strata
	all_transitions <- unique(unlist(lapply(seq_along(results_list), function(scen) {
		lapply(seq_len(n_strata), function(st) {
			td <- results_list[[scen]][[st]]$transition_dynamics
			if (is.null(td)) character(0) else setdiff(colnames(td), "time")
		})
	})))
	transition_names <- sort(all_transitions)
	
	# Per-scenario, per-strata counts (population totals)
	counts_by_scenario <- setNames(vector("list", length(scen_labels)), scen_labels)
	
	for (scen in seq_along(results_list)) {
		scen_name    <- scen_labels[scen]
		scen_counts  <- setNames(vector("list", n_strata), strata_names)
		pooled_accum <- setNames(numeric(length(transition_names)), transition_names)
		
		for (st in seq_len(n_strata)) {
			trans_dt   <- results_list[[scen]][[st]]$transition_dynamics
			trans_cols <- if (is.null(trans_dt)) character(0) else setdiff(colnames(trans_dt), "time")
			
			if (length(trans_cols) > 0) {
				counts <- colSums(as.matrix(trans_dt[, trans_cols, with = FALSE]), na.rm = TRUE)
			} else {
				counts <- numeric(0)
			}
			
			# align to full set of transitions (fill missing with 0)
			aligned <- setNames(numeric(length(transition_names)), transition_names)
			if (length(counts)) {
				common <- intersect(names(counts), transition_names)
				aligned[common] <- counts[common]
			}
			
			# scale to population totals for this strata
			aligned <- aligned * inputs$strata_weights[st] * pop_size
			
			scen_counts[[st]] <- aligned
			pooled_accum <- pooled_accum + aligned
		}
		
		scen_counts[["pooled"]] <- pooled_accum
		counts_by_scenario[[scen_name]] <- scen_counts
	}
	
	# Wide table: rows = transitions, cols = "scenario X | <strata>" and "scenario X | Total"
	col_keys <- unlist(lapply(seq_along(scen_labels), function(s) {
		paste0(scen_labels[s], " | ", c(strata_names, "Total"))   # <- was "pooled"
	}))
	
	counts_mat <- matrix(0, nrow = length(transition_names), ncol = length(col_keys))
	rownames(counts_mat) <- transition_names
	colnames(counts_mat) <- col_keys
	
	for (scen in seq_along(scen_labels)) {
		scen_name <- scen_labels[scen]
		# strata
		for (st in seq_along(strata_names)) {
			key <- paste0(scen_name, " | ", strata_names[st])
			counts_mat[, key] <- counts_by_scenario[[scen_name]][[strata_names[st]]]
		}
		# Total (display) 
		keyp <- paste0(scen_name, " | Total")                   
		counts_mat[, keyp] <- counts_by_scenario[[scen_name]][["pooled"]]
	}
	
	counts_table <- data.table::data.table(
		transition = rownames(counts_mat),
		data.table::as.data.table(counts_mat)
	)
	
	
	list(
		counts_by_scenario = counts_by_scenario,
		counts_table       = counts_table
	)
}








