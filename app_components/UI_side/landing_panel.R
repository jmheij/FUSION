landing_panel <- nav_panel(
	"Introduction",
	layout_column_wrap(
		width = 1/2,
		card(
			card_header(tags$b("Purpose and instructions")),
			card_body(
				h5("Welcome to the FUSION Decision Support Tool"),
				tags$p(class = "medium-text", "The FUSION Tool is a scenario-comparison tool that allows users to explore 'what-if' scenarios in any setting with longitudinal event data and a corresponding multistate model (MSM). It links empirically estimated multi-state dynamics (i.e., msfit objects estimated using the mstate R package) with a flexible way to accrue outcomes of interest, enabling comparison of alternative interventions or policies. The COVID-19 example in", tags$a(href="https://google.com",
																																																																																																																																																																																																																																						"this manuscript"), "is just a case study; the Tool's architecture is domain-agnostic provided the key elements/ingredients are present. With the FUSION Tool, users can model alternative scenarios by adjusting transitions over time, and attach user-defined 'rewards' (e.g., costs, quality of life, costs) to states and transitions in order to quantify the outcomes that matter for their decision. The reward system accommodates any measurable units users wish to track."),
			p(class="medium-text", "To get started (see Figure ->), users must: 1) upload one or more msfit objects (each representing a MSM for a specific population stratum), 2) specify population strata and sizes, 3) define scenarios by scaling transition hazards and assigning rewards, and 4) view descriptive results per scenario, comparative results between scenarios, and download reports of inputs and outputs."),
			tags$p(class="medium-text", "For methodological details and formal definitions, users are referred to the full manuscript and appendix; implementation specifics are documented in the tool’s", tags$a(href="https://google.com", "source code on GitHub.")),
			br(),
			h5("Licensing and citation"),
			tags$b("MIT License"),
			p(class="medium-text","Copyright 2025, Leiden University Medical Center"),
			p(class="medium-text","Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:"),
			tags$ul(class="medium-text",
			tags$li("The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software."),
			tags$li("All research outputs or reports that use the FUSION Tool are requested to reference the original manuscript as follows:")
			),
			p(class="medium-text","pending")
				)
		),
		card( 
			card_header(tags$b("Process overview")),  
			card_body(
				tags$img(
					src = "process_figure.jpg",
					width = "100%",
					height = "auto"
				)
			)
		))
)