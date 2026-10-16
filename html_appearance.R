style_html <- tags$style(HTML("
		/* ------- Text size -------- */
			.medium-text {
        font-size: 14px;
      }
      .small-text {
        font-size: 12px;
      }
								
		/* ------- FOR ERROR APPEARANCE -------- */
		
    .shiny-output-error-validation {
	      color: red; 
	      font-family: monospace;
	      font-weight: bold;
    }
    
	/* -------- FOR EXPAND BUTTON -------- */
		.bslib-full-screen-enter {
      position: absolute;
      top: 0rem !important;   
      right: 0.5rem !important; 
      left: auto !important;    
      bottom: auto !important;  
			opacity: 0.7;
	    z-index: 1000; /* Adjusted z-index to be lower than the modal's */

		}
	
	.bslib-full-screen-enter::after {
      content: ' Expand';  /* Add the word Expand */
      font-size: 0.9rem;
      font-weight: bold;
    }
		
     /* -------- FOR APPEARANCE OF DYNAMIC TABLES -------- */
		
		.vue-input th {
		    background-color: #9ecadb; /* Light blue background */
		    cursor: pointer;
		}
		
		.vue-input td {
				background-color: #e8eced;
				transition: all 0.3s ease-in-out;
		}
		
		.vue-input td:hover {
		    background-color: darkblue;
		    color: white;
		}
		
		.vue-input td, .vue-input th {
		    border: none !important;
				border-radius: 20px !important; 
		    text-align: center !important;
		    vertical-align: middle !important;
				height: 2em !important;
		
				/* Simulated inner border using inset box-shadow */
    		box-shadow: inset 0 0 0 3px white;
		}
		
		.vue-input table tr:first-child th:first-child {
		    background-color: transparent; /* Make the top-left corner transparent */
		}
	

  "))

script_html <- tags$script(HTML("
  // Update active tab on shown.bs.tab event
  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
    var activeTab = $(e.target).data('value');
    Shiny.onInputChange('activeInnerTab', activeTab);
  });

  // Initialize popovers for elements with data-toggle='popover'
  $(document).ready(function(){
    $('[data-toggle=\"popover\"]').popover({
      trigger: 'hover',
      placement: 'top',
    });
  });
"))