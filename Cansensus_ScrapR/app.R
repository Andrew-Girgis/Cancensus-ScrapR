library(shiny)
library(dplyr)
library(cancensus)
library(DT)

# Set your Census API key here (or use options() in your .Rprofile)
# options(cancensus.api_key = "YOUR_API_KEY")

# Function to get vectors for a specific year
get_year_vectors <- function(year) {
  base_vectors <- c("454", "463", "466", "469", "472", "475", "2207", "2209", "2208", 
                    "408", "409", "410", "411", "417", "479", "480", "481", "482", "483",
                    "2309", "2312", "2315", "2318", "2321", "2324", "2327", "2330", "2333", 
                    "2336", "2339", "2342", "2570", "2540", "5795", "5798", "5801", "5804", 
                    "5807", "5810", "5816", "5819", "5822", "5825", "5828", "2427", "2428", 
                    "2429", "2430", "2431", "2432", "2433", "2434", "2435", "2436", "2437", 
                    "2438", "2439", "2440", "2441", "2442", "4893", "4894", "4895", "4896", 
                    "4891", "4892", "4897", "4898", "4899", "4900", "4901", "5099", "5102", 
                    "5108", "5117", "5120", "5123", "5603", "5606", "5609", "5663", "5669", 
                    "5672", "5675", "5678", "5681", "5684", "5687", "5690", "4002", "4806", 
                    "4014", "4044", "4266", "4329", "4611", "4698", "4743", "4800", "3408", 
                    "3411", "3435")
  
  # Add the year prefix to each vector
  vectors <- paste0("v_CA", year, "_", base_vectors)
  
  # Check which vectors exist in the dataset
  all_vectors <- list_census_vectors(paste0("CA", year))
  valid_vectors <- vectors[vectors %in% all_vectors$vector]
  
  return(valid_vectors)
}

# UI
ui <- fluidPage(
  titlePanel("Canadian Census Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Year selection
      radioButtons("year", "Select Census Year:",
                   choices = c("2011", "2016", "2021"),
                   selected = "2016"),
      
      # Region selection
      uiOutput("region_selector"),
      
      # Level selection
      selectInput("level", "Geographic Level:",
                  choices = c("Dissemination Area" = "DA", 
                              "Census Tract" = "CT",
                              "Census Subdivision" = "CSD"),
                  selected = "DA"),
      
      # Download button
      downloadButton("download_data", "Download CSV"),
      
      # Status display
      htmlOutput("status")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Preview", 
                 h3("Data Preview"),
                 DT::dataTableOutput("preview_table")),
        tabPanel("Vector Information", 
                 h3("Census Vector Information"),
                 DT::dataTableOutput("vector_info"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store regions
  regions_data <- reactive({
    req(input$year)
    withProgress(message = 'Loading regions...', {
      regions <- list_census_regions(paste0("CA", input$year))
      # Filter for CSDs
      csd_regions <- regions[regions$level == "CSD", ]
      # Sort by population in descending order
      csd_regions <- csd_regions %>% arrange(desc(pop))
    })
    return(csd_regions)
  })
  
  # Render the region selection UI based on the year
  output$region_selector <- renderUI({
    req(regions_data())
    regions <- regions_data()
    
    # Create named list for the checkboxGroupInput
    region_choices <- setNames(
      regions$region,
      paste0(regions$name, " (Pop: ", format(regions$pop, big.mark=","), ")")
    )
    
    # Render the checkbox group with the top 10 pre-selected
    checkboxGroupInput("regions", "Select Census Subdivisions:",
                       choices = region_choices,
                       selected = regions$region[1:5])
  })
  
  # Reactive expression for vectors
  vectors <- reactive({
    req(input$year)
    withProgress(message = 'Loading vectors...', {
      get_year_vectors(input$year)
    })
  })
  
  # Reactive expression for vector information
  vector_info <- reactive({
    req(vectors())
    withProgress(message = 'Getting vector info...', {
      all_vectors <- list_census_vectors(paste0("CA", input$year))
      all_vectors[all_vectors$vector %in% vectors(), ]
    })
  })
  
  # Output vector information
  output$vector_info <- DT::renderDataTable({
    req(vector_info())
    DT::datatable(vector_info(), options = list(pageLength = 10))
  })
  
  # Reactive expression for census data
  census_data <- reactive({
    req(input$regions, input$year, vectors(), input$level)
    
    if(length(input$regions) == 0) {
      return(NULL)
    }
    
    withProgress(message = 'Fetching census data...', {
      # Get selected vectors
      selected_vectors <- vectors()
      
      # Try to get census data
      tryCatch({
        data <- get_census(
          dataset = paste0("CA", input$year),
          regions = list(CSD = input$regions),
          vectors = selected_vectors,
          level = input$level
        )
        
        return(data)
      }, error = function(e) {
        # Return the error message
        return(NULL)
      })
    })
  })
  
  # Preview table
  output$preview_table <- DT::renderDataTable({
    data <- census_data()
    if(is.null(data)) {
      return(data.frame(Message = "No data available. Please select regions and try again."))
    }
    
    # Limit to first 10 rows and 10 columns for preview
    preview_data <- data[1:min(10, nrow(data)), 1:min(10, ncol(data))]
    DT::datatable(preview_data, options = list(scrollX = TRUE))
  })
  
  # Status message
  output$status <- renderUI({
    data <- census_data()
    
    if(is.null(data)) {
      HTML("<div style='color: red;'>No data available. Please check your selections.</div>")
    } else {
      HTML(paste0("<div style='color: green;'>Data loaded: ", nrow(data), " rows, ", ncol(data), " columns.</div>"))
    }
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("census_data_", input$year, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- census_data()
      if(!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      } else {
        # Create an empty file with an error message
        write.csv(data.frame(Error = "No data available for the selected options."), file, row.names = FALSE)
      }
    }
  )
}

# Run the app
shinyApp(ui, server)

