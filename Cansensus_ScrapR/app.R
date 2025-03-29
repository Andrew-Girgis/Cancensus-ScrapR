library(shiny)
library(dplyr)
library(cancensus)
library(DT)
library(bslib)
library(zip)
library(shinyWidgets)

api_key <- Sys.getenv("CANCENSUS_API_KEY")
options(cancensus.api_key=api_key)
options(cancensus.cache_path ="cansensus_cache")

cancensus::set_api_key(api_key)

# Set up directories
dir.create("census_cache", showWarnings = FALSE)
dir.create("output_data", showWarnings = FALSE)

# Function to get vectors for a specific year
get_year_vectors <- function(year) {
  if (year == "2011") {
    vectors = c("v_CA11F_199", "v_CA11F_200", "v_CA11F_201", "v_CA11F_202", "v_CA11F_203",
                "v_CA11F_86", "v_CA11F_95", "v_CA11F_98", "v_CA11F_101", "v_CA11F_104", "v_CA11F_107", "v_CA11F_211",
                "v_CA11F_212", "v_CA11F_213", "v_CA11F_214", "v_CA11F_215", "v_CA11F_163",
                "v_CA11N_2449", "v_CA11N_2450", "v_CA11N_2451", "v_CA11N_2356", "v_CA11N_2359",
                "v_CA11N_2362", "v_CA11N_2365", "v_CA11N_2368", "v_CA11N_2371", "v_CA11N_2374",
                "v_CA11N_2377", "v_CA11N_2380", "v_CA11N_2383", "v_CA11N_2386", "v_CA11N_2548",
                "v_CA11N_2549", "v_CA11N_2550", "v_CA11N_2551", "v_CA11N_2552", "v_CA11N_2553",
                "v_CA11N_2554", "v_CA11N_2555", "v_CA11N_2556", "v_CA11N_2557", "v_CA11N_2558",
                "v_CA11N_2559", "v_CA11N_2560", "v_CA11N_2606", "v_CA11N_19", "v_CA11N_22",
                "v_CA11N_46", "v_CA11N_505", "v_CA11N_517", "v_CA11N_547", "v_CA11N_760",
                "v_CA11N_820", "v_CA11N_1084", "v_CA11N_1162", "v_CA11N_1204", "v_CA11N_1258",
                "v_CA11N_1264", "v_CA11N_2282", "v_CA11N_2283", "v_CA11N_2284", "v_CA11N_2285",
                "v_CA11N_2286", "v_CA11N_2287", "v_CA11N_2288", "v_CA11N_2289", "v_CA11N_2290",
                "v_CA11N_2291", "v_CA11N_2292", "v_CA11N_1804", "v_CA11N_1807", "v_CA11N_1813", "v_CA11N_1816",
                "v_CA11N_1819", "v_CA11N_1822", "v_CA11N_1993", "v_CA11N_1996", "v_CA11N_1999",
                "v_CA11N_2035", "v_CA11N_2041", "v_CA11N_2044", "v_CA11N_2047", "v_CA11N_2050",
                "v_CA11N_2053", "v_CA11N_2056", "v_CA11N_2059", "v_CA11N_2062", "v_CA11N_2194",
                "v_CA11N_2197", "v_CA11N_2200", "v_CA11N_2203", "v_CA11N_2206", "v_CA11N_2209")
  } else if (year == "2016") {
    vectors = c("v_CA16_454", "v_CA16_463", "v_CA16_466", "v_CA16_469", "v_CA16_472", "v_CA16_475", "v_CA16_2207", "v_CA16_2209", "v_CA16_2208", 
                "v_CA16_408", "v_CA16_409", "v_CA16_410", "v_CA16_411", "v_CA16_417", "v_CA16_479", "v_CA16_480", "v_CA16_481", "v_CA16_482", "v_CA16_483",
                "v_CA16_2309", "v_CA16_2312", "v_CA16_2315", "v_CA16_2318", "v_CA16_2321",
                "v_CA16_2324", "v_CA16_2327", "v_CA16_2330", "v_CA16_2333", "v_CA16_2336", "v_CA16_2339",
                "v_CA16_2342", "v_CA16_2570", "v_CA16_2540", "v_CA16_5795", "v_CA16_5798",
                "v_CA16_5801", "v_CA16_5804", "v_CA16_5807", "v_CA16_5810", "v_CA16_5816", "v_CA16_5819",
                "v_CA16_5822", "v_CA16_5825", "v_CA16_5828",
                "v_CA16_2427", "v_CA16_2428", "v_CA16_2429", "v_CA16_2430", "v_CA16_2431", "v_CA16_2432", "v_CA16_2433", "v_CA16_2434", "v_CA16_2435", "v_CA16_2436",
                "v_CA16_2437", "v_CA16_2438", "v_CA16_2439", "v_CA16_2440", "v_CA16_2441", "v_CA16_2442",
                "v_CA16_4893", "v_CA16_4894", "v_CA16_4895", "v_CA16_4896", "v_CA16_4891", "v_CA16_4892",
                "v_CA16_4897", "v_CA16_4898", "v_CA16_4899", "v_CA16_4900", "v_CA16_4901",
                "v_CA16_5099", "v_CA16_5102", "v_CA16_5108", "v_CA16_5117", "v_CA16_5120", "v_CA16_5123",
                "v_CA16_5603", "v_CA16_5606", "v_CA16_5609", "v_CA16_5663", "v_CA16_5669", "v_CA16_5672", "v_CA16_5675",
                "v_CA16_5678", "v_CA16_5681", "v_CA16_5684", "v_CA16_5687", "v_CA16_5690",
                "v_CA16_4002", "v_CA16_4806", "v_CA16_4014", "v_CA16_4044", "v_CA16_4266", "v_CA16_4329", "v_CA16_4611", "v_CA16_4698",
                "v_CA16_4743", "v_CA16_4800", "v_CA16_3408", "v_CA16_3411", "v_CA16_3435")
  } else if (year == "2021") {
    vectors = c("v_CA21_434", "v_CA21_435", "v_CA21_440", "v_CA21_441", "v_CA21_442",
                "v_CA21_456", "v_CA21_477", "v_CA21_480", "v_CA21_483", "v_CA21_486",
                "v_CA21_489", "v_CA21_493", "v_CA21_494", "v_CA21_495", "v_CA21_496",
                "v_CA21_497", "v_CA21_560", "v_CA21_561", "v_CA21_562", "v_CA21_722",
                "v_CA21_725", "v_CA21_728", "v_CA21_731", "v_CA21_734", "v_CA21_737",
                "v_CA21_740", "v_CA21_743", "v_CA21_746", "v_CA21_749",
                "v_CA21_752", "v_CA21_945", "v_CA21_946", "v_CA21_947", "v_CA21_948",
                "v_CA21_949", "v_CA21_950", "v_CA21_951", "v_CA21_952", "v_CA21_953",
                "v_CA21_954", "v_CA21_955", "v_CA21_956", "v_CA21_957", "v_CA21_958", 
                "v_CA21_959", "v_CA21_960", "v_CA21_1040", "v_CA21_1085", "v_CA21_4407",
                "v_CA21_4410", "v_CA21_4434", "v_CA21_4971", "v_CA21_4974", "v_CA21_4977",
                "v_CA21_5208", "v_CA21_5109", "v_CA21_5205", "v_CA21_5094", "v_CA21_5202",
                "v_CA21_4809", "v_CA21_4306", "v_CA21_4307", "v_CA21_4309", "v_CA21_4310",
                "v_CA21_4311", "v_CA21_4312", "v_CA21_4313", "v_CA21_4314", "v_CA21_4315", 
                "v_CA21_4317", "v_CA21_4318", "v_CA21_5820", "v_CA21_5832", "v_CA21_5841", 
                "v_CA21_5844", "v_CA21_5868", "v_CA21_5880", "v_CA21_5889", "v_CA21_5892",
                "v_CA21_6498", "v_CA21_6501", "v_CA21_6504", "v_CA21_6570", "v_CA21_6576",
                "v_CA21_6579", "v_CA21_6582", "v_CA21_6585", "v_CA21_6588", "v_CA21_6591",
                "v_CA21_6594", "v_CA21_6597", "v_CA21_7638", "v_CA21_7641", "v_CA21_7644", 
                "v_CA21_7647", "v_CA21_7650", "v_CA21_7653", "v_CA21_7659", "v_CA21_7662", 
                "v_CA21_7665", "v_CA21_7668", "v_CA21_7671")
  }
  
  return(vectors)
}

# FUnction to get last two digits of year
get_dataset_id <- function(year) {
  paste0("CA", substr(as.character(year), 3, 4))
}

# Function to get the top 50 cities
get_top_cities <- function(year) {
  regions <- list_census_regions(paste0("CA", substr(as.character(year), 3, 4)))
  csd_regions <- regions[regions$level == "CSD", ]
  
  # Sort by population and take top 50
  top_cities <- csd_regions %>%
    arrange(desc(pop)) %>%
    head(50)
  
  # Clean names for display
  top_cities$name <- gsub("[^A-Za-z0-9_À-ÿ\\-]", "_", top_cities$name)
  
  return(top_cities)
}

# UI
ui <- fluidPage(
  # Theme
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  # Title
  titlePanel("Canadian Census Data Explorer"),
  
  # Layout
  fluidRow(
    column(3,
           wellPanel(
             # Year selection
             radioButtons("year", "Select Census Year:",
                          choices = c("2011", "2016", "2021"),
                          selected = "2016"),
             # Level selection
             radioButtons("level", "Select Geographic Level:",
                          choices = c("Census Tract" = "CT", "Dissemination Area" = "DA", "Census Subdivision" = "CSD"),
                          selected = "CT"),
             
             # City selection
             uiOutput("citySelector"),
             
             # Download button
             downloadButton("downloadData", "Download CSV", 
                            class = "btn-primary btn-block"),
             
             # Status message
             htmlOutput("statusMessage"),
             
             # Help text
             tags$hr(),
             tags$p("Select a year and one or more cities, then click 'Download CSV' to get the data."),
             tags$p("The data is retrieved at the selected geographic level for each selected city.")
           )
    ),
    column(9,
           tabsetPanel(
             # Data preview tab
             tabPanel("Data Preview", 
                      DT::dataTableOutput("dataPreview"),
                      tags$div(
                        style = "margin-top: 20px; color: #666;",
                        tags$p("This preview shows a sample of the data that will be downloaded."),
                        tags$p("The full dataset may contain more rows and columns.")
                      )),
             
             # Vector information tab
             tabPanel("Data Dictionary", 
                      tags$h4("Census Variables Included"),
                      tags$p("This table shows the variables (vectors) included in the selected census year."),
                      DT::dataTableOutput("vectorInfo")),
             
             # About tab
             tabPanel("About", 
                      tags$h3("About This App"),
                      tags$p("This app allows you to download Canadian census data for the top 50 cities by population."),
                      tags$p("Data is sourced from Statistics Canada via the cancensus R package."),
                      tags$h4("How to Use"),
                      tags$ol(
                        tags$li("Select a census year (2011, 2016, or 2021)"),
                        tags$li("Select one or more cities from the dropdown"),
                        tags$li("Choose the geographic level (Dissemination Area or Census Tract)"),
                        tags$li("Click 'Download CSV' to save the data to your computer")
                      ),
                      tags$h4("Notes"),
                      tags$ul(
                        tags$li("Large selections may take a while to process"),
                        tags$li("The variables included differ slightly between census years"),
                        tags$li("The data preview shows only a subset of the complete data")
                      ))
           )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Cache for census data
  options(cancensus.cache_path = "census_cache")
  
  # Reactive value for the selected year's cities
  cities <- reactive({
    req(input$year)
    withProgress(message = 'Loading cities...', {
      get_top_cities(input$year)
    })
  })
  
  # Render the city selection UI
  output$citySelector <- renderUI({
    req(cities())
    city_data <- cities()
    
    # Create named list for pickerInput
    city_choices <- setNames(
      city_data$region,
      paste0(city_data$name, " (Pop: ", format(city_data$pop, big.mark=","), ")")
    )
    
    # Create a picker input with checkbox options
    pickerInput(
      "selectedCities",
      "Select Cities:",
      choices = city_choices,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 3",
        `count-selected-text` = "{0} cities selected",
        `live-search` = TRUE
      )
    )
  })
  
  # Reactive expression for vectors
  vectors <- reactive({
    req(input$year)
    withProgress(message = 'Loading variables...', {
      get_year_vectors(input$year)
    })
  })
  
  # Reactive expression for vector information
  vectorInfo <- reactive({
    req(input$year, vectors())
    withProgress(message = 'Getting variable info...', {
      dataset_id <- paste0("CA", substr(input$year, 3, 4)) 
      all_vectors <- list_census_vectors(dataset_id)
      vector_info <- all_vectors[all_vectors$vector %in% vectors(), c("vector", "label")]
      return(vector_info)
    })
  })
  
  # Output vector information
  output$vectorInfo <- DT::renderDataTable({
    req(vectorInfo())
    DT::datatable(
      vectorInfo(),
      options = list(
        pageLength = 15,
        scrollY = "500px",
        scrollCollapse = TRUE
      ),
      colnames = c("Vector ID", "Description")
    )
  })
  
  # Reactive expression for census data
  censusData <- reactive({
    req(input$selectedCities, input$year, vectors(), input$level)
    
    if(length(input$selectedCities) == 0) {
      return(NULL)
    }
    
    # Calculate total cities to process
    total_cities <- length(input$selectedCities)
    
    withProgress(message = 'Fetching census data...', value = 0, {
      
      # Initialize an empty list to store results
      all_data <- list()
      
      # Loop through each selected city
      for(i in seq_along(input$selectedCities)) {
        # Update progress
        incProgress(1/total_cities, detail = paste("Processing city", i, "of", total_cities))
        
        city_id <- input$selectedCities[i]
        
        # Try to get census data for this city
        tryCatch({
          city_data <- get_census(
            dataset = paste0("CA", substr(input$year, 3, 4)),
            regions = list(CSD = city_id),
            vectors = vectors(),
            level = input$level
          )
          
          # Add to the list
          all_data[[i]] <- city_data
          
        }, error = function(e) {
          # Log the error but continue with other cities
          message("Error fetching data for city ", city_id, ": ", e$message)
        })
      }
      
      # Combine all data frames if we have any
      if(length(all_data) > 0) {
        combined_data <- do.call(rbind, all_data)
        return(combined_data)
      } else {
        return(NULL)
      }
    })
  })
  
  # Preview table
  output$dataPreview <- DT::renderDataTable({
    data <- censusData()
    if (is.null(data)) {
      return(data.frame(Message = "No data available. Please select at least one city and try again."))
    }
    
    # Base preview columns
    preview_cols <- c("GeoUID", "Region Name", "Population", "Households")
    
    # Add a few vector columns (if available)
    vector_cols <- grep("^v_CA", colnames(data), value = TRUE)
    if (length(vector_cols) > 0) {
      preview_cols <- c(preview_cols, head(vector_cols, 5))
    }
    
    # Keep only valid column names
    preview_cols <- preview_cols[preview_cols %in% colnames(data)]
    
    # If no valid columns found, just show first 8 columns
    if (length(preview_cols) == 0) {
      preview_cols <- head(colnames(data), 8)
    }
    
    # Limit to first 10 rows
    preview_data <- head(data[, preview_cols, drop = FALSE], 10)
    
    DT::datatable(
      preview_data,
      options = list(scrollX = TRUE, pageLength = 5),
      caption = "Preview of Census Data (first 10 rows)"
    )
  })
  
  
  # Status message
  output$statusMessage <- renderUI({
    data <- censusData()
    
    if(is.null(data)) {
      if(length(input$selectedCities) == 0) {
        HTML("<div class='alert alert-info'>Please select at least one city to continue.</div>")
      } else {
        HTML("<div class='alert alert-warning'>No data available. Please check your selections.</div>")
      }
    } else {
      HTML(paste0(
        "<div class='alert alert-success'>",
        "<b>Data ready:</b> ", nrow(data), " rows, ", ncol(data), " columns.",
        "</div>"
      ))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("census_", input$year, "_", Sys.Date(), ".zip")
    },
    content = function(file) {
      tryCatch({
        data <- censusData()
        
        # Check data
        if (is.null(data)) {
          stop("No data available for download.")
        }
        
        # Ensure zip lib is loaded
        if (!requireNamespace("zip", quietly = TRUE)) {
          stop("The 'zip' package is required but not installed.")
        }
        
        # Temp files
        temp_csv <- tempfile(fileext = ".csv")
        temp_txt <- tempfile(fileext = ".txt")
        
        # Write CSV and metadata
        write.csv(data, temp_csv, row.names = FALSE)
        writeLines(c(
          "Canadian Census Data Export",
          paste0("Date: ", Sys.Date()),
          paste0("Year: ", input$year),
          paste0("Geographic Level: ", input$level),
          paste0("Selected Cities: ", paste(input$selectedCities, collapse = ", "))
        ), con = temp_txt)
        
        # Zip the files
        zip::zipr(zipfile = file, files = c(temp_csv, temp_txt))
        
      }, error = function(e) {
        # Log error for debug
        message("Download error: ", e$message)
      })
    }
  )
}

# Run the app
shinyApp(ui, server)