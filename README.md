# Cholera Data Insights (R Shiny web application)

# Web Application Link:  [Cholera Data Insights](https://choleradatainsights.shinyapps.io/solve2/)

# Loading required libraries:

    library(shinythemes)     # For applying themes to the Shiny app

    library(leaflet)         # For creating interactive maps

    library(ggplot2)         # For creating charts and plots

    library(dplyr)           # For data manipulation

    library(readxl)          # For reading Excel files

     library(DT)              # For creating interactive data tables


    # Load the dataset
    # The Excel file 'cholera - geoT.xlsx' is assumed to be in the app folder. The data is loaded into 'my_data' and converted to a data frame 'my_data_df'.
    my_data <- read_excel("cholera - geoT.xlsx")
    my_data_df <- as.data.frame(my_data)

    # Summarizing the dataset by country
    # This section groups the data by 'Countries', and for each country calculates:
    # - TotalCases: Sum of cholera cases
    # - TotalFatalities: Sum of fatalities
    # - Average Longitude and Latitude (for mapping purposes)
     country_summary <- my_data_df %>%
     group_by(Countries) %>%
     summarise(
    TotalCases = sum(Cases, na.rm = TRUE),
    TotalFatalities = sum(Fatalities, na.rm = TRUE),
    Longitude = mean(Longitude, na.rm = TRUE),
    Latitude = mean(Latitude, na.rm = TRUE)
    )




  
#
# UI (User Interface) part of the app

    ui <- fluidPage(
    theme = shinytheme("cerulean"),   # Use the 'cerulean' theme for a clean blue look

    # Header section with title, centered and styled with background color and padding
    tags$div(
    style = "text-align: center; background-color: lightblue; padding: 20px; border-radius: 10px;",
    h2("Cholera Data Insights")     # Display app title
    ),
  
    # Navigation bar with two main tabs: Home and Data
    navbarPage(
    "",
    
    # 'Home' tab contains a sidebar to select a country and a map displaying data
    tabPanel("Home",
             sidebarPanel(
               # Dropdown to select a country or view data for all countries
               selectInput("selectCountry", "Choose a country:", choices = c("All Countries", unique(my_data_df$Countries)))
             ),
             mainPanel(
               leafletOutput("world_map")   # Display a world map with outbreak data
             )
    ),
    
    # 'Data' tab has sub-tabs for visualizing statistics, cases, fatalities, and fatality rate
    tabPanel("Data",
             sidebarPanel(
               # Dropdown to select a country and a year range for data filtering
               selectInput("selectCountryData", "Choose a country:", choices = c("All Countries", unique(my_data_df$Countries))),
               tags$h3("Select Year Range:"),   # Year range selection
               selectInput("selectYearRange", "Choose a year range:", choices = c("All Years", "1949-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2016"))
             ),
             mainPanel(
               # Sub-tabs for different data visualizations and a download report button
               tabsetPanel(
                 tabPanel("Statistics", plotOutput("outbreak_plot", height = "250px", width = "90%")),    # Plot for top outbreaks
                 tabPanel("Number of Cases", plotOutput("Number_of_Cases")),                              # Plot for number of cases
                 tabPanel("Number of Fatalities", plotOutput("Number_of_Fatalities")),                    # Plot for number of fatalities
                 tabPanel("Fatality Rate", plotOutput("Fatality_Rate")),                                  # Plot for fatality rate
                 tabPanel("Download Report", downloadButton("download_report", "Download Country Data Report"))  # Button to download report
               )
             )
    )
    )
    )

#
# Server logic part of the app

    server <- function(input, output, session) {

    # Reactive expression to filter the data based on selected country and year range
    filtered_data <- reactive({
     req(input$selectCountryData)    # Ensure the country selection is available
    filtered <- my_data_df
    
    # Filter by selected country
    if (input$selectCountryData != "All Countries") {
      filtered <- filtered %>% filter(Countries == input$selectCountryData)
    }
    
    # Filter by selected year range
    if (input$selectYearRange != "All Years") {
      year_range <- strsplit(input$selectYearRange, "-")[[1]]
      start_year <- as.numeric(year_range[1])
      end_year <- as.numeric(year_range[2])
      filtered <- filtered %>% filter(Year >= start_year & Year <= end_year)
    }
    
    return(filtered)   # Return filtered data
    })
  
    # Output for total cases
    output$total_cases <- renderText({
    total_cases <- sum(filtered_data()$Cases, na.rm = TRUE)
    paste("Total Cases:", total_cases)
    })
  
     # Output for total fatalities
     output$fatalities <- renderText({
    fatalities <- sum(filtered_data()$Fatalities, na.rm = TRUE)
    paste("Total Fatalities:", fatalities)
      })
  
    # Plot for top outbreak years
    output$outbreak_plot <- renderPlot({
    avg_cases <- my_data_df %>%
    summarise(Average = mean(Cases, na.rm = TRUE)) %>%
      pull(Average)
    
    # Filter and summarize data for years with higher than average cases
    outbreak_data <- filtered_data() %>%
      filter(Cases > avg_cases) %>%
      group_by(Year) %>%
      summarise(TotalCases = sum(Cases, na.rm = TRUE)) %>%
      arrange(desc(TotalCases)) %>%
      slice_head(n = 20) %>%
      ungroup()
    
    # Plotting a line chart with outbreak data
    ggplot(outbreak_data, aes(x = factor(Year), y = TotalCases)) +
      geom_line(group = 1, color = "blue", linewidth = 1) +
      geom_point(color = "red") +
      labs(title = "Top Outbreak Years", x = "Years", y = "Number of Cases") +
      theme_minimal()
     })
  
    # Render the world map using Leaflet
    output$world_map <- renderLeaflet({
    map <- leaflet(data = country_summary) %>%
      addTiles() %>%
      # Add circle markers for each country, with color based on total cases
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        color = ~colorQuantile("YlOrRd", TotalCases)(TotalCases),
       popup = ~paste("Country:", Countries, "<br>Total Cases:", TotalCases, "<br>Total Fatalities:", TotalFatalities)
     )
    
    # Zoom in on the selected country if a specific country is chosen
    if (input$selectCountry != "All Countries") {
      selected_country <- country_summary %>%
        filter(Countries == input$selectCountry)
      
      map <- map %>% setView(lng = selected_country$Longitude, lat = selected_country$Latitude, zoom = 4)
    }
    
    map   # Return the map
    })
  
    # Plot for the number of cases
    output$Number_of_Cases <- renderPlot({
    req(filtered_data())   # Ensure filtered data is available
    
    # Plotting a bar chart for the number of cases
    ggplot(filtered_data(), aes(x = Year, y = Cases)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = paste("Number of Cases in", input$selectCountryData), x = "Years", y = "Number of Cases") +
      theme_minimal()
    })
  
    # Plot for the number of fatalities
    output$Number_of_Fatalities <- renderPlot({
    req(filtered_data())
    
    # Plotting a scatter plot for fatalities
    ggplot(filtered_data(), aes(x = Year, y = Fatalities)) +
      geom_point(size = 3, color = "red") +
      labs(title = paste("Number of Fatalities in", input$selectCountryData), x = "Years", y = "Number of Fatalities") +
      theme_minimal()
    })
  
    # Plot for the fatality rate
    output$Fatality_Rate <- renderPlot({
    req(filtered_data())
    
    # Plotting a line chart for the fatality rate (Fatalities/Cases * 100)
    ggplot(filtered_data(), aes(x = Year)) +
      geom_line(aes(y = (Fatalities / Cases) * 100, color = "Fatality Rate")) +
      labs(title = paste("Fatality Rate in", input$selectCountryData), x = "Years", y = "Fatality Rate (%)") +
      theme_minimal()
    })
  
     # Download report feature
    output$download_report <- downloadHandler(
    filename = function() {
      paste(input$selectCountryData, "cholera_report.xlsx", sep = "_")   # Filename format
    },
    content = function(file) {
      write.csv(filtered_data(), file)   # Write the filtered data to a CSV file
    }
    )
    }

    
#    
# Run the app
    shinyApp(ui = ui, server = server)  
