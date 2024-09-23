# Cholera Data Insights (R Shiny web application)


# Loading required libraries:

library(shiny)  # shiny: for building web applications


library(shinythemes)   # shinythemes: to apply themes for UI (User Interface)


library(leaflet)  # leaflet: for interactive maps


library(ggplot2) # ggplot2: for creating plots and data visualizations


library(dplyr) # dplyr: for data manipulation (e.g., filtering, summarizing)


library(readxl) # readxl: to read Excel files


library(DT) # DT: to render DataTables in the UI


#
my_data <- read_excel("D:/stage3/cholera - geo.xlsx")  # to Read the Excel file into R. The dataset contains cholera data.

#
my_data_df <- as.data.frame(my_data)  # Converting the dataset to a data frame for easier manipulation.

# 

country_summary <- my_data_df %>%   #Summarizing the dataset by countries to calculate total cases, fatalities, and average geographic locations (longitude and latitude)    
  group_by(Countries) %>%
  summarise(
    TotalCases = sum(Cases, na.rm = TRUE),  # Summing total cholera cases for each country
    TotalFatalities = sum(Fatalities, na.rm = TRUE),  # Summing total fatalities for each country
    Longitude = mean(Longitude, na.rm = TRUE),  # Averaging longitude values for map display
    Latitude = mean(Latitude, na.rm = TRUE)  # Averaging latitude values for map display
  )

# UI (User Interface) part of the app


ui <- fluidPage(
  # Apply "cerulean" theme from shinythemes to give the app a specific look
  theme = shinytheme("cerulean"),
  
  #
  #Title section with light blue background and centered text
  tags$div(style = "text-align: center; background-color: lightblue; padding: 20px; border-radius: 10px;",
           h2("Cholera Data Insights")),
  
  # 
  #Creating a navigation bar for switching between different pages
  navbarPage(
    "",
    tabPanel("Home",  # First tab for the map
             sidebarPanel(
               # Dropdown for selecting a country, defaulting to "All Countries"
               selectInput("selectCountry", "Choose a country:", choices = c("All Countries", unique(my_data_df$Countries)))
             ),
             mainPanel(
               
               #Leaflet map to display countries and cholera data
               leafletOutput("world_map")
             )
    ),
    tabPanel("Data",  # Second tab for the data and statistics
             sidebarPanel(
               
               #Dropdown for selecting a country for data analysis
               selectInput("selectCountryData", "Choose a country:", choices = c("All Countries", unique(my_data_df$Countries))),
               #Heading and dropdown for selecting a year range
               tags$h3("Select Year Range:"),
               selectInput("selectYearRange", "Choose a year range:",
                           choices = c("All Years", "1949-1959", "1960-1969", "1970-1979", "1980-1989",
                                       "1990-1999", "2000-2009", "2010-2016"))
             ),
             mainPanel(
               
               #Tabbed panel for different types of data and visualizations
               tabsetPanel(
                 tabPanel("Statistics", 
                          # Displaying the total cases and fatalities as text
                          verbatimTextOutput("total_cases"),
                          verbatimTextOutput("fatalities"),
                          # Displaying the outbreak plot
                          plotOutput("outbreak_plot", height = "250px", width = "90%")
                 ),
                 tabPanel("Number of Cases", 
                          #Displaying a plot for number of cases
                          plotOutput("Number_of_Cases")
                 ),
                 tabPanel("Number of Fatalities", 
                          
                          #Displaying a plot for number of fatalities
                          plotOutput("Number_of_Fatalities")
                 ),
                 tabPanel("Fatality Rate", 
                          #Displaying a plot for fatality rate
                          plotOutput("Fatality_Rate")
                 ),
                 tabPanel("Download Report",  
                          #Button to download the country report
                          downloadButton("download_report", "Download Country Data Report")
                 )
               )
             )
    )
  )
)

# Server logic part of the app


server <- function(input, output, session) {
  
  #Reactive expression to filter data based on selected country and year range
  filtered_data <- reactive({
    req(input$selectCountryData)  #Ensures a country is selected before filtering data
    
    filtered <- my_data_df  # Start with the full dataset
    
    if (input$selectCountryData != "All Countries") {
      # If a specific country is selected, filter data to only that country
      filtered <- filtered %>% filter(Countries == input$selectCountryData)
    }
    
    # If a specific year range is selected, filter data to that year range
    if (input$selectYearRange != "All Years") {
      year_range <- strsplit(input$selectYearRange, "-")[[1]]
      start_year <- as.numeric(year_range[1])
      end_year <- as.numeric(year_range[2])
      filtered <- filtered %>% filter(Year >= start_year & Year <= end_year)
    }
    
    return(filtered)  # Return the filtered data
  })
  #
  #Render the total number of cases for the selected country and year range
  output$total_cases <- renderText({
    total_cases <- sum(filtered_data()$Cases, na.rm = TRUE)  # Summing up cases
    paste("Total Cases:", total_cases)  # Displaying the result as text
  })
  #
  #Render the total number of fatalities
  output$fatalities <- renderText({
    fatalities <- sum(filtered_data()$Fatalities, na.rm = TRUE)  # Summing up fatalities
    paste("Total Fatalities:", fatalities)  # Displaying the result as text
  })
  #
  #Render the outbreak plot for the Statistics panel
  output$outbreak_plot <- renderPlot({
    avg_cases <- my_data_df %>%
      summarise(Average = mean(Cases, na.rm = TRUE)) %>%
      pull(Average)  # Calculate the average number of cases
    
    outbreak_data <- filtered_data() %>%
      filter(Cases > avg_cases) %>%
      group_by(Year) %>%
      summarise(TotalCases = sum(Cases, na.rm = TRUE)) %>%
      arrange(desc(TotalCases)) %>%
      slice_head(n = 20) %>%
      ungroup()  # Prepare data for plotting top outbreak years
    
    # Plot showing top outbreak years
    ggplot(outbreak_data, aes(x = factor(Year), y = TotalCases)) +
      geom_line(group = 1, color = "blue", size = 1) +
      geom_point(color = "red") +
      labs(title = "Top Outbreak Years", x = "Years", y = "Number of Cases") +
      theme_minimal()
  })
  
  #
  #Render a Leaflet map for countries
  output$world_map <- renderLeaflet({
    map <- leaflet(data = country_summary) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,  # Longitude from summarized data
        lat = ~Latitude,  # Latitude from summarized data
        color = ~colorQuantile("YlOrRd", TotalCases)(TotalCases),  # Color the markers by the total cases
        popup = ~paste("Country:", Countries, "<br>Total Cases:", TotalCases, "<br>Total Fatalities:", TotalFatalities)  # Popup info
      )
    
    # If a specific country is selected, zoom the map to that country
    if (input$selectCountry != "All Countries") {
      selected_country <- country_summary %>%
        filter(Countries == input$selectCountry)
      
      map <- map %>% setView(lng = selected_country$Longitude, lat = selected_country$Latitude, zoom = 4)
    }
    
    map  # Return the created map
  }) 
  
  #
  #Render a bar plot for the number of cases
  output$Number_of_Cases <- renderPlot({
    req(filtered_data())  # Ensure the filtered data is available
    
    # Bar plot for number of cases by year
    ggplot(filtered_data(), aes(x = Year, y = Cases)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = paste("Number of Cases in", input$selectCountryData), x = "Years", y = "Number of Cases") +
      theme_minimal()
  })
  
  #
  #Render a scatter plot for fatalities
  output$Number_of_Fatalities <- renderPlot({
    req(filtered_data())  # Ensure the filtered data is available
    
    # Scatter plot for fatalities by year
    ggplot(filtered_data(), aes(x = Year, y = Fatalities)) +
      geom_point(size = 3, color = "red") +
      labs(title = paste("Number of Fatalities in", input$selectCountryData), x = "Years", y = "Number of Fatalities") +
      theme_minimal()
  })
  
  #
  #Render a line plot for the fatality rate
  output$Fatality_Rate <- renderPlot({
    req(filtered_data())  # Ensure the filtered data is available
    
    # Line plot for fatality rate over the years
    ggplot(filtered_data(), aes(x = Year)) +
      geom_line(aes(y = (Fatalities / Cases) * 100, color = "Fatality Rate")) +
      labs(title = paste("Fatality Rate in", input$selectCountryData), x = "Years", y = "Fatality Rate (%)") +
      theme_minimal()
  })
  
  #
  #Report generation: Allows downloading a report based on the selected country and year range
  output$download_report <- downloadHandler(
    filename = function() {
      paste(input$selectCountryData, "-cholera-report.csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file)  # Write the filtered data to a CSV file
    }
  )
}
#
# Run the app
shinyApp(ui = ui, server = server)
