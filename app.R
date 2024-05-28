# List of packages to check and install if needed
packages_to_install <- c("shiny", "ggplot2", "dplyr","plotly","reshape2","corrplot","lubridate","shinyjs")

# Check and install packages if needed
for (package in packages_to_install) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
}

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(reshape2)
library(corrplot)
library(lubridate)
library(shinyjs)
# reading our data
data <- read.csv("supermarket_sales - Sheet1.csv")
# ==========================[1] DATA PREPROCESSING:=====================================

# Check if there are any duplicated rows
anyDuplicated(data)
# extract column month from the Date column
data$Month <- format(as.Date(data$Date, format = "%m/%d/%y"), "%b")
# Drop specified columns
data <- data %>%
  select(-Invoice.ID, -Time, -cogs, -gross.margin.percentage)

# View unique values in 'Branch' and 'Customer type'
unique_branch <- unique(data$Branch)
unique_customer_type <- unique(data$Customer.type)
unique_gender <- unique(data$Gender)
# Print unique values
cat("Unique values in 'Branch':", unique_branch, "\n")
cat("Unique values in 'Customer type':", unique_customer_type, "\n")
cat("Unique values in 'Gender':", unique_gender, "\n")

# Summary statistics for numeric columns
summary(data)

# check nulls
any(is.na(data))
# get columns info
str(data)

data$Date <- mdy(data$Date)

str(data$Date)

# Display the first few rows of the data
head(data)
# ===========================SHINY APP=================================================
# Define UI for application that draws a histogram
ui <- fluidPage(

  useShinyjs(),  # Initialize shinyjs
  
  headerPanel(
    div(
      h1("Supermarket Sales Dataset Explorer", class = "title-class"),
      actionButton("toggleMode", "Toggle Dark Mode")
    )
  ),
  
  # Tabs
  tabsetPanel(
  # ================== TAP 1 ============================
    tabPanel("Category and Comparison", 
             sidebarLayout(
               sidebarPanel(
                 # Dropdown menu for selecting specific category
                 selectInput("categoryDropdown", "Select Category", choices = c("Product.line", "Gender", "Branch", "Payment", "Customer.type")),
                 # Radio buttons for selecting unit
                 radioButtons("unitToggle", "Switch Between Units:",
                              choices = c("Absolute Values", "Percentage Changes"),
                              selected = "Absolute Values"),
                 # Checkboxes to choose between different categories for comparison
                 checkboxGroupInput("compareCheckbox", "Select Categories for Comparison", choices = c("Rating", "Unit.price", "Quantity", "Tax.5.", "Total", "gross.income")),
              ),
               
               # Main panel with plots
               mainPanel(
                 
                 fluidRow(
                   
                   column(6, plotOutput("dynamicPieChart"), br(), br(), height = 400),
                   column(8, plotlyOutput("ratingBoxPlot"), br(), br(), height = 200),
                   column(8, plotlyOutput("UnitpriceBoxPlot"), br(), br(), height = 200),
                   column(8, plotlyOutput("QuantityBoxPlot"), br(), br(), height = 200),
                   column(8, plotlyOutput("Tax.5.BoxPlot"), br(), br(), height = 200),
                   column(8, plotlyOutput("TotalBoxPlot"), br(), br(), height = 200),
                   column(8, plotlyOutput("grossincomeBoxPlot"), br(), br(), height = 200)
                 )
               )
             )
    ),
 # ================== TAP 2 ============================
    tabPanel("Date and Mode", 
             sidebarLayout(
               sidebarPanel(
                 
                   selectInput(inputId = "dv", label = "Category",
                               choices = c("Unit.price", "Quantity","Tax.5.","gross.income","Total","Rating"),
                               selected = "Unit.price"),
                   selectInput(inputId = "Branch", "Branch",
                               choices = c("A", "B", "C"),
                               multiple = TRUE,
                               selected = c("A")),
                   dateRangeInput(inputId = "date", label = "Date range",
                                  start = min(data$Date),
                                  end   = max(data$Date)),
                   
                   # Radio buttons for choosing between daily and cumulative data
                   radioButtons("dataMode", "Select Data Mode", choices = c("Daily", "Cumulative"), selected = "Daily"),
                 ),
            
               # Main panel with plots
               mainPanel(
                 column(12, plotlyOutput("plot"), br(), br()),
                 column(12, plotlyOutput("dateHistogram"), br(), br()),
                 column(12, plotlyOutput("cumulativeDateHistogram"), br(), br())

               )
             )
    ),
 # ================== TAP 3 ============================
    tabPanel("Visualization Options", 
             sidebarLayout(
               sidebarPanel(

                 selectInput("productLineDropdown", "Select Product Line", choices = unique(data$Product.line)),

                 # Customer type selector
                 selectInput("customer_type_selector", "Select Customer Type:",
                             c("All", "Member", "Normal"), selected = "All"),
                 
                 p("---------------------------------------------------------------------"),
                 # Checkboxes to dynamically show/hide different visualizations
                 checkboxGroupInput("visualizationCheckbox", "Select Visualizations to Display", choices = c("Line Chart", "Histogram", "Heatmap", "Scatter Total Gender Plot", "Combined Plot"))
                 
                 ),
               
               # Main panel with plots (if any)
               mainPanel(
                 plotlyOutput("histPlot"),
                 column(12, plotlyOutput("dateLineChart"), br(), br()),
                 column(12, plotlyOutput("histogram"), br(), br()),
                 column(12, plotOutput("heatmap"), br(), br(), height = 800 ),
                 column(12, plotlyOutput("scatterTotalGenderPlot"), br(), br()),
                 column(12, plotlyOutput("combinedPlot", height = 400)),
                 
               )
             )
    ),
 # ================== TAP 4 ============================
 tabPanel("City Dropdown tab", 
          sidebarLayout(
            sidebarPanel(
              # Checkboxes to dynamically show/hide different visualizations
              selectInput("cityDropdown", "Select City", choices = unique(data$City),selected = "Yangon"),

            ),
            # Main panel with plots (if any)
            mainPanel(
               plotOutput("barPlot"),
               
              )
            )),
 
 # ============================= TAB 5 =======================================
 # New tab for instructions
 tabPanel("How to Use",
          h2("Welcome to the Supermarket Sales Dashboard!", style = "color:green"),
          p("This dashboard allows you to explore and visualize the Supermarket Sales dataset."),
          
          h3("Category and Comparison", class = "custom-header"),
          h4("1. Select a category from the dropdown menu to explore different aspects of the dataset.", class = "custom-paragraph"),
          h4("2. Choose between 'Absolute Values' and 'Percentage Changes' for units.", class = "custom-paragraph"),
          h4("3. Use checkboxes to select categories for comparison.", class = "custom-paragraph"),
          h4("4. View box plots for selected categories.", class = "custom-paragraph"),
          h4("5. Explore the distribution pie chart based on the selected category.", class = "custom-paragraph"),
          h4("6. Customize the analysis based on your preferences.", class = "custom-paragraph"),
          
          h3("Date and Mode", class = "custom-header"),
          h4("1. Choose a category from the dropdown menu.", class = "custom-paragraph"),
          h4("2. Select branches, date range, and daily/cumulative data mode.", class = "custom-paragraph"),
          h4("3. View scatter plots and histograms for selected data.", class = "custom-paragraph"),
          h4("4. Customize the analysis based on your preferences.", class = "custom-paragraph"),
          h4("5. Toggle between 'Daily' and 'Cumulative' modes for date histograms.", class = "custom-paragraph"),
          
          h3("Visualization Options", class = "custom-header"),
          h4("1. Select visualizations using checkboxes.", class = "custom-paragraph"),
          h4("2. Explore visualizations like line charts, histograms, heatmaps, and more.", class = "custom-paragraph"),
          h4("3. Customize the dashboard based on your preferences.", class = "custom-paragraph"),
          h4("4. Clear selected visualizations by unchecking checkboxes.", class = "custom-paragraph"),
          h4("5. Adjust the analysis based on your needs.", class = "custom-paragraph"),
          
          h3("Dark/Light Mode", class = "custom-header"),
          h4("1. Toggle between Dark and Light mode using the 'Toggle Dark Mode' button.", class = "custom-paragraph"),
          h4("2. Adjust the visualization styles based on your preference.", class = "custom-paragraph"),
          h4("3. Enhance the dashboard appearance for better readability.", class = "custom-paragraph"),
          h4("4. Toggle the mode as needed to match your working environment.", class = "custom-paragraph"),
          
          h3("Tips and Notes", class = "custom-header"),
          h4("1. Hover over different elements in plots for additional information.", class = "custom-paragraph"),
          h4("2. Experiment with different combinations of selections for a more in-depth analysis.", class = "custom-paragraph"),
          h4("3. Feel free to reach out for support or additional features.", class = "custom-paragraph"),
          h4("4. Explore the dataset from various angles to gain insights.", class = "custom-paragraph"),
          h4("5. Have fun exploring the Supermarket Sales data!", class = "custom-paragraph"),
 ),
 
))
# Define server logic required to draw the plots
# ======================== SERVER =====================================
server <- function(input, output) {
  
  # =================== Histogram  ========================
  output$histPlot <- renderPlotly({
    selected_customer_type <- input$customer_type_selector
    data1 <- filtered_data()
    
    if (selected_customer_type == "All") {
      ggplot(data1, aes(Quantity, fill = Customer.type)) +
        geom_histogram(binwidth = 1, position = "dodge") +
        labs(title = "Histogram of Quantity by Customer Type",
             x = "Quantity",
             fill = "Customer Type")
    } else {
      ggplot(subset(data1, Customer.type == selected_customer_type), 
             aes(Quantity, fill = Customer.type)) +
        geom_histogram(binwidth = 1, position = "dodge") +
        labs(title = paste("Histogram of Quantity for Customer Type:", 
                           selected_customer_type),
             x = "Quantity",
             fill = "Customer Type")
    }
  })

  # ========================  BAR PLOT =====================================
  selectedCityData <- reactive({
    # Make sure the column names are case-sensitive and match your actual dataset
    subset(data, City == input$cityDropdown)
  })
  
  # Render the bar plot
  output$barPlot <- renderPlot({
    # Make sure the column names are case-sensitive and match your actual dataset
    ggplot(selectedCityData(), aes(x = City, y = Total)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Bar Plot for", input$cityDropdown))
  })
  selected_data <- reactive({
    df[, input$dataDropdown]
  })

  # ====================== DARK / LIGHT MODE =============================
  
  current_mode <- reactiveVal(1)
  observe({
    if (current_mode() %% 2 == 0) {
      # Apply dark mode styles
      shinyjs::runjs("document.body.style.backgroundColor = '#333';")
      shinyjs::runjs("$('.navbar').css('background-color', '#222');")  
      shinyjs::runjs("$('.plotly').css('background-color', '#222');")  
      shinyjs::runjs("$('.plot-container').css('background-color', '#222');") 
      shinyjs::runjs("$('.title-class').css('color', '#fff');")  # White title color
      shinyjs::runjs("$('.custom-paragraph').css('color', '#ccc');")  # Light gray text color
      shinyjs::runjs("$('.custom-header').css('color', '#aa7b3b');")

    } else {
      # Apply light mode styles
      shinyjs::runjs("document.body.style.backgroundColor = '#fff';")
      shinyjs::runjs("$('.navbar').css('background-color', '#f8f9fa');")
      shinyjs::runjs("$('.plotly').css('background-color', '#fff');")
      shinyjs::runjs("$('.plot-container').css('background-color', '#fff');")
      shinyjs::runjs("$('.title-class').css('color', '#000');")  # Black title color
      shinyjs::runjs("$('.custom-paragraph').css('color', '#000');")  # Black text color
      shinyjs::runjs("$('.custom-header').css('color', '#aa433b');")
    }
  })
  
  # Toggle button logic
  observeEvent(input$toggleMode, {
    current_mode(current_mode() + 1)
  })

  # ============================= handler drop down menu =========================================
  
  observe({
    selected_column <- input$categoryDropdown
    selected_unit <- input$unitToggle
    
    output$dynamicPieChart <- renderPlot({
      if (!is.null(selected_column) && selected_column != "") {
        # Calculate either absolute values or percentage changes based on user selection
        if (selected_unit == "Absolute Values") {
          column_counts <- table(data[[selected_column]])
        } else if (selected_unit == "Percentage Changes") {
          column_counts <- table(data[[selected_column]]) / nrow(data) * 100
        }
        
        column_data <- data.frame(Category = names(column_counts), Count = as.numeric(column_counts))
        
        ggplot(column_data, aes(x = "", y = Count, fill = Category)) +
          geom_bar(width = 1, stat = "identity", color = "white") +
          coord_polar(theta = "y") +
          labs(title = paste("Distribution of", selected_column, "(", selected_unit, ")")) +
          geom_text(aes(label = paste0(round(Count, 1), ifelse(selected_unit == "Percentage Changes", "%", ""))),
                    position = position_stack(vjust = 0.5))
      } else {
        # If no column is selected, render an empty plot
        ggplot() + theme_void()
      }
    })
  })
  
  # =========================== handler CheckBox =========================================
  observe({
    selected_comparison_categories <- input$compareCheckbox
    
    output$ratingBoxPlot <- renderPlotly(NULL)
    output$UnitpriceBoxPlot <- renderPlotly(NULL)
    output$QuantityBoxPlot <- renderPlotly(NULL)
    output$Tax.5.BoxPlot <- renderPlotly(NULL)
    output$TotalBoxPlot <- renderPlotly(NULL)
    output$grossincomeBoxPlot <- renderPlotly(NULL)
    if ("Rating" %in% selected_comparison_categories) {
      output$ratingBoxPlot <- renderPlotly({
        ggplot(data, aes(x = "", y = Rating)) +
          geom_boxplot(fill = "lightblue") +
          labs(title = "Box Plot of Rating")
      })
    }
    if ("Unit.price" %in% selected_comparison_categories) {
      # Box plot for Unit.price column with numbers
      output$UnitpriceBoxPlot <- renderPlotly({
        ggplot(data, aes(x = "", y = Unit.price )) +
          geom_boxplot(fill = "lightblue") +
          labs(title = "Box Plot of Unit price ")
      })
    }
    if ("Quantity" %in% selected_comparison_categories) {
      output$QuantityBoxPlot <- renderPlotly({
        ggplot(data, aes(x = "", y = Quantity )) +
          geom_boxplot(fill = "lightblue") +
          labs(title = "Box Plot of Quantity ")
      })
    }
    if ( "Tax.5." %in% selected_comparison_categories) {
      # Box plot for Tax.5. column with numbers
      output$Tax.5.BoxPlot <- renderPlotly({
        ggplot(data, aes(x = "", y = Tax.5.)) +
          geom_boxplot(fill = "lightblue") +
          labs(title = "Box Plot of Tax.5.")
      })
    }
    if ("Total" %in% selected_comparison_categories) {
      output$TotalBoxPlot <- renderPlotly({
        ggplot(data, aes(x = "", y =  Total)) +
          geom_boxplot(fill = "lightblue") +
          labs(title = "Box Plot of  Total")
      })
    }
    if ("gross.income" %in% selected_comparison_categories) {
      output$grossincomeBoxPlot <- renderPlotly({
        ggplot(data, aes(x = "", y =  gross.income)) +
          geom_boxplot(fill = "lightblue") +
          labs(title = "Box Plot of  gross.income")
      })
    }
  })
  # =========================== handler selected_data_mode=========================================
  observe({
    selected_data_mode <- input$dataMode
    
    if (selected_data_mode == "Daily") {
      output$dateHistogram <- renderPlotly({
        # Create a histogram of dates
        ggplot(data, aes(x = as.Date(Date, format = "%m/%d/%y"))) +
          geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
          labs(title = "Histogram of Dates", x = "Date", y = "Count") +
          theme_minimal()
      })
      
      # Clear the cumulative histogram output
      output$cumulativeDateHistogram <- renderPlotly(NULL)
    } else if (selected_data_mode == "Cumulative") {
      output$cumulativeDateHistogram <- renderPlotly({
        # Create a cumulative histogram of dates
        data$Date <- as.Date(data$Date, format = "%m/%d/%y")
        cumulative_freq <- data %>%
          group_by(Date) %>%
          summarise(Count = n()) %>%
          arrange(Date) %>%
          mutate(Cumulative_Count = cumsum(Count))
        
        ggplot(cumulative_freq, aes(x = Date, y = Cumulative_Count)) +
          geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha = 0.7) +
          labs(title = "Cumulative Histogram of Dates", x = "Date", y = "Cumulative Count") +
          theme_minimal()
      })
      
      # Clear the daily histogram output
      output$dateHistogram <- renderPlotly(NULL)
    }
  })
  # =========================== handler selected_visualizations =========================================
  
  filtered_data <- reactive({
    subset(data,
           Branch %in% input$Branch &
             Date >= input$date[1] & Date <= input$date[2] &
             (`Product.line` %in% input$productLineDropdown))
  })
  output$plot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes_string(x = "Date", y = input$dv, color = "Branch")) +
      geom_point(alpha = 0.5)
    
    ggplotly(gg) %>%
      layout(title = "Scatter Plot", xaxis = list(title = "Date"), yaxis = list(title = input$dv))
  })
  
  
  observe({
    selected_visualizations <- input$visualizationCheckbox
    
    # Reset the UI to remove existing visualizations
    lapply(c("histogram", "dateLineChart", "heatmap", "scatterTotalGenderPlot", "combinedPlot"), function(p) {
      output[[p]] <- renderPlotly(NULL)
    })
    output$histogram <- renderPlotly(NULL)
    output$dateLineChart <- renderPlotly(NULL)
    output$heatmap <- renderPlot(NULL)
    output$scatterTotalGenderPlot <- renderPlotly(NULL)
    output$combinedPlot <- renderPlotly(NULL)
    
    # Render selected visualizations based on checkboxes
    if ("Histogram" %in% selected_visualizations) {
      output$histogram <- renderPlotly({
        
        ggplot(data, aes(x = Rating)) +
          geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
          labs(title = "Histogram of Rating", x = "Rating", y = "Count") +
          theme_minimal()
        
      })
    }
    if ("Line Chart" %in% selected_visualizations) {
      # Line chart for Date & gross.income columns
      output$dateLineChart <- renderPlotly({
        data %>%
          filter(!is.na(Date)) %>%
          group_by(Date) %>%
          summarise(gross_income = sum(gross.income)) %>%
          plot_ly(x = ~Date, y = ~gross_income, type = "scatter", mode = "lines") %>%
          layout(title = "Total gross income Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Total gross income"))
      })
    }
    if ("Heatmap" %in% selected_visualizations) {
      output$heatmap <- renderPlot({
        numeric_data <- data %>%
          select(Unit.price, Quantity, Tax.5., Total, gross.income, Rating)
        
        # Calculate the correlation matrix
        correlation_matrix <- cor(numeric_data)
        
        # Plot the heatmap with larger text and title
        corrplot(correlation_matrix, 
                 method = "number", 
                 col = colorRampPalette(c("black", "red"))(20),
                 addCoef.col = "white", 
                 number.cex = 1.0,  # Adjust the text size
                 tl.cex = 1.2,     # Adjust the title size
                 title = "Correlation Heatmap of Numeric Columns",
                 title.cex = 1.5)  # Adjust the main title size
      })
    }
    if ("Scatter Total Gender Plot" %in% selected_visualizations) {
      output$scatterTotalGenderPlot <- renderPlotly({
        # Scatter plot for Total and Gender with colors and bigger size
        ggplot(data, aes(x = Gender, y = Total, color = Gender)) +
          geom_point(size = 3) +  # Adjust the size as needed
          labs(title = "Scatter Plot of Total Sales and Gender") +
          scale_color_manual(values = c("Male" = "blue", "Female" = "pink"))  # Adjust colors as needed
      })
    }
    if ("Combined Plot" %in% selected_visualizations) {
      output$combinedPlot <- renderPlotly({
        
        # Create a data frame for plotting
        plot_data <- data %>% 
          group_by(`Product.line`, Month) %>%
          summarise(Count = n())
        
        # Create a bar chart
        ggplot(plot_data, aes(x = `Product.line`, y = Count, fill = Month)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Product Line and Monthly Distribution") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
          geom_text(aes(label = Count), position = position_dodge(width = 1), vjust = -0.5, size = 4)
      })
    }
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
