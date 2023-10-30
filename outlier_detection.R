# Importing required libraries
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(labeling)
library(ggridges)
library(gridExtra)

 
# Initiating a "Not Selected" value variable for default field value
none <- "Not Selected"


# Creating about Authors page for our app 
about_page <- tabPanel(
  title = "About",
  titlePanel("Application details"),
  tabsetPanel(
    tabPanel(
      title = "User Manual",
      HTML(
        "<h2 style='font-size: 24px; color: #D3D3D3;'>User Manual</h2>
        <p style='font-size: 16px; color: #777;'>Welcome to the Outlier Detection App. This user manual will guide you on how to use the app effectively.</p>
        <h3 style='font-size: 18px; color: #D3D3D3;'>Step 1: Upload Data</h3>
        <p style='font-size: 16px; color: #777;'>Start by uploading a CSV file for outlier detection. Click the 'Upload a csv for outlier detection' button and select your data file.</p>
        <h3 style='font-size: 18px; color: #D3D3D3;'>Step 2: Select Variables</h3>
        <p style='font-size: 16px; color: #777;'>The app will automatically detect numeric and categoric features in your data. The numeric variable selection dropdown will only show numeric features. Select your first numeric feature and the second feature could be either categorical or another numeric variable.</p>
        <h3 style='font-size: 18px; color: #D3D3D3;'>Step 3: Run Analysis</h3>
        <p style='font-size: 16px; color: #777;'>Click the 'It's go time!' button to initiate the analysis. The app will generate outlier plots and summary tables based on your selections. If you select both numeric features- a scatter plot will be shown otherwise a numeric-categoric variable combination will show outlier plots factored by category</p>
        <h3 style='font-size: 18px; color: #FF0000;'>CAVEATS</h3>
        <p style='font-size: 16px; color: #777;' <br>-> If the count of categories in second variable you select is large, then legible plots might not be rendered.<br>-> If the second variable is 'Not Selected' then a boxplot of first variable will be rendered.<br>-> If both variables are 'Not Selected' then an error message will popup.</p>"
        )
    ),
    tabPanel(
      title = "Authors",
      HTML(
        "<h2 style='font-size: 24px; color: #777;'>Authors</h2>
        <p style='font-size: 18px; color: #D3D3D3;'>Tushar Badhwar (tb3070)</p>
        <p style='font-size: 18px; color: #D3D3D3;'>Harmeet Singh Bagga (hb2783)</p>"
      )
    )
  )
)


# Main control panel page for our app
main_page <- tabPanel(
  title = "Find outliers in your data!",
  titlePanel("Control Panel"),
  sidebarLayout(
    sidebarPanel(
      title = "Control Panel",
      fileInput("csv_input", "Upload a csv for outlier detection", accept = ".csv"),
      selectInput("num_var", "Choose numeric variable", choices = c(none)),
      selectInput("cat_var", "Choose a categoric or numeric variable", choices = c(none)),
      br(),
      actionButton("run_button", "Its go time!", icon = icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Outlier Visualizations",
          plotOutput("plot_1")
        ),
        tabPanel(
          title = "Additional Information",
          fluidRow(
            column(width = 4, strong(textOutput("num_var_title"))),
            column(width = 4, strong(textOutput("cat_var_title")))
          ),
          fluidRow(
            column(width = 4, tableOutput("num_var_summary_table")),
            column(width = 4, tableOutput("cat_var_summary_table"))
          )
        )
      )
    )
  )
)

# Function which will be called to create plot based on whether the second variable is categorical/numeric
plotting_function <- function(data_input, num_var, cat_var) # correct
{
  if(num_var == none)
    {
      showModal(modalDialog(
      title = "Error",
      "Select variables before plotting.",
      easyClose = TRUE,
      footer = NULL
    ))
  }
  
  else if(num_var != none && cat_var == none)
  {
    ggplot(data = data_input,aes_string(x = num_var)) +
      geom_boxplot(alpha=0.3) + 
      labs(title = "Box Plot", x = num_var) +
      theme_classic() +
      theme(
        plot.title = element_text(color = "grey", size = 16, face = "bold", hjust = 0.0),
        plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(face = "italic"),
        legend.position = "none"
      )
  }
  
  else if(num_var != none && cat_var != none && is.numeric(data_input[[cat_var]]))
    {
      ggplot(data = data_input,aes_string(x = num_var, y = cat_var)) +
      geom_point() + 
      labs(title = "Scatter Plot", x = num_var, y = cat_var) +
      theme(
        plot.title = element_text(size=8, face= "bold", colour= "grey" ),
        axis.title.x = element_text(size=10, face="bold", colour = "black"),    
        axis.title.y = element_text(size=10, face="bold", colour = "black"),    
        axis.text.x = element_text(size=8, face="bold", colour = "black"), 
        axis.text.y = element_text(size=8, face="bold", colour = "black"),
        legend.position = "none")
    }
  else if(num_var != none && cat_var != none && is.numeric(data_input[[cat_var]])==FALSE)
    {
    if(length(unique(data_input[[cat_var]]))>=12)
    {
      showModal(modalDialog(
        title = "Warning",
        paste("The categorical variable you selected,", data_input$cat_var, "has more than 12 levels. The boxplots might not be legible in this version of the app"),
        easyClose = TRUE,
        footer = modalButton("Dismiss"),
        fade=TRUE
      ))
    }
    data_input[,(cat_var):= as.factor(data_input[,get(cat_var)])]
    x<-ggplot(data = data_input,aes_string(x = num_var, y = cat_var, fill = cat_var)) +
    geom_boxplot(alpha=0.3) + 
    labs(title = "Box Plot", x = num_var, y = cat_var) +
    theme_classic() +
      theme(
        plot.title = element_text(color = "grey", size = 16, face = "bold", hjust = 0.0),
        plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(face = "italic"),
        legend.position = "none"
      )
    y<-ggplot(data = data_input,aes_string(x = num_var, y = cat_var, fill = cat_var)) +
      geom_density_ridges2(color="red",fill="orange",alpha=0.2) + 
      labs(title = "Ridgeline Plot", x = num_var, y = cat_var) + 
      theme_classic() +
      theme(
        plot.title = element_text(color = "grey", size = 16, face = "bold", hjust = 0.0),
        plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
        plot.caption = element_text(face = "italic"),
        legend.position = "none"
      )
    grid.arrange(x, y, ncol = 1, nrow=2)
    }
  
}


# Function to create summary table for a numeric variable
create_num_var_table <- function(data_input, num_var) # correct
{
  if(num_var != none)
  {
    single_columns <- data_input[,get(num_var)]
    Descriptions <- c("Mean", "25th %ile","Median", "75th %ile",
                      "90th %ile","IQR","Upper Bound","Lower Bound")
    Actuals <- c(
                  round(mean(single_columns, na.rm = TRUE), 2),
                  round(quantile(single_columns, 0.25, na.rm = TRUE), 2),
                  round(median(single_columns, na.rm = TRUE), 2),
                  round(quantile(single_columns, 0.75, na.rm = TRUE), 2),
                  round(quantile(single_columns, 0.90, na.rm = TRUE), 2),
                  round(IQR(single_columns, na.rm = TRUE), 2),
                  round(quantile(single_columns, 0.75, na.rm = TRUE)+1.5*IQR(single_columns, na.rm = TRUE), 2),
                  round(quantile(single_columns, 0.25, na.rm = TRUE)-1.5*IQR(single_columns, na.rm = TRUE), 2)
                )
    data.table(Descriptions, Actuals)
  }
}



# Function to create summary table for a categorical variable
categoric_table <- function(data_input, cat_var){ # correct
  if(cat_var != none){
    categoric_counts <- data_input[,.N, by = get(cat_var)]
    categoric_counts <- setnames(categoric_counts,c("Category", "Frequency"))
    categoric_counts
  }
}

# App UI definition
ui <- navbarPage(
  title = "Outlier detection app",
  theme = shinytheme('slate'),
  main_page,
  about_page
)

# Server logic
server <- function(input, output){
  
  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
  })
  
  
  filter_numeric_columns <- function(df) {
    numeric_columns <- sapply(df, is.numeric)
    numeric_column_names <- names(df)[numeric_columns]
    return(numeric_column_names)
  }
  
  observeEvent(data_input(),{
    filtered_choices <- filter_numeric_columns(data_input())
    choices_num <- c(none,filtered_choices)
    choices_all <- c(none,names(data_input()))
    updateSelectInput(inputId = "num_var", choices = choices_num)
    updateSelectInput(inputId = "cat_var", choices = choices_all)
  })
  
  
  num_var <- eventReactive(input$run_button,input$num_var)
  cat_var <- eventReactive(input$run_button,input$cat_var)
  
  # plot
  
  plot_1 <- eventReactive(input$run_button,{
    plotting_function(data_input(), num_var(), cat_var())
  })
  
  output$plot_1 <- renderPlot(plot_1())
  
  # 1-d summary tables
  
  output$num_var_title <- renderText(paste("Numeric Variable:",num_var()))
  
  num_var_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var())
  })
  
  output$num_var_summary_table <- renderTable(num_var_summary_table(),colnames = FALSE)
  
  
  # Checking if the second table is numeric or categorical and printing resulting table based on that
  # Wrap the code in a reactive context to use cat_var()
  cat_var_summary_table <- eventReactive(input$run_button, {
    cat_var_value <- cat_var()
    
    # Check if the selected cat_var is numeric
    is_numeric_cat_var <- is.numeric(data_input()[[cat_var_value]])
    
    if (is_numeric_cat_var) {
      # If it's numeric then numeric combined table is rendered
      output$cat_var_title <- renderText(paste("Numeric Variable:", cat_var_value))
      return(create_num_var_table(data_input(), cat_var_value))
    } else {
      # If it's categorical then categorical combined table is rendered
      output$cat_var_title <- renderText(paste("Categorical Variable:", cat_var_value))
      #checked
      return(categoric_table(data_input(), cat_var_value))
    }
  })
  
  output$cat_var_summary_table <- renderTable(cat_var_summary_table(), colnames = TRUE)
  options(shiny.maxRequestSize=25*1024^2) # allocating memory of 25MB per request 
  
}


# Initializing the Shiny app
shinyApp(ui = ui, server = server)



