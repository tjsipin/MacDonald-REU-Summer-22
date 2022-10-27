# Preliminaries ----



# Libraries
library(shiny)
library(dplyr)
library(readr)
library(randomForest)
library(ggplot2)
library(caret)
library(rsample)

# Data 
data <- read_csv('../data/monthly_short.csv') %>% 
  select(-c(...1)) %>% 
  filter(Year %in% c(2014)) %>% 
  rename(CL = `Cutaneous Leishmaniasis`)

# Set seed to get same train and test split every time
set.seed(123)

# Data partitioning
split <- initial_split(data, prop = 0.7)
train <- training(split) %>% as.data.frame()
test <- testing(split)

# Load our random forest rf
load('../causality/monthly_short_rf') # rf

# Load our partial plots
load('../causality/plots/CL_vs_Population_pP')
load('../causality/plots/CL_vs_LST_Day_pP')
load('../causality/plots/CL_vs_NDVI_pP')
load('../causality/plots/CL_vs_EVI_pP')
load('../causality/plots/CL_vs_Precip_pP')
load('../causality/plots/CL_vs_SWOccurrence_pP')
load('../causality/plots/CL_vs_AvgRad_pP')
load('../causality/plots/CL_vs_area_mn_forest_pP')
load('../causality/plots/CL_vs_enn_mn_forest_pP')
load('../causality/plots/CL_vs_pland_forest_pP')
load('../causality/plots/CL_vs_te_forest_pP')

# Define UI for app ----
ui <- fluidPage(
  
  # App title
  titlePanel("Monthly Random Forest"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: variable
      selectInput(inputId = 'variable',
                  label = 'Choose an explanatory variable for Partial Dependence Plot',
                  # choices = (c('../causality/images/CL_vs_Population_pP.png' = 'Population',
                  #              'Daytime Land Surface Temperature' = '../causality/images/CL_vs_LST_Day_pP.png',
                  #              'NDVI' = '../causality/images/CL_vs_NDVI_pP.png',
                  #              'EVI' = '../causality/images/CL_vs_EVI_pP.png',
                  #              'Precipitation' = '../causality/images/CL_vs_Precip_pP.png', 
                  #              'Average Radiance' = '../causality/images/CL_vs_AvgRad_pP.png',
                  #              'Standing Water Occurrence' = '../causality/images/CL_vs_SWOccurrence_pP.png',
                  #              'Proportion of Land that is Forest' = '../causality/images/CL_vs_pland_forest_pP.png',
                  #              'Mean Patch Area of Forest' = '../causality/images/CL_vs_area_mn_forest_pP.png',
                  #              'Mean Patch Isolation of Forest' = '../causality/images/CL_vs_enn_mn_forest_pP.png',
                  #              'Total Edge of Forest' = '../causality/images/CL_vs_te_forest_pP.png')
                  #   )
                  choices = c(
                    'Population' = 'CL_vs_Population_pP',
                    'Daytime Land Surface Temperature' = 'CL_vs_LST_Day_pP',
                    'NDVI' = 'CL_vs_NDVI_pP',
                    'EVI' = 'CL_vs_EVI_pP',
                    'Precipitation' = 'CL_vs_Precip_pP',
                    'Average Radiance' = 'CL_vs_AvgRad_pP',
                    'Standing Water Occurrence' = 'CL_vs_SWOccurrence_pP',
                    'Proportion of Land that is Forest' = 'CL_vs_pland_forest_pP',
                    'Mean Patch Area of Forest' = 'CL_vs_area_mn_forest_pP',
                    'Mean Patch Isolation of Forest' = 'CL_vs_enn_mn_forest_pP',
                    'Total Edge of Forest' = 'CL_vs_te_forest_pP')
                  ),
      # Input: alpha_ for intensity of color on differences plot
      sliderInput(
        'alpha_1',
        'True Value:',
        min = 0, max = 1, value = 100
      ),
      
      sliderInput(
        'alpha_2',
        'Prediction:',
        min = 0, max = 1, value = 100
      ),
      
      sliderInput(
        'alpha_3', 
        'Difference:',
        min = 0, max = 1, value = 100
      ),
      
      # Show Test mse
      h5(textOutput('mse'))
      
      
      ),
  
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Formatted text for caption
      h3(textOutput('caption')),
      
      # Output: Partial dependence plot on selected explanatory variable 
      plotOutput("partialDependencePlot"),
      
      # Output: Graph of predicted vs. true
      plotOutput('differencesPlot')
    )
  )
)

server <- function(input, output) {
  
  # Helper functions ----
  clean <- function(x) {
    switch(x,
           'CL_vs_Population_pP' = 'Population',
           'CL_vs_LST_Day_pP' = 'Daytime Land Surface Temperature',
           'CL_vs_NDVI_pP' = 'NDVI',
           'CL_vs_EVI_pP' = 'EVI',
           'CL_vs_Precip_pP' = 'Precipitation',
           'CL_vs_AvgRad_pP' = 'Average Radiance',
           'CL_vs_SWOccurrence_pP' = 'Standing Water Occurrence',
           'CL_vs_pland_forest_pP' = 'Proportion of Land that is Forest',
           'CL_vs_area_mn_forest_pP' = 'Mean Patch Area of Forest',
           'CL_vs_enn_mn_forest_pP' = 'Mean Patch Isolation of Forest',
           'CL_vs_te_forest_pP' = 'Total Edge of Forest')
  }
  
  varName <- function(x) {
    switch(x,
           'CL_vs_Population_pP' = 'Population',
           'CL_vs_LST_Day_pP' = 'LST_Day',
           'CL_vs_NDVI_pP' = 'NDVI',
           'CL_vs_EVI_pP' = 'EVI',
           'CL_vs_Precip_pP' = 'Precip',
           'CL_vs_AvgRad_pP' = 'AvgRad',
           'CL_vs_SWOccurrence_pP' = 'SWOccurrence',
           'CL_vs_pland_forest_pP' = 'pland_forest',
           'CL_vs_area_mn_forest_pP' = 'area_mn_forest',
           'CL_vs_enn_mn_forest_pP' = 'enn_mn_forest',
           'CL_vs_te_forest_pP' = 'te_forest')
  }
  
  # Compute graph text based on explanatory variable
  graphText <- reactive({
    paste('Partial Dependence on ', clean(input$variable))
  })
  
  # Return the graph text for printing as a caption
  output$caption <- renderText({
    graphText()
  })
  

  
  
  # Generate a partial dependence plot of the requested variable
  output$partialDependencePlot <- renderPlot(
    # choice_X <- eval(parse(text = input$variable))$x,
    # choice_Y <- eval(parse(text = input$variable))$y,
    ggplot() + 
      geom_line(aes(x = eval(parse(text = input$variable))$x,
                    y = eval(parse(text = input$variable))$y)) + 
      ylab('Cutaneous Leishmaniasis') + 
      xlab(clean(input$variable))
  )
  
  # Needed for predicted vs. true plot
  test_df <- test %>% 
    mutate(prediction = predict(rf, test)) %>% 
    mutate(difference = CL - prediction) %>% 
    mutate(Index = rownames(test))

  
  
  # Generate a predicted versus true values plot given the requested variable
  output$differencesPlot <- renderPlot(
    ggplot(test_df) + 
      geom_line(aes(x = eval(parse(text = varName(input$variable))), 
                    y = CL,
                    color = 'true'),
                alpha = input$alpha_1) + 
      geom_line(aes(x = eval(parse(text = varName(input$variable))),
                    y = prediction,
                    color = 'prediction'),
                alpha = input$alpha_2) + 
      geom_line(aes(x = eval(parse(text = varName(input$variable))),
                    y = difference,
                    color = 'difference'),
                alpha = input$alpha_3) + 
      xlab(clean(input$variable))
  )
  
  train_test_mse <- reactive({
    paste('Train MSE:', rf$mse[500], '\n',
          'Test MSE:', ModelMetrics::mse(actual = test_df$CL, predicted = test_df$prediction))
  })
  
  output$mse <- renderText({
    train_test_mse()
  }) 
}


############ 

shinyApp(ui = ui, server = server)