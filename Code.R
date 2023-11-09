library(shiny)

# Define the calorie database as a data frame
calorie_database <- data.frame(
  Ingredient = c(
    'apple', 'banana', 'avocado', 'apricot', 'blackberries', 'blackcurrant', 'cherries',
    'clementine', 'coconut (fresh)', 'cranberries', 'cucumber', 'dates (dried)', 'figs (fresh)',
    'grapefruit', 'kiwi', 'lemon', 'lime', 'lychee', 'mango', 'acorn squash', 'artichoke',
    'asparagus', 'beetroot', 'broccoli', 'brussels sprout', 'butternut squash', 'cabbage',
    'carrot', 'cauliflower', 'celery', 'chicory', 'corn', 'edamame', 'green beans',
    'iceberg lettuce', 'kale', 'leek', 'mushroom', 'onion', 'peas', 'peppers (red)', 'potato',
    'pumpkin', 'radish', 'romaine lettuce', 'spinach', 'bean sprouts', 'turnips', 'yam',
    'zucchini / courgette', 'ahi tuna', 'albacore', 'catfish', 'caviar', 'crab', 'eel',
    'flounder', 'grouper', 'herring', 'lobster', 'mussels', 'oysters', 'salmon', 'scallops',
    'sea bass', 'shrimp', 'smelt', 'squid', 'tilapia', 'trout', 'whitefish', 'yellowfin tuna',
    'bacon (pork)', 'chicken breast', 'chicken wings', 'chicken thighs', 'chicken eggs', 'duck (no skin)',
    'escargots', 'lamb', 'liver', 'sausage (chicken)', 'sausage (turkey)', 'sausage (pork)', 'quail eggs',
    'turkey (dark meat)', 'turkey (white meat)', 'venison', 'avocado oil', 'canola oil', 'coconut oil',
    'corn oil', 'olive oil', 'peanut oil', 'safflower oil', 'soybean oil', 'sunflower oil', 'butter',
    'buttermilk (1%)', 'cheddar cheese', 'cottage cheese (1%)', 'cream (heavy)', 'cream cheese',
    'evaporated milk', 'ghee', 'goats milk', 'ice cream (vanilla)', 'kefir', 'ricotta cheese',
    'skim milk', 'sour cream', 'soy milk', 'swiss cheese', 'yogurt whole milk'),
  Calories = c(
    37, 51, 134, 34, 21, 24, 36, 39, 351, 15, 15, 227, 209, 25, 42, 15, 9, 36, 39, 40, 41, 29, 42,
    35, 51, 36, 27, 10, 30, 8, 11, 54, 140, 25, 10, 30, 20, 8, 43, 70, 21, 97, 13, 33, 15, 24, 30,
    23, 153, 10, 120, 128, 95, 250, 87, 184, 91, 92, 158, 90, 86, 81, 183, 88, 97, 92, 96, 148, 134,
    108, 240, 148, 110, 133, 155, 195, 90, 122, 119, 172, 196, 318, 158, 184, 104, 157, 883, 883, 861,
    857, 880, 883, 883, 883, 883, 716, 41, 403, 72, 347, 231, 142, 899, 71, 207, 67, 174, 38, 214, 46,
    380, 61, 55, 62
  )
)

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Calorie Calculator"),
  sidebarLayout(
    sidebarPanel(
      textInput("ingredient", "Enter an ingredient:"),
      numericInput("weight", "Enter the weight (in grams):", value = 100),
      actionButton("add_button", "Add Ingredient"),
      actionButton("reset_button", "Reset"),
      textOutput("message")
    ),
    mainPanel(
      h4("Calories Summary"),
      verbatimTextOutput("calorie_summary")
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output) {
  total_calories <- reactiveVal(0)
  ingredients <- reactiveVal(data.frame(Ingredient = character(0), Calories = numeric(0)))

  observeEvent(input$add_button, {
    ingredient <- tolower(input$ingredient)
    weight <- input$weight

    if (ingredient %in% calorie_database$Ingredient) {
      calorie_per_100g <- calorie_database$Calories[calorie_database$Ingredient == ingredient]
      calories <- (calorie_per_100g * weight) / 100
      total_calories(total_calories() + calories)

      new_ingredient <- data.frame(Ingredient = ingredient, Calories = calories)
      ingredients(rbind(ingredients(), new_ingredient))

      output$message <- renderText({
        paste(ingredient, "contributes", calories, "calories to the dish.")
      })

    } else {
      output$message <- renderText({
        paste(ingredient, "is not in the database. Please add it with its calorie content.")
      })
    }
  })

  observeEvent(input$reset_button, {
    total_calories(0)
    ingredients(data.frame(Ingredient = character(0), Calories = numeric(0)))
    output$message <- renderText({""})
  })

  output$calorie_summary <- renderPrint({
    cat("The total calorie count of the dish is", total_calories(), "calories.")
    ingredients()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
