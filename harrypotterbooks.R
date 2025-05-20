# ===============================================
# Fill in the following fields
# ===============================================
# Title: Shiny App 2 - Text Analysis
# Description: Sentiment Analysis and Word Trends for Harry Potter Series
# Details: Analysis 1 provides a line graph and table of Afinn Lexicon sentiment scores of the chosen book. Analysis 2 provides a line graph and table of mentions of a word in a chosen book. 
# Author: Arin Telimi
# Date: 11/15/24


# ===============================================
# Required packages
# (you can use other packages if you want to)
# ===============================================
library(shiny)
library(tidyverse)  # for data manipulation and graphics
library(tidytext)   # for text mining
library(DT)         # to render Data Tables nicely
library(plotly)     # if you are interested in using ggplotly()



# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with harry potter data)

#table of data
tbl = read_csv(file = "harry_potter_books.csv")

book_names = unique(tbl$book)

# Split books into 100 sections
tbl = tbl |>
  group_by(book) |>
  mutate(section = ceiling(row_number() %/% (n() / 100))) |>
  ungroup()

# Tokenize the data=
tbl_tokens = tbl |>
  unnest_tokens(word, text)



# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  # App Title
  titlePanel("Text Analysis of Harry Potter Books"),
  
  # Input widgets
  fluidRow(
    column(4,
           p(em("Analysis 1 & 2")),
           selectInput(inputId = "selected_book", 
                       label = "Select a book", 
                       choices = c(book_names, "All books"),
                       selected = book_names[1])
    ), 
    column(3,
           p(em("Analysis 1 & 2")),
           checkboxInput(inputId = "highlight_top", 
                         label = "Highlight Top Sections", 
                         value = FALSE)
    ), 
    column(3,
           p(em("Analysis 1")),
           radioButtons(inputId = "stopwords", 
                        label = "Include Stopwords?", 
                        choices = c("Yes" = "opt1", "No" = "opt2"), 
                        selected = "opt1")
    ),
    column(2,
           p(em("Analysis 2")),
           textInput(inputId = "selected_word", 
                     label = "Enter a single word", 
                     value = "Harry")
    )
  ),
  
  hr(),
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # Customize the following output elements with your own outputs
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              tabPanel("Analysis 1 (Sentiment)",
                       h3("Sentiment Analysis"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis 2 (Word Trend)", 
                       h3("Word Trend Analysis"),
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  selected_tokens = reactive({
    tokens = tbl_tokens
    
    if (input$stopwords == "opt2") {
      tokens = tokens |>
        anti_join(stop_words, by = "word")
    }
    
    if (input$selected_book != "All books") {
      tokens = tokens |>
        filter(book == input$selected_book)
    }
    
    tokens
  })

  #  empty input errors default to " "
  safe_word_input = reactive({
    if (input$selected_word == "") {
      return(" ")  # Default word
    } else {
      return(input$selected_word)
    }
  })
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  output$plot1 <- renderPlot({
    
    tokens = selected_tokens()
    afinn = read_csv("afinn.csv")
    
    sentiment_scores = tokens |>
      inner_join(afinn, by = "word") |>
      group_by(book, section) |>
      summarize(average_score = mean(value, na.rm = TRUE))
    
    plot = ggplot(sentiment_scores, aes(x = section, y = average_score, color = book)) +
      geom_line() +
      scale_x_continuous(breaks = c(1, 50, 100), labels = c("Beginning", "Middle", "End")) +
      labs(title = paste("AFINN Sentiment Analysis throughout", input$selected_book),
           subtitle = "Higher score correlates to Positive Sentiment, Lower score correlates to Negative Sentiment",
           x = "", y = "Average Sentiment Score")
    
    if (input$highlight_top) {
      top_sections = sentiment_scores |>
        arrange(desc(average_score)) |>
        head(5)  # Highlight top 5 sections
      
      plot = plot + 
        geom_point(data = top_sections, aes(x = section, y = average_score), color = "red", size = 3)
    }
    plot
  })
  
  output$table1 = renderDataTable({
    
    tokens = selected_tokens()
    afinn = read_csv("afinn.csv")
    
    average_scores = tokens |>
      inner_join(afinn, by = "word") |>
      group_by(book) |>
      summarize(average_score = mean(value, na.rm = TRUE)) |>
      arrange(desc(average_score)) |>
      rename("Book Title" = book, "Average Sentiment Score" = average_score)
    
    
    datatable(average_scores)
  }) 
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  output$plot2 <- renderPlot({
    
    word_lower = tolower(safe_word_input())
    
    trend_data = tbl_tokens |>
      filter(book == input$selected_book | input$selected_book == "All books") |>
      filter(str_detect(tolower(word), word_lower)) |>
      group_by(book, section) |>
      summarize(mentions = n())
    
    plot2 = ggplot(trend_data, aes(x = section, y = mentions, color = book)) +
      geom_line() +
      scale_x_continuous(breaks = c(1, 50, 100), labels = c("Beginning", "Middle", "End")) +
      labs(title = paste("Word Trend Analysis for", input$selected_word, "in", input$selected_book),
           x = "",
           y = "Mentions")
    
    if (input$highlight_top) {
      top_sections = trend_data |>
        arrange(desc(mentions)) |>
        head(5)  
      
      plot2 = plot2 + 
        geom_point(data = top_sections, aes(x = section, y = mentions), color = "red", size = 3)
    }
    plot2
  })
  
  output$table2 <- renderDataTable({
    
    word_lower = tolower(safe_word_input())
    
    count_data = tbl_tokens |>
      filter(str_detect(word, word_lower)) |>
      group_by(book) |>
      summarize(total_mentions = n()) |>
      arrange(desc(total_mentions)) |>
      rename("Book Title" = book, "Total Mentions of chosen word" = total_mentions)
    
    
    datatable(count_data)
  })
}


# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

