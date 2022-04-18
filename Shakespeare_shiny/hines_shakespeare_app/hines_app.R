books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

library(shiny)
library(tidytext)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)


# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}


# task6: add in shinythemes function

ui <- fluidPage( theme = shinytheme('cyborg'),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  # task2: add in the inputs in the sidebarPanel
  
  sidebarLayout(sidebarPanel(
    selectInput(inputId = "name", label = "book",
                choices = books),
    
    checkboxInput(inputId = "stopwords", label = "Remove Stopwords",
                  value = TRUE),
    actionButton(inputId = 'run', label = 'update graph'),
    
    hr(),
    h3("Word Cloud Settings"),
    sliderInput(inputId = "maxwords", label = "Maximum # of Words",
                min = 10, max = 200, value = 100, step = 10),
    
    sliderInput(inputId = "sizemax", label = "Maximum Length of Words",
                min = 1, max = 8, value = 4),
    
    sliderInput(inputId = "sizemin", label = "Minimum Length of Words",
                min = .1, max = 4, value = .5),
    
    hr(),
    
    h3("Word Count Settings"),
    
    sliderInput(inputId = "countmin", label = "Minimum Wordcount Threshold",
                min = 10, max = 100, value = 25),
    
    sliderInput(inputId = "font", label = "Font Size",
                min = 8, max = 30, value = 14)),
    
    
    
    # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
    
    mainPanel(
    tabsetPanel(
      tabPanel("Word Cloud", plotOutput('cloud', height = '600px')), 
      tabPanel("Word Counts", plotOutput('freq', height = '600px')), 
      
  ))))
    
    
  
  
  
  
  
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights


server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
#getFreq<-  reactive({input$name, input$stopwords})
  freq <- eventReactive(input$run,{
    #v <- freq()
     withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$name, input$stopwords) # ... = replace with the two inputs from Task 2
    })
   })
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$sizemax, input$sizemin),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
  })
    
  
    output$freq <- renderPlot({
      v <- freq()
      pal <- brewer.pal(8,"Dark2")
      
      v %>% 
        filter(n > input$countmin) %>%
        ggplot(aes(x = reorder(word, n), y = n)) +
                 theme(text = element_text(size = input$font),axis.text.x=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), #remove x axis labels
                       
                       
                       axis.ticks.y=element_blank()) + geom_bar(stat='identity') + coord_flip() #+ height = "600px"
               
        
    # p_scatter <- ggplot(data = countries_data_2011,
    #                    aes_string(x = input$x_axis, y = input$y_axis,
     #                              color = "continent",
      #                             size = input$point_size,
       #                            label = "country"))+
      #geom_point(alpha = input$alpha_level)+
      #guides(size = FALSE)+
      #theme_minimal()
    


  
  
})
}

shinyApp(ui = ui, server = server)

