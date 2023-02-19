library(shiny)
library(tidyverse)

# Application
ui <- fluidPage(
  fluidRow(column(12
                  , align="center"
                  , tags$div(
                    class = "title-app"
                    , tags$h1("Rock-Paper-Scissors game")
                    , tags$h6("Play by clicking on icons. You can chose PC algorithm to play against.")
                    , selectInput("algorithm", "Choose PC algorithm:",
                                  c("Random" = "random"
                                    ,"Rotation" = "rotation")
                                  )
                    )
                  ,tags$head(tags$style("#result{font-size: 35px;
                                 font-style: italic;
                                 style=\"text-align: center;\"
                                 }")
                             )
                  , tags$br()
                  , fluidRow(
                      fluidRow(
                        column(3)
                        , column(2, actionButton("human_rock", "Rock", align="center", icon(name = "hand-rock"), style='font-size:200%'))
                        , column(2, actionButton("human_paper", "Paper", align="center", icon(name = "hand-paper"), style='font-size:200%'))
                        , column(2, actionButton("human_scissors", "Scissors", align="center", icon(name = "hand-scissors"), style='font-size:200%'))
                        )
                      , tags$br()
                      , fluidRow(column(4)
                        , column(2, htmlOutput("Human_move"))
                        , column(2, htmlOutput("PC_move"))
                        )
                      , fluidRow(column(12,htmlOutput("result"), align="center"))
                      , tags$br()
                      , hr()
                      , tags$h4("Summary table")
                      , fluidRow(
                        column(2)
                        ,column(8, DT::dataTableOutput("summary_table"))
                        )
                      , hr()
                      , tags$h4("History table")
                      , tags$h6("Here you can see history. Row number 1. is the latest move")
                      , fluidRow(
                        column(2)
                        ,column(8, DT::dataTableOutput("result_table"))
                        )
                      )
                  )
           )
  )

server <- function(input, output){
  possibilities_table <- crossing(Computer = c("Rock","Paper","Scissors")
                                  , Human = c("Rock","Paper","Scissors")) %>%
    add_column(Result = c("Draw", "You Lose", "You Win"
                          , "You Win", "Draw" , "You Lose"
                          , "You Lose", "You Win", "Draw"))
  
  values <- reactiveValues()
  values$Result_table <- data.frame(Computer = as.character()
                             , Human = as.character()
                             , Result = as.character())

# define random move ------------------------------------------------------
  # Define random move
  RPS_random <- function(input_RPS, possibilities_table_input = possibilities_table){
    moves <- c("Rock", "Paper", "Scissors")
    PC_move <- moves[sample(1:3, 1)]

    # pull result based on human and pc move
    Result_move <- possibilities_table_input %>%
      filter(Computer == PC_move, Human == input_RPS) %>%
      select(Result) %>%
      pull()
    # create output
    return(data.frame(Computer = PC_move
             , Human = input_RPS
             , Result = Result_move))
  }
# RPS_random("Rock")

# Change by last move -----------------------------------------------------
# Define change by last move
# if human lose, in next move PC assume player will change shape/move
  RPS_change_by_last_move <- function(input_RPS, possibilities_table_input = possibilities_table){
    # if human win, he/she tends to make same move >> try to beat previous human move
    
    # if human lost previous round, he will change his move this round and try to beat move PC just played >> 
    # in order to beat human, play what he just played in previous move
    
    # if draw, make just random move
    # https://arxiv.org/pdf/1404.5199v1.pdf
    # https://www.youtube.com/watch?v=rudzYPHuewc&ab_channel=Numberphile
    moves <- c("Rock", "Paper", "Scissors")
    
    # if first move, Result_table has 0 rows, then make random move
    if (nrow(values$Result_table) == 0) {
        PC_move <- moves[sample(1:3, 1)]
    
    # Human Win, tends to play the same move >> beat him
    } else if (values$Result_table %>%
               slice_head() %>%
               select(Result) %>%
               pull() == "You Win") {
        # find last human move
        last_human_move <- values$Result_table %>%
          slice_head() %>%
          select(Human) %>%
          pull()
        
        # find what beats the last human move
        PC_move <- possibilities_table %>%
          filter(Human == last_human_move
                 , Result == "You Lose") %>%
          select(Computer) %>%
          pull()
    
    # if draw, just make random move    
    } else if  (values$Result_table %>%
                slice_head() %>%
                select(Result) %>%
                pull() == "Draw") {
        PC_move <- moves[sample(1:3, 1)]
    
    # if PC win, human will try to beat PC, anticipate that and play what just human played
    } else if (values$Result_table %>%
               slice_head() %>%
               select(Result) %>%
               pull() == "You Lose") {
        # find human move, assign pc move
        PC_move <- values$Result_table %>%
          slice_head() %>%
          select(Human) %>%
          pull()
    }

    # pull result based on human and pc move
    Result_move <- possibilities_table_input %>%
      filter(Computer == PC_move, Human == input_RPS) %>%
      select(Result) %>%
      pull()
        
    # create output
    return(data.frame(Computer = PC_move
                      , Human = input_RPS
                      , Result = Result_move))
  }
  
  observeEvent(input$human_rock, {
    if (input$algorithm == "random"){values$Result_table <- rbind(RPS_random("Rock"), values$Result_table)}
    if (input$algorithm == "rotation"){values$Result_table <- rbind(RPS_change_by_last_move("Rock"), values$Result_table)}
  })
  
  observeEvent(input$human_paper, {
    if (input$algorithm == "random"){values$Result_table <- rbind(RPS_random("Paper"), values$Result_table)}
    if (input$algorithm == "rotation"){values$Result_table <- rbind(RPS_change_by_last_move("Paper"), values$Result_table)}
  })
  
  observeEvent(input$human_scissors, {
    if (input$algorithm == "random"){values$Result_table <- rbind(RPS_random("Scissors"), values$Result_table)}
    if (input$algorithm == "rotation"){values$Result_table <- rbind(RPS_change_by_last_move("Scissors"), values$Result_table)}
  })
  # render round result
  output$result <- renderText({
    if(nrow(values$Result_table)==0) {
      return(paste("<span style=\"color:blue\">Make move!</span>"))
      } else if (values$Result_table[1,]$Result == "You Win") {
        return(paste("<span style=\"color:green\">You Win!!!</span>"))
        } else if (values$Result_table[1,]$Result == "You Lose") {
          return(paste("<span style=\"color:red\">You Lose!!!</span>"))
        } else {
          return(paste("<span style=\"color:purple\">Draw</span>"))
          }
    })
  
  # render pc move and human move
  output$PC_move <- renderText({
    if(nrow(values$Result_table)==0) {
      return(paste(""))
    } else {
      return(paste0("PC move: ", values$Result_table[1,]$Computer))
    }
  })
  
  # render human move and human move
  output$Human_move <- renderText({
    if(nrow(values$Result_table)==0) {
      return(paste(""))
    } else {
      return(paste0("Your move: ",values$Result_table[1,]$Human))
    }
  })
  
  output$result_table <- DT::renderDataTable(
    values$Result_table)
  
  output$summary_table <- DT::renderDataTable( 
    values$Result_table %>%
      group_by(Result) %>%
      summarise(Numbers = n()) %>%
      ungroup() %>%
      group_by() %>%
      mutate(Percentage = round(Numbers / sum(Numbers), digits = 3)) %>%
      select(Result, Percentage, Numbers) %>%
      DT::datatable(options = list(searching = FALSE
                                   , paging = FALSE
                                   , lengthChange = FALSE
                                   , info = FALSE
                                   )
                    , rownames= FALSE) %>%
      DT::formatPercentage(2))
}

shinyApp(ui, server)
