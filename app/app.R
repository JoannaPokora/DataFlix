library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)

library(ggplot2)
library(data.table)
library(ggiraph)
library(DT)
library(paletteer)
library(scales)

source("app_supplementary/vis_module.R")
source("app_supplementary/heatmap_module.R")
source("app_supplementary/dist_module.R")

ui <- navbarPage(
  title = "DataFlix",
  theme = shinytheme("sandstone"),
  
  tags$head(
    tags$style(HTML("
      .info-box {
        background-color: #f5f5f5;
        border-radius: 10px;
        box-shadow: 1px 1px 5px rgba(0,0,0,0.1);
        padding: 20px;
        text-align: center;
        height: 100px;
        width: 245px;
        margin-bottom: 20px;
      }
      .info-title {
        font-size: 16px;
        color: #666;
      }
      .info-value {
        font-size: 28px;
        font-weight: bold;
        color: #333;
      }
    "))
  ),
  
  tabPanel("Info",
           br(),
           uiOutput("info"),
           br(),
           br(),
           column(10, offset = 1,
                  plotOutput("hist_in_time")
           )
  ),
  
  tabPanel("Średnia ocena vs liczba ocen",
           sidebarLayout(
             sidebarPanel(
               sliderInput("year_scatter",
                           "Rok premiery",
                           value = c(1950, 2024),
                           min = 1950,
                           max = 2024,
                           sep = "",
                           ticks = FALSE
               ),
               pickerInput("genre_scatter",
                 "Gatunki",
                 choices = c("Akcja", "Przygodowy", "Animacja", "Komedia",
                             "Kryminalny", "Dokumentalny", "Dramat",
                             "Familijny", "Fantasy", "Historyczny", "Horror",
                             "Muzyczny", "Mystery", "Romans", "Sci-Fi",
                             "TV Film", "Thriller", "Wojenny", "Western"),
                 selected = c("Akcja", "Przygodowy", "Animacja", "Komedia",
                              "Kryminalny", "Dokumentalny", "Dramat",
                              "Familijny", "Fantasy", "Historyczny", "Horror",
                              "Muzyczny", "Mystery", "Romans", "Sci-Fi",
                              "TV Film", "Thriller", "Wojenny", "Western"),
                 options = pickerOptions(
                   actionsBox = TRUE, 
                   selectedTextFormat = "count > 3",
                   liveSearch = TRUE
                 ),
                 multiple = TRUE
               ),
               numericInput("min_vote_scatter",
                            "Minimalna liczba ocen",
                            min = 0,
                            max = 8000,
                            step = 200,
                            value = 0
               ),
               pickerInput("language_scatter",
                           "Oryginalny język",
                           choices = c("angielski", "francuski", "hiszpański",
                                       "japoński", "niemiecki", "portugalski",
                                       "chiński", "włoski", "rosyjski",
                                       "koreański", "czeski", "arabski",
                                       "niderlandzki", "szwedzki", "hindi",
                                       "turecki", "polski"),
                           selected = c("angielski", "francuski", "hiszpański",
                                        "japoński", "niemiecki", "portugalski",
                                        "chiński", "włoski", "rosyjski",
                                        "koreański", "czeski", "arabski",
                                        "niderlandzki", "szwedzki", "hindi",
                                        "turecki", "polski"),
                           options = pickerOptions(
                             actionsBox = TRUE, 
                             selectedTextFormat = "count > 3",
                             liveSearch = TRUE
                           ),
                           multiple = TRUE
               ),
               br(),
               div(style = "text-align: center;",
                   actionBttn("apply_filtering",
                              label = "filtruj",
                              style = "fill",
                              color = "success"
                   )
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Wykres",
                          br(),
                          uiOutput("scatter_info"),
                          br(),
                          column(11,
                                 withSpinner(girafeOutput("main_scatter"))
                          ),
                          br()
                 ),
                 tabPanel("Tabela",
                          br(),
                          column(10, offset = 1,
                                 withSpinner(DTOutput("scatter_table")),
                                 br()
                          )
                 )
               )
             )
           )
  ),
  
  tabPanel("Wizualizacje",
           tabsetPanel(
             tabPanel(
               "Oryginalny język",
               vis_UI("language")
             ),
             tabPanel(
               "Gatunek",
               vis_UI("genre")
             ),
             tabPanel(
               "Rok premiery",
               vis_UI("year")
             )
           )
  ),
  
  tabPanel("Rozkłady",
           sidebarLayout(
             sidebarPanel(
               selectInput("selected_type",
                           "Wybierz zmienną:",
                           choices = c("język", "gatunek", "rok")
               ),
               selectInput("selected_value",
                           "Wybierz wartość:",
                           choices = character(0)
              )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Średnia ocena",
                   dist_UI("vote_dist")
                 ),
                 tabPanel("Liczba ocen",
                   dist_UI("pop_dist")
                 )
               )
             )
           )
  ),
  
  tabPanel("Heatmapy",
           tabsetPanel(
             tabPanel("Język",
                      br(),
                      fluidRow(
                        column(5, offset = 1,
                               heatmap_UI("heatmap_lang_pop") 
                        ),
                        column(5,
                               heatmap_UI("heatmap_lang_vote")
                        )
                      ),
                      br()
             ),
             tabPanel("Gatunek",
                      br(),
                      fluidRow(
                        column(5, offset = 1,
                               heatmap_UI("heatmap_genre_pop")
                        ),
                        column(5,
                               heatmap_UI("heatmap_genre_vote")
                        )
                      ),
                      br()
             )
           )
  )
)

server <- function(input, output) {
  dat <- readRDS("data/movies.RDS")
  genres <- readRDS("data/genres.RDS")
  
  output[["info"]] <- renderUI({
    fluidRow(
      column(3, align = "center",
             div(class = "info-box",
                 div(class = "info-title", "Liczba filmów"),
                 div(class = "info-value", paste0(nrow(dat)))
             )
      ),
      column(3, align = "center",
             div(class = "info-box",
                 div(class = "info-title", "Liczba gatunków"),
                 div(class = "info-value",
                     paste0(length(unique(unlist(dat[["genre_ids"]])))))
             )
      ),
      column(3, align = "center",
             div(class = "info-box",
                 div(class = "info-title", "Liczba oryginalnych języków"),
                 div(class = "info-value",
                     paste0(length(unique(dat[["original_language"]]))))
             )
      ),
      column(3, align = "center",
             div(class = "info-box",
                 div(class = "info-title", "Zakres lat"),
                 div(class = "info-value",
                     paste0(min(dat[["year"]]), "-", max(dat[['year']])))
             )
      )
    )
  })
  
  output[["hist_in_time"]] <- renderPlot({
    ggplot(dat, aes(x = year)) +
      geom_histogram(color = "white", fill = "skyblue", binwidth = 1) +
      labs(x = "Rok", y = "Liczba filmów",
           title = "Liczba filmów w kolejnych latach") +
      theme_light()
  })
  
  scatter_dat <- eventReactive(input[["apply_filtering"]], {
    req(
      input[["year_scatter"]],
      input[["genre_scatter"]],
      input[["language_scatter"]],
      input[["min_vote_scatter"]]
    )
    
    dat[
      year >= input[["year_scatter"]][1] &
        year <= input[["year_scatter"]][2] &
        any(genres[genre %in% input[["genre_scatter"]], id] %in% genre_ids) &
        language %in% input[["language_scatter"]] &
        vote_count >= input[["min_vote_scatter"]]
    ]
  })
  
  output[["scatter_info"]] <- renderUI({
    req(scatter_dat)
    
    fluidRow(
      column(4, align = "center",
             div(class = "info-box",
                 div(class = "info-title", "Średnia ocena"),
                 div(class = "info-value",
                     paste0(round(mean(scatter_dat()[["vote_average"]]), 3)))
             )
      ),
      column(4, align = "center",
             div(class = "info-box",
                 div(class = "info-title", "Średnia liczba ocen"),
                 div(class = "info-value",
                     paste0(round(mean(scatter_dat()[["vote_count"]]))))
             )
      ),
      column(4, align = "center",
             div(class = "info-box",
                 div(class = "info-title", "Liczba filmów na wykresie"),
                 div(class = "info-value",
                     paste0(nrow(scatter_dat())))
             )
      )
    )
  })
  
  output[["main_scatter"]] <- renderGirafe({
    req(scatter_dat)
    
    scatter_dat()[, tooltip := paste0(
      title,
      "<br>Gatunki: ",
      sapply(genre_ids, function(ids) {
        paste0(genres[id %in% ids, genre], collapse = ", ")
      }),
      "<br>Oryginalny język: ",
      language,
      "<br>Data premiery: ",
      release_date,
      "<br>Ocena: ",
      vote_average,
      "<br>Liczba ocen: ",
      vote_count
    )]
    
    plt <- ggplot(scatter_dat(),
                  aes(x = vote_average, y = vote_count, tooltip = tooltip)) +
      geom_point_interactive() +
      labs(x = "Średnia ocena", y = "Liczba ocen") +
      theme_light()
    
    girafe(ggobj = plt,
           options = list(
             opts_toolbar(saveaspng = FALSE),
             opts_zoom(min = 0.5, max = 5)
             ))
  })
  
  output[["scatter_table"]] <- renderDT({
    req(scatter_dat)
    
    dat_to_show <- scatter_dat()[,
      .(title,
        genre = sapply(
          genre_ids,
          function(ids) paste(genres[id %in% ids, genre], collapse = ", ")
        ),
        language,
        release_date,
        vote_average,
        vote_count)
    ]
    names(dat_to_show) <- c("Tytuł", "Gatunki", "Oryginalny język",
                            "Data premiery", "Średnia ocena", "Liczba ocen")
    dat_to_show
  })
  
  dane_jezyk <- dat[, .(
    srednia_ocena = round(mean(vote_average, na.rm = TRUE), 2),
    srednia_liczba_ocen = round(mean(vote_count, na.rm = TRUE), 2),
    liczba_filmow = .N
    ), by = language]
  
  dane_jezyk_udzial <- dat[, .N, by = language]
  dane_jezyk_udzial[, procent := round(N / sum(N) * 100, 1)]
  
  vis_SERVER("language", dane_jezyk, dane_jezyk_udzial)
  
  filmy_gatunki <- data.table::copy(dat)
  
  filmy_gatunki <- filmy_gatunki[, .(id = unlist(genre_ids), year,
                                     vote_average, vote_count, language)]
  filmy_gatunki <- filmy_gatunki[genres, on = .(id), genre := i.genre]
  
  dane_gatunek <- filmy_gatunki[, .(
    srednia_ocena = round(mean(vote_average, na.rm = TRUE), 2),
    srednia_liczba_ocen = round(mean(vote_count, na.rm = TRUE), 2),
    liczba_filmow = .N
  ), by = genre]
  
  dane_gatunek_udzial <- filmy_gatunki[, .N, by = genre]
  dane_gatunek_udzial[, procent := round(N / sum(N) * 100, 1)]
  
  vis_SERVER("genre", dane_gatunek, dane_gatunek_udzial)
  
  dane_rok <- dat[, .(
    srednia_ocena = round(mean(vote_average, na.rm = TRUE), 2),
    srednia_liczba_ocen = round(mean(vote_count, na.rm = TRUE), 2),
    liczba_filmow = .N
  ), by = year]
  
  dane_rok_udzial <- dat[, .N, by = year]
  dane_rok_udzial[, procent := round(N / sum(N) * 100, 1)]
  
  vis_SERVER("year", dane_rok, dane_rok_udzial)
  
  observe({
    req(input[["selected_type"]])
    
    updateSelectInput(
      inputId = "selected_value",
      choices = switch(
        input[["selected_type"]],
        "język" = c("angielski", "francuski", "hiszpański",
                    "japoński", "niemiecki", "portugalski",
                    "chiński", "włoski", "rosyjski",
                    "koreański", "czeski", "arabski",
                    "niderlandzki", "szwedzki", "hindi",
                    "turecki", "polski"),
        "gatunek" = genres[["genre"]],
        "rok" = 1950:2024
      )
    )
  })
  
  observe({
    dist_SERVER("vote_dist", filmy_gatunki, input[["selected_type"]],
                input[["selected_value"]])
    dist_SERVER("pop_dist", filmy_gatunki, input[["selected_type"]],
                input[["selected_value"]])
  })
  
  heatmap_SERVER("heatmap_lang_pop", dat)
  heatmap_SERVER("heatmap_lang_vote", dat)
  heatmap_SERVER("heatmap_genre_pop", filmy_gatunki)
  heatmap_SERVER("heatmap_genre_vote", filmy_gatunki)
}

shinyApp(ui = ui, server = server)
