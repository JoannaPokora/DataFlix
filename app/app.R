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

ui <- navbarPage(
  id = "main",
  title = "Filmy",
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
  
  tabPanel("Ocena vs liczba ocen",
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
                 choices = c("Action", "Adventure", "Animation", "Comedy",
                             "Crime", "Documentary", "Drama", "Family",
                             "Fantasy", "History", "Horror", "Music", "Mystery",
                             "Romance", "Science Fiction", "TV Movie",
                             "Thriller", "War", "Western"),
                 selected = c("Action", "Adventure", "Animation", "Comedy",
                              "Crime", "Documentary", "Drama", "Family",
                              "Fantasy", "History", "Horror", "Music", "Mystery",
                              "Romance", "Science Fiction", "TV Movie",
                              "Thriller", "War", "Western"),
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
                          column(12,
                                 withSpinner(girafeOutput("main_scatter"))
                          )        
                 ),
                 tabPanel("Tabela",
                          br(),
                          column(10, offset = 1,
                                 withSpinner(DTOutput("scatter_table"))      
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
  
  tabPanel("Heatmapy",
           tabsetPanel(
             tabPanel("Liczba ocen",
                      br(),
                      fluidRow(
                        heatmap_UI("heatmap_lang_pop"),
                        heatmap_UI("heatmap_genre_pop")
                      ),
                      br()
             ),
             tabPanel("Ocena",
                      br(),
                      fluidRow(
                        heatmap_UI("heatmap_lang_vote"),
                        heatmap_UI("heatmap_genre_vote")
                      ),
                      br()
             )
           )
  )
)

server <- function(input, output) {
  dat <- readRDS("data/movies.RDS")
  genres <- readRDS("data/genres.RDS")
  
  language_keys <- c(angielski = "en", francuski = "fr", hiszpański =  "es",
                     japoński = "ja", niemiecki = "de", portugalski = "pt",
                     chiński = "zh", włoski = "it", rosyjski = "ru",
                     koreański = "ko", czeski = "cs", arabski = "ar",
                     niderlandzki = "nl", szwedzki = "sv", hindi = "hi",
                     turecki = "tr", polski = "pl")
  
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
        any(genres[name %in% input[["genre_scatter"]], id] %in% genre_ids) &
        original_language %in% language_keys[input[["language_scatter"]]] &
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
        paste0(genres[id %in% ids, name], collapse = ", ")
      }),
      "<br>Oryginalny język: ",
      names(language_keys)[match(original_language, language_keys)],
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
          function(ids) paste(genres[id %in% ids, name], collapse = ", ")
        ),
        original_language =
          names(language_keys)[match(original_language, language_keys)],
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
    ), by = original_language]
  
  dane_jezyk <- dane_jezyk[order(-srednia_ocena)]
  
  vis_SERVER("language", dane_jezyk)
  
  gatunki_slownik <- data.table(
    id = c(28, 12, 16, 35, 80, 99, 18, 10751, 14, 36, 27, 10402, 9648,
           10749, 878, 10770, 53, 10752, 37),
    nazwa = c("Akcja", "Przygodowy", "Animacja", "Komedia", "Kryminalny", "Dokumentalny",
              "Dramat", "Familijny", "Fantasy", "Historyczny", "Horror", "Muzyczny",
              "Mystery", "Romans", "Sci-Fi", "TV Film", "Thriller", "Wojenny", "Western")
  )
  
  filmy_gatunki <- dat[, .(title, vote_average, vote_count, genre_ids)]
  filmy_gatunki <- filmy_gatunki[
    , .(id_gatunku = unlist(genre_ids)), 
    by = .(title, vote_average, vote_count)
  ][, id_gatunku := as.integer(id_gatunku)]
  
  filmy_gatunki <- merge(filmy_gatunki, gatunki_slownik, by.x = "id_gatunku", by.y = "id", all.x = TRUE)
  dane_gatunek <- filmy_gatunki[, .(
    srednia_ocena = round(mean(vote_average, na.rm = TRUE), 2),
    srednia_liczba_ocen = round(mean(vote_count, na.rm = TRUE), 2),
    liczba_filmow = .N
  ), by = nazwa][order(-srednia_ocena)]
  
  vis_SERVER("genre", dane_gatunek)
  
  dane_rok <- dat[, .(
    srednia_ocena = round(mean(vote_average, na.rm = TRUE), 2),
    srednia_liczba_ocen = round(mean(vote_count, na.rm = TRUE), 2),
    liczba_filmow = .N
  ), by = year][order(year)]
  
  vis_SERVER("year", dane_rok)
  
  heatmap_dat <- data.table::copy(dat)
  heatmap_dat[, original_language :=
                names(language_keys)[match(original_language, language_keys)]]
  
  genres_unnested <- heatmap_dat[, .(id = unlist(genre_ids), year,
                                     vote_average, vote_count)]
  genres_unnested <- genres_unnested[genres, on = .(id)]
  
  heatmap_SERVER("heatmap_lang_pop", heatmap_dat)
  heatmap_SERVER("heatmap_lang_vote", heatmap_dat)
  heatmap_SERVER("heatmap_genre_pop", genres_unnested)
  heatmap_SERVER("heatmap_genre_vote", genres_unnested)
}

shinyApp(ui = ui, server = server)
