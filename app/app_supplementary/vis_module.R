vis_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(6,
             plotOutput(ns("pop_barplot"))
      ),
      column(6,
             plotOutput(ns("vote_barplot"))
      )
    ),
    fluidRow(
      column(12,
             plotOutput(ns("share_barplot"))
      )
    ),
    tags$h3("Podsumowanie"),
    fluidRow(
      column(4,
             dataTableOutput(ns("pop_table"))
      ),
      column(4,
             dataTableOutput(ns("vote_table"))
      ),
      column(4,
             dataTableOutput(ns("share_table"))
      )
    )
  )
}

vis_SERVER <- function(id, dat, udzial_dat){
  moduleServer(id, function(input, output, session){
    
    output$pop_barplot <- renderPlot({
      if(id == "language"){
        ggplot(dat, aes(x = reorder(language, -srednia_liczba_ocen), y = srednia_liczba_ocen)) +
          geom_col(fill = "darkorange") +
          labs(title = "Średnia liczba ocen wg języka", x = "Język", y = "Średnia liczba ocen") +
          theme_minimal()
      } else if(id == "genre"){
        ggplot(dat, aes(x = reorder(genre, -srednia_liczba_ocen), y = srednia_liczba_ocen)) +
          geom_col(fill = "tomato") +
          labs(title = "Średnia liczba ocen wg gatunku", x = "Gatunek", y = "Średnia liczba ocen") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else if(id == "year"){
        ggplot(dat, aes(x = year, y = srednia_liczba_ocen)) +
          geom_col(fill = "chocolate4") +
          labs(title = "Średnia liczba ocen wg roku", x = "Rok", y = "Średnia liczba ocen") +
          theme_minimal() +
          scale_x_continuous(limits = c(1950, 2025))
      }
    })
    
    output$vote_barplot <- renderPlot({
      if(id == "language"){
        ggplot(dat, aes(x = reorder(language, -srednia_ocena), y = srednia_ocena)) +
          geom_col(fill = "steelblue") +
          labs(title = "Średnia ocena wg języka", x = "Język", y = "Średnia ocena") +
          theme_minimal()
      } else if(id == "genre"){
        ggplot(dat, aes(x = reorder(genre, -srednia_ocena), y = srednia_ocena)) +
          geom_col(fill = "forestgreen") +
          labs(title = "Średnia ocena wg gatunku", x = "Gatunek", y = "Średnia ocena") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else if(id == "year"){
        ggplot(dat, aes(x = year, y = srednia_ocena)) +
          geom_col(fill = "aquamarine") +
          labs(title = "Średnia ocena wg roku", x = "Rok", y = "Średnia ocena") +
          theme_minimal() +
          scale_x_continuous(limits = c(1950, 2025))
      }
    })
    
    output$share_barplot <- renderPlot({
      if(id == "language"){
        ggplot(udzial_dat, aes(x = reorder(language, -procent), y = procent)) +
          geom_col(fill = "skyblue") +
          geom_text(aes(label = paste0(procent, "%")), vjust = -0.5) +
          labs(title = "Procentowy udział filmów wg języka", x = "Język", y = "Udział [%]") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else if(id == "genre"){
        ggplot(udzial_dat, aes(x = reorder(genre, -procent), y = procent)) +
          geom_col(fill = "mediumseagreen") +
          geom_text(aes(label = paste0(procent, "%")), vjust = -0.5, size = 3) +
          labs(title = "Procentowy udział filmów wg gatunku", x = "Gatunek", y = "Udział [%]") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else if(id == "year"){
        ggplot(udzial_dat, aes(x = factor(year), y = procent)) +
          geom_col(fill = "orchid") +
          geom_text(aes(label = paste0(procent, "%")), vjust = -0.5) +
          labs(title = "Procentowy udział filmów wg roku", x = "Rok", y = "Udział [%]") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_x_discrete(limits = as.character(1950:2025))
      }
    })
    
    # Tabele
    output$pop_table <- DT::renderDataTable({
      if(id == "language"){
        dat[, .(Język = language, Średnia_liczba_ocen = srednia_liczba_ocen)]
      } else if(id == "genre"){
        dat[, .(Gatunek = genre, Średnia_liczba_ocen = srednia_liczba_ocen)]
      } else if(id == "year"){
        dat[, .(Rok = year, Średnia_liczba_ocen = srednia_liczba_ocen)]
      }
    }, options = list(pageLength = 10), rownames = FALSE)
    
    output$vote_table <- DT::renderDataTable({
      if(id == "language"){
        dat[, .(Język = language, Średnia_ocena = srednia_ocena)]
      } else if(id == "genre"){
        dat[, .(Gatunek = genre, Średnia_ocena = srednia_ocena)]
      } else if(id == "year"){
        dat[, .(Rok = year, Średnia_ocena = srednia_ocena)]
      }
    }, options = list(pageLength = 10), rownames = FALSE)
    
    output$share_table <- DT::renderDataTable({
      if(id == "language"){
        udzial_dat[, .(Język = language, Udział_procent = procent)]
      } else if(id == "genre"){
        udzial_dat[, .(Gatunek = genre, Udział_procent = procent)]
      } else if(id == "year"){
        udzial_dat[, .(Rok = year, Udział_procent = procent)]
      }
    }, options = list(pageLength = 10), rownames = FALSE)
  })
}