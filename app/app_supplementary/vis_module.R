vis_UI <- function(id){
  ns <- NS(id)
  
  fluidRow(
    column(6,
           plotOutput(ns("pop_barplot"))
    ),
    column(6,
           plotOutput(ns("vote_barplot"))
    )
  )
}

vis_SERVER <- function(id, dat){
  moduleServer(id, function(input, output, session){
    output[["pop_barplot"]] <- renderPlot({
      if(id == "language"){
        ggplot(dat, aes(x = reorder(original_language, -srednia_liczba_ocen), y = srednia_liczba_ocen)) +
          geom_col(fill = "darkorange") +
          labs(title = "Średnia liczba ocen wg języka", x = "Język", y = "Średnia liczba ocen") +
          theme_minimal()
        }else if(id == "genre"){
          ggplot(dat, aes(x = reorder(nazwa, -srednia_liczba_ocen), y = srednia_liczba_ocen)) +
            geom_col(fill = "tomato") +
            labs(title = "Średnia liczba ocen wg gatunku", x = "Gatunek", y = "Średnia liczba ocen") +
            theme_minimal()
        }else if(id == "year"){
          ggplot(dat, aes(x = year, y = srednia_liczba_ocen)) +
            geom_col(fill = "chocolate4") +
            labs(title = "Średnia liczba ocen wg roku", x = "Rok", y = "Średnia liczba ocen") +
            theme_minimal()
        }
    })
    
    output[["vote_barplot"]] <- renderPlot({
      if(id == "language"){
        ggplot(dat, aes(x = reorder(original_language, -srednia_ocena), y = srednia_ocena)) +
          geom_col(fill = "steelblue") +
          labs(title = "Średnia ocena wg języka", x = "Język", y = "Średnia ocena") +
          theme_minimal() 
      }else if(id == "genre"){
        ggplot(dat, aes(x = reorder(nazwa, -srednia_ocena), y = srednia_ocena)) +
          geom_col(fill = "forestgreen") +
          labs(title = "Średnia ocena wg gatunku", x = "Gatunek", y = "Średnia ocena") +
          theme_minimal()
      }else if(id == "year"){
        ggplot(dat, aes(x = year, y = srednia_ocena)) +
          geom_col(fill = "aquamarine") +
          labs(title = "Średnia ocena wg roku", x = "Rok", y = "Średnia ocena") +
          theme_minimal()
      }
    })
  })
}