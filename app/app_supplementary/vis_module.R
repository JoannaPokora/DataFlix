vis_UI <- function(id){
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      column(6, offset = 1,
             plotOutput(ns("pop_barplot"))
      ),
      column(3, offset = 1,
             DTOutput(ns("pop_table")),
      )
    ),
    br(),
    br(),
    fluidRow(
      column(6, offset = 1,
             plotOutput(ns("vote_barplot"))
      ),
      column(3, offset = 1,
             DTOutput(ns("vote_table")),
      )
    ),
    br(),
    br(),
    fluidRow(
      column(6, offset = 1,
             plotOutput(ns("share_barplot"))
      ),
      column(3, offset = 1,
             DTOutput(ns("share_table")),
      )
    ),
    br()
  )
}

vis_SERVER <- function(id, dat, udzial_dat){
  moduleServer(id, function(input, output, session){
    settings <- switch(id,
                       "language" = c("Język", "języka", "darkorange", "tomato",
                                      "chocolate4"),
                       "genre" = c("Gatunek", "gatunku", "skyblue",
                                   "steelblue", "steelblue4"),
                       "year" = c("Rok", "roku", "lightblue", "mediumseagreen",
                                  "orchid"))
    
    create_plot <- function(dat, id, settings, type){
      desc <- switch(type,
                     "srednia_liczba_ocen" = c(rep("Średnia liczba ocen", 2), 3),
                     "srednia_ocena" = c(rep("Średnia ocena", 2), 4),
                     "procent" = c("Udział [%]", "Procentowy udział filmów", 5))
      renderPlot({
        ggplot(dat, aes(x = get(id), y = get(type))) +
          geom_col(fill = settings[as.numeric(desc[3])]) +
          labs(title = paste0(desc[2], " wg ", settings[2]),
               x = settings[1], y = desc[1]) +
          theme_light() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
    }
    
    output$pop_barplot <- create_plot(dat, id, settings, "srednia_liczba_ocen")
    output$vote_barplot <- create_plot(dat, id, settings, "srednia_ocena")
    output$share_barplot <- create_plot(udzial_dat, id, settings, "procent")
    
    # Tabele
    create_table <- function(dat, id, settings, type){
      desc <- switch(type,
                     "srednia_liczba_ocen" = c(rep("Średnia liczba ocen", 2), 3),
                     "srednia_ocena" = c(rep("Średnia ocena", 2), 4),
                     "procent" = c("Udział [%]", "Procentowy udział filmów", 5))

      DT::renderDT({
        dat <- dat[, mget(c(id, type))]
        colnames(dat) <- c(settings[1], desc[1])
        dat
      }, options = list(pageLength = 5), rownames = FALSE, selection = "none")
    }
    
    output$pop_table <- create_table(dat, id, settings, "srednia_liczba_ocen")
    output$vote_table <- create_table(dat, id, settings, "srednia_ocena")
    output$share_table <- create_table(udzial_dat, id, settings, "procent")
  })
}