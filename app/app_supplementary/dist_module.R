dist_UI <- function(id){
  ns <- NS(id)
  
  column(10, offset = 1,
         br(),
         withSpinner(plotOutput(ns("density"))),
         withSpinner(plotOutput(ns("boxplot"))),
         withSpinner(plotOutput(ns("qqplot"))),
         br()
  )
}

dist_SERVER <- function(id, filmy_gatunki, type, value){
  moduleServer(id, function(input, output, session){
    org_type <- switch(type,
                       "gatunek" = "genre",
                       "język" = "language",
                       "rok" = "year")
    if(type != "gatunek"){
      dane_kategoria <- filmy_gatunki[, .SD, .SDcols = !c("genre")]
      dane_kategoria <- unique(dane_kategoria)
    }
    
    dane_kategoria <- filmy_gatunki[get(org_type) == value]
    
    plt_type <- switch(id,
                       "vote_dist" = c("vote_average", "ocen", "ocena"),
                       "pop_dist" = c("vote_count", "liczby ocen",
                                      "liczba ocen"))
    
    output$density <- renderPlot({
      ggplot(dane_kategoria, aes(x = get(plt_type[1]))) +
        geom_density(fill = "skyblue", alpha = 0.5) +
        labs(title = paste0("Rozkład ", plt_type[2]),
             x = plt_type[3], y = "Gęstość") +
        theme_light()
    })
    
    output$boxplot <- renderPlot({
      ggplot(dane_kategoria, aes(y = get(plt_type[1]))) +
        geom_boxplot(fill = "lightgreen") +
        labs(title = paste("Boxplot ", plt_type[2]),
             y = plt_type[3]) +
        theme_light()
    })
    
    output$qqplot <- renderPlot({
      ggplot(dane_kategoria, aes(sample = get(plt_type[1]))) +
        stat_qq() +
        stat_qq_line() +
        labs(title = paste("QQ-Plot ", plt_type[2])) +
        theme_light()
    })
  })
}