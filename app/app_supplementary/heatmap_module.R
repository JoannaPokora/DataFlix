heatmap_UI <- function(id){
  ns <- NS(id)
  
  column(6,
         plotOutput(ns("heatmap"), height = "1000px")
  )
}

heatmap_SERVER <- function(id, dat){
  moduleServer(id, function(input, output, session){
    variables <- switch(
      id,
      "heatmap_lang_pop" = c("vote_count", "language"),
      "heatmap_lang_vote" = c("vote_average", "language"),
      "heatmap_genre_pop" = c("vote_count", "genre"),
      "heatmap_genre_vote" = c("vote_average", "genre") 
    )
    
    x_lab <- ifelse(variables[2] == "language",
                    "Oryginalny język",
                    "Gatunek")
    fill_name <- ifelse(variables[1] == "vote_count",
                        "Średnia liczba ocen",
                        "Średnia ocena")
    title_lab <- paste0("Średnia ",
                        ifelse(variables[1] == "vote_count",
                               "liczba ocen",
                               "ocena"),
                        " w zależności od roku i ",
                        ifelse(variables[2] == "language",
                               "oryginalnego języka",
                               "gatunku"))
    limits <- if(variables[1] == "vote_count")
      c(0, 6200)
    else
      c(0, 10)
    
    summarised_dat <- dat[, .(av = mean(get(variables[1]))),
                          by = c(variables[2], "year")]
    
    colors <- as.character(paletteer_d("ggsci::light_green_material"))
    pal_fun <- gradient_n_pal(colors)
    
    output[["heatmap"]] <- renderPlot({
      ggplot(summarised_dat,
             aes(x = get(variables[2]), y = as.factor(year), fill = av)) +
        geom_tile() +
        labs(x = x_lab, y = "Rok", title = title_lab) +
        scale_fill_gradientn(colours = colors, name = fill_name,
                             limits = limits) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    })
  })
}