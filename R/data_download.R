library(TMDb)
library(data.table)

set.seed(12)
options(timeout = 60)

readRenviron(".Renviron")
api_key <- Sys.getenv("TMDB_API_KEY")

perc <- 0.009
page_items <- 20

genres <- as.data.table(TMDb::genres_movie_list(api_key)$genres)
genres[, genre := c("Akcja", "Przygodowy", "Animacja", "Komedia", "Kryminalny",
                    "Dokumentalny", "Dramat", "Familijny", "Fantasy",
                    "Historyczny", "Horror", "Muzyczny", "Mystery", "Romans",
                    "Sci-Fi", "TV Film", "Thriller", "Wojenny", "Western")]
saveRDS(genres, "genres.RDS")

keys <- list(with_genres = genres[["id"]],
             year = as.character(1950:2024),
             language = c("en", "fr", "es", "ja", "de", "pt", "zh", "it", "ru",
                          "ko", "cs", "ar", "nl", "sv", "hi", "tr", "pl"),
             vote_average = 0:10,
             vote_count = seq(0, 8000, by = 1000))

pages <- list(with_genres = list(),
              year = list(),
              language = list(),
              vote_average = list(),
              vote_count = list())

pages <- mapply(function(key_type, key_type_name){
  lapply(1:length(key_type), function(key_num){
    args <- list(api_key = api_key)
    
    if(key_type_name %in% names(keys)[1:3])
      args[[key_type_name]] <- key_type[key_num]
    else if(key_num == length(key_type))
      return(NULL)
    else{
      args[[paste0(key_type_name, ".gte")]] <- key_type[key_num]
      args[[paste0(key_type_name, ".lte")]] <- key_type[key_num + 1]
    }
    
    pages_num <- do.call(discover_movie, args)$total_pages
    
    if(pages_num * perc * page_items < 1)
      return(NULL)
    else if(pages_num * perc < 1)
      pages <- sample(1:pages_num, 1)
    else
      pages <- sample(1:min(pages_num, 500), round(pages_num * perc))
  })
}, keys, names(keys), SIMPLIFY = FALSE)

movies <- data.table("genre_ids" = character(),
                     "id" = numeric(),
                     "original_language" = character(),
                     "title" = character(),
                     "popularity" = numeric(),
                     "release_date" = character(),
                     "vote_average" = numeric(),
                     "vote_count" = numeric())

for(key_type_num in 1:length(keys)){
  for(key_num in 1:length(keys[[key_type_num]])){
    args <- list(api_key = api_key)
    
    print(paste0("Key type: ", key_type_num, ". Key: ", key_num, "."))
    
    if(is.null(keys[[key_type_num]][key_num]))
      next
    else if(key_type_num < 4)
      args[[names(keys)[key_type_num]]] <- keys[[key_type_num]][key_num]
    else{
      args[[paste0(names(keys)[key_type_num], ".gte")]] <-
        keys[[key_type_num]][key_num]
      args[[paste0(names(keys)[key_type_num], ".lte")]] <-
        keys[[key_type_num]][key_num + 1]
    }
    
    for(page_num in pages[[key_type_num]][[key_num]]){
      args[["page"]] <- page_num
      
      found_movies <- tryCatch(
        do.call(discover_movie, args)[["results"]],
        error = function(e){
          warning(paste0("Error in key type ", key_type_num, ", key ", key_num,
                         ", page ", page_num))
          NULL
        }
      )
      
      if(page_num * perc < 1)
        found_movies <- found_movies[1:round(page_num * perc * page_items),]
      
      movies <- rbind(movies, found_movies[, names(movies)])
    }
  }
}

found_movies <- discover_movie(api_key = api_key,
                               language = "hi",
                               page = 353)[["results"]]
movies <- rbind(movies, found_movies[, names(movies)])

language_keys <- data.table(
  language = c("angielski", "francuski", "hiszpański",
               "japoński", "niemiecki", "portugalski",
               "chiński", "włoski", "rosyjski",
               "koreański", "czeski", "arabski",
               "niderlandzki", "szwedzki", "hindi",
               "turecki", "polski"),
  id = c("en", "fr", "es", "ja", "de", "pt", "zh", "it", "ru", "ko", "cs",
         "ar", "nl", "sv", "hi", "tr", "pl")
)

filtered_dat <- unique(movies[original_language %in% language_keys[["id"]],],
                       by = "id")
filtered_dat <- filtered_dat[language_keys, on = .(original_language == id)]
filtered_dat <- filtered_dat[!is.na(release_date) & nchar(release_date) > 0,]
filtered_dat[, year := as.integer(substr(release_date, 1, 4))]
filtered_dat <- filtered_dat[year >= min(keys[["year"]]) &
                               year <= max(keys[["year"]]),]

saveRDS(filtered_dat, "movies.RDS")
movies <- readRDS("app/data/new_movies.RDS")

