library(tidyverse) # ggplot, dplyr, tidyr, readr, purrr, tibble, stringr, forcats
library(rvest) # interacting with html and webcontent
library(glue) # used to combine strings in clever ways
library(tidytext) 
library(wordcloud2)
library(tibble)
library(textdata)
library()
dataset <- read_csv("final_compiled_imdb_dataset_081120.csv")
view(dataset)

#---------------- SCRAPE ROTTEN TOMATOES MOVIE REVIEWS ----------------#

scrape_review <- function(movie_name) {
  
  url_movie_name <- str_replace_all(movie_name, " ", "_")
  
  # Get main url
  url <- glue("https://www.rottentomatoes.com/m/{url_movie_name}/reviews")
  
  # Load first page to get page review count
  page_number <- url %>% 
    read_html() %>% 
    html_nodes(".content > div:nth-child(1) > span:nth-child(2)") %>% 
    html_text() %>% 
    str_trim() 
  
  
  
  max_page <- as.numeric(strsplit(page_number, "\\s+")[[1]][4])
  
  reviews <- c() 
  
  # Loop through each review page
  for (n in 1:max_page){
    
    review_url <- glue("https://www.rottentomatoes.com/m/{url_movie_name}/reviews?type=&sort=&page={n}")
    
    # Loop through each review 
    for (i in 1:20) {
      
      Sys.sleep(1)
      cat("<") # Progression bar
    
      node_element <- glue("div.row:nth-child({i}) > div:nth-child(2) > div:nth-child(2) > div:nth-child(2) > div:nth-child(1)")
      review <- review_url %>% 
        read_html() %>% 
        html_nodes(node_element) %>% 
        html_text() %>% 
        str_trim() 
         
      if (nchar(review) > 0) {
        reviews <- c(reviews, review)
      }
    }
  }

  return(data.frame(review = reviews))

}

#---------------- SCRAPE ROTTEN TOMATOES MOVIE INFO ----------------#

scrape_info <- function(url_movie_name) {
  
  url_movie_name <- str_replace_all(movie_name, " ", "_")
  
  # Get main url
  url <- glue("https://www.rottentomatoes.com/m/{url_movie_name}")
  
  # Load first page to get page review count
  info <- url %>% 
    read_html() %>% 
    html_nodes("#movieSynopsis") %>% 
    html_text() %>% 
    str_trim() 
  
  return(data.frame(review = c(info)))
  
}

movies_url_by_genre <- read_csv("genre_movies.csv")

scrape_info_genre <- function(genre, sort) {
  
  if (sort == "top") {
    url_list <- unname(unlist(movies_url_by_genre[which(movies_url_by_genre$Final_Genre == genre),][,-1][1,1:5]))
  } else {
    url_list <- unname(unlist(movies_url_by_genre[which(movies_url_by_genre$Final_Genre == genre),][,-1][1,6:10]))
  }
  
  result <- c()
  
  for (i in 1:length(url_list)) {
    Sys.sleep(1)
    cat("<")
    
    url <- url_list[i]
    
    info <- url %>% 
      read_html() %>% 
      html_nodes("#movieSynopsis") %>% 
      html_text() %>% 
      str_trim() 
    
    result <- c(result, info)
  }
  
  return(data.frame(review = result))
}

top_description <- scrape_info_genre("Comedy", "top")
tidy_data <- clean_data(top_description)
frequency_table <- create_frequency_table(tidy_data)
view(frequency_table)
generate_word_cloud(frequency_table)
  
#--------------------- TIDY DATA ---------------------#

clean_data <- function(review_df, movie_name) {
  tidy_data <- review_df %>%
    unnest_tokens(word, review) %>%
    group_by(word) %>%
    filter(n() > 0) %>%
    ungroup()
  
  stop_words <- stop_words
  customized_stop_words <- c("film", "movies", "films")

  tidy_data <- tidy_data %>%
    anti_join(stop_words) %>%
    filter(!word %in% customized_stop_words)
  
  return(tidy_data)
}

#---------------- CREATE FREQUENCY TABLE ----------------#

create_frequency_table <- function (tidy_data) {
  
  frequency_table <- tidy_data %>% 
    group_by(word) %>% 
    summarize(frequency = n()) %>% 
    arrange(desc(frequency))
  
  return(frequency_table)
}

#---------------- CREATE WORD CLOUD ----------------#

generate_word_cloud <- function(frequency_table) {
  frequency_table %>% 
    wordcloud2(backgroundColor = "black",
               color = "random-light")
}

#---------------- PERFORM SENTIMENT ANALYSIS ----------------#

perform_sentiment_analysis <- function(tidy_data) {
  AFINN <- get_sentiments("afinn")
  
  # Assigning scores using AFINN dictionary
  sentiment_reviews <- tidy_data %>% 
    inner_join(AFINN, by = "word") %>%
    mutate(total_sentiment = mean(value)) %>% 
    group_by(word) %>%
    summarize(sentiment = mean(value),
              words = n()) 
  # %>%
  # ungroup() %>% 
  # filter(words > 3) 
  return(sentiment_reviews)
}

perform_overall_sentiment_analysis <- function(tidy_data) {
  AFINN <- get_sentiments("afinn")
  
  # Assigning scores using AFINN dictionary
  sentiment_reviews <- tidy_data %>% 
    inner_join(AFINN, by = "word") %>%
    mutate(total_sentiment = mean(value)) 
  
  return(sentiment_reviews)
}

# Identifying the most negative word
sentiment_reviews %>%
  arrange(sentiment,
          words)

#----------------------------------------------------------#
movie_name <- "The Notebook"

# For Movie Reviews ####
scraped_data <- scrape_review(movie_name)
tidy_data <- clean_data(scraped_data, movie_name)
frequency_table <- create_frequency_table(tidy_data)
generate_word_cloud(frequency_table)


sentiment_result <- perform_sentiment_analysis(tidy_data)
sentiment_result
view(scraped_data)

# For Movie Info ####

# Get list of unique genres ---- SHOW FRONTEND
unique_genre <- unique(dataset %>% 
  select(Final_Genre))
write.csv(unique_genre, "genre_movies.csv")
print(unique_genre)


top_movies <- dataset %>% 
  filter(Final_Genre == "Comedy") %>% 
  select(Movie, Rating) %>% 
  arrange(desc(Rating)) %>%
  slice(1:10)

nrow(dataset)

top_movies[1,1]

# Select by genre and sort by rating 

sorted_data <- dataset %>% 
  filter(Final_Genre == "Drama") %>% 
  select(Movie, Rating) %>% 
  arrange(desc(Rating)) 

top10 <- sorted_data[1:10,]$Movie
top10

for (i in nrow(top10$Movie)) {
  
}

review <- scrape_review("Aloko Udapadi")

view(sorted_data)

sorted_data[1: (nrow(sorted_data)*0.1),]

# Get top 5 movies ---- SHOW FRONTEND
top_5_movies <- sorted_data[[1]][1:5]
top_5_movies

# Get info data
scraped_info_data <- scrape_info(movie_name)

tidy_data <- clean_data(scraped_info_data, movie_name)

frequency_table <- create_frequency_table(tidy_data)

generate_word_cloud(frequency_table)


perform_sentiment_analysis(tidy_data)
perform_overall_sentiment_analysis(tidy_data)

movies_url_by_genre <- read_csv("genre_movies.csv")
movies_url_by_genre
unname(unlist(movies_url_by_genre[which(movies_url_by_genre$Final_Genre == "Comedy"),][,-1][1,12]))[1]
url_list

unname(unlist(movies_url_by_genre[which(movies_url_by_genre$Final_Genre == "Comedy"),][,-1][1,2]))[1]

unname(unlist(movies_url_by_genre[which(movies_url_by_genre$Final_Genre == "Comedy"),][,-1][1,11]))[1]

scrape_release_date <- function(url) {
  # Load first page to get page review count
  release_date <- url %>% 
    read_html() %>% 
    html_nodes("li.meta-row:nth-child(4) > div:nth-child(2) > time:nth-child(1)") %>% 
    html_text() %>% 
    str_trim() 
}
li.meta-row:nth-child(8) > div:nth-child(2) > time:nth-child(1)

release_date <- scrape_release_date(url_list)
release_date

movies_url_by_genre

unname(unlist(movies_url_by_genre[which(movies_url_by_genre$Final_Genre == "Comedy"),][,-1][1,9]))[2]

movies_url_by_genre <- read_csv("genre_movies.csv")
unname(unlist(movies_url_by_genre[which(movies_url_by_genre$Final_Genre == "Comedy"),][,-1][1,11]))[1]


release_date <- unname(unlist(movies_url_by_genre[which(movies_url_by_genre$Final_Genre == "Comedy"),][,-1][1,14]))[1]
bottom_movie <- unname(unlist(movies_url_by_genre[which(movies_url_by_genre$Final_Genre == "Comedy"),][,-1][1,12]))[1]
release_date
bottom_movie


release_date <- unname(unlist(movies_url_by_genre[which(movies_url_by_genre$Final_Genre == input$genre),][,-1][1,13]))[1]
top_movie <- unname(unlist(movies_url_by_genre[which(movies_url_by_genre$Final_Genre == input$genre),][,-1][1,11]))[1]

library(httr)
library(jsonlite)

top_5_movies <- sorted_data[[1]][1:5]
top_5_movies[1]
get_description <- function(movie_list) {
  result <- c()
  
  for (i in 1:length(movie_list)) {
    url <- "https://movie-database-imdb-alternative.p.rapidapi.com/"
    
    queryString <- list(
      t = movie_list[i],
      r = "json"
    )
    
    response <- GET(url, add_headers("x-rapidapi-key" = '15144c1dfemshaf1c02563d16c51p1566d7jsn925f71e1209f', "x-rapidapi-host" = 'movie-database-imdb-alternative.p.rapidapi.com'), query = queryString, content_type("application/octet-stream"))
    data = fromJSON(rawToChar(response$content))
    result <- c(result, data$Plot)
  }
  
  return(data.frame(name = movie_list, description = result))
}

description_data <- get_description(top_5_movies)
view(description_data)

tokenized_description <- description_data %>% 
  unnest_tokens(word, description) %>% 
  anti_join(stop_words)
tokenized_description
AFINN <- get_sentiments("afinn")

sentiments_description <- tokenized_description %>% 
  inner_join(AFINN, by="word") %>% 
  group_by(name) %>% 
  summarize(sentiment = mean(value), words = n())

sentiments_description


des_df <- data.frame(description <- description_data)
glimpse(des_df)

clean_data(des_df$description)



tidy_data <- clean_data(description_data)
tidy_data
frequency_table <- create_frequency_table(tidy_data)
frequency_table %>%
  wordcloud2(backgroundColor = "black",
             color = "random-light")
frequency_table
view(description_data)

clean_data <- function(review_df) {
  tidy_data <- review_df %>%
    unnest_tokens(word, description) %>%
    group_by(word) %>%
    filter(n() > 0) %>%
    ungroup()
  
  stop_words <- stop_words
  customized_stop_words <- c("film", "movies", "films")
  
  tidy_data <- tidy_data %>%
    anti_join(stop_words) %>%
    filter(!word %in% customized_stop_words)
  
  return(tidy_data)
}

#---------------- CREATE FREQUENCY TABLE ----------------#

create_frequency_table <- function (tidy_data) {
  
  frequency_table <- tidy_data %>% 
    group_by(word) %>% 
    summarize(frequency = n()) %>% 
    arrange(desc(frequency))
  
  return(frequency_table)
}

#---------------- CREATE WORD CLOUD ----------------#

generate_word_cloud <- function(frequency_table) {
  frequency_table %>% 
    wordcloud2(backgroundColor = "black",
               color = "random-light")
}

dataset <- read_csv("final_compiled_imdb_dataset_111120.csv")

getTopMovies <- function() {
  result <- dataset %>%
    filter(Final_Genre == "Comedy") %>%
    select(Movie, Rating) %>%
    arrange(desc(Rating)) %>%
    slice(1:10)
  
  return(result)
}

scrape_description(getTopMovies()$Movie)

install.packages("ggwordcloud")



#############
dataset <- read_csv("final_compiled_imdb_dataset_111120.csv")

genre_list <- c(unique(as.character(dataset$Final_Genre)))

get_description <- function(movie_list) {
  result <- c()
  
  for (i in 1:length(movie_list)) {
    url <- "https://movie-database-imdb-alternative.p.rapidapi.com/"
    
    queryString <- list(
      t = movie_list[i],
      r = "json"
    )
    
    response <- GET(url, add_headers("x-rapidapi-key" = '15144c1dfemshaf1c02563d16c51p1566d7jsn925f71e1209f', "x-rapidapi-host" = 'movie-database-imdb-alternative.p.rapidapi.com'), query = queryString, content_type("application/octet-stream"))
    data = fromJSON(rawToChar(response$content))
    result <- c(result, data$Plot)
  }
  
  return(data.frame(name = movie_list, description = result))
}

for (i in 1:length(genre_list)) {
  movie_list <- dataset %>%
    filter(Final_Genre == genre_list[i]) %>%
    select(Movie, Rating) %>%
    arrange(desc(Rating)) %>%
    slice(1:10)
  
  df <- get_description(movie_list$Movie)
  write.csv(df, paste0(genre_list[i],".csv",""))
}



