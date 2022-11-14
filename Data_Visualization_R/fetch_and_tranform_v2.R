setwd("/Users/antoine/Documents/Polytech/SI5/Information Visualization/Wasabi-dataset-03-10-2022") # set the working directory ; change the path to the one where you have this file in your computer

#install the packages (only once)
install.packages("httr") # to make http requests to the wasabi api
install.packages("jsonlite") # to handle json data

# Loading packages (to be done every time the script is executed)
library(httr)
library(jsonlite)
library(data.table)
library(tidyverse)


offset = 72000 # where to start searching for data in the dataset ; here start the search from the 72000th record

get_url <- "wasabi.i3s.unice.fr/api/v1/artist_all/" # this url gets data for all artists in the wasabi API
# see https://wasabi.i3s.unice.fr/apidoc/ to know what pattern to use to recover the data you want

albums_df <- tibble()
songs_df <- tibble()
artist_df <- tibble()

for (i in 1:15) {
  print(paste(i, " requesting data..."))
  
  req <- GET(url = paste0(get_url, offset)) # do the request
  
  data_json <- fromJSON(content(req, "text"), simplifyVector = FALSE) # recover the data from the request in a JSON format
  
  data_list <- sapply(data_json, function(x) { # transform each list inside the json file into a tibble ; return a list with several tibbles
    x[sapply(x, is.null)] <- NA
    unlist(x)
    as_tibble(t(x))
  })
  
  data_df <- rbindlist(data_list, use.names = TRUE, fill = TRUE) # concatenate all tibbles in data_list into a single tibble
  
  print(paste("creating album dataset..."))
  # create a dataset with all albums
  albums_list <- flatten(data_df$albums) # data_df$albums is a list of lists of lists -> flatten transform it into a single list of lists
  
  albums_list <- sapply(albums_list, function(x) { # transform each list inside albums_list into a tibble
    x[sapply(x, is.null)] <- NA
    unlist(x)
    as_tibble(t(x))
  })
  
  albums_temp <- rbindlist(albums_list, use.names = TRUE, fill = TRUE) # bind all tibbles inside albums_list into a single tibble
  
  albums_df <- albums_df %>% bind_rows(albums_temp %>% select(-cover, -songs) %>% mutate_all(as.character))
  
  print("creating songs dataset...")
  #create a dataset with all songs for the 200 artists recovered
  
  songs_list <- sapply( flatten(albums_temp$songs), function(x) { # transform each list inside albums_temp$songs into a tibble
    x[sapply(x, is.null)] <- NA
    unlist(x)
    as_tibble(t(x))
  })
  
  songs_temp <- rbindlist(songs_list, use.names = TRUE, fill = TRUE) %>% rename(id = "_id") # bind all tibbles inside songs_list into a single tibble
  df1 <- songs_temp %>% select(-availableCountries)
  df2 <- songs_temp %>% select(id, availableCountries) %>% 
    unnest(availableCountries) %>% group_by(id) %>%
    mutate(availableCountries = paste(availableCountries, collapse = ', ')) %>%
    distinct(id, .keep_all = TRUE) %>%
    ungroup()
  
  songs_temp <- df1 %>% left_join(df2, by = "id") %>%
    select(-animux_paths, -deezer_mapping) %>%
    mutate_all(as.character)
  
  saveRDS(songs_temp, paste0("songs_all_artists_", offset, ".rds"))
  
  songs_df <- songs_df %>% bind_rows(songs_temp) # concatenate the new data
  
  
  print("creating artist dataset...")
  # preparing a dataset with all attributes of artist, except by albums and songs and a few non-important attributes such as location (same as locationIndo), endArea and picture
  # members would have to be transformed into a new tibble, but no joining variable is available, so it has been excluded for now
  variables <- c("locationInfo", "genres", "labels", "nameVariations", "nameVariations_fold", "urls", "subject", "associatedMusicalArtist", "dbp_genre", "recordLabel")
  
  artist_temp <- data_df %>% rename(id = "_id")
  
  for (v in variables) {
    df1 <- artist_temp %>% select(-v)
    df2 <- artist_temp %>% select(id, v) %>% unnest(v) %>% group_by(id) %>%
      mutate_at(v, funs(paste(., collapse = ', '))) %>%
      distinct(id, .keep_all = TRUE) %>%
      ungroup()
    
    artist_temp <- df1 %>% left_join(df2, by = "id")
  }
  
  artist_temp <- artist_temp %>% select(-location, -endArea, -picture, -members, -albums) %>% # eliminate the variables that we don't need
    unnest_wider(lifeSpan, names_sep = ".") %>% # expand the lifeSpan json object into columns of the tibble
    mutate_all(as.character) # transform all variables into character for consistency
  
  artist_df <- artist_df %>% bind_rows(artist_temp) # concatenate the new data 
  
  offset <- offset + 200 # increase the offset where to start the search for data in the wasabi database
}

# filtrage des datasets :

songs_df <- songs_df[,c("id","id_album","title","language_detect","genre","producer","recordLabel","writer","recorded")]
albums_df <- albums_df[,c("_id","name","genre","id_artist","title","country","language")]
artist_df <- artist_df[,c("id","name","locationInfo","genres","type","gender","recordLabel","labels")]

# save data as RDS objects to be reused in R
saveRDS(songs_df, "my_new_songs.rds")
saveRDS(albums_df, "my_new_albums.rds")
saveRDS(artist_df, "my_new_artists.rds")

# songs_df is too big to be written as csv
# write_csv(songs_df, "songs_all_artists_3000.csv")
# write_csv(albums_df, "albums_all_artists_3000.csv")
# write_csv(x = artist_df, file = "wasabi_all_artists_3000.csv")