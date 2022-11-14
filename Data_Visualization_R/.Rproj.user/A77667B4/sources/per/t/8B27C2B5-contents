install.packages('tidyverse')
install.packages("rjson")
library('tidyverse')

my_albums <- readRDS("my_new_albums.rds")
my_artists <- readRDS("my_new_artists.rds")
my_songs <- readRDS("my_new_songs.rds")

my_songs <- subset(my_songs, my_songs$recordLabel != "NULL" & my_songs$recordLabel != "list()" & my_songs$writer != "list()" & my_songs$producer != "list()")
title_songs <- my_songs["title"]

my_songs_v2 <- my_songs

# On parse le champ writer pour avoir une liste propre
my_songs_v2 <- my_songs_v2 %>%
  mutate(writer = as.list(strsplit(gsub('\"',"",str_sub(writer, 7, -3)),", ")))

# On calcule le nombre de writers sur une chanson
my_songs_v2 <- my_songs_v2 %>%
  mutate(nb_writers = lengths(writer)) %>%
  select(c(1:8), nb_writers, everything())

# On parse le champ recordLabel pour avoir une liste propre
my_songs_v2 <- my_songs_v2 %>%
  mutate(recordLabel = as.list(strsplit(gsub('\"',"",str_sub(recordLabel, 7, -3)),", ")))

# On calcule le nombre de labels sur une chanson
my_songs_v2 <- my_songs_v2 %>%
  mutate(nb_record_labels = lengths(recordLabel)) %>%
  select(c(1:7), nb_record_labels, everything())

# On parse le champ producer pour avoir une liste propre
my_songs_v2 <- my_songs_v2 %>%
  mutate(producer = as.list(strsplit(gsub('\"',"",str_sub(producer, 7, -3)),", ")))

# On calcule le nombre de producers sur une chanson
my_songs_v2 <- my_songs_v2 %>%
  mutate(nb_producers = lengths(producer)) %>%
  select(c(1:6), nb_producers, everything())

# On parse le champ genre pour avoir une liste propre
my_songs_v2 <- my_songs_v2 %>%
  mutate(genre = as.list(strsplit(gsub('\"',"",str_sub(genre, 7, -3)),", ")))

# Création de notre data frame (table) collabs
collabs <- data.frame(source="",type1="",location_1="",target="",type2="",location_2="",list_of_songs="",genres="",percent="",value= as.numeric(0))


#collaborations entre artistes (writers)

#Pour chaque chanson
for(i in 1:nrow(my_songs_v2)){
  a <- 1
  b <- a + 1
  n <- my_songs_v2$nb_writers[i]
  #S'il y a au moins 2 writers on crée une ligne pour chaque collab possible dans la chanson (ex: A,B,C collaborent donc il y a une ligne A-B, B-C et A-C)
  while(a!=n){
    already_present <- 0
    while(b<=n){
      already_present <- 0
      for(k in 1:nrow(collabs)){
        if(((collabs$source[k] == my_songs_v2$writer[[i]][a] & collabs$target[k] == my_songs_v2$writer[[i]][b]) | (collabs$source[k] == my_songs_v2$writer[[i]][b] & collabs$target[k] == my_songs_v2$writer[[i]][a])) & collabs$type1[k]=="writer" & collabs$type2[k]=="writer") {
          already_present <- k
        }
      }
      if (already_present == 0){
        j <- nrow(collabs)
        collabs$source[j] <- my_songs_v2$writer[[i]][a]
        collabs$target[j] <- my_songs_v2$writer[[i]][b]
        collabs$type1[j] <- "writer"
        collabs$type2[j] <- "writer"
        if(length(my_artists$locationInfo[my_artists$name == collabs$source[j]]) != 0){
          collabs$location_1[j] <- my_artists$locationInfo[my_artists$name == collabs$source[j]]
        }
        if(length(my_artists$locationInfo[my_artists$name == collabs$target[j]]) != 0){
          collabs$location_2[j] <- my_artists$locationInfo[my_artists$name == collabs$target[j]]
        }
        collabs$list_of_songs[j] <- my_songs_v2$title[i]
        collabs$genres[j] <- my_songs_v2$genre[i]
        collabs$percent[j] <- collabs$genres[j]
        collabs$value[j] <- as.numeric(collabs$value[j]) + 1
        collabs[nrow(collabs)+1,] <- c("","","","","","","","","",as.numeric(0))
      }
      else {
        # On évite de mettre des chansons en doublons pour ne pas fausser le nombre réel de collaborations
        if(length(grep(my_songs_v2$title[i],collabs$list_of_songs[already_present],fixed = TRUE)) == 0){
          collabs$value[already_present] <- as.numeric(collabs$value[already_present]) + 1
          collabs$list_of_songs[already_present] <- paste(collabs$list_of_songs[already_present],my_songs_v2$title[i],sep="_ ")
        }
        # On construit la liste de tous les genres présents pour chaque collaboration
        if(length(my_songs_v2$genre[[i]])!=0){
          for(g in 1:length(my_songs_v2$genre[[i]])){
            if(!(my_songs_v2$genre[[i]][g] %in% collabs$genres[[already_present]])){
              collabs$genres[[already_present]][length(collabs$genres[[already_present]]) + 1] <- my_songs_v2$genre[[i]][g]
            }
          }
        }
      }
      b <- b + 1
    }
    a <- a + 1
    b <- a + 1
  }
}

#collaborations entre producteurs (producers)

for(i in 1:nrow(my_songs_v2)){
  already_present <- 0
  a <- 1
  b <- a + 1
  n <- my_songs_v2$nb_producers[i]
  while(a!=n){
    while(b<=n){
      for(k in 1:nrow(collabs)){
        if(((collabs$source[k] == my_songs_v2$producer[[i]][a] & collabs$target[k] == my_songs_v2$producer[[i]][b]) | (collabs$source[k] == my_songs_v2$producer[[i]][b] & collabs$target[k] == my_songs_v2$producer[[i]][a])) & collabs$type1[k]=="producer" & collabs$type2[k]=="producer") {
          already_present <- k
        }
      }
      if (already_present == 0){
        j <- nrow(collabs)
        collabs$source[j] <- my_songs_v2$producer[[i]][a]
        collabs$target[j] <- my_songs_v2$producer[[i]][b]
        collabs$type1[j] <- "producer"
        collabs$type2[j] <- "producer"
        if(length(my_artists$locationInfo[my_artists$name == collabs$source[j]]) != 0){
          collabs$location_1[j] <- my_artists$locationInfo[my_artists$name == collabs$source[j]]
        }
        if(length(my_artists$locationInfo[my_artists$name == collabs$target[j]]) != 0){
          collabs$location_2[j] <- my_artists$locationInfo[my_artists$name == collabs$target[j]]
        }
        collabs$list_of_songs[j] <- my_songs_v2$title[i]
        collabs$genres[j] <- my_songs_v2$genre[i]
        collabs$percent[j] <- collabs$genres[j]
        collabs$value[j] <- as.numeric(collabs$value[j]) + 1
        collabs[nrow(collabs)+1,] <- c("","","","","","","","","",as.numeric(0))
      }
      else {
        if(length(grep(my_songs_v2$title[i],collabs$list_of_songs[already_present],fixed = TRUE)) == 0){
          collabs$value[already_present] <- as.numeric(collabs$value[already_present]) + 1
          collabs$list_of_songs[already_present] <- paste(collabs$list_of_songs[already_present],my_songs_v2$title[i],sep="_ ")
        }
        if(length(my_songs_v2$genre[[i]])!=0){
          for(g in 1:length(my_songs_v2$genre[[i]])){
            if(!(my_songs_v2$genre[[i]][g] %in% collabs$genres[[already_present]])){
              collabs$genres[[already_present]][length(collabs$genres[[already_present]]) + 1] <- my_songs_v2$genre[[i]][g]
            }
          }
        }
      }
      b <- b + 1
    }
    a <- a + 1
    b <- a + 1
  }
}

#collaborations entre labels (recordLabel)

for(i in 1:nrow(my_songs_v2)){
  already_present <- 0
  a <- 1
  b <- a + 1
  n <- my_songs_v2$nb_record_labels[i]
  while(a!=n){
    while(b<=n){
      for(k in 1:nrow(collabs)){
        if(((collabs$source[k] == my_songs_v2$recordLabel[[i]][a] & collabs$target[k] == my_songs_v2$recordLabel[[i]][b]) | (collabs$source[k] == my_songs_v2$recordLabel[[i]][b] & collabs$target[k] == my_songs_v2$recordLabel[[i]][a])) & collabs$type1[k]=="label" & collabs$type2[k]=="label") {
          already_present <- k
        }
      }
      if (already_present == 0){
        j <- nrow(collabs)
        collabs$source[j] <- my_songs_v2$recordLabel[[i]][a]
        collabs$target[j] <- my_songs_v2$recordLabel[[i]][b]
        collabs$type1[j] <- "label"
        collabs$type2[j] <- "label"
        if(length(my_artists$locationInfo[my_artists$name == collabs$source[j]]) != 0){
          collabs$location_1[j] <- my_artists$locationInfo[my_artists$name == collabs$source[j]]
        }
        if(length(my_artists$locationInfo[my_artists$name == collabs$target[j]]) != 0){
          collabs$location_2[j] <- my_artists$locationInfo[my_artists$name == collabs$target[j]]
        }
        collabs$list_of_songs[j] <- my_songs_v2$title[i]
        collabs$genres[j] <- my_songs_v2$genre[i]
        collabs$percent[j] <- collabs$genres[j]
        collabs$value[j] <- as.numeric(collabs$value[j]) + 1
        collabs[nrow(collabs)+1,] <- c("","","","","","","","","",as.numeric(0))
      }
      else {
        if(length(grep(my_songs_v2$title[i],collabs$list_of_songs[already_present],fixed = TRUE)) == 0){
          collabs$value[already_present] <- as.numeric(collabs$value[already_present]) + 1
          collabs$list_of_songs[already_present] <- paste(collabs$list_of_songs[already_present],my_songs_v2$title[i],sep="_ ")
        }
        if(length(my_songs_v2$genre[[i]])!=0){
          for(g in 1:length(my_songs_v2$genre[[i]])){
            if(!(my_songs_v2$genre[[i]][g] %in% collabs$genres[[already_present]])){
              collabs$genres[[already_present]][length(collabs$genres[[already_present]]) + 1] <- my_songs_v2$genre[[i]][g]
            }
          }
        }
      }
      b <- b + 1
    }
    a <- a + 1
    b <- a + 1
  }
}

#collaborations entre artistes et labels (writer/labels)

for(i in 1:nrow(my_songs_v2)){
  already_present <- 0
  a <- 1
  b <- 1
  x <- my_songs_v2$nb_writers[i]
  y <- my_songs_v2$nb_record_labels[i]
  while(a<=x){
    while(b<=y){
      for(k in 1:nrow(collabs)){
        if(collabs$source[k] == my_songs_v2$writer[[i]][a] & collabs$target[k] == my_songs_v2$recordLabel[[i]][b] & collabs$type1[k]=="writer" & collabs$type2[k]=="label") {
          already_present <- k
        }
      }
      if (already_present == 0){
        j <- nrow(collabs)
        collabs$source[j] <- my_songs_v2$writer[[i]][a]
        collabs$target[j] <- my_songs_v2$recordLabel[[i]][b]
        collabs$type1[j] <- "writer"
        collabs$type2[j] <- "label"
        if(length(my_artists$locationInfo[my_artists$name == collabs$source[j]]) != 0){
          collabs$location_1[j] <- my_artists$locationInfo[my_artists$name == collabs$source[j]]
        }
        if(length(my_artists$locationInfo[my_artists$name == collabs$target[j]]) != 0){
          collabs$location_2[j] <- my_artists$locationInfo[my_artists$name == collabs$target[j]]
        }
        collabs$list_of_songs[j] <- my_songs_v2$title[i]
        collabs$genres[j] <- my_songs_v2$genre[i]
        collabs$percent[j] <- collabs$genres[j]
        collabs$value[j] <- as.numeric(collabs$value[j]) + 1
        collabs[nrow(collabs)+1,] <- c("","","","","","","","","",as.numeric(0))
      }
      else {
        if(length(grep(my_songs_v2$title[i],collabs$list_of_songs[already_present],fixed = TRUE)) == 0){
          collabs$value[already_present] <- as.numeric(collabs$value[already_present]) + 1
          collabs$list_of_songs[already_present] <- paste(collabs$list_of_songs[already_present],my_songs_v2$title[i],sep="_ ")
        }
        if(length(my_songs_v2$genre[[i]])!=0){
          for(g in 1:length(my_songs_v2$genre[[i]])){
            if(!(my_songs_v2$genre[[i]][g] %in% collabs$genres[[already_present]])){
              collabs$genres[[already_present]][length(collabs$genres[[already_present]]) + 1] <- my_songs_v2$genre[[i]][g]
            }
          }
        }
      }
      b <- b + 1
    }
    a <- a + 1
    b <- 1
  }
}

#collaborations entre producteurs et labels (producer/labels)

for(i in 1:nrow(my_songs_v2)){
  already_present <- 0
  a <- 1
  b <- 1
  x <- my_songs_v2$nb_producers[i]
  y <- my_songs_v2$nb_record_labels[i]
  while(a<=x){
    while(b<=y){
      for(k in 1:nrow(collabs)){
        if(collabs$source[k] == my_songs_v2$producer[[i]][a] & collabs$target[k] == my_songs_v2$recordLabel[[i]][b] & collabs$type1[k]=="producer" & collabs$type2[k]=="label") {
          already_present <- k
        }
      }
      if (already_present == 0){
        j <- nrow(collabs)
        collabs$source[j] <- my_songs_v2$producer[[i]][a]
        collabs$target[j] <- my_songs_v2$recordLabel[[i]][b]
        collabs$type1[j] <- "producer"
        collabs$type2[j] <- "label"
        if(length(my_artists$locationInfo[my_artists$name == collabs$source[j]]) != 0){
          collabs$location_1[j] <- my_artists$locationInfo[my_artists$name == collabs$source[j]]
        }
        if(length(my_artists$locationInfo[my_artists$name == collabs$target[j]]) != 0){
          collabs$location_2[j] <- my_artists$locationInfo[my_artists$name == collabs$target[j]]
        }
        collabs$list_of_songs[j] <- my_songs_v2$title[i]
        collabs$genres[j] <- my_songs_v2$genre[i]
        collabs$percent[j] <- collabs$genres[j]
        collabs$value[j] <- as.numeric(collabs$value[j]) + 1
        collabs[nrow(collabs)+1,] <- c("","","","","","","","","",as.numeric(0))
      }
      else {
        if(length(grep(my_songs_v2$title[i],collabs$list_of_songs[already_present],fixed = TRUE)) == 0){
          collabs$value[already_present] <- as.numeric(collabs$value[already_present]) + 1
          collabs$list_of_songs[already_present] <- paste(collabs$list_of_songs[already_present],my_songs_v2$title[i],sep="_ ")
        }
        if(length(my_songs_v2$genre[[i]])!=0){
          for(g in 1:length(my_songs_v2$genre[[i]])){
            if(!(my_songs_v2$genre[[i]][g] %in% collabs$genres[[already_present]])){
              collabs$genres[[already_present]][length(collabs$genres[[already_present]]) + 1] <- my_songs_v2$genre[[i]][g]
            }
          }
        }
      }
      b <- b + 1
    }
    a <- a + 1
    b <- 1
  }
}

#collaborations entre artistes et producteurs (writer/producer)

for(i in 1:nrow(my_songs_v2)){
  already_present <- 0
  a <- 1
  b <- 1
  x <- my_songs_v2$nb_writers[i]
  y <- my_songs_v2$nb_producers[i]
  while(a<=x){
    while(b<=y){
      for(k in 1:nrow(collabs)){
        if(collabs$source[k] == my_songs_v2$writer[[i]][a] & collabs$target[k] == my_songs_v2$producer[[i]][b] & collabs$type1[k]=="writer" & collabs$type2[k]=="producer") {
          already_present <- k
        }
      }
      if (already_present == 0){
        j <- nrow(collabs)
        collabs$source[j] <- my_songs_v2$writer[[i]][a]
        collabs$target[j] <- my_songs_v2$producer[[i]][b]
        collabs$type1[j] <- "writer"
        collabs$type2[j] <- "producer"
        if(length(my_artists$locationInfo[my_artists$name == collabs$source[j]]) != 0){
          collabs$location_1[j] <- my_artists$locationInfo[my_artists$name == collabs$source[j]]
        }
        if(length(my_artists$locationInfo[my_artists$name == collabs$target[j]]) != 0){
          collabs$location_2[j] <- my_artists$locationInfo[my_artists$name == collabs$target[j]]
        }
        collabs$list_of_songs[j] <- my_songs_v2$title[i]
        collabs$genres[j] <- my_songs_v2$genre[i]
        collabs$percent[j] <- collabs$genres[j]
        collabs$value[j] <- as.numeric(collabs$value[j]) + 1
        collabs[nrow(collabs)+1,] <- c("","","","","","","","","",as.numeric(0))
      }
      else {
        if(length(grep(my_songs_v2$title[i],collabs$list_of_songs[already_present],fixed = TRUE)) == 0){
          collabs$value[already_present] <- as.numeric(collabs$value[already_present]) + 1
          collabs$list_of_songs[already_present] <- paste(collabs$list_of_songs[already_present],my_songs_v2$title[i],sep="_ ")
        }
        if(length(my_songs_v2$genre[[i]])!=0){
          for(g in 1:length(my_songs_v2$genre[[i]])){
            if(!(my_songs_v2$genre[[i]][g] %in% collabs$genres[[already_present]])){
              collabs$genres[[already_present]][length(collabs$genres[[already_present]]) + 1] <- my_songs_v2$genre[[i]][g]
            }
          }
        }
      }
      b <- b + 1
    }
    a <- a + 1
    b <- 1
  }
}

# Construction de la liste des chansons pour chaque collaboration
for (g in 1:(as.numeric(nrow(collabs))-1)){
  if(!length(grep("_ ",collabs$list_of_songs[g])) == 0){
    collabs$list_of_songs[g] <- strsplit(as.character(collabs$list_of_songs[g]), split ="_ ")
  }
}

# On crée une liste répertoriant le part d'utilisation du genre pour une collaboration précise percent[1] correspond à la part du genre[1]
for (g in 1:(as.numeric(nrow(collabs))-1)){
  for(p in 1:length(collabs$genres[[g]])){
    collabs$percent[[g]][p] <- 0
  }

  for(x in 1:(length(collabs$list_of_songs[[g]]))){
    for(y in 1:length(collabs$genres[[g]])){
      if(length(collabs$genres[[g]][y])!=0){
        if(collabs$genres[[g]][y] %in% my_songs_v2$genre[my_songs_v2$title == collabs$list_of_songs[[g]][x]][[1]]){
          collabs$percent[[g]][y] <- as.numeric(collabs$percent[[g]][y]) + 1
        }
      }
    }
  }
}

for (g in 1:(as.numeric(nrow(collabs))-1)){
  for(y in 1:length(collabs$genres[[g]])){
    if(length(collabs$genres[[g]][y])!=0){
      if(collabs$percent[[g]][y] !=0){
        collabs$percent[[g]][y] <- (as.numeric(collabs$percent[[g]][y])/as.numeric(collabs$value[g]))*100
      }
    }
  }
}

# On retire les potentielles auto-collaborations pour des artistes ayant plusieurs roles du type producer-writer 
collabs <- subset(collabs, collabs$source != collabs$target)

# création du JSON correspondant au dataframe collabs
my_json <- jsonlite::toJSON(collabs,pretty = T)
write(my_json,"collabs.JSON")


saveRDS(collabs, "collabsV1.rds")

my_artists_v2 <- my_artists
my_artists_v2 <- subset(my_artists_v2, !is.na(my_artists_v2$recordLabel))

my_artists_v2 <- my_artists_v2 %>%
  mutate(recordLabel = as.list(strsplit(recordLabel,", ")))

# On répertorie les artistes dont les collaborations sont inconnus car non présents dans la table my_songs

unknown_collab_artists <- data.frame(name="",statut="",location="")

for(i in 1:nrow(my_artists_v2)){
  unknown <- 0
  
  r <- nrow(unknown_collab_artists)
  
  for(c in 1:nrow(collabs)){
    if(my_artists_v2$name[i] == collabs$source[c] | my_artists_v2$name[i] == collabs$target[c]){
      unknown <- 1
    }
  }
  
  if(unknown ==0){
    unknown_collab_artists$name[r] <- my_artists_v2$name[i]
    unknown_collab_artists$statut[r] <- "writer"
    unknown_collab_artists$location[r] <- my_artists_v2$locationInfo[i]
    unknown_collab_artists[r+1,] <- c("","","")
  }
  
  r <- nrow(unknown_collab_artists)
  
    for(l in 1:length(my_artists_v2$recordLabel[[i]])){
      for(c in 1:nrow(collabs)){
        if(my_artists_v2$recordLabel[[i]][l] == collabs$source[c] | my_artists_v2$recordLabel[[i]][l] == collabs$target[c]){
          unknown <- 1
        }
      }
      if(unknown ==0){
        unknown_collab_artists$name[r] <- my_artists_v2$recordLabel[[i]][l]
        unknown_collab_artists$statut[r] <- "label"
        unknown_collab_artists[r+1,] <- c("","","")
      }
    }
}

# Création du JSON unknown collabs
my_json_2 <- jsonlite::toJSON(unknown_collab_artists,pretty = T)
write(my_json_2,"unknown_collabs.JSON")

write_csv(collabs,"/Users/antoine/Documents/Polytech/SI5/Information Visualization/collabsV1.csv")
write_csv(my_songs_v2,"/Users/antoine/Documents/Polytech/SI5/Information Visualization/my_songs.csv")

