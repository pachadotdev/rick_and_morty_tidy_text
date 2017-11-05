# 1: load packages

if (!require("pacman")) install.packages("pacman")
#run sudo apt-get install libpoppler-cpp-dev to install pdftools
p_load(data.table,dplyr,stringr,rvest,janitor)
p_load_gh("fkeck/subtools")

##########################################################################################

# 2: scrap BoJack Horseman subtitles

url_subtitles = read_html("https://www.opensubtitles.org/en/ssearch/sublanguageid-eng/idmovie-181349")

subtitles_tables = url_subtitles %>%
  html_nodes(xpath='//*[(@id = "search_results")]') %>%
  html_table(fill = TRUE)

links_2 = url_subtitles %>% 
  html_nodes("a") %>% 
  html_attr("href")

titles_2 = url_subtitles %>% 
  html_nodes("a") %>% 
  html_attr("title")

links_all_seasons = tibble(link = links_2, title = titles_2) %>% 
  filter(!is.na(title)) %>% 
  distinct(link, .keep_all = T) %>% 
  mutate(link = str_replace_all(link, "^/en/search/", "https://www.opensubtitles.org/download/s/")) %>% 
  filter(grepl("imdb", link)) %>%
  mutate(title = str_replace_all(title, "\"BoJack Horseman\" ", ""),
         season = c(rep("S01",12),rep("S02",13),rep("S03",12),rep("S04",11)),
         episode = c(1:12,0:12,1:12,1:8,10:12),
         episode = if_else(nchar(episode) == 1, paste0("E0",episode), paste0("E",episode)),
         file_name = paste(season, episode, str_replace_all(title, "[[:punct:]]", ""), sep = "_"),
         file_name = str_replace_all(file_name, " ", "_")) %>% 
  select(episode, season, title, file_name, link)

try(dir.create("02_other_shows_subtitles"))
try(dir.create("02_other_shows_subtitles/bojack_horseman"))
try(dir.create("02_other_shows_subtitles/bojack_horseman/subtitles"))
try(dir.create("02_other_shows_subtitles/bojack_horseman/subtitles_extraction"))

fwrite(links_all_seasons, "02_other_shows_subtitles/bojack_horseman_links_all_seasons.csv")

for(j in 1:nrow(links_all_seasons)) {
  if(!file.exists(paste0("02_other_shows_subtitles/bojack_horseman/subtitles/",links_all_seasons$file_name[j],".zip"))) {
    Sys.sleep(sample(seq(0.5, 1.5, by=0.5), 1))
    download.file(links_all_seasons$link[j], paste0("02_other_shows_subtitles/bojack_horseman/subtitles/",links_all_seasons$file_name[j],".zip"), method = "wget")
  }
}

subtiles_zip = list.files("02_other_shows_subtitles/bojack_horseman/subtitles/", recursive = T) %>% 
  paste0("02_other_shows_subtitles/bojack_horseman/subtitles/", .)

for(j in 1:length(subtiles_zip)) {
  try(dir.create(paste0("02_other_shows_subtitles/bojack_horseman/subtitles_extraction/",links_all_seasons$file_name[j])))
  unzip(subtiles_zip[[j]], exdir = paste0("02_other_shows_subtitles/bojack_horseman/subtitles_extraction/",links_all_seasons$file_name[j]), overwrite = F)
}

subtitles_srt = tibble(sub_location = list.files("02_other_shows_subtitles/bojack_horseman/subtitles_extraction/", recursive = T)) %>% 
  mutate(sub_location = paste0("02_other_shows_subtitles/bojack_horseman/subtitles_extraction/",sub_location),
         folder = as.character(str_extract_all(sub_location, ".*/")),
         file_size = file.size(sub_location)) %>% 
  group_by(folder) %>% 
  filter(file_size == max(file_size)) %>% 
  distinct(folder, .keep_all = T) %>% 
  mutate(episode_name = str_replace_all(sub_location, "02_other_shows_subtitles/bojack_horseman/subtitles_extraction/", ""),
         episode_name = str_replace_all(episode_name, "/.*", ""),
         season = str_replace_all(substr(episode_name,1,7), "_.*", ""),
         episode = str_replace_all(substr(episode_name,1,7), ".*_", ""),
         episode_name = str_replace_all(episode_name, "S[0-9][0-9]_E[0-9][0-9]_", ""),
         episode_name = str_replace_all(episode_name, "_", " "))

for(j in 1:nrow(subtitles_srt)) {
  system(paste0("recode UTF-8 -f \"", subtitles_srt$sub_location[j], "\""))
}

for(j in 1:nrow(subtitles_srt)) {
  assign(paste(subtitles_srt$season[j],subtitles_srt$episode[j], sep = "_"),
         as_tibble(read.subtitles(subtitles_srt$sub_location[j])$subtitles) %>%
           clean_names() %>%
           rename(linenumber = id) %>% 
           filter(nchar(text) > 0) %>%
           mutate(text = iconv(tolower(text)) ,
                  season = subtitles_srt$season[j],
                  episode = subtitles_srt$episode[j],
                  episode_name = subtitles_srt$episode_name[j])
  )
}

bojack_horseman_subs = mget(ls(pattern = "S[0-9][0-9]_E[0-9][0-9]")) %>% 
  bind_rows()

rm(list = ls(pattern = "S[0-9][0-9]_E[0-9][0-9]"))

fwrite(bojack_horseman_subs, "02_other_shows_subtitles/bojack_horseman_subs.csv")

list_subtitles_all = tibble(sub_location = list.files("02_other_shows_subtitles/bojack_horseman/subtitles_extraction/", recursive = T)) %>% 
  mutate(sub_location = paste0("02_other_shows_subtitles/bojack_horseman/subtitles_extraction/",sub_location),
         folder = as.character(str_extract_all(sub_location, ".*/")),
         file_size = file.size(sub_location)) 

list_subtitles_keep = list_subtitles_all %>% 
  group_by(folder) %>% 
  filter(file_size == max(file_size)) %>% 
  distinct(folder, .keep_all = T)

list_subtitles_remove = list_subtitles_all %>%   
  anti_join(list_subtitles_keep)

for(j in 1:nrow(list_subtitles_remove)) {
  try(file.remove(list_subtitles_remove$sub_location[j]))
}

##########################################################################################

# 3: scrap Gravity Falls subtitles

url_subtitles = read_html("https://www.opensubtitles.org/en/ssearch/sublanguageid-eng/idmovie-131837")

subtitles_tables = url_subtitles %>%
  html_nodes(xpath='//*[(@id = "search_results")]') %>%
  html_table(fill = TRUE)

links_2 = url_subtitles %>% 
  html_nodes("a") %>% 
  html_attr("href")

titles_2 = url_subtitles %>% 
  html_nodes("a") %>% 
  html_attr("title")

links_all_seasons = tibble(link = links_2, title = titles_2) %>% 
  filter(!is.na(title)) %>% 
  distinct(link, .keep_all = T) %>% 
  mutate(link = str_replace_all(link, "^/en/search/", "https://www.opensubtitles.org/download/s/")) %>% 
  filter(grepl("imdb", link)) %>%
  mutate(title = str_replace_all(title, "\"Gravity Falls\" ", ""),
         season = c(rep("S01",20),rep("S02",20)),
         episode = c(1:20,1:20),
         episode = if_else(nchar(episode) == 1, paste0("E0",episode), paste0("E",episode)),
         file_name = paste(season, episode, str_replace_all(title, "[[:punct:]]", ""), sep = "_"),
         file_name = str_replace_all(file_name, " ", "_")) %>% 
  select(episode, season, title, file_name, link)

try(dir.create("02_other_shows_subtitles"))
try(dir.create("02_other_shows_subtitles/gravity_falls"))
try(dir.create("02_other_shows_subtitles/gravity_falls/subtitles"))
try(dir.create("02_other_shows_subtitles/gravity_falls/subtitles_extraction"))

fwrite(links_all_seasons, "02_other_shows_subtitles/gravity_falls_links_all_seasons.csv")

for(j in 1:nrow(links_all_seasons)) {
  if(!file.exists(paste0("02_other_shows_subtitles/gravity_falls/subtitles/",links_all_seasons$file_name[j],".zip"))) {
    Sys.sleep(sample(seq(0.5, 1.5, by=0.5), 1))
    download.file(links_all_seasons$link[j], paste0("02_other_shows_subtitles/gravity_falls/subtitles/",links_all_seasons$file_name[j],".zip"), method = "wget")
  }
}

subtiles_zip = list.files("02_other_shows_subtitles/gravity_falls/subtitles/", recursive = T) %>% 
  paste0("02_other_shows_subtitles/gravity_falls/subtitles/", .)

for(j in 1:length(subtiles_zip)) {
  try(dir.create(paste0("02_other_shows_subtitles/gravity_falls/subtitles_extraction/",links_all_seasons$file_name[j])))
  unzip(subtiles_zip[[j]], exdir = paste0("02_other_shows_subtitles/gravity_falls/subtitles_extraction/",links_all_seasons$file_name[j]), overwrite = F)
}

subtitles_srt = tibble(sub_location = list.files("02_other_shows_subtitles/gravity_falls/subtitles_extraction/", recursive = T)) %>% 
  mutate(sub_location = paste0("02_other_shows_subtitles/gravity_falls/subtitles_extraction/",sub_location),
         folder = as.character(str_extract_all(sub_location, ".*/")),
         file_size = file.size(sub_location)) %>% 
  group_by(folder) %>% 
  filter(file_size == max(file_size)) %>% 
  distinct(folder, .keep_all = T) %>% 
  mutate(episode_name = str_replace_all(sub_location, "02_other_shows_subtitles/gravity_falls/subtitles_extraction/", ""),
         episode_name = str_replace_all(episode_name, "/.*", ""),
         season = str_replace_all(substr(episode_name,1,7), "_.*", ""),
         episode = str_replace_all(substr(episode_name,1,7), ".*_", ""),
         episode_name = str_replace_all(episode_name, "S[0-9][0-9]_E[0-9][0-9]_", ""),
         episode_name = str_replace_all(episode_name, "_", " "))

for(j in 1:nrow(subtitles_srt)) {
  system(paste0("recode UTF-8 -f \"", subtitles_srt$sub_location[j], "\""))
}

for(j in 1:nrow(subtitles_srt)) {
  assign(paste(subtitles_srt$season[j],subtitles_srt$episode[j], sep = "_"),
         as_tibble(read.subtitles(subtitles_srt$sub_location[j])$subtitles) %>%
           clean_names() %>%
           rename(linenumber = id) %>% 
           filter(nchar(text) > 0) %>%
           mutate(text = iconv(tolower(text)) ,
                  season = subtitles_srt$season[j],
                  episode = subtitles_srt$episode[j],
                  episode_name = subtitles_srt$episode_name[j])
  )
}

gravity_falls_subs = mget(ls(pattern = "S[0-9][0-9]_E[0-9][0-9]")) %>% 
  bind_rows()

rm(list = ls(pattern = "S[0-9][0-9]_E[0-9][0-9]"))

fwrite(gravity_falls_subs, "02_other_shows_subtitles/gravity_falls_subs.csv")

list_subtitles_all = tibble(sub_location = list.files("02_other_shows_subtitles/gravity_falls/subtitles_extraction/", recursive = T)) %>% 
  mutate(sub_location = paste0("02_other_shows_subtitles/gravity_falls/subtitles_extraction/",sub_location),
         folder = as.character(str_extract_all(sub_location, ".*/")),
         file_size = file.size(sub_location)) 

list_subtitles_keep = list_subtitles_all %>% 
  group_by(folder) %>% 
  filter(file_size == max(file_size)) %>% 
  distinct(folder, .keep_all = T)

list_subtitles_remove = list_subtitles_all %>%   
  anti_join(list_subtitles_keep)

for(j in 1:nrow(list_subtitles_remove)) {
  try(file.remove(list_subtitles_remove$sub_location[j]))
}

##########################################################################################

# 4: scrap Archer subtitles

url_subtitles = read_html("https://www.opensubtitles.org/en/ssearch/sublanguageid-eng/idmovie-54780")

subtitles_tables = url_subtitles %>%
  html_nodes(xpath='//*[(@id = "search_results")]') %>%
  html_table(fill = TRUE)

links_2 = url_subtitles %>% 
  html_nodes("a") %>% 
  html_attr("href")

titles_2 = url_subtitles %>% 
  html_nodes("a") %>% 
  html_attr("title")

links_all_seasons = tibble(link = links_2, title = titles_2) %>% 
  filter(!is.na(title)) %>% 
  distinct(link, .keep_all = T) %>% 
  mutate(link = str_replace_all(link, "^/en/search/", "https://www.opensubtitles.org/download/s/")) %>% 
  filter(grepl("imdb", link)) %>%
  mutate(title = str_replace_all(title, "\"Archer\" ", ""),
         season = c(rep("S01",10),rep("S02",13),rep("S03",13),rep("S04",13),rep("S05",13),rep("S06",13),rep("S07",10),rep("S08",8)),
         episode = c(1:10,1:13,1:13,1:13,1:13,1:13,1:10,1:8),
         episode = if_else(nchar(episode) == 1, paste0("E0",episode), paste0("E",episode)),
         file_name = paste(season, episode, str_replace_all(title, "[[:punct:]]", ""), sep = "_"),
         file_name = str_replace_all(file_name, " ", "_")) %>% 
  select(episode, season, title, file_name, link)

try(dir.create("02_other_shows_subtitles"))
try(dir.create("02_other_shows_subtitles/archer"))
try(dir.create("02_other_shows_subtitles/archer/subtitles"))
try(dir.create("02_other_shows_subtitles/archer/subtitles_extraction"))

fwrite(links_all_seasons, "02_other_shows_subtitles/archer_links_all_seasons.csv")

for(j in 1:nrow(links_all_seasons)) {
  if(!file.exists(paste0("02_other_shows_subtitles/archer/subtitles/",links_all_seasons$file_name[j],".zip"))) {
    Sys.sleep(sample(seq(1, 3, by=0.5), 1))
    download.file(links_all_seasons$link[j], paste0("02_other_shows_subtitles/archer/subtitles/",links_all_seasons$file_name[j],".zip"), method = "wget")
  }
}

subtiles_zip = list.files("02_other_shows_subtitles/archer/subtitles/", recursive = T) %>% 
  paste0("02_other_shows_subtitles/archer/subtitles/", .)

for(j in 1:length(subtiles_zip)) {
  try(dir.create(paste0("02_other_shows_subtitles/archer/subtitles_extraction/",links_all_seasons$file_name[j])))
  unzip(subtiles_zip[[j]], exdir = paste0("02_other_shows_subtitles/archer/subtitles_extraction/",links_all_seasons$file_name[j]), overwrite = F)
}

subtitles_srt = tibble(sub_location = list.files("02_other_shows_subtitles/archer/subtitles_extraction/", recursive = T)) %>% 
  mutate(sub_location = paste0("02_other_shows_subtitles/archer/subtitles_extraction/",sub_location),
         folder = as.character(str_extract_all(sub_location, ".*/")),
         file_size = file.size(sub_location)) %>% 
  group_by(folder) %>% 
  filter(file_size == max(file_size)) %>% 
  distinct(folder, .keep_all = T) %>% 
  mutate(episode_name = str_replace_all(sub_location, "02_other_shows_subtitles/archer/subtitles_extraction/", ""),
         episode_name = str_replace_all(episode_name, "/.*", ""),
         season = str_replace_all(substr(episode_name,1,7), "_.*", ""),
         episode = str_replace_all(substr(episode_name,1,7), ".*_", ""),
         episode_name = str_replace_all(episode_name, "S[0-9][0-9]_E[0-9][0-9]_", ""),
         episode_name = str_replace_all(episode_name, "_", " "))

for(j in 1:nrow(subtitles_srt)) {
  system(paste0("recode UTF-8 -f \"", subtitles_srt$sub_location[j], "\""))
}

for(j in 1:nrow(subtitles_srt)) {
  assign(paste(subtitles_srt$season[j],subtitles_srt$episode[j], sep = "_"),
         as_tibble(read.subtitles(subtitles_srt$sub_location[j])$subtitles, encoding = "ASCII") %>%
           clean_names() %>%
           rename(linenumber = id) %>% 
           filter(nchar(text) > 0) %>%
           mutate(text = iconv(tolower(text)) ,
                  season = subtitles_srt$season[j],
                  episode = subtitles_srt$episode[j],
                  episode_name = subtitles_srt$episode_name[j])
  )
}

archer_subs = mget(ls(pattern = "S[0-9][0-9]_E[0-9][0-9]")) %>% 
  bind_rows()

rm(list = ls(pattern = "S[0-9][0-9]_E[0-9][0-9]"))

fwrite(archer_subs, "02_other_shows_subtitles/archer_subs.csv")

list_subtitles_all = tibble(sub_location = list.files("02_other_shows_subtitles/archer/subtitles_extraction/", recursive = T)) %>% 
  mutate(sub_location = paste0("02_other_shows_subtitles/archer/subtitles_extraction/",sub_location),
         folder = as.character(str_extract_all(sub_location, ".*/")),
         file_size = file.size(sub_location)) 

list_subtitles_keep = list_subtitles_all %>% 
  group_by(folder) %>% 
  filter(file_size == max(file_size)) %>% 
  distinct(folder, .keep_all = T)

list_subtitles_remove = list_subtitles_all %>%   
  anti_join(list_subtitles_keep)

for(j in 1:nrow(list_subtitles_remove)) {
  try(file.remove(list_subtitles_remove$sub_location[j]))
}


