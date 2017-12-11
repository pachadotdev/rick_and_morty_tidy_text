# 1: load packages

if (!require("pacman")) install.packages("pacman")
p_load(data.table,dplyr,stringr,rvest,janitor)
p_load_gh("fkeck/subtools")

##########################################################################################

# 2: scrap subtitles

url_subtitles = read_html("https://www.opensubtitles.org/en/ssearch/sublanguageid-eng/idmovie-167129")

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
  mutate(title = str_replace_all(title, "\"Rick and Morty\" ", ""),
         season = c(rep("S01",11),rep("S02",10),rep("S03",10)),
         episode = c(1:11,1:10,1:10),
         episode = if_else(nchar(episode) == 1, paste0("E0",episode), paste0("E",episode)),
         file_name = paste(season, episode, str_replace_all(title, "[[:punct:]]", ""), sep = "_"),
         file_name = str_replace_all(file_name, " ", "_")) %>% 
  select(episode, season, title, file_name, link)

try(dir.create("01_rick_and_morty"))

fwrite(links_all_seasons, "01_rick_and_morty/rick_and_morty_links_all_seasons.csv")

##########################################################################################

# 3: save subtitles

try(dir.create("01_rick_and_morty/subtitles"))

for(j in 1:nrow(links_all_seasons)) {
  if(!file.exists(paste0("01_rick_and_morty/subtitles/",links_all_seasons$file_name[j],".zip"))) {
    Sys.sleep(sample(seq(0.5, 1.5, by=0.5), 1))
    download.file(links_all_seasons$link[j], paste0("01_rick_and_morty/subtitles/",links_all_seasons$file_name[j],".zip"), method = "wget")
  }
}

subtiles_zip = list.files("01_rick_and_morty/subtitles/", recursive = T) %>% 
  paste0("01_rick_and_morty/subtitles/", .)

try(dir.create("01_rick_and_morty/subtitles_extraction/"))

for(j in 1:length(subtiles_zip)) {
  try(dir.create(paste0("01_rick_and_morty/subtitles_extraction/",links_all_seasons$file_name[j])))
  unzip(subtiles_zip[[j]], exdir = paste0("01_rick_and_morty/subtitles_extraction/",links_all_seasons$file_name[j]), overwrite = F)
}

subtitles_srt = tibble(sub_location = list.files("01_rick_and_morty/subtitles_extraction/", recursive = T)) %>% 
  mutate(sub_location = paste0("01_rick_and_morty/subtitles_extraction/",sub_location),
         folder = as.character(str_extract_all(sub_location, ".*/")),
         file_size = file.size(sub_location)) %>% 
  group_by(folder) %>% 
  filter(file_size == max(file_size)) %>% 
  distinct(folder, .keep_all = T) %>% 
  mutate(episode_name = str_replace_all(sub_location, "01_rick_and_morty/subtitles_extraction/", ""),
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

rick_and_morty_subs = mget(ls(pattern = "S[0-9][0-9]_E[0-9][0-9]")) %>% 
  bind_rows()

rm(list = ls(pattern = "S[0-9][0-9]_E[0-9][0-9]"))

fwrite(rick_and_morty_subs, "01_rick_and_morty/rick_and_morty_subs.csv")

list_subtitles_all = tibble(sub_location = list.files("01_rick_and_morty/subtitles_extraction/", recursive = T)) %>% 
  mutate(sub_location = paste0("01_rick_and_morty/subtitles_extraction/",sub_location),
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

# 4: scrap transcripts

# url_transcripts = read_html("https://en.wikipedia.org/wiki/List_of_Rick_and_Morty_episodes")
# 
# episodes_tables = url_transcripts %>%
#   html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "wikiepisodetable", " " ))]') %>%
#   html_table()
# 
# for(j in 1:3) {
#   assign(paste("season",j, sep = "_"), as_tibble(episodes_tables[[j]]) %>% 
#            clean_names() %>%
#            mutate(title = str_replace_all(title, "\"", ""),
#                   title = str_replace_all(title, "[^[:alnum:][:blank:]+?&/\\-]", ""))
#   )  
# }
# 
# try(dir.create("01_rick_and_morty"))
# 
# url_transcripts_2 = read_html("http://rickandmorty.wikia.com/wiki/List_of_episodes") 
# 
# tables = url_transcripts_2 %>% 
#   html_nodes(xpath = '//table')
# 
# links = url_transcripts_2 %>% 
#   html_nodes("a") %>% 
#   html_attr("href")
# 
# titles = url_transcripts_2 %>% 
#   html_nodes("a") %>% 
#   html_attr("title")
# 
# links_titles = tibble(link = links, title = titles) %>% 
#   filter(!is.na(title)) %>% 
#   distinct(link, .keep_all = T) %>% 
#   mutate(link = paste0("http://rickandmorty.wikia.com",link),
#          link = paste0(link,"/Transcript")) %>% 
#   mutate(title_low = tolower(title)) %>% 
#   distinct(title_low, .keep_all = T) %>% 
#   mutate(title_10 = substr(title,1,10)) %>% 
#   select(-title_low,-title) 
# 
# for(j in 1:3) {
#   season_n = tables[[j+1]] %>% 
#     html_table() %>%
#     as_tibble() %>% 
#     clean_names() %>% 
#     rename(cum_no = x) %>% 
#     mutate(title = gsub("\"", "", title))
#   
#   assign(paste("season",j, sep = "_"), season_n)
#   
#   assign(paste("links_season",j, sep = "_"), season_n %>% 
#            select(no, title) %>% 
#            rename(episode = no) %>% 
#            mutate(episode = ifelse(nchar(episode) == 1, paste0("E0",episode), paste0("E",episode)),
#                   season = paste0("S0",j),
#                   file_name = paste(season, episode, str_replace_all(title, "[[:punct:]]", ""), sep = "_"),
#                   file_name = str_replace_all(file_name, " ", "_")) %>% 
#            mutate(title_10 = substr(title,1,10)) %>% 
#            left_join(links_titles, by = "title_10") %>% 
#            select(episode,season,title,everything()) %>% 
#            select(-title_10)
#   )
#   
#   rm(season_n)
# }
# 
# links_all_seasons = bind_rows(links_season_1, links_season_2, links_season_3) %>% 
#   mutate(link = str_replace_all(link, "Episode", "episode"))

##########################################################################################

# 5: save transcripts

# try(dir.create("transcripts"))
# 
# for(j in 1:nrow(links_all_seasons)) {
#   if(!file.exists(paste0("transcripts/",links_all_seasons$file_name[j]))) {
#     read_html(links_all_seasons$link[j]) %>% 
#       html_nodes(xpath = '//*[(@id = "mw-content-text")] | //p') %>% 
#       html_text() %>% 
#       write_lines(path = paste0("transcripts/",links_all_seasons$file_name[j]))
#   }
# }
