url_subtitles = read_html("https://www.opensubtitles.org/en/ssearch/sublanguageid-eng/idmovie-464588")

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
  mutate(title = str_replace_all(title, "\"Stranger Things\" ", ""),
         title = str_replace_all(title, ".*\\: ", ""),
         season = c(rep("S01",8),rep("S02",9)),
         episode = c(1:8,1:9),
         episode = if_else(nchar(episode) == 1, paste0("E0",episode), paste0("E",episode)),
         file_name = paste(season, episode, str_replace_all(title, "[[:punct:]]", ""), sep = "_"),
         file_name = str_replace_all(file_name, " ", "_")) %>% 
  select(episode, season, title, file_name, link)

try(dir.create("02_other_shows_subtitles"))
try(dir.create("02_other_shows_subtitles/stranger_things"))
try(dir.create("02_other_shows_subtitles/stranger_things/subtitles"))
try(dir.create("02_other_shows_subtitles/stranger_things/subtitles_extraction"))

fwrite(links_all_seasons, "02_other_shows_subtitles/stranger_things_links_all_seasons.csv")

for(j in 1:nrow(links_all_seasons)) {
  if(!file.exists(paste0("02_other_shows_subtitles/stranger_things/subtitles/",links_all_seasons$file_name[j],".zip"))) {
    Sys.sleep(sample(seq(1, 3, by=0.5), 1))
    download.file(links_all_seasons$link[j], paste0("02_other_shows_subtitles/stranger_things/subtitles/",links_all_seasons$file_name[j],".zip"), method = "wget")
  }
}

subtiles_zip = list.files("02_other_shows_subtitles/stranger_things/subtitles/", recursive = T) %>% 
  paste0("02_other_shows_subtitles/stranger_things/subtitles/", .)

for(j in 1:length(subtiles_zip)) {
  try(dir.create(paste0("02_other_shows_subtitles/stranger_things/subtitles_extraction/",links_all_seasons$file_name[j])))
  unzip(subtiles_zip[[j]], exdir = paste0("02_other_shows_subtitles/stranger_things/subtitles_extraction/",links_all_seasons$file_name[j]), overwrite = F)
}

# I deleted Gravity.Falls.S01E17.Boyz.Crazy.720p-Strajow.srt bc of broken encoding

subtitles_srt = tibble(sub_location = list.files("02_other_shows_subtitles/stranger_things/subtitles_extraction/", recursive = T)) %>% 
  mutate(sub_location = paste0("02_other_shows_subtitles/stranger_things/subtitles_extraction/",sub_location),
         folder = as.character(str_extract_all(sub_location, ".*/")),
         file_size = file.size(sub_location)) %>% 
  group_by(folder) %>% 
  filter(file_size == max(file_size)) %>% 
  distinct(folder, .keep_all = T) %>% 
  mutate(episode_name = str_replace_all(sub_location, "02_other_shows_subtitles/stranger_things/subtitles_extraction/", ""),
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

stranger_things_subs = mget(ls(pattern = "S[0-9][0-9]_E[0-9][0-9]")) %>% 
  bind_rows()

rm(list = ls(pattern = "S[0-9][0-9]_E[0-9][0-9]"))

fwrite(stranger_things_subs, "02_other_shows_subtitles/stranger_things_subs.csv")

list_subtitles_all = tibble(sub_location = list.files("02_other_shows_subtitles/stranger_things/subtitles_extraction/", recursive = T)) %>% 
  mutate(sub_location = paste0("02_other_shows_subtitles/stranger_things/subtitles_extraction/",sub_location),
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
