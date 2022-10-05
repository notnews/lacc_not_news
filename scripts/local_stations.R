## Set the working directory 
setwd(githubdir)
setwd("not_news/not_news_us_dev/")

## Load the library
library("tidyverse")

## Clean the raw data set, separate networks into different columns
df_local <- read_csv("output/handcoded/local_all_raw.csv") %>% 
  filter(`Call letters` != "KVMD") %>% 
  rename(station = `Call letters`,
         location = `City and state`) %>% 
  separate(`Network(s)`, into = c("x", "y", "z", "t", "v", "k", "j", "w", "r", "a", "l", "p", "o", "b"), sep = "([0-9][.][0-9])") %>% 
  pivot_longer(cols = -c(station, location), names_to = "x1", values_to = "x2") %>% 
  mutate(x2 = str_replace_all(x2, "[:digit:]", ""),
         x2 = str_trim(x2),
         x2 = str_squish(x2),
         x2 = str_replace_all(x2, "^$", NA_character_),
         x2 = str_replace_all(x2, " ", NA_character_)) %>% 
  drop_na() %>% 
  pivot_wider(values_from = "x2", names_prefix = "st_", names_from = "x1") %>% 
  unite(network, st_y:st_b, sep = ", ") %>% 
  mutate(network = str_replace_all(network, ", NA", ""))

## Possible that in our data the -TV postfix from a lot of the stations does not have the -
df_local_tv <-
  df_local %>% 
  filter(str_detect(station, '-TV|-DT')) %>% 
  mutate(station = str_replace(station, "-TV", "TV"),
         station = str_replace(station, "-DT", "DT"))

## Removing the -TV postfix all together
df_local_notv <-
  df_local %>% 
  filter(str_detect(station, '-TV|-DT')) %>% 
  mutate(station = str_remove_all(station, "-TV"),
         station = str_remove_all(station, "-DT"))

## Bind together all different versions of the -TV data 
df_local <-
  df_local %>% 
  bind_rows(df_local_tv, df_local_notv) %>% 
  separate(location, into = c("city", "state"), sep = ", ") %>% 
  mutate(state = ifelse(is.na(state) & city %in% c("Stockton", "Anaheim"), "California", state))

## Write to file
write_csv(df_local, "output/handcoded/local_all_for_analysis.csv")

## Clean up 
rm(df_local, df_local_notv, df_local_tv)


df_network <- read_csv("output/handcoded/networks_all_raw.csv") %>% 
  filter(!is.na(name)) %>% 
  mutate(launched = str_remove_all(launched, "\\(as Pax TV\\)"),
         households_reached = str_remove_all(households_reached, "\\(OTA only\\)"),
         name = str_replace_all(name, "\\[[^\\]]*\\]\\s*", ""),
         owner = str_replace_all(owner, "\\[[^\\]]*\\]\\s*", ""),
         launched = str_replace_all(launched, "\\[[^\\]]*\\]\\s*", ""),
         households_reached = str_replace_all(households_reached, "\\[[^\\]]*\\]\\s*", ""),
         name = str_trim(name),
         year = ifelse(name == "Shop LC" & is.na(launched), "2007", launched),
         commercial = ifelse(str_detect(description, "Major commercial network"),1,0),
         public = ifelse(str_detect(description, "Public"),1,0))

# TODO add more labels to the shows 

## Write to file
write_csv(df_network, "output/handcoded/networks_all_for_analysis.csv")
