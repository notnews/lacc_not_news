#####################################################################################
##
##    File Name:        02_2_lacc.R
##    Date:             2019-05-20
##    Author:           Daniel Weitzel
##    Email:            daniel.weitzel@utexas.edu
##    Purpose:          Reduce the LACC data
##    Date Used:        2019-05-31
##    Data Used:        
##    Output File:      (none)
##    Data Output:      
##    Data Webpage:     (none)
##    Log File:         (none)
##    Notes:            
##
#####################################################################################

## Setting working directory
setwd(githubdir)
setwd("not_news/not_news_us_dev/")

## Loading libraries
library("tidyverse")
library("lubridate")
library("xtable")
library("goji")

# Custom ggplot theme
cust_theme <- theme_minimal() +
  theme(panel.grid.major = element_line(color = "#e1e1e1",  linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        legend.key       = element_blank(),
        legend.key.width = unit(1, "cm"),
        axis.title   = element_text(size = 10, color = "#555555"),
        axis.text    = element_text(size = 10, color = "#555555"),
        axis.title.x = element_text(vjust = 1, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(vjust = 1),
        axis.ticks   = element_line(color = "#e1e1e1", linetype = "dotted", size = .2),
        axis.text.x  = element_text(vjust = .3),
        plot.margin = unit(c(.5, .75, .5, .5), "cm"))

soft_news <- c("Arts", "Books", "Classifieds", "Dining", "Editorial", "Leisure", "Local", 
               "Obits", "Other", "Real Estate", "Sports", "Style", "Travel")  

hard_news <- c("Business Finance", "Foreign News", "National", "Science","Health")

	
## Script below does not need to be sourced. 
## Files have been saved and are loaded later
# source("scripts/02_1_lacc_reduction.R")

## Load the data
df_05 <- read.csv("~/Dropbox/db2/tv/reduced/df_05.csv", na.strings=c("","NA"))
df_06 <- read.csv("~/Dropbox/db2/tv/reduced/df_06.csv", na.strings=c("","NA"))
df_07 <- read.csv("~/Dropbox/db2/tv/reduced/df_07.csv", na.strings=c("","NA"))
df_08 <- read.csv("~/Dropbox/db2/tv/reduced/df_08.csv", na.strings=c("","NA"))
df_09 <- read.csv("~/Dropbox/db2/tv/reduced/df_09.csv", na.strings=c("","NA"))
df_10 <- read.csv("~/Dropbox/db2/tv/reduced/df_10.csv", na.strings=c("","NA"))
df_11 <- read.csv("~/Dropbox/db2/tv/reduced/df_11.csv", na.strings=c("","NA"))
df_12 <- read.csv("~/Dropbox/db2/tv/reduced/df_12.csv", na.strings=c("","NA"))

df_lacc <- rbind(df_05, df_06, df_07, df_08, df_09, df_10, df_11, df_12)
rm(df_05, df_06, df_07, df_08, df_09, df_10, df_11, df_12)

## Load the prediction data
df_lacc_pred  <- read.csv("output/predictions/lacc_nyt_pred.csv")

## Summary Statistics of the LACC data set
df_lacc <- 
  df_lacc %>% 
  mutate(datetime = substring(x3, 1, 15),
         x3 = substring(x3, 17)) %>% 
  separate(datetime, into = c("dates", "time"), sep = "_") %>% 
  separate(x3, into = c("network", "drop"), sep = "_", remove = FALSE) %>% 
  select(-drop) %>% 
  mutate(x3 = str_replace_all(x3, network, ""), 
         x3 = as.character(x3),
         x3 = str_replace_all(x3, "_", " "),
         x3 = gsub("(^\\s+)|(\\s+$)", "", x3),
         x3 = sub("^\\s+", "", x3),
         x3 = gsub('[\n\r]',' ',x3),
         x3 = base::trimws(x3, which = "left"),
         x3 = base::trimws(x3, which = "right"),
         network = ifelse(network == "KTTV-FOX", "KTTV", network),
         #network = ifelse(network == "CSPAN2", "CSPAN", network),
         x3 = ifelse(x3 == "20-20 Xmas at the White House Barbara Walters Interviews the Obamas", 
                     "20-20", x3),
         x3 = gsub(("1 of 7|2 of 7|3 of 7|4 of 7|5 of 7|6 of 7|7 of 7|1 of 6|2 of 6|3 of 6|
                    4 of 6|5 of 6|6 of 6|1 of 5|2 of 5|3 of 5|4 of 5|5 of 5|1 of 4|2 of 4|
                    3 of 4|4 of 4|1 of 3|2 of 3|3 of 3|1 of 2|2 of 2|Pt 2"), replacement = "", x = x3),
         x3 = gsub(("Midterm Election Night Coverage 1|Midterm Election Night Coverage 2|
                    Midterm Election Night Coverage 3|Midterm Election Night Coverage 4|
                    Midterm Election Night Coverage 5|Midterm Election Night Coverage 6|
                    Midterm Election Night Coverage 7|Midterm Election Night Coverage 8|
                    Midterm Election Night Coverage 9|Midterm Election Night Coverage 10"), 
                   replacement = "Midterm Election Night Coverage", x = x3),
         x3 = gsub(("Anderson Cooper Extra Edition 6PM"), replacement = "Anderson Cooper Extra Edition", x3),
         x3 = gsub(("The Chris Matthews Show 0|The Chris Matthews Show 10|The Chris Matthews Show 20"), 
                   replacement = "The Chris Matthews Show", x3),
         debate = grepl("debate|convention", x3, ignore.case = TRUE),
         caucus = grepl("caucus", x3, ignore.case = TRUE),
         primaries = grepl("primaries|primary", x3, ignore.case = TRUE),
         delegates = grepl("delegates", x3, ignore.case = TRUE),
         qtime = grepl("Prime Minister Question Time|state of the union", x3, ignore.case = TRUE),
         hearing = grepl("earing", x3, ignore.case = TRUE),
         campaign = grepl("campaign|Fox News Election Coverage 2008", x3, ignore.case = TRUE),
         larry = grepl("larry", x3, ignore.case = TRUE),
         rtwh = grepl("Road to", x3, ignore.case = TRUE),
         pmt = grepl("Piers", x3, ignore.case = TRUE),
         confirmation = grepl("confirmation", x3, ignore.case = TRUE),
         ch4 = grepl("Channel", x3, ignore.case = TRUE),
         x3 = ifelse(ch4 == TRUE, "Channel 4 News", x3),
         x3 = ifelse(larry == TRUE, "Larry King", x3),
         x3 = ifelse(rtwh == TRUE, "Road to White House", x3),
         x3 = ifelse(pmt == TRUE, "Piers Morgan Tonight", x3),
         x3 = ifelse(debate == TRUE, "Debate/Caucus/Primary/Hearing/Campaign", 
                     ifelse(caucus == TRUE, "Debate/Caucus/Primary/Hearing/Campaign",
                            ifelse(primaries == TRUE, "Debate/Caucus/Primary/Hearing/Campaign",
                                   ifelse(delegates == TRUE, "Debate/Caucus/Primary/Hearing/Campaign",
                                          ifelse(qtime == TRUE, "Debate/Caucus/Primary/Hearing/Campaign",
                                                 ifelse(hearing == TRUE, "Debate/Caucus/Primary/Hearing/Campaign", 
                                                        ifelse(campaign == TRUE, "Debate/Caucus/Primary/Hearing/Campaign",
                                                               ifelse(confirmation == TRUE,"Debate/Caucus/Primary/Hearing/Campaign",
                                                                      x3)))))))),
         x3 = gsub(("1 am|1 AM|1am|1AM|2 am|2 AM|2am|2AM|3 am|3 AM|3am|3AM|4 am|4 AM|4am|4AM|
                      5 am|5 AM|5am|5AM|6 am|6 AM|6am|6AM|7 am|7 AM|7am|7AM|8 am|8 AM|8am|8AM|
                      9 am|9 AM|9am|9AM|10 am|10 AM|10am|10AM|11 am|11 AM|11am|11AM|12 pm|12 PM|
                      12pm|12PM|noon|Noon|NOON|1 am|at 1 AM|at 1am|at 1AM|at 2 am|at 2 AM|
                      at 2am|at 2AM|at 3 am|at 3 AM|at 3am|at 3AM|at 4 am|at 4 AM|at 4am|
                      at 4AM|at 5 am|at 5 AM|at 5am|at 5AM|at 6 am|at 6 AM|at 6am|at 6AM|
                      at 7 am|at 7 AM|at 7am|at 7AM|at 8 am|at 8 AM|at 8am|at 8AM|at 9 am|
                      at 9 AM|at 9am|at 9AM|at 10 am|at 10 AM|at 10am|at 10AM|at 11 am|
                      at 11 AM|at 11am|at 11AM|at 12 pm|at 12 PM|at 12pm|at 12PM|at noon|
                     at Noon|at NOON"), replacement = "", x3),
         x3 = gsub(("1 pm|1 PM|1pm|1PM|2 pm|2 PM|2pm|2PM|3 pm|3 PM|3pm|3PM|4 pm|4 PM|4pm|4PM|
                      5 pm|5 PM|5pm|5PM|afternoon|AFTERNOON"), replacement = "", x3),
         x3 = gsub(("6 pm|6 PM|6pm|6PM|7 pm|7 PM|7pm|7PM|8 pm|8 PM|8pm|8PM|
                    9 pm|9 PM|9pm|9PM|10 pm|10 PM|10pm|10PM|11 pm|11 PM|
                   11pm|11PM|12 am|12 AM|12am|12AM|midnight|MIDNIGHT"), replacement = "", x3),
         x3 = gsub(("Eyewitness News 1|Eyewitness News 430PM|Eyewitness News "), replacement = "Eyewitness News", x3),
         x3 = gsub(("Morning News at 6"), replacement = "Morning News", x3),
         x3 = gsub(("9 News at"), replacement = "9 News", x3),
         x3 = gsub(("CBS 2 News at 6|CBS 2 News at 5|CBS 2 News at 11|CBS 2 News at 1"), replacement = "CBS News", x3),
         x3 = gsub(("Morning News at 5|Morning News at 7"), replacement = "Morning News", x3),
         x3 = gsub(("Prime News at"), replacement = "Prime News", x3),
         x3 = gsub(("ABC World News Saturday|ABC World News Saturday with David Muir|ABC World News Sunday"), 
                   replacement = "ABC World News", x3),
         x3 = gsub(("1 News|4 News at"), replacement = "News", x3),
         x3 = gsub(("Eyewitness News 430PM|Eyewitness News Sat|Eyewitness Newsmakers"), replacement = "Eyewitness News", x3),
         x3 = gsub(("In The Arena"), replacement = "In the Arena", x3),
         x3 = gsub(("Primetime"), replacement = "Prime Time", x3),
         x3 = gsub(("World News With Charles Gibson|World News With Diane Sawyer"), replacement = "World News", x3),
         x3 = gsub(("On the Record Greta Van Susteren|On the Record with Greta Van Susteren"), replacement = "On the Record", x3),
         x3 = gsub(("NBC Nightly News Saturday|NBC Nightly News Sunday|NBC Nightly News Weekend"), replacement = "NBC Nightly News", x3),
         x3 = gsub(("Good Morning America Sunday"), replacement = "Good Morning America", x3),
         x3 = gsub(("Noticiero Univision Fin de Semana|Noticiero Univision Ultima Hora"), replacement = "Noticiero Univision", x3),
         x3 = gsub(("Larry King Saturday|Larry King Sunday|Larry King Live"), replacement = "Larry King", x3),
         x3 = gsub(("This Week with George Stephanopoulos"), replacement = "This Week", x3),
         x3 = gsub(("News Live|News Saturday|News Sunday"), replacement = "News", x3),
         network = gsub(("Current"), replacement = "MSNBC", network),
         network = gsub(("KCET"), replacement = "KOCE", network),
         network = gsub(("CNN-Headline"), replacement = "CNN", network),
         x3 = gsub(("Le Journal de France 2|Le Journal de la RTBF|Le Journal de la TSR|Le Journal de Radio-Canada"), 
                   replacement = "Le Journal", x3),
         x3 = gsub(("Special Report with Bret Baier|Special Report with Brit Hume"), replacement = "Special Report", x3),
         x3 = gsub(("Countdown With Keith Olbermann"), replacement = "Countdown with Keith Olbermann", x3),
         x3 = gsub(("CBS Evening News|CBS News Morning"), replacement = "CBS News", x3),
         x3 = gsub(("Morning News "), replacement = "Morning News", x3),
         x3 = gsub(("7 News at 1"), replacement = "7 News", x3))%>% 
  select(-c(debate, caucus, primaries, delegates, ch4)) %>% 
  filter(network != "TV5") %>% 
  filter(network != "KMEX") %>% 
  filter(network != "Presidential") %>% 
  filter(network != "Court")


df_lacc$dates = lubridate::ymd(df_lacc$dates)

## Summary Stats of the data set
df_lacc_unique <-
  df_lacc %>% 
  mutate(x3 = ifelse(x3 == "", NA, x3)) %>% 
  filter(!is.na(x3)) %>% 
  group_by(x3, network) %>% 
  add_count(x3, network) %>% 
  ungroup() %>% 
  mutate(x3 = ifelse(n < 10, "Other", x3),
         network = str_replace(network, "FOX-News", "Fox News"),
         x3 = str_replace(x3, "The OReilly Factor", "The O\'Reilly Factor"),
         x3 = str_replace(x3, "Tonight Show with Conan OBrien", "Tonight Show with Conan O\'Brien"),
         x3 = str_replace(x3, "Ten OClock News", "Ten O\'Clock News"),
         x3 = str_replace(x3, "The Last Word With Lawrence ODonnell", "The Last Word With Lawrence O\'Donnell"),
         x3 = str_replace(x3, "Late Night with Conan OBrien", "Late Night with Conan O\'Brien")) %>% 
  select(-n) %>% 
  group_by(x3, network) %>% 
  add_count(x3, network) %>% 
  mutate(min_date = min(dates),
         max_date = max(dates)) %>% 
  select(x3, network, min_date, max_date, n) %>% 
  group_by(x3, network, min_date, max_date, n) %>% 
  rename(program = x3,
         count = n) %>% 
  unique() %>% 
  ungroup() %>% 
  arrange(network,program) %>% 
  filter(count > 99) %>% 
  select(network, program, min_date, max_date, count)  %>% 
  separate(min_date, into = c("year", "month", "day")) %>% 
  select(-day) %>% 
  unite("min_date", c("year", "month"), sep = "-") %>% 
  separate(max_date, into = c("year", "month", "day")) %>% 
  select(-day) %>% 
  unite("max_date", c("year", "month"), sep = "-") %>% 
  rename("Channel" = network, 
         "Program" = program, 
         "Start Date" = min_date, 
         "End Date" = max_date, 
         "Total" = count)


print(xtable(df_lacc_unique, type = "latex", tabular.environment= "longtable",
             caption = "LACC - Summary Statistics",
             label = "tab:lacc_program_summary"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/02_2_lacc_summary.tex")

rm(df_lacc_unique)

## Summary on the show level
df_lacc_show <-
  df_lacc %>% 
  select(network, x3, dates) %>% 
  rename(show = x3) %>% 
  group_by(network, show) %>% 
  add_count(network, show) %>% 
  mutate(min_date = min(dates),
         max_date = max(dates)) %>% 
  select(network, show, min_date, max_date) %>% 
  add_count(network, show) %>% 
  rename(count = n) %>% 
  unique() %>% 
  ungroup() %>% 
  arrange(network, show) %>% 
  filter(count > 99) %>% 
  mutate(network = str_replace(network, "FOX-", "Fox "),
         network = str_replace(network, "ComedyCentral", "Comedy Central")) %>% 
  separate(min_date, into = c("year", "month", "day")) %>% 
  select(-day) %>% 
  unite("min_date", c("year", "month"), sep = "-") %>% 
  separate(max_date, into = c("year", "month", "day")) %>% 
  select(-day) %>% 
  unite("max_date", c("year", "month"), sep = "-") %>% 
  rename("Channel" = network, 
         "Show" = show, 
         "Start Date" = min_date, 
         "End Date" = max_date, 
         "Total" = count)


print(xtable(df_lacc_show, type = "latex", tabular.environment= "longtable",
             caption = "LACC - Show Summary Statistics",
             label = "tab:lacc_show"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/02_4_lacc_summary_show.tex")

rm(df_lacc_show)

## Summary on the network level
df_lacc_network <-
  df_lacc %>% 
  select(network, dates) %>% 
  group_by(network) %>% 
  add_count(network) %>% 
  mutate(min_date = min(dates),
         max_date = max(dates)) %>% 
  select(network, min_date, max_date) %>% 
  add_count(network) %>% 
  rename(count = n) %>% 
  unique() %>% 
  ungroup() %>% 
  arrange(network) %>% 
  filter(count > 99) %>% 
  mutate(network = str_replace(network, "FOX-", "Fox "),
         network = str_replace(network, "ComedyCentral", "Comedy Central")) %>% 
  separate(min_date, into = c("year", "month", "day")) %>% 
  select(-day) %>% 
  unite("min_date", c("year", "month"), sep = "-") %>% 
  separate(max_date, into = c("year", "month", "day")) %>% 
  select(-day) %>% 
  unite("max_date", c("year", "month"), sep = "-") %>% 
  rename("Channel" = network, 
         "Start Date" = min_date, 
         "End Date" = max_date, 
         "Total" = count)

print(xtable(df_lacc_network, type = "latex", tabular.environment= "longtable",
             caption = "LACC - Channel Summary Statistics",
             label = "tab:lacc_channels"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/02_3_lacc_summary_channels.tex")

rm(df_lacc_network)

## PREDICTION

## Share of news type by channe;
lacc_pred_channel <-
  df_lacc_pred  %>%
  select(c(channel.name, label_name)) %>% 
  rename(news_source = channel.name) %>% 
  filter(news_source != "TV5") %>% 
  filter(news_source!= "KMEX") %>% 
  filter(news_source != "Presidential") %>% 
  filter(news_source != "Court") %>% 
  filter(news_source != "Current") %>% 
  mutate(news_source = str_replace(news_source, "KCET", "KOCE"),
         news_source = str_replace(news_source, "FOX-", "Fox "),
         news_source = str_replace(news_source, "KTTV-FOX", "KTTV"),
         news_source = str_replace(news_source, "-Headline", ""),
         #news_source = str_replace(news_source, "CSPAN2", "CSPAN"),
         news_source = str_replace(news_source, "ComedyCentral", "Comedy Central")) %>% 
  add_count(news_source) %>% 
  rename(total = n) %>% 
  add_count(news_source, label_name) %>% 
  rename(label = n) %>% 
  mutate(news_source = trimws(news_source, which = c("left"))) %>% 
  unique() %>% 
  mutate(percent  = label/total,
         percent = round(percent,2)) %>% 
  select(-label) %>% 
  spread(key = label_name, value = percent) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  rename("Channel" = news_source,
         "Total" = total,
         "Business" = "Business Finance", 
         "Foreign" = "Foreign News") %>% 
  filter(Total > 99) 



print(xtable(lacc_pred_channel, type = "latex", tabular.environment= "longtable",
             caption = "LACC Prediction - Share of News Types by Channel",
             label = "tab:lacc_predict_channel"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/02_4_lacc_pred_summary_channel.tex")

rm(lacc_pred_channel)


## LACC Prediction - Share of News Types by Show

df_lacc_pred_show <-
  df_lacc_pred %>%
  select(c(channel.name, program.name, label_name)) %>% 
  rename(news_source = channel.name,
         show = program.name) %>% 
  filter(news_source != "TV5") %>% 
  filter(news_source!= "KMEX") %>% 
  filter(news_source != "Presidential") %>% 
  filter(news_source != "Court") %>% 
  filter(news_source != "Current") %>% 
  mutate(news_source = str_replace(news_source, "KCET", "KOCE"),
         news_source = str_replace(news_source, "FOX-", "Fox "),
         news_source = str_replace(news_source, "KTTV-FOX", "KTTV"),
         news_source = str_replace(news_source, "-Headline", ""),
         #news_source = str_replace(news_source, "CSPAN2", "CSPAN"),
         news_source = str_replace(news_source, "ComedyCentral", "Comedy Central")) %>% 
  add_count(news_source, show) %>% 
  rename(total = n) %>% 
  add_count(news_source, show, label_name) %>% 
  rename(label = n) %>% 
  mutate(news_source = str_squish(news_source)) %>% 
  unique() %>% 
  mutate(percent  = label/total,
         percent = round(percent,2)) %>% 
  select(-label) %>% 
  spread(key = label_name, value = percent) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  rename("Channel" = news_source,
         "Show" = show,
         "Total" = total,
         "Business" = "Business Finance", 
         "Foreign" = "Foreign News") %>% 
  filter(Total > 99) 


print(xtable(df_lacc_pred_show, type = "latex", tabular.environment= "longtable",
             caption = "LACC Prediction - Share of News Types by Channel",
             label = "tab:lacc_predict_show"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/02_4_lacc_pred_summary_show.tex")

rm(df_lacc_pred_show)


## LACC Prediction - Hard vs Soft news
df_lacc_pred_hardsoft <-
  df_lacc_pred %>%
  select(c(channel.name, label_name)) %>% 
  rename(news_source = channel.name) %>% 
  filter(news_source != "TV5") %>% 
  filter(news_source!= "KMEX") %>% 
  filter(news_source != "Presidential") %>% 
  filter(news_source != "Court") %>% 
  filter(news_source != "Current") %>% 
  mutate(news_source = str_replace(news_source, "KCET", "KOCE"),
         news_source = str_replace(news_source, "FOX-", "Fox "),
         news_source = str_replace(news_source, "KTTV-FOX", "KTTV"),
         news_source = str_replace(news_source, "-Headline", ""),
         #news_source = str_replace(news_source, "CSPAN2", "CSPAN"),
         news_source = str_replace(news_source, "ComedyCentral", "Comedy Central")) %>% 
  add_count(news_source) %>% 
  rename(total = n) %>% 
  mutate(label_name = ifelse(label_name == "Business Finance" | label_name == "Foreign News" |
                               label_name == "National" | label_name == "Science" | label_name == "Health",
                             "Hard", "Soft")) %>% 
  add_count(news_source, label_name) %>% 
  rename(label = n) %>% 
  unique() %>% 
  mutate(percent  = label/total,
         percent = round(percent,2)) %>% 
  select(-label) %>% 
  spread(key = label_name, value = percent) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  rename(Source = news_source,
         Total = total) %>% 
  filter(Total > 99) %>% 
  mutate(Source = trimws(Source, which = c("left"))) %>% 
  arrange(Source)


print(xtable(df_lacc_pred_hardsoft, type = "latex", tabular.environment= "longtable",
             caption = "LACC Prediction - Share of Hard and Soft News",
             label = "tab:lacc_predict_hard_soft"),
      caption.placement = "top",
      include.rownames=FALSE,
      file = "tabs/02_5_lacc_pred_hard_soft.tex")

rm(df_lacc_pred_hardsoft)


df_lacc_pred  <- 
  df_lacc_pred  %>% 
  select(-c(uid, month, date)) %>% 
  rename(news_source = channel.name) %>% 
  filter(news_source != "TV5") %>% 
  filter(news_source!= "KMEX") %>% 
  filter(news_source != "Presidential") %>% 
  filter(news_source != "Court") %>% 
  filter(news_source != "Current") %>% 
  mutate(news_source = str_replace(news_source, "KCET", "KOCE"),
         news_source = str_replace(news_source, "FOX-", "Fox "),
         news_source = str_replace(news_source, "KTTV-FOX", "KTTV"),
         news_source = str_replace(news_source, "-Headline", ""),
         #news_source = str_replace(news_source, "CSPAN2", "CSPAN"),
         news_source = str_replace(news_source, "ComedyCentral", "Comedy Central")) %>% 
  add_count(news_source) %>% 
  rename(total = n) %>% 
  add_count(news_source, year) %>% 
  rename(total_yearly = n)

# lacc_pred_sum <-
#    lacc_pred %>%
#    select(-c(total, label_name, program.name)) %>%
#    group_by(news_source) %>%
#    summarise_all(mean)
# write_csv(lacc_pred_sum, "~/Dropbox/Research/Repos/not_news/not_news_us_dev/output/lacc_pred_label_means.csv")

df_lacc_pred  <- 
  df_lacc_pred  %>%  
  mutate(pred_soft = rowSums(select(., one_of(soft_news))),
         pred_hard = rowSums(select(., one_of(hard_news))),
         pred = pred_soft + pred_hard) %>% 
  select(news_source, year, label_name, pred_soft, pred_hard, pred, total)

main_outlets <- c("CNBC",
                  "KCBS", 
                  "CSPAN",
                  "MSNBC",
                  "Fox News",
                  "CNN")

lacc_pred_yearly <- 
  df_lacc_pred %>%
  select(news_source, year, pred_soft, pred_hard) %>% 
  #, pred_hard
  group_by(news_source, year) %>%
  summarize(mean = mean(pred_soft),
            sd   = sd(pred_soft),
            mean_p2sd = mean + 2 * sd,
            mean_m2sd = mean - 2 * sd) %>%
  ungroup() %>% 
  filter(news_source %in% main_outlets)

lacc_pred_by_outlet <- 
  df_lacc_pred %>% 
  group_by(news_source) %>%
  summarize(share_of_not_news = mean(pred_soft))

avg_outlet_prop <- mean(lacc_pred_by_outlet$share_of_not_news, na.rm = T)

lcols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
linetype = c("dashed", "dotted", "dotdash", "longdash", "twodash", "4C88C488", "12345678")

ggplot(lacc_pred_yearly, aes(x = year, y= mean, group = news_source)) + 
  geom_line(aes(color = news_source)) + 
  #geom_line(aes(color = news_source, linetype = news_source)) + 
  scale_color_manual(name = "News Source", values = lcols) +
  #scale_linetype_manual(name = "News Source", values = linetype) +
  geom_hline(yintercept= avg_outlet_prop, color = "black", linetype = "dashed") + 
  ylim(0,1) +
  theme_minimal() + xlab("Year") + ylab("Share of Soft News in an Outlet")

ggsave("figs/lacc_soft_time.pdf")

## Dot plot
lacc_pred_by_outlet$news_source  <- droplevels(factor(lacc_pred_by_outlet$news_source))
lacc_pred_by_outlet$news_source = factor(lacc_pred_by_outlet$news_source, levels = lacc_pred_by_outlet$news_source[order(lacc_pred_by_outlet$share_of_not_news)])
#write_csv(lacc_pred_by_outlet, "output/lacc_pred_by_outlet.csv")

main_outlets <- c("ESPN", 
                  "HBO", 
                  "CNBC",
                  "KCBS", 
                  "KNBC",
                  "CSPAN",
                  "MSNBC",
                  "Fox News",
                  "CNN",
                  "Discovery",
                  "KTTV")

lacc_pred_by_outlet_main = subset(lacc_pred_by_outlet, news_source %in% main_outlets)

ggplot(lacc_pred_by_outlet_main , aes(share_of_not_news, news_source, order = share_of_not_news,)) +
  geom_point(aes(alpha = .8)) +
  ylab("") +
  xlab("") +
  scale_colour_manual(values = c("#dd3333", "#3333dd")) +
  scale_x_continuous("Proportion of Soft News Stories", breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1))) +
  cust_theme +
  theme(legend.position = "none")

ggsave("figs/lacc_pred_dotplot.pdf")

## Density Plot
ggplot(lacc_pred_by_outlet, aes(share_of_not_news)) +
  geom_density(aes(y = ..scaled..), color = "#42C4C7", alpha = 0.35) +
  annotate("text", label = paste("Mean =", nolead0s(round(avg_outlet_prop, 2))), x = .54, y = 0.85, size = 3, colour = "black") + 
  geom_vline(xintercept = mean(lacc_pred_by_outlet$share_of_not_news, na.rm = T), col = "#cc0000", alpha = .75, linetype = "dotted") + 
  xlab("Share of Soft News in an Outlet") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  scale_x_continuous(breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  cust_theme 

ggsave("figs/lacc_pred_density_outlet.pdf")


ggplot(df_lacc_pred, aes(pred_soft)) +
  geom_density(aes(y = ..scaled..), color = "#42C4C7", alpha = 0.35) +
  annotate("text", label = paste("Mean =", nolead0s(round(mean(df_lacc_pred$pred_soft), 2))), x = .50, y = 1, size = 3, colour = "black") + 
  geom_vline(xintercept = mean(df_lacc_pred$pred_soft, na.rm = T), col = "#cc0000", alpha = .75, linetype = "dotted") + 
  xlab("Share of Soft News in an Article") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  scale_x_continuous(breaks = seq(0, 1, .1), labels = nolead0s(seq(0, 1, .1)), expand = c(.03, .03)) +
  cust_theme 

ggsave("figs/lacc_pred_density_article.pdf")




