#####################################################################################
##
##    File Name:        02_1_lacc_reduction.R
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

## Load Libraries
library(tidyverse)
library(rio)
library(httr)
  
## 2005 
lof_05 <- list.files(path = "~/Dropbox/db2/tv/2005", 
                     recursive = TRUE,
                     pattern = "\\.txt$", 
                     full.names = TRUE)

lof_05 <- lof_05[sapply(lof_05, file.size) > 10]

df_05 <- lof_05 %>%
  purrr::map_df(read_table, 
                col_names = FALSE) %>% 
  filter(str_detect(X1, "TOP\\|")) %>% 
  tidyr::separate(X1, into = c("x1", "x2", "x3"), sep = "\\|") %>% 
  select(x3)

export(df_05, "~/Dropbox/db2/tv/recuced/df_05.csv")
rm(lof_05, df_05)

## 2006 
lof_06 <- list.files(path = "~/Dropbox/db2/tv/2006", 
                     recursive = TRUE,
                     pattern = "\\.txt$", 
                     full.names = TRUE)

lof_06 <- lof_06[sapply(lof_06, file.size) > 10]


df_06 <- lof_06 %>%
  purrr::map_df(read_table, 
                col_names = FALSE) %>% 
  filter(str_detect(X1, "TOP\\|")) %>% 
  tidyr::separate(X1, into = c("x1", "x2", "x3"), sep = "\\|") %>% 
  select(x3)

export(df_06, "~/Dropbox/db2/tv/recuced/df_06.csv")
rm(lof_06, df_06)

## 2007 
lof_07 <- list.files(path = "~/Dropbox/db2/tv/2007", 
                     recursive = TRUE,
                     pattern = "\\.txt$", 
                     full.names = TRUE)

lof_07 <- lof_07[sapply(lof_07, file.size) > 10]


df_07 <- lof_07 %>%
  purrr::map_df(read_table, 
                col_names = FALSE) %>% 
  filter(str_detect(X1, "TOP\\|")) %>% 
  tidyr::separate(X1, into = c("x1", "x2", "x3"), sep = "\\|") %>% 
  select(x3)

export(df_07, "~/Dropbox/db2/tv/recuced/df_07.csv")
rm(lof_07, df_07)

## 2008 
lof_08 <- list.files(path = "~/Dropbox/db2/tv/2008", 
                     recursive = TRUE,
                     pattern = "\\.txt$", 
                     full.names = TRUE)

lof_08 <- lof_08[sapply(lof_08, file.size) > 10]


df_08 <- lof_08 %>%
  purrr::map_df(read_table, 
                col_names = FALSE) %>% 
  filter(str_detect(X1, "TOP\\|")) %>% 
  tidyr::separate(X1, into = c("x1", "x2", "x3"), sep = "\\|") %>% 
  select(x3)

export(df_08, "~/Dropbox/db2/tv/recuced/df_08.csv")
rm(lof_08, df_08)

## 2009 
lof_09 <- list.files(path = "~/Dropbox/db2/tv/2009", 
                     recursive = TRUE,
                     pattern = "\\.txt$", 
                     full.names = TRUE)

lof_09 <- lof_09[sapply(lof_09, file.size) > 10]


df_09 <- lof_09 %>%
  purrr::map_df(read_table, 
                col_names = FALSE)  %>% 
  filter(str_detect(X1, "TOP\\|"))  %>% 
  tidyr::separate(X1, into = c("x1", "x2", "x3"), sep = "\\|") %>% 
  select(x3)

export(df_09, "~/Dropbox/db2/tv/recuced/df_09.csv")
rm(lof_09, df_09)

## 2010 
lof_10 <- list.files(path = "~/Dropbox/db2/tv/2010", 
                     recursive = TRUE,
                     pattern = "\\.txt$", 
                     full.names = TRUE)

lof_10 <- lof_10[sapply(lof_10, file.size) > 10]


df_10 <- lof_10 %>%
  purrr::map_df(read_table, 
                col_names = FALSE) 

df_10 <- df_10 %>% 
  filter(str_detect(X1, "TOP\\|"))  %>% 
  tidyr::separate(X1, into = c("x1", "x2", "x3"), sep = "\\|") %>% 
  select(x3)

export(df_10, "~/Dropbox/db2/tv/recuced/df_10.csv")
rm(lof_10, df_10)

## 2011 
lof_11 <- list.files(path = "~/Dropbox/db2/tv/2011", 
                     recursive = TRUE,
                     pattern = "\\.txt$", 
                     full.names = TRUE)

lof_11 <- lof_11[sapply(lof_11, file.size) > 10]


df_11 <- lof_11 %>%
  purrr::map_df(read_table, 
                col_names = FALSE)  %>% 
  filter(str_detect(X1, "TOP\\|"))  %>% 
  tidyr::separate(X1, into = c("x1", "x2", "x3"), sep = "\\|") %>% 
  select(x3)

export(df_11, "~/Dropbox/db2/tv/recuced/df_11.csv")
rm(lof_11, df_11)

## 2012 
lof_12 <- list.files(path = "~/Dropbox/db2/tv/2012", 
                     recursive = TRUE,
                     pattern = "\\.txt$", 
                     full.names = TRUE)

lof_12 <- lof_12[sapply(lof_12, file.size) > 10]

df_12 <- lof_12 %>%
  purrr::map_df(read_table, 
                col_names = FALSE) %>% 
  filter(str_detect(X1, "TOP\\|")) %>% 
  tidyr::separate(X1, into = c("x1", "x2", "x3"), sep = "\\|") %>% 
  select(x3)

export(df_12, "~/Dropbox/db2/tv/recuced/df_12.csv")
rm(lof_12, df_12)


