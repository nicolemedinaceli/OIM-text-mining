#=================================================================#

# Project: IOM 2020
# Objective:  web jobs offers scrapping 
#               - computrabajo.com
# comments: pojeda@aru.org.bo

#=================================================================#
rm(list=ls())
setwd("E:/!_CLOUD/_ARU2020/_OIMtrata/_scrap")

#------------------------ loading libraries --------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(rvest)
library(xml2)
library(stringi)
library(data.table)
library(stringr)
library(haven)
library(reshape2)
library(haven)
library(tm)
library(wordcloud)
library(Rstem)

                      ################################
                      ################################
                      # - main script starts here -  #



suburls <- c()
title <- c()
city  <- c()
area  <- c()
idep  <- c()
text  <- c()
j = 0
skip = 0

ipage<-"https://www.computrabajo.com.bo/ofertas-de-trabajo/?p="

for(i in 1:13){ # to april 13th the web has 13 active pages
   print(i)
   url<-paste0(ipage,i) #surfes through pages
   www<-read_html(url)
   suburl<-  www %>% # gets individual url's from each job announcement
    html_nodes(".js-o-link") %>%
    html_attr("href")
    for (k in suburl){  # exlplores data from each sub-url
      print(k)
      url2<-paste0("https://www.computrabajo.com.bo",k)
      print(url2)
     # print("hola!")
      #--------------------job title --------------------#
      link<-try(read_html(url2), silent =TRUE)
      if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
        print("skipping suburl")
        skip = skip + 1
      }
      else {
        print("not skipping")
        get_title <-  link %>%
          html_nodes("h1") %>%
          html_text()
     
        title <- c(title,get_title)
        # ------------------ city, area and text ---------------------------#
        get_call <- link %>%
          html_nodes("li") %>%
          html_text()
        
          idep <- c(idep, get_call[25])
          city <- c(city, get_call[26])
          area <- c(area, get_call[27])
          text <- c(text, get_call[29])
      }
      print(j)
      j=j+1
   
     }
    suburls <- append(suburls,suburl)  
  }
total_obs = j
total_failed = skip
#====================== tweaking extracted text =========================#
df <- data.frame(title,area, idep, city, text)


df$title <- trimws(df$title) 
df$text  <- trimws(gsub("Descripción", "", df$text))
df$area  <- gsub("/","", df$area)
df <- mutate_all(df, funs(tolower))

save(df,file="computrabajo.RData")

  
  
  