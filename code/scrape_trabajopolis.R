#=================================================================#

#Project: OIM 2020 - Text Mining
#Objective:  web jobs offers scrapping - Trabajopolis
#Comments: nmedinaceli@aru.org.bo
#Notas: Se hizo la división por departamento ya que esta fue la única manera
#       para poder obtener la información de lugar, empleador y algunos 
#       detalles más que eran importantes.

#=================================================================#

          #Directorio y librerias

rm(list=ls())
setwd("C:/Users/PC/Documents/Pasantía Aru/proyecto_trata/_txtmng")

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

        # Main code 
suburls <- c()
title <- c()
idep  <- c()
text  <- c()
date <- c()
employer <- c()
j = 0
skip = 0
###################################################################################################
# Santa Cruz - 44pgs
ipage <- "https://www.trabajopolis.bo/bolsa-de-trabajo-empleos-en-bolivia/Santa+Cruz+de+la+Sierra/?searchId=142899.1721&action=search&page="
for(i in 1:44){
  url<-paste0(ipage,i) #surfes through pages
  www<-read_html(url)
  suburl<-  www %>% # gets individual url's from each job announcement
    html_nodes(".anuncio-estandar-titulo") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  get_employer <- www %>%
    html_nodes(".anuncio-estandar-empresa") %>%
    html_nodes("a") %>%
    html_text()
  employer<-c(employer,get_employer)
  
    for (k in suburl){  # exlplores data from each sub-url
      print(k)
      url2<-k
      print(url2)
    
      #--------------------job title --------------------#
      link<-try(read_html(url2), silent =TRUE)
      if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
        print("skipping suburl")
        skip = skip + 1
      }
      
    else {
      print("not skipping")
      
      get_title <-  link %>%
        html_nodes("[name=keywords]") %>%
        html_attr("content") 
      title <- c(title,get_title)
      
      get_date <-  link %>%
        html_nodes(".date") %>%
        html_text()
      date <- c(date,get_date[1])  
      
      get_text <- link %>%
        html_nodes("p") %>%
        html_text()
      
      for(t in 2:length(get_text)){
        get_text[1] <- str_c(get_text[1],get_text[t], sep=",")
      }
      
      idep<-c(idep,"Santa Cruz")
      
      text<-c(text,get_text[1])
      
      }
      
      print(j)
      j=j+1
   
    }
  suburls <- append(suburls,suburl)
}  
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, employer, date, text)
  df$title <- trimws(df$title)
  save(df,file="SCZ.RData")
#################################################################################################################  

  # La Paz - 20pgs
  ipage <- "https://www.trabajopolis.bo/bolsa-de-trabajo-empleos-en-bolivia/La+Paz/?searchId=142899.1722&action=search&page="
  for(i in 1:20){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes(".anuncio-estandar-titulo") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    get_employer <- www %>%
      html_nodes(".anuncio-estandar-empresa") %>%
      html_nodes("a") %>%
      html_text()
    employer<-c(employer,get_employer)
    
    for (k in suburl){  # exlplores data from each sub-url
      print(k)
      url2<-k
      print(url2)
      
      #--------------------job title --------------------#
      link<-try(read_html(url2), silent =TRUE)
      if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
        print("skipping suburl")
        skip = skip + 1
      }
      
      else {
        print("not skipping")
        
        get_title <-  link %>%
          html_nodes("[name=keywords]") %>%
          html_attr("content") 
        title <- c(title,get_title)
        
        get_date <-  link %>%
          html_nodes(".date") %>%
          html_text()
        date <- c(date,get_date[1])  
        
        get_text <- link %>%
          html_nodes("p") %>%
          html_text()
        
        for(t in 2:length(get_text)){
          get_text[1] <- str_c(get_text[1],get_text[t], sep=",")
        }
        
        idep<-c(idep,"La Paz")
        
        text<-c(text,get_text[1])
        
      }
      
      print(j)
      j=j+1
      
    }
    suburls <- append(suburls,suburl)
  }  
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, employer, date, text)
  df$title <- trimws(df$title)
  save(df,file="LPZ.RData")
################################################################################################################ 
   
  # Cochabamba - 9pgs
  ipage <- "https://www.trabajopolis.bo/bolsa-de-trabajo-empleos-en-bolivia/Cochabamba/?searchId=142899.1723&action=search&page="
  for(i in 1:9){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes(".anuncio-estandar-titulo") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    get_employer <- www %>%
      html_nodes(".anuncio-estandar-empresa") %>%
      html_nodes("a") %>%
      html_text()
    employer<-c(employer,get_employer)
    
    for (k in suburl){  # exlplores data from each sub-url
      print(k)
      url2<-k
      print(url2)
      
      #--------------------job title --------------------#
      link<-try(read_html(url2), silent =TRUE)
      if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
        print("skipping suburl")
        skip = skip + 1
      }
      
      else {
        print("not skipping")
        
        get_title <-  link %>%
          html_nodes("[name=keywords]") %>%
          html_attr("content") 
        title <- c(title,get_title)
        
        get_date <-  link %>%
          html_nodes(".date") %>%
          html_text()
        date <- c(date,get_date[1])  
        
        get_text <- link %>%
          html_nodes("p") %>%
          html_text()
        
        for(t in 2:length(get_text)){
          get_text[1] <- str_c(get_text[1],get_text[t], sep=",")
        }
        
        idep<-c(idep,"Cochabamba")
        
        text<-c(text,get_text[1])
        
      }
      
      print(j)
      j=j+1
      
    }
    suburls <- append(suburls,suburl)
  }  
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, employer, date, text)
  df$title <- trimws(df$title)
  save(df,file="CBBA.RData")
################################################################################

  # El Alto - 2pgs
  ipage <- "https://www.trabajopolis.bo/bolsa-de-trabajo-empleos-en-bolivia/El+Alto/?searchId=142899.1724&action=search&page="
  for(i in 1:2){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes(".anuncio-estandar-titulo") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    get_employer <- www %>%
      html_nodes(".anuncio-estandar-empresa") %>%
      html_nodes("a") %>%
      html_text()
    employer<-c(employer,get_employer)
    
    for (k in suburl){  # exlplores data from each sub-url
      print(k)
      url2<-k
      print(url2)
      
      #--------------------job title --------------------#
      link<-try(read_html(url2), silent =TRUE)
      if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
        print("skipping suburl")
        skip = skip + 1
      }
      
      else {
        print("not skipping")
        
        get_title <-  link %>%
          html_nodes("[name=keywords]") %>%
          html_attr("content") 
        title <- c(title,get_title)
        
        get_date <-  link %>%
          html_nodes(".date") %>%
          html_text()
        date <- c(date,get_date[1])  
        
        get_text <- link %>%
          html_nodes("p") %>%
          html_text()
        
        for(t in 2:length(get_text)){
          get_text[1] <- str_c(get_text[1],get_text[t], sep=",")
        }
        
        idep<-c(idep,"El Alto")
        
        text<-c(text,get_text[1])
        
      }
      
      print(j)
      j=j+1
      
    }
    suburls <- append(suburls,suburl)
  }  
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, employer, date, text)
  df$title <- trimws(df$title)
  save(df,file="ALTO.RData")
  ############################################################################################
  
  # Tarija - 2pgs
  ipage <- "https://www.trabajopolis.bo/bolsa-de-trabajo-empleos-en-bolivia/Tarija/?searchId=142899.1725&action=search&page="
  for(i in 1:2){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes(".anuncio-estandar-titulo") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    get_employer <- www %>%
      html_nodes(".anuncio-estandar-empresa") %>%
      html_nodes("a") %>%
      html_text()
    employer<-c(employer,get_employer)
    
    for (k in suburl){  # exlplores data from each sub-url
      print(k)
      url2<-k
      print(url2)
      
      #--------------------job title --------------------#
      link<-try(read_html(url2), silent =TRUE)
      if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
        print("skipping suburl")
        skip = skip + 1
      }
      
      else {
        print("not skipping")
        
        get_title <-  link %>%
          html_nodes("[name=keywords]") %>%
          html_attr("content") 
        title <- c(title,get_title)
        
        get_date <-  link %>%
          html_nodes(".date") %>%
          html_text()
        date <- c(date,get_date[1])  
        
        get_text <- link %>%
          html_nodes("p") %>%
          html_text()
        
        for(t in 2:length(get_text)){
          get_text[1] <- str_c(get_text[1],get_text[t], sep=",")
        }
        
        idep<-c(idep,"Tarija")
        
        text<-c(text,get_text[1])
        
      }
      
      print(j)
      j=j+1
      
    }
    suburls <- append(suburls,suburl)
  }  
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, employer, date, text)
  df$title <- trimws(df$title)
  save(df,file="TJA.RData")
##############################################################################################################
  
  # Sucre - 2pgs
  ipage <- "https://www.trabajopolis.bo/bolsa-de-trabajo-empleos-en-bolivia/Sucre/?searchId=142899.1726&action=search&page="
  for(i in 1:2){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes(".anuncio-estandar-titulo") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    get_employer <- www %>%
      html_nodes(".anuncio-estandar-empresa") %>%
      html_nodes("a") %>%
      html_text()
    employer<-c(employer,get_employer)
    
    for (k in suburl){  # exlplores data from each sub-url
      print(k)
      url2<-k
      print(url2)
      
      #--------------------job title --------------------#
      link<-try(read_html(url2), silent =TRUE)
      if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
        print("skipping suburl")
        skip = skip + 1
      }
      
      else {
        print("not skipping")
        
        get_title <-  link %>%
          html_nodes("[name=keywords]") %>%
          html_attr("content") 
        title <- c(title,get_title)
        
        get_date <-  link %>%
          html_nodes(".date") %>%
          html_text()
        date <- c(date,get_date[1])  
        
        get_text <- link %>%
          html_nodes("p") %>%
          html_text()
        
        for(t in 2:length(get_text)){
          get_text[1] <- str_c(get_text[1],get_text[t], sep=",")
        }
        
        idep<-c(idep,"Sucre")
        
        text<-c(text,get_text[1])
        
      }
      
      print(j)
      j=j+1
      
    }
    suburls <- append(suburls,suburl)
  }  
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, employer, date, text)
  df$title <- trimws(df$title)
  save(df,file="SUC.RData")
###########################################################################################################
  
  # Trinidad - 2pgs
  ipage <- "https://www.trabajopolis.bo/bolsa-de-trabajo-empleos-en-bolivia/Trinidad/?searchId=142899.1727&action=search&page="
  for(i in 1:2){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes(".anuncio-estandar-titulo") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    get_employer <- www %>%
      html_nodes(".anuncio-estandar-empresa") %>%
      html_nodes("a") %>%
      html_text()
    employer<-c(employer,get_employer)
    
    for (k in suburl){  # exlplores data from each sub-url
      print(k)
      url2<-k
      print(url2)
      
      #--------------------job title --------------------#
      link<-try(read_html(url2), silent =TRUE)
      if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
        print("skipping suburl")
        skip = skip + 1
      }
      
      else {
        print("not skipping")
        
        get_title <-  link %>%
          html_nodes("[name=keywords]") %>%
          html_attr("content") 
        title <- c(title,get_title)
        
        get_date <-  link %>%
          html_nodes(".date") %>%
          html_text()
        date <- c(date,get_date[1])  
        
        get_text <- link %>%
          html_nodes("p") %>%
          html_text()
        
        for(t in 2:length(get_text)){
          get_text[1] <- str_c(get_text[1],get_text[t], sep=",")
        }
        
        idep<-c(idep,"Trinidad")
        
        text<-c(text,get_text[1])
        
      }
      
      print(j)
      j=j+1
      
    }
    suburls <- append(suburls,suburl)
  }  
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, employer, date, text)
  df$title <- trimws(df$title)
  save(df,file="TRI.RData")
#############################################################################################################3

  # Potosi - 2pgs
  ipage <- "https://www.trabajopolis.bo/bolsa-de-trabajo-empleos-en-bolivia/Potos%C3%AD/?searchId=142899.1728&action=search&page="
  for(i in 1:2){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes(".anuncio-estandar-titulo") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    get_employer <- www %>%
      html_nodes(".anuncio-estandar-empresa") %>%
      html_nodes("a") %>%
      html_text()
    employer<-c(employer,get_employer)
    
    for (k in suburl){  # exlplores data from each sub-url
      print(k)
      url2<-k
      print(url2)
      
      #--------------------job title --------------------#
      link<-try(read_html(url2), silent =TRUE)
      if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
        print("skipping suburl")
        skip = skip + 1
      }
      
      else {
        print("not skipping")
        
        get_title <-  link %>%
          html_nodes("[name=keywords]") %>%
          html_attr("content") 
        title <- c(title,get_title)
        
        get_date <-  link %>%
          html_nodes(".date") %>%
          html_text()
        date <- c(date,get_date[1])  
        
        get_text <- link %>%
          html_nodes("p") %>%
          html_text()
        
        for(t in 2:length(get_text)){
          get_text[1] <- str_c(get_text[1],get_text[t], sep=",")
        }
        
        idep<-c(idep,"Potosi")
        
        text<-c(text,get_text[1])
      }
      
      print(j)
      j=j+1
      
    }
    suburls <- append(suburls,suburl)
  }  
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, employer, date, text)
  df$title <- trimws(df$title)
  save(df,file="POT.RData")
##################################################################################################  

  # Oruro - 1pgs
  url <- "https://www.trabajopolis.bo/bolsa-de-trabajo-empleos-en-bolivia/Oruro/"
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes(".anuncio-estandar-titulo") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    get_employer <- www %>%
      html_nodes(".anuncio-estandar-empresa") %>%
      html_nodes("a") %>%
      html_text()
    employer<-c(employer,get_employer)
    
    for (k in suburl){  # exlplores data from each sub-url
      print(k)
      url2<-k
      print(url2)
      
      #--------------------job title --------------------#
      link<-try(read_html(url2), silent =TRUE)
      if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
        print("skipping suburl")
        skip = skip + 1
      }
      
      else {
        print("not skipping")
        
        get_title <-  link %>%
          html_nodes("[name=keywords]") %>%
          html_attr("content") 
        title <- c(title,get_title)
        
        get_date <-  link %>%
          html_nodes(".date") %>%
          html_text()
        date <- c(date,get_date[1])  
        
        get_text <- link %>%
          html_nodes("p") %>%
          html_text()
        
        for(t in 2:length(get_text)){
          get_text[1] <- str_c(get_text[1],get_text[t], sep=",")
        }
        
        idep<-c(idep,"Oruro")
        
        text<-c(text,get_text[1])
        
      }
      
      print(j)
      j=j+1
      
    }
    suburls <- append(suburls,suburl)
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, employer, date, text)
  df$title <- trimws(df$title)
  save(df,file="ORU.RData")
###############################################################################
  
  # Cobija - 1pgs
  url <- "https://www.trabajopolis.bo/bolsa-de-trabajo-empleos-en-bolivia/Cobija/"
  www<-read_html(url)
  suburl<-  www %>% # gets individual url's from each job announcement
    html_nodes(".anuncio-estandar-titulo") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  get_employer <- www %>%
    html_nodes(".anuncio-estandar-empresa") %>%
    html_nodes("a") %>%
    html_text()
  employer<-c(employer,get_employer)
  
  for (k in suburl){  # exlplores data from each sub-url
    print(k)
    url2<-k
    print(url2)
    
    #--------------------job title --------------------#
    link<-try(read_html(url2), silent =TRUE)
    if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
      print("skipping suburl")
      skip = skip + 1
    }
    
    else {
      print("not skipping")
      
      get_title <-  link %>%
        html_nodes("[name=keywords]") %>%
        html_attr("content") 
      title <- c(title,get_title)
      
      get_date <-  link %>%
        html_nodes(".date") %>%
        html_text()
      date <- c(date,get_date[1])  
      
      get_text <- link %>%
        html_nodes("p") %>%
        html_text()
      
      for(t in 2:length(get_text)){
        get_text[1] <- str_c(get_text[1],get_text[t], sep=",")
      }
      
      idep<-c(idep,"Cobija")
      
      text<-c(text,get_text[1])
      
    }
    
    print(j)
    j=j+1
    
  }
  suburls <- append(suburls,suburl)
  
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, employer, date, text)
  df$title <- trimws(df$title)
  save(df,file="COB.RData")
############################################################################################################33
  
  # Otros Departamentos - 1pgs
  url <- "https://www.trabajopolis.bo/bolsa-de-trabajo-empleos-en-bolivia/Otra+Ciudad+de+Bolivia/"
  www<-read_html(url)
  suburl<-  www %>% # gets individual url's from each job announcement
    html_nodes(".anuncio-estandar-titulo") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  get_employer <- www %>%
    html_nodes(".anuncio-estandar-empresa") %>%
    html_nodes("a") %>%
    html_text()
  employer<-c(employer,get_employer)
  
  for (k in suburl){  # exlplores data from each sub-url
    print(k)
    url2<-k
    print(url2)
    
    #--------------------job title --------------------#
    link<-try(read_html(url2), silent =TRUE)
    if (link[1] == "Error in open.connection(x, \"rb\") : HTTP error 410.\n"){
      print("skipping suburl")
      skip = skip + 1
    }
    
    else {
      print("not skipping")
      
      get_title <-  link %>%
        html_nodes("[name=keywords]") %>%
        html_attr("content") 
      title <- c(title,get_title)
      
      get_date <-  link %>%
        html_nodes(".date") %>%
        html_text()
      date <- c(date,get_date[1])  
      
      get_text <- link %>%
        html_nodes("p") %>%
        html_text()
      
      for(t in 2:length(get_text)){
        get_text[1] <- str_c(get_text[1],get_text[t], sep=",")
      }
      
      idep<-c(idep,"Otro Dpto")
      
      text<-c(text,get_text[1])
      
    }
    
    print(j)
    j=j+1
    
  }
  suburls <- append(suburls,suburl)
  
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, employer, date, text)
  df$title <- trimws(df$title)
  save(df,file="ODP.RData")
#############################################################################################
                # Unir las bases departamentales

load ("ALTO.RData")
df01<-df
load ("CBBA.RData")
df02<-df
load ("COB.RData")
df03<-df
load ("LPZ.RData")
df04<-df
load ("ODP.RData")
df05<-df
load ("ORU.RData")
df06<-df
load ("POT.RData")
df07<-df
load ("SCZ.RData")
df08<-df
load ("SUC.RData")
df09<-df
load ("TJA.RData")
df10<-df
load ("TRI.RData")
df11<-df
dftotal<-rbind(df01,df02,df03,df04,df05,df06,df07,df08,df09,df10,df11)
save(dftotal,file="trab_bolivia.RData")