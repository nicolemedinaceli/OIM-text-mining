#=================================================================#

# Project: IOM 2020
# Objective:  web jobs offers scrapping 
#               - skokka.bo
# comments: fcalderon@aru.org.bo
#Notas: El objetivo de este script es armar una base de datos de la página
#Skokka. Para este caso, se tomó en cuenta la sección de Escorts. El procedimiento
#consiste en dividir el scrapping por departamentos/ciudades. En ese caso se toman en cuenta
#las siguientes: Cochabamba, El Alto, La Paz, Santa Cruz, Sucre y Oruro
#Uno de los problemas que existen, sin embargo, es que el código puede tener interrupciones
#inesperadas e inexplicables de momento. Esto sucedió en todos los casos menos para Oruro.
#En ese sentido, es por eso que en los otros departamentos se encuentran loops "incompletos", es decir
#que no comienzan en 1:x, sino que comienzan en cualquier otro número. Esto se debe a que al haber sido interrumpidas 
#inexplicablemente las operaciones, me vi forzado a hacer la operación por partes. Esta razón explica también la creación 
#de múltiples bases por departamento, las cuales son unidas al final de este script. 
#Finalmente, quiero mencionar que la intención de este scrapping era recolectar 35,000 anuncios. Sin embargo, debido a las interrupciones
#inexplicables, solamente se pudieron recopilar hasta el momento 4167 observaciones. Las mismas se encuentran en la base
#SkokkaEscorts-Total.RData, la cual está ordenada por departamento y por fecha más reciente. Un pequeño problema que no pude resolver todavía
#es que el número de cada observación de esa base no está enlistado del 1 al 4167 sino que por las operaciones que hace el rbind, el número adquiere
#un subfijo. Pese a eso, el orden se mantiene como el explicado líneas arriba (departamento fecha)

#=================================================================#
rm(list=ls())
setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")

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
#library(Rstem)

                      ################################
                      ################################
                      # - main script starts here -  #


edad <- c()
suburls <- c()
title <- c()
idep  <- c()
text  <- c()
tipo <- c()
date <- c()
j = 0
skip = 0

#### ESCORTS######
#### COCHABAMBA #####
###310 paginas ####
    
  
  ipage<-"https://bo.skokka.com/escorts/cochabamba/?p="
  for(i in 74:310){
  url<-paste0(ipage,i) #surfes through pages
  www<-read_html(url)
  suburl<-  www %>% # gets individual url's from each job announcement
    html_nodes("p") %>%
    html_nodes("a") %>%
    html_attr("href")
  suburl <- suburl[1:25]
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
      html_nodes(".main-title") %>%
      html_text()
    
    title <- c(title,get_title)
    # ------------------ edad,departamento ---------------------------#
    get_call <- link %>%
      html_nodes("h6") %>%
      html_nodes("b") %>%
      html_text()
    get_call2 <- link %>%
      html_nodes("div") %>%
      html_nodes("p") %>%
      html_text()
    get_call3 <- link %>%
      html_nodes(".date") %>%
      html_text()
    edad<-c(edad,get_call[1])
    idep<-c(idep,"Cochabamba")
    text <- c(text, get_call2[1])
    tipo <- c(tipo, "Escort")
    date <- c(date, get_call3[1])

  }

  print(j)
  j=j+1
  }
  suburls <- append(suburls,suburl) 

  }
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, edad, tipo, date, text)
  #df1 <-df[14:25,]
  #save(df1,file="Escorts-Cbba8.RData")
  df$title <- trimws(df$title)
  save(df,file="Escorts-Cbba9.RData")
  
  
  ##LA PAZ##
  rm(list=ls())
  setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")
  
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
  #library(Rstem)
  
  ################################
  ################################
  # - main script starts here -  #
  
  
  edad <- c()
  suburls <- c()
  title <- c()
  idep  <- c()
  text  <- c()
  tipo <- c()
  date <- c()
  j = 0
  skip = 0
  
  #### ESCORTS######
  #### LA PAZ #####
  ###310 paginas ####
  
  
  ipage<-"https://bo.skokka.com/escorts/la-paz/?p="
  for(i in 19:310){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes("p") %>%
      html_nodes("a") %>%
      html_attr("href")
    suburl <- suburl[1:25]
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
          html_nodes(".main-title") %>%
          html_text()
        
        title <- c(title,get_title)
        # ------------------ edad,departamento ---------------------------#
        get_call <- link %>%
          html_nodes("h6") %>%
          html_nodes("b") %>%
          html_text()
        get_call2 <- link %>%
          html_nodes("div") %>%
          html_nodes("p") %>%
          html_text()
        get_call3 <- link %>%
          html_nodes(".date") %>%
          html_text()
        edad<-c(edad,get_call[1])
        idep<-c(idep,"La Paz")
        text <- c(text, get_call2[1])
        tipo <- c(tipo, "Escort")
        date <- c(date, get_call3[1])
        
      }
      
      print(j)
      j=j+1
    }
    suburls <- append(suburls,suburl) 
    
  }
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, edad, tipo, date, text)
  #df1 <-df[19:25,]
  #save(df1,file="Escorts-LaPaz6.RData")
  df$title <- trimws(df$title)
  save(df,file="Escorts-LaPaz7.RData")

  ## SANTA CRUZ ##
  rm(list=ls())
  setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")
  
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
  #library(Rstem)
  
  ################################
  ################################
  # - main script starts here -  #
  
  
  edad <- c()
  suburls <- c()
  title <- c()
  idep  <- c()
  text  <- c()
  tipo <- c()
  date <- c()
  j = 0
  skip = 0
  
  #### ESCORTS######
  #### SANTA CRUZ #####
  ###610 paginas ####
  
  
  ipage<-"https://bo.skokka.com/escorts/santa-cruz/?p="
  for(i in 50:610){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes("p") %>%
      html_nodes("a") %>%
      html_attr("href")
    suburl <- suburl[1:25]
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
          html_nodes(".main-title") %>%
          html_text()
        
        title <- c(title,get_title)
        # ------------------ edad,departamento ---------------------------#
        get_call <- link %>%
          html_nodes("h6") %>%
          html_nodes("b") %>%
          html_text()
        get_call2 <- link %>%
          html_nodes("div") %>%
          html_nodes("p") %>%
          html_text()
        get_call3 <- link %>%
          html_nodes(".date") %>%
          html_text()
        edad<-c(edad,get_call[1])
        idep<-c(idep,"Santa Cruz")
        text <- c(text, get_call2[1])
        tipo <- c(tipo, "Escort")
        date <- c(date, get_call3[1])
        
      }
      
      print(j)
      j=j+1
    }
    suburls <- append(suburls,suburl) 
    
  }
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, edad, tipo, date, text)
  #df1 <-df[7:25,]
  #save(df1,file="Escorts-SantaCruz7.RData")
  df$title <- trimws(df$title)
  save(df,file="Escorts-SantaCruz8.RData")
  
  
  
  ####EL ALTO####
  rm(list=ls())
  setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")
  
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
  #library(Rstem)
  
  ################################
  ################################
  # - main script starts here -  #
  
  
  edad <- c()
  suburls <- c()
  title <- c()
  idep  <- c()
  text  <- c()
  tipo <- c()
  date <- c()
  j = 0
  skip = 0
  
  #### ESCORTS######
  #### EL ALTO #####
  ###105 paginas ####
  
  
  ipage<-"https://bo.skokka.com/escorts/el-alto/?p="
  for(i in 14:105){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes("p") %>%
      html_nodes("a") %>%
      html_attr("href")
    suburl <- suburl[1:25]
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
          html_nodes(".main-title") %>%
          html_text()
        
        title <- c(title,get_title)
        # ------------------ edad,departamento ---------------------------#
        get_call <- link %>%
          html_nodes("h6") %>%
          html_nodes("b") %>%
          html_text()
        get_call2 <- link %>%
          html_nodes("div") %>%
          html_nodes("p") %>%
          html_text()
        get_call3 <- link %>%
          html_nodes(".date") %>%
          html_text()
        edad<-c(edad,get_call[1])
        idep<-c(idep,"El Alto")
        text <- c(text, get_call2[1])
        tipo <- c(tipo, "Escort")
        date <- c(date, get_call3[1])
        
      }
      
      print(j)
      j=j+1
    }
    suburls <- append(suburls,suburl) 
    
  }
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, edad, tipo, date, text)
  #df1 <-df[17:25,]
  #save(df1,file="Escorts-ElAlto4.RData")
  df$title <- trimws(df$title)
  save(df,file="Escorts-ElAlto5.RData")
  
  
  ## SUCRE ##
  rm(list=ls())
  setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")
  
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
  #library(Rstem)
  
  ################################
  ################################
  # - main script starts here -  #
  
  
  edad <- c()
  suburls <- c()
  title <- c()
  idep  <- c()
  text  <- c()
  tipo <- c()
  date <- c()
  j = 0
  skip = 0
  
  #### ESCORTS######
  #### SUCRE #####
  ###90 paginas ####
  
  
  ipage<-"https://bo.skokka.com/escorts/sucre/?p="
  for(i in 10:90){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes("p") %>%
      html_nodes("a") %>%
      html_attr("href")
    suburl <- suburl[1:25]
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
          html_nodes(".main-title") %>%
          html_text()
        
        title <- c(title,get_title)
        # ------------------ edad,departamento ---------------------------#
        get_call <- link %>%
          html_nodes("h6") %>%
          html_nodes("b") %>%
          html_text()
        get_call2 <- link %>%
          html_nodes("div") %>%
          html_nodes("p") %>%
          html_text()
        get_call3 <- link %>%
          html_nodes(".date") %>%
          html_text()
        edad<-c(edad,get_call[1])
        idep<-c(idep,"Sucre")
        text <- c(text, get_call2[1])
        tipo <- c(tipo, "Escort")
        date <- c(date, get_call3[1])
        
      }
      
      print(j)
      j=j+1
    }
    suburls <- append(suburls,suburl) 
    
  }
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, edad, tipo, date, text)
  #df1 <-df[3:25,]
  #save(df1,file="Escorts-Sucre2.RData")
  df$title <- trimws(df$title)
  save(df,file="Escorts-Sucre3.RData")
  
  
  ## ORURO ##
  rm(list=ls())
  setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")
  
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
  #library(Rstem)
  
  ################################
  ################################
  # - main script starts here -  #
  
  
  edad <- c()
  suburls <- c()
  title <- c()
  idep  <- c()
  text  <- c()
  tipo <- c()
  date <- c()
  j = 0
  skip = 0
  
  #### ESCORTS######
  #### ORURO #####
  ###9 paginas ####
  
  
  ipage<-"https://bo.skokka.com/escorts/oruro/?p="
  for(i in 1:9){
    url<-paste0(ipage,i) #surfes through pages
    www<-read_html(url)
    suburl<-  www %>% # gets individual url's from each job announcement
      html_nodes("p") %>%
      html_nodes("a") %>%
      html_attr("href")
    suburl <- suburl[1:25]
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
          html_nodes(".main-title") %>%
          html_text()
        
        title <- c(title,get_title)
        # ------------------ edad,departamento ---------------------------#
        get_call <- link %>%
          html_nodes("h6") %>%
          html_nodes("b") %>%
          html_text()
        get_call2 <- link %>%
          html_nodes("div") %>%
          html_nodes("p") %>%
          html_text()
        get_call3 <- link %>%
          html_nodes(".date") %>%
          html_text()
        edad<-c(edad,get_call[1])
        idep<-c(idep,"Oruro")
        text <- c(text, get_call2[1])
        tipo <- c(tipo, "Escort")
        date <- c(date, get_call3[1])
        
      }
      
      print(j)
      j=j+1
    }
    suburls <- append(suburls,suburl) 
    
  }
  total_obs = j
  total_failed = skip
  
  #====================== tweaking extracted text =========================#
  df <- data.frame(title,idep, edad, tipo, date, text)
  df$title <- trimws(df$title)
  save(df,file="Escorts-Oruro.RData")
  
  
  
  rm(list=ls())
  setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")
  load("Escorts-Cbba.RData")
  df01<-df
  load("Escorts-Cbba2.RData")
  df02<-df1
  load("Escorts-Cbba3.RData")
  df03<-df
  load("Escorts-Cbba4.RData")
  df04<-df1
  load("Escorts-Cbba5.RData")
  df05<-df
  load("Escorts-Cbba6.RData")
  df06<-df1
  load("Escorts-Cbba7.RData")
  df07<-df
  dfcbba<-rbind(df01,df02,df03,df04,df05,df06,df07)
  save(dfcbba,file="Escorts-CbbaTotal.RData")
  
  
  rm(list=ls())
  setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")
  load("Escorts-ElAlto.RData")
  df01<-df
  load("Escorts-ElAlto2.RData")
  df02<-df1
  load("Escorts-ElAlto3.RData")
  df03<-df  
  load("Escorts-ElAlto4.RData")
  df04<-df1  
  dfelalto<-rbind(df01,df02,df03,df04)
  save(dfelalto,file="Escorts-ElAltoTotal.RData")
  
  rm(list=ls())
  setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")
  load("Escorts-LaPaz.RData")
  df01<-df
  load("Escorts-LaPaz2.RData")
  df02<-df1
  load("Escorts-LaPaz3.RData")
  df03<-df  
  load("Escorts-LaPaz4.RData")
  df04<-df1  
  load("Escorts-LaPaz5.RData")
  df05<-df  
  load("Escorts-LaPaz6.RData")
  df06<-df1  
  dflapaz<-rbind(df01,df02,df03,df04,df05,df06)
  save(dflapaz,file="Escorts-LaPazTotal.RData")
  
  
  rm(list=ls())
  setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")
  load("Escorts-SantaCruz.RData")
  df01<-df
  load("Escorts-SantaCruz2.RData")
  df02<-df1
  load("Escorts-SantaCruz3.RData")
  df03<-df  
  load("Escorts-SantaCruz4.RData")
  df04<-df  
  load("Escorts-SantaCruz5.RData")
  df05<-df1  
  load("Escorts-SantaCruz6.RData")
  df06<-df  
  load("Escorts-SantaCruz7.RData")
  df07<-df1  
  dfscz<-rbind(df01,df02,df03,df04,df05,df06,df07)
  save(dfscz,file="Escorts-SantaCruzTotal.RData")
  
  
  rm(list=ls())
  setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")
  load("Escorts-Sucre.RData")
  df01<-df
  load("Escorts-Sucre2.RData")
  df02<-df1
  dfsucre<-rbind(df01,df02)
  save(dfsucre,file="Escorts-SucreTotal.RData")
  
  
  rm(list=ls())
  setwd("C:\\Users\\Fabian\\OneDrive\\ARU\\Proyecto_OIM\\R")
  load("Escorts-CbbaTotal.RData")
  load("Escorts-ElAltoTotal.RData")
  load("Escorts-LaPazTotal.RData")
  load("Escorts-Oruro.RData")
  load("Escorts-SantaCruzTotal.RData")
  load("Escorts-SucreTotal.RData")
  dftotal<-rbind(dfcbba,dfelalto,dflapaz,df,dfscz,dfsucre)
  save(dftotal,file="Escorts-Total.RData")
  