library(RSelenium)
library(rvest)
library(parallel)
library(doParallel)
library(foreach)
library(jsonlite)
library(dplyr)
library(httr)

###############################################################################################################################
#################################################### Helpers ##################################################################

extractText <- function(x, css){
  x <- x %>% html_node(css) %>% html_text() %>% trimws()
  return(x)
}

extractAttr <- function(x, css, attr){
  x <- read_html(x) %>% html_nodes(css) %>% html_text(attr)
  return(x)
}


### inf scroll
### scroll -> compute winYoffset -> scroll -> compute new winYoffset -> compare
### if(old == new) break -> bottom of the page
scroll_inf <- function(drv){
  i <- 1
  while(TRUE){
    drv$executeScript(script = "window.scrollTo(0, document.body.scrollHeight);")
    lastHeight <- drv$executeScript(script = "return window.pageYOffset;")[[1]]
    Sys.sleep(i)
    drv$executeScript(script = "window.scrollTo(0, document.body.scrollHeight);")
    newHeight <- drv$executeScript(script = "return window.pageYOffset;")[[1]]
    Sys.sleep(i)
    if(lastHeight == newHeight){
      break()
    }
    i <- i + 0.01
  }
}


##############################################################################################################################


##############################################################################################################################
####################################################### Navigation ###########################################################
  
  firefoxProxyProfile <- makeFirefoxProfile(list(
    "network.proxy.ssl" = "proxy.insro.local", 
    "network.proxy.ssl_port" = 8080L,
    "network.proxy.type" = 1L
  ))

  
  rmdSel <- remoteDriver(remoteServerAddr = "127.0.0.1",
                         port = 4444L,
                         browserName = "firefox", 
                         extraCapabilities = firefoxProxyProfile)  

  prod_links <- list()
  
  rmdSel$open()
  rmdSel$navigate(mag_list[[1]]$url_mag)
  links_lvl1 <- read_html(rmdSel$getPageSource()[[1]]) %>%
                html_nodes(mag_list[[1]]$navigation$css_lvl1) %>%
                html_attr("href")
  for(j in links_lvl1){
    rmdSel$navigate(paste0(mag_list[[1]]$url_mag, j))
    links_lvl2 <- read_html(rmdSel$getPageSource()[[1]]) %>%
                  html_nodes(mag_list[[1]]$navigation$css_lvl2) %>%
                  html_attr("href")
    for(k in links_lvl2){
      rmdSel$navigate(paste0(mag_list[[1]]$url_mag, k))
      scroll_inf(rmdSel) 
      prod_links[[length(prod_links) + 1]] <- paste0(mag_list[[1]]$url_mag, k,
                                                     read_html(rmdSel$getPageSource()[[1]]) %>%
                                                       html_nodes(mag_list[[1]]$navigation$css_lvl3) %>%
                                                       html_attr("href"))
    }
  }
  
  
  
  links <- unique(unlist(prod_links))    
  ##############################################################################################################################
  ####################################################### Extraction ###########################################################
  
  cl <- makeCluster(3) 
  registerDoParallel(cl)
  
  clusterEvalQ(cl,{
    library(RSelenium)
    library(rvest)
    library(xml2)
    firefoxProxyProfile <- makeFirefoxProfile(list(
      "network.proxy.ssl" = "proxy.insro.local", 
      "network.proxy.ssl_port" = 8080L,
      "network.proxy.type" = 1L
    ))
    rmdSel <- remoteDriver(remoteServerAddr = "127.0.0.1",
                           port = 4444L,
                           browserName = "firefox", 
                           extraCapabilities = firefoxProxyProfile) 
  })
  
  resp <- foreach(x=1:4) %dopar%{
    rmdSel$open()
    rmdSel$navigate(links[x])
    nume <- extractAttr(rmdSel$getPageSource()[[1]], mag_list[[1]]$extraction$nume, "href")
    pret <- extractAttr(rmdSel$getPageSource()[[1]], mag_list[[1]]$extraction$pret, "href")
    pret_cant <- extractAttr(rmdSel$getPageSource()[[1]], mag_list[[1]]$extraction$pret_cant, "href")
    if(is.na(mag_list[[1]]$extraction$categorie)){
      #  categorie <-   
    } else{
      categorie <- extractAttr(rmdSel$getPageSource()[[1]], mag_list[[1]]$extraction$categorie, "href")
    }
    descriere <- extractAttr(rmdSel$getPageSource()[[1]], mag_list[[1]]$extraction$descriere, "href")
    data_extragere <- Sys.Date()
    magazin <- mag_list[[1]]$nume
    date_extrase <- list(nume = nume, 
                         pret = pret, 
                         pret_cant = pret_cant, 
                         categorie = categorie, 
                         descriere = descriere, 
                         data_extragere = data_extragere, 
                         magazin = magazin)
  }
  
  clusterEvalQ(cl, {
    rmdSel$close()
  })
  
  
  stopImplicitCluster()
  
  resp <- unlist(resp, recursive = FALSE)
  