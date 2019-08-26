library(RSelenium)
library(rvest)
library(parallel)
library(doParallel)
library(foreach)
library(jsonlite)

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

  # driver<- rsDriver(browser=c("firefox"))
  # rmdSel <- driver[["client"]]
  # rmdSel$open()
  # rmdSel$navigate("https://www.cora.ro")
  
  rmdSel <- remoteDriver(remoteServerAddr = "127.0.0.1",
                         port = 4444L,
                         browserName = "firefox")

  prod_links <- list()
  
  rmdSel$open()
  rmdSel$navigate(mag_list[[1]]$url_mag)
  links_lvl1 <- read_html(rmdSel$getPageSource()[[1]]) %>% html_nodes(mag_list[[1]]$navigation$css_lvl1) %>% html_attr("href")
  for(j in links_lvl1){
    rmdSel$navigate(paste0(mag_list[[1]]$url_mag, j))
    links_lvl2 <- read_html(rmdSel$getPageSource()[[1]]) %>% html_nodes(mag_list[[1]]$navigation$css_lvl2) %>% html_attr("href")
    for(k in links_lvl2){
      rmdSel$navigate(paste0(mag_list[[1]]$url_mag, k))
      scroll_inf(rmdSel) 
      prod_links[[length(prod_links) + 1]] <- paste0(mag_list[[1]]$url_mag, k, read_html(rmdSel$getPageSource()[[1]]) %>%
                                                       html_nodes(mag_list[[1]]$navigation$css_lvl3) %>%
                                                       html_attr("href"))
    }
  }
  
  
  
  
  ##############################################################################################################################
  ####################################################### Extraction ###########################################################
    library(RSelenium)
    library(rvest)
    library(xml2)
    rmdSel <- remoteDriver(remoteServerAddr = "127.0.0.1",
                           port = 4444L,
                           browserName = "firefox")
    
  nume <- c()
  pret <- c()
  categorie <- c()
  descriere <- c()
  data_extragere <- c()
  magazin <- c()
  for (i in 1:length(prod_links)) {
    for (j in 1:length(prod_links[[i]])) {
    rmdSel$open()
    rmdSel$navigate(prod_links[[i]][j])
    num <- extractAttr(prod_links[[i]][j], mag_list[[1]]$extraction$nume, "href")
    pre <- extractAttr(prod_links[[i]][j], mag_list[[1]]$extraction$pret, "href")
    #pret_cant <- extractAttr(prod_links[[i]][1], mag_list[[1]]$extraction$pret_cant, "href")
    if(is.na(mag_list[[1]]$extraction$categorie)){
      #  categorie <-   
    } else{
      categ <- extractAttr(prod_links[[i]][j], mag_list[[1]]$extraction$categorie, "href")
    }
    desc <- extractAttr(prod_links[[i]][j], mag_list[[1]]$extraction$descriere, "href")
    data_extr <- Sys.Date()
    mag <- mag_list[[1]]$nume
    nume <- c(nume,num)
    pret <- c(pret,pre)
    categorie <- c(categorie,categ)
    descriere <- c(descriere,desc)
    data_extragere <- c(data_extragere,data_extr)
    magazin <- c(magazin, mag)
    rmdSel$close()
  }
}
  df <- data.frame(nume,pret,categorie, descriere, data_extragere, magazin)
    
 


write.csv(resp, paste0(mag_list$nume, ".csv"), row.names = FALSE)

set_config(use_proxy("http://proxy.insro.local", 8080))

rmdSel <- remoteDriver(remoteServerAddr = "127.0.0.1",
                       port = 4444L,
                       browserName = "firefox")

rmdSel$open()
rmdSel$navigate("https://www.cora.ro")

driver <- rsDriver(browser=c("firefox"))
driver$client$navigate("https://www.cora.ro")



links_lvl1 <- read_html(rmdSel$getPageSource()[[1]]) %>% html_nodes(mag_list[[1]]$navigation$css_lvl1) %>% html_attr("href")
library(dplyr)


driver<- rsDriver(browser=c("firefox"))
rmdSel <- driver[["client"]]
rmdSel$open()
rmdSel$navigate("https://www.cora.ro")
