# mega image collect
# install libs in case they are not present
install.packages(c("rvest","jsonlite"))

# load libs
library(httr)
library(rvest)
library(jsonlite)


# set url 

mega_url <- "https://www.mega-image.ro/"

# extract categories url's
mega_category_links <- html_session(mega_url) %>% read_html() %>% html_nodes(css = "div.level-container > ul > li > a") %>% html_attr("href")

params1 <- "/getSearchPageData?pageSize="

# download
mega_products_list <- lapply(mega_category_links, mega_products_test, 1000)




########## function for dl #########


mega_products_test <- function(x, pageSize = integer(1000L)){
                      url_category <- paste0(mega_url, x, params1, pageSize) 
                      response <- GET(url_category)
                      content_mega <- readBin(response$content, "text")
                      df_product <- fromJSON(content_mega, simplifyDataFrame = TRUE)
                      nume <- df_product$results$name
                      pret <- df_product$results$price$value
                      pret_cantitate <- df_product$results$price$unitPrice
                      descriere <- df_product$results$description
                      categorie <- rep(df_product$categoryName, length(nume))
                      data_dl <- rep(Sys.Date(), length(nume))
                      magazin <- rep("mega_image", length(nume))
                      df_mega <- data.frame(nume, pret, pret_cantitate, descriere, categorie, data_dl, magazin, stringsAsFactors = FALSE)
                      return(df_mega)
}


######### Reduce all df's from list #############################

mega <- Reduce(function(...) merge(..., all = TRUE), mega_products_list) 
write.csv(mega, paste0("mega_", Sys.Date(),"_.csv" ), row.names = FALSE)
