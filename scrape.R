brand <- "Lenovo"
type <- "Desktop"
series <- "desktop-6x-serie"
modell <- "62-desktop-2122"


scrape_page <- function(...) {
  
  Sys.sleep(1)
  # create url
  arguments <- map_chr(.x = list(...), .f = ~.x)
  url_temp <- str_c(c("https://www.ipc-computer.de", arguments), collapse = "/")
  url <- str_c(url_temp, "/")
  
  # identify page type
  html_code <- rvest::read_html(url)
  is_parts <- html_code %>% 
    html_element("p") %>%
    html_text() %>%
    str_detect("Ersatzteile für:")
  
  # use correct function
  if(is_parts) {
    get_parts(url)
  } else {
    get_next_lvl_info(url)
  }
}
get_next_lvl_info <- function(url) {
  
  html_code <- rvest::read_html(url)
  hrefs <- html_code %>% 
    html_elements("a") %>%
    html_attr("href")
  
  url_adjusted <- str_remove(url, "https://www.ipc-computer.de")
  pattern <- str_c("^", url_adjusted, ".+/$")
  hrefs_best_pos <- hrefs %>% str_which(pattern)
  hrefs[hrefs_best_pos] %>% str_remove(url_adjusted) %>% str_remove_all("/")
}
get_parts <- function(url){
  
  html_code <- rvest::read_html(url)
  hrefs <- html_code %>% 
    html_elements("a") %>%
    html_attr("href")
  titles <- html_code %>% 
    html_elements("a") %>%
    html_attr("title")
  
  
  pattern <- str_c("^", url, ".+")
  hrefs_best_pos <- hrefs %>% str_which(pattern)
  titles[hrefs_best_pos]
}


scrape_page("dell", "desktop", "chromebox-serie", "chromebox-3010")


brand <- get_next_lvl_info()[[1]]
type <- get_next_lvl_info(brand)
series <- get_next_lvl_info(brand, type)[[1]]
modell <- get_next_lvl_info(brand, type, series)[[1]]
modell2 <- get_next_lvl_info(brand, type, series, modell, modell2)

parts <- get_parts(brand, type, series, modell)


url <- "https://www.ipc-computer.de/acer/aio/aspire-aio-serie/"

create_url <- function(...){
  arguments_list <- list(...)
  arguments <- map_chr(.x = arguments_list, .f = ~str_replace_all(str_to_lower(.x), " ", "-"))
  url_temp <- str_c(c("https://www.ipc-computer.de", arguments), collapse = "/")
  str_c(url_temp, "/")
}
get_brands <- function() {
  
  Sys.sleep(1)
  url <- create_url()
  html_code <- rvest::read_html(url)
  hrefs <- html_code %>% 
    html_elements("a") %>%
    html_attr("href")
  
  url_adjusted <- str_remove(url, "https://www.ipc-computer.de")
  pattern <- str_c("^", url_adjusted, ".+/$")
  hrefs_best_pos <- hrefs %>% str_which(pattern)
  hrefs[hrefs_best_pos] %>% str_remove(url_adjusted) %>% str_remove_all("/")
}
get_type_per_brand <- function(brand) {
  Sys.sleep(1)
  url <- create_url(brand)
  html_code <- rvest::read_html(url)
  hrefs <- html_code %>% 
    html_elements("a") %>%
    html_attr("href")
  
  url_adjusted <- str_remove(url, "https://www.ipc-computer.de")
  pattern <- str_c("^", url_adjusted, ".+/$")
  hrefs_best_pos <- hrefs %>% str_which(pattern)
  hrefs[hrefs_best_pos] %>% str_remove(url_adjusted) %>% str_remove_all("/")
}
get_series_per_type <- function(brand, type) {
  Sys.sleep(1)
  url <- create_url(brand, type)
  html_code <- rvest::read_html(url)
  hrefs <- html_code %>% 
    html_elements("a") %>%
    html_attr("href")
  
  url_adjusted <- str_remove(url, "https://www.ipc-computer.de")
  pattern <- str_c("^", url_adjusted, ".+/$")
  hrefs_best_pos <- hrefs %>% str_which(pattern)
  hrefs[hrefs_best_pos] %>% str_remove(url_adjusted) %>% str_remove_all("/")
  
}
get_modell_per_series <- function(brand, type, series) {
  Sys.sleep(1)
  url <- create_url(brand, type, series)
  html_code <- rvest::read_html(url)
  hrefs <- html_code %>% 
    html_elements("a") %>%
    html_attr("href")
  
  url_adjusted <- str_remove(url, "https://www.ipc-computer.de")
  pattern <- str_c("^", url_adjusted, ".+/$")
  hrefs_best_pos <- hrefs %>% str_which(pattern)
  hrefs[hrefs_best_pos] %>% str_remove(url_adjusted) %>% str_remove_all("/")
  
}
get_parts_per_modell <- function(brand, type, series, modell){
  Sys.sleep(1)
  url <- create_url(brand, type, series, modell)
  html_code <- rvest::read_html(url)
  hrefs <- html_code %>% 
    html_elements("a") %>%
    html_attr("href")
  titles <- html_code %>% 
    html_elements("a") %>%
    html_attr("title")
  
  pattern <- str_c("^", url, ".+")
  hrefs_best_pos <- hrefs %>% str_which(pattern)
  titles[hrefs_best_pos]
}

pattern <- str_c("^", "ab", ".+")
test <- c("ab", "abc")
str_which(test, pattern)