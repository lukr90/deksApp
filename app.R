library(shiny)
library(purrr)
library(dplyr)
library(stringr)
library(shinyjs)
library(rvest)
library(polite)
library(readr)

# brand <- "asus"
# type <- "mainboard"
# modell <- "maximus-ix-extreme"
# get_parts(brand, type, modell, NULL)
# get_parts(brand, type)
# 
# get_next_lvl_info(brand, type, modell, NULL)

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
get_next_lvl_info <- function(...) {
    
    #Sys.sleep(1)
    arg_list <- list(...)
    arg_list_filt <- keep(.x = arg_list, .p = ~ .x != "") 
    arguments <- map_chr(.x = arg_list_filt, .f = ~.x)
    url_temp <- str_c(c("https://www.ipc-computer.de", arguments), collapse = "/")
    url <- str_c(url_temp, "/")
    
    html_code <- rvest::read_html(url)
    
    is_parts <- html_code %>% 
        html_element("p") %>%
        html_text() %>%
        str_detect("Ersatzteile für:")
    
    if(!is_parts) {
        hrefs <- html_code %>% 
            html_elements("a") %>%
            html_attr("href")
        
        url_adjusted <- str_remove(url, "https://www.ipc-computer.de")
        pattern <- str_c("^", url_adjusted, ".+/$")
        hrefs_best_pos <- hrefs %>% str_which(pattern)
        hrefs[hrefs_best_pos] %>% str_remove(url_adjusted) %>% str_remove_all("/")   
    } else {
        return()
    }
}
get_parts <- function(...){
    
    #Sys.sleep(1)
    arg_list <- list(...)
    arg_list_filt <- keep(.x = arg_list, .p = ~ .x != "") 
    arguments <- map_chr(.x = arg_list_filt, .f = ~.x)
    url_temp <- str_c(c("https://www.ipc-computer.de", arguments), collapse = "/")
    url <- str_c(url_temp, "/")
    
    html_code <- rvest::read_html(url)
    
    is_parts <- html_code %>% 
        html_element("p") %>%
        html_text() %>%
        str_detect("Ersatzteile für:")
    
    
    if(!is_parts) {
        hrefs <- html_code %>% 
            html_elements("a") %>%
            html_attr("href")
        
        url_adjusted <- str_remove(url, "https://www.ipc-computer.de")
        pattern <- str_c("^", url_adjusted, ".+/$")
        hrefs_best_pos <- hrefs %>% str_which(pattern)
        tibble(Part = hrefs[hrefs_best_pos] %>% str_remove(url_adjusted) %>% str_remove_all("/"))
    } else {
        hrefs <- html_code %>% 
            html_elements("a") %>%
            html_attr("href")
        titles <- html_code %>% 
            html_elements("a") %>%
            html_attr("title")
        
        
        pattern <- str_c("^", url, ".+")
        hrefs_best_pos <- hrefs %>% str_which(pattern)
        
        hrefs[hrefs_best_pos]
        
        titels_with_na <- titles[hrefs_best_pos]
        tibble(Part = titels_with_na[!is.na(titels_with_na)])
    }
}

top_header = "Ersatzteile für Dekoelektro"
top_description = "Ersatzteile unter anderem für Desktop/Notebooks von Herstellern wie Acer, Asus, Dell, Fujitsu, HP, Lenovo, Medion, MSI. Durchsuche insgesamt über 1.000.000 Ersatzteile."


ui = fluidPage(
    
    column(width = 10, offset = 1,
    
    shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    
    
    shiny::tags$header(
    shiny::tags$div(id = "top_header",
                    
                    shiny::tags$h4(top_header)),
    
    shiny::tags$div(id = "top_description",
                    
                    shiny::tags$p(top_description))),
    
    column(width = 3,
    
    shiny::tags$div(id = "input",
                        
                        shiny::tags$h4("Auswahlmöglichkeiten"),

    shiny::tags$section(id = "input1",
    
                    selectizeInput(inputId = "select_brand", label = "Marke", choices = get_next_lvl_info())),
    
    shinyjs::hidden(
    
    shiny::tags$section(id = "input2",
    
                    selectizeInput(inputId = "select_type", label = "Typ", choices = NULL)),
    
    shiny::tags$section(id = "input3",
    
                    selectizeInput(inputId = "select_series", label = "Serien", choices = NULL)),
    
    shiny::tags$section(id = "input4",
    
                    selectizeInput(inputId = "select_modell", label = "Modelle", choices = NULL)),
    
    shiny::tags$section(id = "input5",

                    selectizeInput(inputId = "select_parts", label = "Zubehör", choices = NULL)))),
    
    shiny::tags$div(id = "button_section",

    actionButton(inputId = "get_tbl", label = "Tibble", icon = icon("coins")),
    
    downloadButton(outputId = "download_tbl", label = "Download"))),
    
    column(width = 9,
           
    shiny::tags$div(id = "output",
           
           shiny::tags$h4("Ergebnisse"),
           
           tableOutput(outputId = "result_table"))),
    
    shiny::tags$script(
        src = "myscript.js"),
    useShinyjs()

))

server = function(input, output, session){
    
    shinyjs::disable(id = "download_tbl")

    observeEvent(input$get_tbl, {
        shinyjs::enable(id = "download_tbl")
    })

    observeEvent(input$select_brand, {
        
        shinyjs::show(id = "input2")
        #shinyjs::disable(id = "secondInput")
        
        updateSelectizeInput(session = session,
                          inputId = "select_type",
                          label = "Typ", 
                          choices = get_next_lvl_info(input$select_brand), 
                          options = list(placeholder = "Wähle einen Typ"), selected = "NULL")
    }, ignoreInit = TRUE)

    observeEvent(input$select_type, {
        
        shinyjs::show(id = "input3")
        
        updateSelectizeInput(session = session,
                          inputId = "select_series",
                          label = "Serien",
                          choices = get_next_lvl_info(input$select_brand, input$select_type), 
                          options = list(placeholder = "Wähle eine Serie"), selected = "NULL")
    }, ignoreInit = TRUE)

    observeEvent(input$select_series, {
        
        shinyjs::show(id = "input4")
        
        updateSelectizeInput(session = session,
                          inputId = "select_modell",
                          label = "Modelle",
                          choices = get_next_lvl_info(input$select_brand, input$select_type, input$select_series), 
                          options = list(placeholder = "Wähle ein Modell"), selected = "NULL")
    }, ignoreInit = TRUE)

    observeEvent(input$select_modell, {
        
        shinyjs::show(id = "input5")
        
        updateSelectizeInput(session = session,
                          inputId = "select_parts",
                          label = "Zubehör",
                          choices = get_next_lvl_info(input$select_brand, input$select_type, input$select_series, input$select_modell), 
                          options = list(placeholder = "Wähle ein Modell"), selected = "NULL")
    }, ignoreInit = TRUE)
    
    
    result_tbl = eventReactive(input$get_tbl, {

        tbl = get_parts(input$select_brand, input$select_type, input$select_series, input$select_modell) %>%
            bind_cols(
                tibble(Brand = input$select_brand,
                       Type = input$select_type,
                       Serie = input$select_series,
                       Modell = input$select_modell)
            ) %>%
            relocate(Part, .after = last_col()) %>%
            select(where(~!all(.x == "")))

    })

    output$result_table = renderTable({
        
        result_tbl()
    })

    output$download_tbl = downloadHandler(
        filename = function(){
            paste("Ichbindurch-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file){
            write_csv2(result_tbl(), file)}
    )
}

shinyApp(ui, server)
