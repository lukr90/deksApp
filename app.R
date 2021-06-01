library(shiny)
library(purrr)
library(dplyr)
library(stringr)
library(shinyjs)
library(rvest)
library(polite)
library(readr)
library(reactable)

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
        str_detect("Ersatzteile fÃ¼r:")
    
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

top_header = "spare parts for deko.elektro"
top_description = "Spare parts for desktop/notebooks from manufacturers such as Acer, Asus, Dell, Fujitsu, HP, Lenovo, Medion, MSI, among others. Search a total of over 1,000,000 spare parts."


ui = fluidPage(
    
    tags$header(class = "head_container",
        tags$div(class = "title_container",
        tags$a("company.name", href = "#")),
        
        tags$div(class = "nav_container",
        tags$nav(
            tags$ul(
                
                tags$div(class = "dropdown",
                
                tags$li(tags$a("Projects", href = "#")),
                
                tags$div(class = "dropdown_content",
                         
                tags$a("Project one", href = "#"),
                tags$a("Project two", href = "#"),
                tags$a("Project three", href = "#"))
                ),
                
                tags$li(tags$a("People", href = "#")),
                tags$li(tags$a("Philosophy", href = "#")),
                
                tags$div(class = "dropdown",
                
                tags$li(tags$a("Ps & Pbes", href = "#")),
                
                tags$div(class = "dropdown_content",
                
                tags$a("Ps", href = "#"),
                tags$a("Pbes", href = "#"))
                )
        ))),
        
        tags$div(class = "contact_container",
            tags$button("Contact", class = "custom_btn contact_btn"))
    ),
    
    tags$section(class = "demo_section",
        tags$div(class = "demo_section_head",
        tags$h4("Demo project:"),
        tags$h5(top_header)),
        tags$div(class = "demo_section_desc",
        tags$p(top_description)),
        
    tags$div(class = "demo_section_fraction",
        
    tags$div(class = "demo_section_inputs",

    selectizeInput(inputId = "select_brand", label = "Brand", choices = get_next_lvl_info(), options = list(items = NULL, placeholder = "select a brand")),

    selectizeInput(inputId = "select_type", label = "Typs", choices = NULL, options = list(placeholder = "select a brand first")),

    selectizeInput(inputId = "select_series", label = "Serieses", choices = NULL, options = list(placeholder = "select a brand and type first")),

    selectizeInput(inputId = "select_modell", label = "Modells", choices = NULL, options = list(placeholder = "select a brand, type and series first")),

    selectizeInput(inputId = "select_parts", label = "Parts", choices = NULL, options = list(placeholder = "select a brand, type, series and modell first")),
    
    actionButton(inputId = "get_tbl", label = "Tibble", icon = icon("coins")),
    
    downloadButton(outputId = "download_tbl", label = "Download")),
    
    tags$div(class = "demo_section_ouputs",
        
        shinyjs::hidden(
        tags$h5("results", class = "table_title", id = "table_title")),
        reactableOutput(outputId = "result_table"))),
    
    tags$div(class = "demo_section_footer",
        tags$a("more details about the project here", href = "#"))),
    
    shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    shiny::tags$script(
        src = "myscript.js"),
    useShinyjs()

)

server = function(input, output, session){
    
    shinyjs::disable(id = "download_tbl")

    observeEvent(input$get_tbl, {
        shinyjs::enable(id = "download_tbl")
        shinyjs::show(id = "table_title")
    })

    observeEvent(input$select_brand, {

        updateSelectizeInput(session = session,
                          inputId = "select_type",
                          label = "Types", 
                          choices = get_next_lvl_info(input$select_brand), 
                          options = list(placeholder = "select a type", items = NULL))
    }, ignoreInit = TRUE)

    observeEvent(input$select_type, {
        
        updateSelectizeInput(session = session,
                          inputId = "select_series",
                          label = "Serieses",
                          choices = get_next_lvl_info(input$select_brand, input$select_type), 
                          options = list(placeholder = "select a series", items = NULL))
    }, ignoreInit = TRUE)

    observeEvent(input$select_series, {
        
        updateSelectizeInput(session = session,
                          inputId = "select_modell",
                          label = "Modells",
                          choices = get_next_lvl_info(input$select_brand, input$select_type, input$select_series), 
                          options = list(placeholder = "select a modell", items = NULL))
    }, ignoreInit = TRUE)

    observeEvent(input$select_modell, {
        
        updateSelectizeInput(session = session,
                          inputId = "select_parts",
                          label = "Parts",
                          choices = get_next_lvl_info(input$select_brand, input$select_type, input$select_series, input$select_modell), 
                          options = list(placeholder = "select a part", items = NULL))
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

    output$result_table = renderReactable({
        
        result_tbl() %>%
            reactable(data = .,
                      height = 300,
                      filterable = TRUE, 
                      showPageSizeOptions = TRUE, 
                      showSortable = TRUE, 
                      showSortIcon = TRUE, 
                      resizable = TRUE, 
                      defaultColDef = colDef(headerClass = "table_header"))
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
