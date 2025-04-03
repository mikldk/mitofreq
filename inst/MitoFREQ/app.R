#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(dplyr)
library(tibble)
library(mitofreq)


TLHG_choices <- mitofreq::d_helix_TLHG_freq |> pull(TLHG)
N_TLHG_all <- mitofreq::d_helix_TLHG_freq |> pull(N) |> sum()

fmt <- function(x, digits = 0, ...) {
  formatC(x, format = "f", big.mark = ",", decimal.mark = ".", digits = digits, ...)
}

extend_profile_to_helix_positions <- function(variants, tlhg) {
  # variants <- c("263G", "9150G")
  # tlhg <- "H"
  tlhg <- unique(tlhg)
  stopifnot(length(tlhg) == 1L)
  
  pos <- gsub("^([0-9]+).*$", "\\1", variants) |> as.integer()
  base <- gsub("^[0-9]+(.*)$", "\\1", variants) 
  
  d_profile <- tibble(Position = pos, 
                      Base = base, 
                      Variant = variants)
  
  d_tmp_helix_HG <- mitofreq::d_helix_SNV_freq_long |> 
    filter(TLHG == tlhg) 
  
  # Not found
  if (nrow(d_tmp_helix_HG) <= 0L) {
    return(NULL)
  }
  
  d_TLHG_info <- mitofreq::d_helix_TLHG_freq |> 
    filter(TLHG == tlhg) 
  if (nrow(d_TLHG_info) != 1L) {
    return(NULL)
  }
  TLHG_N <- d_TLHG_info |> pull(N)
  
  # Remove positions not in Helix:
  d_variants_ignored <- d_profile |> 
    anti_join(d_tmp_helix_HG, by = "Position")
  d_only_helix <- d_profile |> 
    semi_join(d_tmp_helix_HG, by = "Position")
  
  # Extend by ALL Helix positions:
  d_all_helix <- d_tmp_helix_HG |> 
    left_join(d_only_helix |> select(Position, Base, Variant), by = "Position")
  
  #d_all_helix |> print(n = Inf)
  #d_all_helix |> filter(!is.na(Base))
  
  d_all_helix_SNV_prob <- d_all_helix |> 
    mutate(Base = ifelse(is.na(Base), Ref, Base)) |> 
    mutate(n_SNV = case_when(
      Base == Ref ~ n_Ref,
      Base == Alt ~ n_Alt,
      TRUE ~ NA_real_
    )) |> 
    mutate(p_SNV = n_SNV / TLHG_N)

  return(list(
    d_profile_ext = d_all_helix_SNV_prob,
    d_variants_ignored = d_variants_ignored
  ))
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MitoFREQ"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          textInput(inputId = "selected_variants", 
                    label = "Mitogenome (variants to rCRS)",
                    value = ""),
          
          selectInput(inputId = "selected_tlhg", 
                      label = "Select TLHG", 
                      choices = TLHG_choices),
          
          hr(),
          
          h3("Examples"),
          
          a("Example 1 (H)", href = "?tlhg=H&v=263G 309.1C 309.2C 315.1C 477C 750G 1438G 3010A 4769G 8860G 9150G 9380A 15326G 16263C 16519C"),
          br(),
          a("Example 2 (T)", href = "?tlhg=T&v=73G 263G 309.1C 309.2C 315.1C 524.1A 524.2C 709A 750G 1018T 1438G 1888A 2706G 3229.1A 4216C 4769G 4917G 6152C 7028T 8697A 8703T 8860G 10192Y 10463C 10685A 11251G 11719A 12633A 13368A 14766T 14905A 15326G 15452A 15607G 15928A 16126C 16163G 16186T 16189C 16294T 16390A 16519C"),
          br(),
          a("Example 3 (J)", href = "?tlhg=J&v=73G 150T 152C 195C 215G 263G 295T 309.1C 315.1C 319C 489C 513A 750G 822R 1438G 2706G 4216C 4769G 7028T 7080C 7476T 7789A 8270T 8281- 8282- 8283- 8284- 8285- 8286- 8287- 8288- 8289- 8860G 9265R 10398G 10499G 11251G 11377A 11719A 12612G 13708A 13722G 14133G 14766T 15257A 15326G 15452A 16069T 16126C 16145A 16189C 16231C 16261T"),
          br(),
          a("Example 4 (U)", href = "?tlhg=U&v=47A 73G 150T 263G 315.1C 750G 1438G 1721T 2706G 2757G 3197C 3212T 4732G 4769G 4843T 7028T 7768G 8497G 8860G 9477A 11467G 11719A 12308G 12372A 13617C 13637G 14182C 14766T 14956C 15326G 16189C 16192T 16270T 16398A"),
          br(),
          a("Example 5 (W)", href = "?tlhg=W&v=73G 119C 189G 195C 204C 207A 263G 315.1C 709A 750G 1243C 1438G 2706G 2905R 3505G 3795T 4769G 5046A 5460A 5495C 7028T 7864T 8251A 8860G 8994A 11674T 11719A 11947G 12414C 12705T 14766T 15326G 15884C 16223T 16292T 16519C"),
          br(),
          a("Example 6 (P)", href = "?tlhg=P&v=73G 152C 263G 315.1C 523del 524del 750G 1438G 1719A 1811G 2638C 2706G 3635C 4769G 5460A 7028T 8860G 9141C 9755A 10223G 10398G 10810C 11641G 11719A 12358G 14766T 14798C 15314A 15326G 15607G 16184T 16319A 16391A"),
          
          hr(),
          
          h3("LR"),
          h4("TLHG frequency"),
          textOutput("lr_tlhg_freq"),
          h4("Rare SNV position chosen"),
          textOutput("lr_snv"),
          h4("SNV frequency"),
          textOutput("lr_snv_freq"),
          h4("Estimated population frequency"),
          textOutput("lr_popfreq"),
          h4("LR"),
          textOutput("lr")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h2("Input profile"),
          tableOutput('profile_selected'),
          
          h3("Selected TLHG"),
          textOutput("txt_selected_tlhg"),
          
          h3("Profile positions summary"),
          tableOutput('profile_position_summary'),
          
          h3("Extended profile (only non-rCRS)"),
          p("Sorted by position."),
          dataTableOutput('profile_extended_nonrcrs'),
          
          h3("Extended profile (all)"),
          p("Sorted by SNV frequency."),
          dataTableOutput('profile_extended')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query[['v']])) {
      updateTextInput(session, "selected_variants", value = query[['v']])
    }
    
    if (!is.null(query[['tlhg']])) {
      idx <- which(TLHG_choices == query[['tlhg']])
      
      if (length(idx) == 1L & idx > 0L) {
        updateSelectInput(session = session, 
                          inputId = "selected_tlhg", 
                          selected = TLHG_choices[idx])
      }
    }
  })
  
  
  reac_selected_variants <- reactive({
    req(input$selected_variants)
    
    vars <- input$selected_variants
    
    vars <- gsub(",", " ", vars, fixed = TRUE)
    vars <- gsub(" [ ]+", " ", vars)
    
    vars <- strsplit(x = vars, 
                     split = " ",
                     fixed = TRUE)[[1]]
    
    vars <- vars[nchar(vars) >= 1L]
    
    return(vars)
  })
  
  
  reac_selected_tlhg <- reactive({
    req(input$selected_tlhg)
    return(input$selected_tlhg)
  })
  
  
  
  reac_extended_result <- reactive({
    req(reac_selected_variants())
    req(reac_selected_tlhg())
    req(reac_selected_variants())
    
    vars <- reac_selected_variants()
    tlhg <- req(reac_selected_tlhg())
    
    ext_res <- extend_profile_to_helix_positions(variants = vars,
                                                 tlhg = tlhg)
    
    return(ext_res)
  })
  
  reac_extended_profile <- reactive({
    req(reac_selected_variants())
    req(reac_selected_tlhg())
    
    ext_res <- reac_extended_result()

    d_profile_ext <- ext_res$d_profile_ext
    return(d_profile_ext)
  })
  
  variants_ignored <- reactive({
    req(reac_selected_variants())
    req(reac_selected_tlhg())
    
    ext_res <- reac_extended_result()
    
    d_variants_ignored <- ext_res$d_variants_ignored
    return(d_variants_ignored)
  })
  
  rare_SNV <- reactive({
    req(reac_selected_variants())
    req(reac_selected_tlhg())
    
    ext_res <- reac_extended_result()
    
    d_profile_ext <- ext_res$d_profile_ext
    
    d_SNV <- d_profile_ext |> 
      filter(p_SNV > 0) |> 
      ungroup() |> 
      slice_min(p_SNV, n = 1, with_ties = FALSE)
    
    return(d_SNV)
  })
  
  lr_ingridients <- reactive({
    tlhg <- req(reac_selected_tlhg())
    n_tlhg <- d_helix_TLHG_freq |> filter(TLHG == tlhg) |> pull(N)
    
    d_SNV <- req(rare_SNV())
    
    tlhg_num <- n_tlhg
    tlhg_den <- N_TLHG_all
    
    snv_num <- d_SNV |> pull(n_SNV)
    snv_den <- (d_SNV |> pull(n_Ref)) + (d_SNV |> pull(n_Alt))
    
    popfreq <- (tlhg_num / tlhg_den) * (snv_num / snv_den)
    
    list(tlhg_num = tlhg_num, tlhg_den = tlhg_den,
         snv_num = snv_num, snv_den = snv_den, 
         popfreq = popfreq)
  })
  
  output$txt_selected_tlhg <- renderText({
    x <- req(reac_selected_tlhg())
    isolate(x)
  })
  
  
  output$profile_selected <- renderTable({
    req(reac_selected_tlhg())
    req(reac_selected_variants())
    
    vars <- reac_selected_variants()
    tlhg <- req(reac_selected_tlhg())

    data.frame(
      Variants = paste0(vars, collapse = " "),
      TLHG = tlhg
    )
  })
  
  output$profile_position_summary <- renderTable({
    ext_res <- req(reac_extended_result())
    vars <- reac_selected_variants()
    
    var_ignored <- ext_res$d_variants_ignored |> pull(Variant)
    var_used <- ext_res$d_profile_ext |> filter(!is.na(Variant)) |> pull(Variant)
    unaccounted <- setdiff(vars, c(var_ignored, var_used))
    
    x <- tribble(
      ~Type, ~Count, ~Variants,
      "Variants given", length(vars), vars |> paste0(collapse = ", "),
      "Ignored", length(var_ignored), var_ignored |> paste0(collapse = ", "),
      "Used", length(var_used), var_used |> paste0(collapse = ", "),
      "Variants unaccounted for", length(unaccounted), unaccounted |> paste0(collapse = ", ")
    )
    
    return(x)
  })
  
  
  # output$profile_ignored <- renderDataTable({
  #   d_variants_ignored <- req(variants_ignored()) |> 
  #     select(-Base)
  #   datatable(d_variants_ignored, rownames= FALSE)
  # })
  
  
  
  output$profile_extended_nonrcrs <- renderDataTable({
    d_profile_ext <- req(reac_extended_profile()) |> 
      
      filter(!is.na(Variant)) |> 
      
      select(Position, Ref, Alt, n_Ref, n_Alt, SNV = Variant, n = n_SNV) 
    
    datatable(d_profile_ext, rownames = FALSE)
  })
  
  
  output$profile_extended <- renderDataTable({
    d_profile_ext <- req(reac_extended_profile()) |> 
      #mutate(rCRS = ifelse(Base == Ref, "x", "")) |> 
      arrange(p_SNV) |> 
      select(Position, Ref, Alt, n_Ref, n_Alt, SNV = Variant, n = n_SNV) 
    datatable(d_profile_ext, rownames = FALSE)
  })
  
  
  output$lr_tlhg_freq <- renderText({
    tlhg <- req(reac_selected_tlhg())
    n_tlhg <- d_helix_TLHG_freq |> filter(TLHG == tlhg) |> pull(N)
    isolate(paste0("TLHG ", tlhg, " was observed ", fmt(n_tlhg), " out of a total of ", fmt(N_TLHG_all), "."))
  })
  
  output$lr_snv <- renderText({
    d_SNV <- req(rare_SNV())
    isolate(paste("Position ", d_SNV |> pull(Position) |> fmt()))
  })
  
  output$lr_snv_freq <- renderText({
    lr_res <- req(lr_ingridients())
    
    isolate(paste0("SNV was observed ", fmt(lr_res$snv_num), " out of a total of ", fmt(lr_res$snv_den), "."))
  })
  
  output$lr_popfreq <- renderText({
    lr_res <- req(lr_ingridients())

    isolate(paste0("(", 
           fmt(lr_res$tlhg_num), " / ", fmt(lr_res$tlhg_den), ") x (", 
           fmt(lr_res$snv_num), " / ", fmt(lr_res$snv_den), ") = 10^(", fmt(log10(lr_res$popfreq), digits = 4), ") = ",
           "1 : ", fmt(1 / lr_res$popfreq)))
  })
  
  output$lr <- renderText({
    lr_res <- req(lr_ingridients())
    
    isolate(paste0("1 / 10^(", fmt(log10(lr_res$popfreq), digits = 4), ") = ", fmt(1 / lr_res$popfreq)))
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
