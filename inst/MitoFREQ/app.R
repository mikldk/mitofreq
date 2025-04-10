library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(DT)
library(dplyr)
library(tibble)
library(readxl)


# Run locally:
# git commit -am "Shiny app updated, ready for deployment" && git push
# remotes::install_github('mikldk/mitofreq')
# rsconnect::deployApp("inst/MitoFREQ")
library(mitofreq)


TLHG_choices <- mitofreq::d_helix_TLHG_freq |> pull(TLHG)

fmt <- function(x, digits = 0, ...) {
  formatC(x, format = "f", big.mark = ",", decimal.mark = ".", digits = digits, ...)
}


ui <- navbarPage("MitoFREQ",
                 
                 tabPanel("Input",
                          
                          column(4,
                                 
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
                                 br(),
                                 a("Example 7 (H)", href = "?tlhg=H&v=21AT 263G 309.1C"),
                                 br(),
                                 a("Example 8 (J)", href = "?tlhg=J&v=23C 73G 150T 152C 195C 215G 263G 295T 309.1C 315.1C 319C 489C 513A 750G 822R 1438G 2706G 4216C 4769G 7028T 7080C 7476T 7789A 8270T 8281- 8282- 8283- 8284- 8285- 8286- 8287- 8288- 8289- 8860G 9265R 10398G 10499G 11251G 11377A 11719A 12612G 13708A 13722G 14133G 14766T 15257A 15326G 15452A 16069T 16126C 16145A 16189C 16231C 16261T"),
                                 br(),
                                 a("Example 9 (H1c full)", href = "?tlhg=H&range=1-16569&v=93G 263G 315.1C 477C 750G 1438G 3010A 4769G 8860G 15326G 16519C"),
                                 br(),
                                 a("Example 10 (H1c CR)", href = "?tlhg=H&range=16024-576&v=93G 263G 315.1C 477C 16519C")
                                 
                                 
                          ),
                          
                          column(8,
                                 
                                 h3("Profile"),
                                 
                                 textInput(inputId = "selected_variants", 
                                           label = "Mitogenome (variants to rCRS)",
                                           value = ""),
                                 
                                 textInput(inputId = "selected_range", 
                                           label = "Range",
                                           value = "1-16569"),
                                 
                                 textInput(inputId = "selected_range_exclusions", 
                                           label = "Range exclusions",
                                           value = ""),
                                 
                                 selectInput(inputId = "selected_tlhg", 
                                             label = "Select TLHG", 
                                             choices = TLHG_choices),
                                 
                                 # fileInput(inputId = "custom_TLHG_freq2", 
                                 #           label = "Custom TLHG distribution", 
                                 #           accept = ".xlsx"),
                                 
                                 uiOutput(outputId = "custom_TLHG_freq_output"),
                                 
                                 tags$a(href = "TLHG_freq.xlsx", 
                                        class = "btn btn-primary", 
                                        "Download template (Excel)", 
                                        download = NA, 
                                        target = "_blank"),
                                 
                                 actionButton('reset_custom_TLHG_freq', 'Reset TLHG distribution'),
                                 
                                 
  
                          )
                          
                 ),
                 
                 
                 
                 tabPanel("Results",
                          
                          fluidRow(
                            column(12,
                                   h3("Input profile"),
                                   tableOutput('profile_selected')
                            )
                          ),
                          
                          fluidRow(
                            column(6,
                                   h1("HelixMTdb"),
                                   
                                   h2("LR"),
                                   
                                   h4("TLHG frequency"),
                                   textOutput("helix_lr_tlhg_freq"),
                                   h4("Rare SNV position chosen"),
                                   textOutput("helix_lr_snv"),
                                   h4("SNV frequency"),
                                   textOutput("helix_lr_snv_freq"),
                                   h4("Estimated population frequency"),
                                   textOutput("helix_lr_popfreq"),
                                   
                            ),
                            
                            
                            column(6,
                                   h1("GnomAD"),
                                   
                                   
                                   h2("LR"),
                                   
                                   h4("TLHG frequency"),
                                   textOutput("gnomAD_lr_tlhg_freq"),
                                   h4("Rare SNV position chosen"),
                                   textOutput("gnomAD_lr_snv"),
                                   h4("SNV frequency"),
                                   textOutput("gnomAD_lr_snv_freq"),
                                   h4("Estimated population frequency"),
                                   textOutput("gnomAD_lr_popfreq"),
                                   

                            ),
                          ),
                          
                          
                          
                          fluidRow(
                            column(6,
                                   
                                   h4("LR"),
                                   bslib::value_box(
                                     title = "LR",
                                     value = textOutput("helix_lr"),
                                     showcase = bsicons::bs_icon("calculator-fill"),
                                     theme = "primary"
                                   ),
                            ),
                            
                            column(6,
                                   h4("LR"),
                                   bslib::value_box(
                                     title = "LR",
                                     value = textOutput("gnomAD_lr"),
                                     showcase = bsicons::bs_icon("calculator-fill"),
                                     theme = "primary"
                                   ),
                            )
                          ),
                          
                          
                          
                          
                          fluidRow(
                            column(6,
                                   h2("Details"),
                                   
                                   h4("Profile positions summary"),
                                   tableOutput('helix_profile_position_summary'),
                            ),
                            
                            column(6,
                                   h2("Details"),
                                   
                                   h4("Profile positions summary"),
                                   tableOutput('gnomAD_profile_position_summary'),
                            )
                          ),
                          
                          
                          fluidRow(
                            column(6,
                                   h4("Extended profile (only non-rCRS)"),
                                   p("Sorted by position."),
                                   dataTableOutput('helix_profile_extended_nonrcrs'),
                            ),
                            
                            column(6,
                                   h4("Extended profile (only non-rCRS)"),
                                   p("Sorted by position."),
                                   dataTableOutput('gnomAD_profile_extended_nonrcrs'),
                            )
                          ),
                          

                          
                          
                          fluidRow(
                            column(6,
                                   h4("Extended profile (all)"),
                                   p("Sorted by SNV frequency."),
                                   dataTableOutput('helix_profile_extended')
                            ),
                            
                            column(6,
                                   h4("Extended profile (all)"),
                                   p("Sorted by SNV frequency."),
                                   dataTableOutput('gnomAD_profile_extended')
                            )
                          ),
                 ),
                 
                 tabPanel("Notes",
                          
                          column(6,
                                 
                                 h3("Data source"),
                                 
                                 h4("HelixMTdb"),
                                 tags$ul(
                                   tags$li("HelixMTdb data source: ", 
                                           tags$a("Bolze et al. (2020). \"A catalog of homoplasmic and heteroplasmic mitochondrial DNA variants in humans\"", href = "https://www.biorxiv.org/content/10.1101/798264v3")),
                                   tags$li("HelixMTdb datasets made available via the R package \"", 
                                           tags$a("mitofreq", href = "https://github.com/mikldk/mitofreq"), 
                                           "\":"),
                                   tags$ul(
                                     tags$li("TLHG distribution: ", tags$code("d_helix_TLHG_freq"), " (write ", tags$code("?d_helix_TLHG_freq"), " for documentation)"),
                                     tags$li("Raw data on homoplasmic variants: ", tags$code("d_helix"), " (write ", tags$code("?d_helix"), " for documentation)"),
                                     tags$li("SNV frequencies (after exclusion cf. below): ", tags$code("d_helix_refined_long"), " (write ", tags$code("?d_helix_refined_long"), " for documentation)")
                                   )
                                 ),
                                 
                                 
                                 h4("GnomAD"),
                                 tags$ul(
                                   tags$li(tags$a("gnomAD v3.1 Mitochondrial DNA Variants:", href = "https://gnomad.broadinstitute.org/news/2020-11-gnomad-v3-1-mitochondrial-dna-variants/")),
                                   tags$li("GnomAD datasets made available via the R package \"", 
                                           tags$a("mitofreq", href = "https://github.com/mikldk/mitofreq"), 
                                           "\":"),
                                   tags$ul(
                                     tags$li("TLHG distribution: ", tags$code("d_gnomAD_TLHG_freq"), " (write ", tags$code("?d_gnomAD_TLHG_freq"), " for documentation)"),
                                     tags$li("Raw data on homoplasmic variants: ", tags$code("d_gnomAD"), " (write ", tags$code("?d_gnomAD"), " for documentation)"),
                                     tags$li("SNV frequencies (after exclusion cf. below): ", tags$code("d_gnomAD_refined_long"), " (write ", tags$code("?d_gnomAD_refined_long"), " for documentation)")
                                   )
                                 )
                                 
                                
                          ),
                          
                          column(6,
                                 
                                 h3("TLHG frequencies"),
                                 tags$ul(
                                   tags$li("TLHG frequency is based on distribution from Helix unless a custom is provided.")
                                 ),
                                 
                                 h3("SNV exclusion criteria notes"),
                                 tags$ul(
                                   tags$li("SNVs with more than one reference were excluded."),
                                   tags$ul(
                                     tags$li("Exclusions can be summarised by ", tags$code("d_helix |> count(ExcludeReason)"), ".")
                                   ),
                                   tags$li("SNVs must have been seen at least twice (globally, not within TLHG)."),
                                   tags$ul(
                                     tags$li("Try e.g. Example 8 where the TLHG frequency is only 1."),
                                     tags$li("More details can be found in R: ", tags$code("d_helix_refined_long |> filter(n == 1L)"), ".")
                                   )
                                 )
                                 
                          )
                 )
                 
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #rv <- reactiveValues(error_count = 0)
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query[['v']])) {
      updateTextInput(session, "selected_variants", value = query[['v']])
    }
    
    if (!is.null(query[['range']])) {
      updateTextInput(session, "selected_range", value = query[['range']])
    }
    
    if (!is.null(query[['rangeex']])) {
      updateTextInput(session, "selected_range_exclusions", value = query[['rangeex']])
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
    
    
    pos <- mitofreq::positions_from_variants(vars)
    bases <- mitofreq::bases_from_variants(vars)
    bases <- gsub("[^A-Z]", "", bases)
    
    if (!all(pos >= 1L & pos <= 16569)) {
      #rv$error_count <- rv$error_count + 1
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Positions must be between 1 and 16,569.",
        type = "error"
      )
    }
    
    return(vars)
  })
  
  reac_selected_range <- reactive({
    req(input$selected_range)
    
    range <- input$selected_range
    range <- mitofreq::parse_range(range)
    
    range_ex <- input$selected_range_exclusions
    range_ex <- mitofreq::parse_range(range_ex)
    
    range <- setdiff(range, range_ex)
    
    if (any(is.na(range))) {
      #rv$error_count <- rv$error_count + 1
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Range contains invalid characters. Only digits, commas, and hyphens are allowed.",
        type = "error"
      )
    }
    
    return(range)
  })
  
  
  reac_selected_tlhg <- reactive({
    req(input$selected_tlhg)
    return(input$selected_tlhg)
  })
  
  
  
  values <- reactiveValues(
    upload_state = NULL
  )
  
  
  reset_trigger <- reactiveVal(0)
  observe({
    reset_trigger(isolate(reset_trigger()) + 1)
  })
  
  observeEvent(input$custom_TLHG_freq, {
    values$upload_state <- 'uploaded'
  })
  
  observeEvent(input$reset_custom_TLHG_freq, {
    values$upload_state <- 'reset'
    reset_trigger(reset_trigger() + 1)
  })
  
  
  output$custom_TLHG_freq_output <- renderUI({
    req(reset_trigger())
    
    fileInput(inputId = "custom_TLHG_freq", 
              label = "Custom TLHG distribution", 
              accept = ".xlsx")
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
  
  ##############################################################################
  # HELIX START
  ##############################################################################
  
  helix_reac_get_selected_tlhg_dist <- reactive({
    if (is.null(values$upload_state)) {
      return(d_helix_TLHG_freq)
    } else if (values$upload_state == 'reset') {
      return(d_helix_TLHG_freq)
    } 
    
    # else if (values$upload_state == 'uploaded') 
    
    d <- tryCatch({
      readxl::read_excel(input$custom_TLHG_freq$datapath) |> as_tibble()
    }, warning = function(w) {
      d_helix_TLHG_freq
    }, error = function(e) {
      d_helix_TLHG_freq
    })
    
    if (!isTRUE(all.equal(colnames(d), c("TLHG", "N")))) {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Column names must be TLHG and N.",
        type = "error"
      )
      return(d_helix_TLHG_freq)
    } 
    
    if (!isTRUE(all.equal(d$TLHG, c("A", "C", "D", "E", "F", "G", "H", "HV", "I", "J", "K", "L0", 
                                    "L1", "L2", "L3", "L4-6", "M", "N", "O", "P", "Q", "R/B", "S", 
                                    "T", "U", "V", "W", "X", "Y", "Z")))) {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "TLHGs (A, C, D, ...) must follow template.",
        type = "error"
      )
      
      return(d_helix_TLHG_freq)
    } 
    
    return(d)
  })
  
  helix_reac_extended_result <- reactive({
    req(reac_selected_variants())
    req(reac_selected_tlhg())
    req(reac_selected_range())
    
    vars <- reac_selected_variants()
    tlhg <- req(reac_selected_tlhg())
    range <- reac_selected_range()
    
    ext_res <- mitofreq::extend_profile(
      variants = vars, 
      tlhg = tlhg,
      range = range, 
      d_SNV_long = d_helix_refined_long
    )
    
    return(ext_res)
  })
  
  helix_reac_extended_profile <- reactive({
    req(reac_selected_variants())
    req(reac_selected_tlhg())
    
    ext_res <- helix_reac_extended_result()
    
    d_profile_ext <- ext_res$d_profile_ext
    return(d_profile_ext)
  })
  
  helix_rare_SNV <- reactive({
    req(reac_selected_variants())
    req(reac_selected_tlhg())
    
    ext_res <- helix_reac_extended_result()
    
    d_profile_ext <- ext_res$d_profile_ext
    
    d_SNV <- d_profile_ext |> 
      filter(p_Base > 0) |> 
      ungroup() |> 
      slice_min(p_Base, n = 1, with_ties = FALSE)
    
    return(d_SNV)
  })
  
  helix_lr_ingridients <- reactive({
    tlhg <- req(reac_selected_tlhg())
    d_tlhg_dist <- req(helix_reac_get_selected_tlhg_dist())
    
    n_tlhg <- d_tlhg_dist |> filter(TLHG == tlhg) |> pull(N)
    
    d_SNV <- req(helix_rare_SNV())
    
    tlhg_num <- n_tlhg
    tlhg_den <- d_tlhg_dist |> pull(N) |> sum()
    
    snv_num <- d_SNV |> pull(n_Base)
    snv_den <- d_SNV |> pull(N_TLHG)
    
    popfreq <- (tlhg_num / tlhg_den) * (snv_num / snv_den)
    
    list(tlhg_num = tlhg_num, tlhg_den = tlhg_den,
         snv_num = snv_num, snv_den = snv_den, 
         popfreq = popfreq)
  })
  
  
  
  output$helix_profile_position_summary <- renderTable({
    ext_res <- req(helix_reac_extended_result())
    vars <- reac_selected_variants()
    
    var_ignored_range <- ext_res$d_variants_ignored_range |> pull(Variant)
    var_ignored_helix <- ext_res$d_variants_ignored |> pull(Variant)
    var_used <- ext_res$d_profile_ext |> 
      filter(BaseType == "Alt") |> 
      mutate(Variant = paste0(Position, Profile)) |> 
      pull(Variant)

    unaccounted <- setdiff(mitofreq::positions_from_variants(vars), 
                           mitofreq::positions_from_variants(c(var_ignored_range, var_ignored_helix, var_used)))
    
    x <- tribble(
      ~Type, ~Count, ~Positions,
      "Variants given", length(vars), vars |> paste0(collapse = ", "),
      "Positions ignored (range)", length(var_ignored_range), var_ignored_range |> paste0(collapse = ", "),
      "Positions ignored (no Helix info)", length(var_ignored_helix), var_ignored_helix |> paste0(collapse = ", "),
      "Positions used", length(var_used), var_used |> paste0(collapse = ", "),
      "Positions unaccounted for", length(unaccounted), unaccounted |> paste0(collapse = ", ")
    )
    
    return(x)
  })
  
  
  
  
  
  output$helix_profile_extended_nonrcrs <- renderDataTable({
    d_profile_ext <- req(helix_reac_extended_profile()) |> 
      filter(BaseType == "Alt") |> 
      select(Position, BaseType, Ref, Profile, N_TLHG, n_Base, p_Base) 
    
    datatable(d_profile_ext, rownames = FALSE)
  })
  
  
  output$helix_profile_extended <- renderDataTable({
    d_profile_ext <- req(helix_reac_extended_profile()) |> 
      #mutate(rCRS = ifelse(Base == Ref, "x", "")) |> 
      arrange(p_Base) |> 
      select(Position, BaseType, Ref, Profile, N_TLHG, n_Base, p_Base) 
    
    datatable(d_profile_ext, rownames = FALSE)
  })
  
  
  output$helix_lr_tlhg_freq <- renderText({
    tlhg <- req(reac_selected_tlhg())
    d_tlhg_dist <- req(helix_reac_get_selected_tlhg_dist())
    
    n_tlhg <- d_tlhg_dist |> filter(TLHG == tlhg) |> pull(N)
    N_TLHG_all <- d_tlhg_dist |> pull(N) |> sum()
    
    isolate(paste0("TLHG ", tlhg, " was observed ", fmt(n_tlhg), " out of a total of ", fmt(N_TLHG_all), "."))
  })
  
  output$helix_lr_snv <- renderText({
    d_SNV <- req(helix_rare_SNV())
    isolate(d_SNV |> mutate(Var = paste0(Position, Profile)) |> pull(Var))
  })
  
  output$helix_lr_snv_freq <- renderText({
    lr_res <- req(helix_lr_ingridients())
    d_SNV <- req(helix_rare_SNV())
    tlhg <- req(reac_selected_tlhg())
    var <- d_SNV |> mutate(Var = paste0(Position, Profile)) |> pull(Var)
    
    isolate(paste0(var, " was observed ", fmt(lr_res$snv_num), " out of a total of ", fmt(lr_res$snv_den), " in TLHG ", tlhg, "."))
  })
  
  output$helix_lr_popfreq <- renderText({
    lr_res <- req(helix_lr_ingridients())
    
    isolate(paste0("(", 
                   fmt(lr_res$tlhg_num), " / ", fmt(lr_res$tlhg_den), ") x (", 
                   fmt(lr_res$snv_num), " / ", fmt(lr_res$snv_den), ") = 10^(", fmt(log10(lr_res$popfreq), digits = 4), ") = ",
                   "1 : ", fmt(1 / lr_res$popfreq)))
  })
  
  output$helix_lr <- renderText({
    lr_res <- req(helix_lr_ingridients())
    
    isolate(paste0("1 / 10^(", fmt(log10(lr_res$popfreq), digits = 4), ") = ", fmt(1 / lr_res$popfreq)))
  })
  
  ##############################################################################
  # HELIX END
  ##############################################################################
  
  
  
  
  
  
  
  
  
  
  ##############################################################################
  # GNOMAD START
  ##############################################################################
  
  gnomAD_reac_get_selected_tlhg_dist <- reactive({
    if (is.null(values$upload_state)) {
      return(d_gnomAD_TLHG_freq)
    } else if (values$upload_state == 'reset') {
      return(d_gnomAD_TLHG_freq)
    } 
    
    # else if (values$upload_state == 'uploaded') 
    
    d <- tryCatch({
      readxl::read_excel(input$custom_TLHG_freq$datapath) |> as_tibble()
    }, warning = function(w) {
      d_gnomAD_TLHG_freq
    }, error = function(e) {
      d_gnomAD_TLHG_freq
    })
    
    if (!isTRUE(all.equal(colnames(d), c("TLHG", "N")))) {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Column names must be TLHG and N.",
        type = "error"
      )
      return(d_gnomAD_TLHG_freq)
    } 
    
    if (!isTRUE(all.equal(d$TLHG, c("A", "C", "D", "E", "F", "G", "H", "HV", "I", "J", "K", "L0", 
                                    "L1", "L2", "L3", "L4-6", "M", "N", "O", "P", "Q", "R/B", "S", 
                                    "T", "U", "V", "W", "X", "Y", "Z")))) {
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "TLHGs (A, C, D, ...) must follow template.",
        type = "error"
      )
      
      return(d_gnomAD_TLHG_freq)
    } 
    
    return(d)
  })
  
  gnomAD_reac_extended_result <- reactive({
    req(reac_selected_variants())
    req(reac_selected_tlhg())
    req(reac_selected_range())
    
    vars <- reac_selected_variants()
    tlhg <- req(reac_selected_tlhg())
    range <- reac_selected_range()
    
    ext_res <- mitofreq::extend_profile(
      variants = vars, 
      tlhg = tlhg,
      range = range, 
      d_SNV_long = d_gnomAD_refined_long
    )
    
    return(ext_res)
  })
  
  gnomAD_reac_extended_profile <- reactive({
    req(reac_selected_variants())
    req(reac_selected_tlhg())
    
    ext_res <- gnomAD_reac_extended_result()
    
    d_profile_ext <- ext_res$d_profile_ext
    return(d_profile_ext)
  })
  
  gnomAD_rare_SNV <- reactive({
    req(reac_selected_variants())
    req(reac_selected_tlhg())
    
    ext_res <- gnomAD_reac_extended_result()
    
    d_profile_ext <- ext_res$d_profile_ext
    
    d_SNV <- d_profile_ext |> 
      filter(p_Base > 0) |> 
      ungroup() |> 
      slice_min(p_Base, n = 1, with_ties = FALSE)
    
    return(d_SNV)
  })
  
  gnomAD_lr_ingridients <- reactive({
    tlhg <- req(reac_selected_tlhg())
    d_tlhg_dist <- req(gnomAD_reac_get_selected_tlhg_dist())
    
    n_tlhg <- d_tlhg_dist |> filter(TLHG == tlhg) |> pull(N)
    
    d_SNV <- req(gnomAD_rare_SNV())
    
    tlhg_num <- n_tlhg
    tlhg_den <- d_tlhg_dist |> pull(N) |> sum()
    
    snv_num <- d_SNV |> pull(n_Base)
    snv_den <- d_SNV |> pull(N_TLHG)
    
    popfreq <- (tlhg_num / tlhg_den) * (snv_num / snv_den)
    
    list(tlhg_num = tlhg_num, tlhg_den = tlhg_den,
         snv_num = snv_num, snv_den = snv_den, 
         popfreq = popfreq)
  })
  
  
  
  output$gnomAD_profile_position_summary <- renderTable({
    ext_res <- req(gnomAD_reac_extended_result())
    vars <- reac_selected_variants()
    
    var_ignored_range <- ext_res$d_variants_ignored_range |> pull(Variant)
    var_ignored_gnomAD <- ext_res$d_variants_ignored |> pull(Variant)
    var_used <- ext_res$d_profile_ext |> 
      filter(BaseType == "Alt") |> 
      mutate(Variant = paste0(Position, Profile)) |> 
      pull(Variant)
    
    unaccounted <- setdiff(mitofreq::positions_from_variants(vars), 
                           mitofreq::positions_from_variants(c(var_ignored_range, var_ignored_gnomAD, var_used)))
    
    x <- tribble(
      ~Type, ~Count, ~Positions,
      "Variants given", length(vars), vars |> paste0(collapse = ", "),
      "Positions ignored (range)", length(var_ignored_range), var_ignored_range |> paste0(collapse = ", "),
      "Positions ignored (no gnomAD info)", length(var_ignored_gnomAD), var_ignored_gnomAD |> paste0(collapse = ", "),
      "Positions used", length(var_used), var_used |> paste0(collapse = ", "),
      "Positions unaccounted for", length(unaccounted), unaccounted |> paste0(collapse = ", ")
    )
    
    return(x)
  })
  
  
  
  
  
  output$gnomAD_profile_extended_nonrcrs <- renderDataTable({
    d_profile_ext <- req(gnomAD_reac_extended_profile()) |> 
      filter(BaseType == "Alt") |> 
      select(Position, BaseType, Ref, Profile, N_TLHG, n_Base, p_Base) 
    
    datatable(d_profile_ext, rownames = FALSE)
  })
  
  
  output$gnomAD_profile_extended <- renderDataTable({
    d_profile_ext <- req(gnomAD_reac_extended_profile()) |> 
      #mutate(rCRS = ifelse(Base == Ref, "x", "")) |> 
      arrange(p_Base) |> 
      select(Position, BaseType, Ref, Profile, N_TLHG, n_Base, p_Base) 
    
    datatable(d_profile_ext, rownames = FALSE)
  })
  
  
  output$gnomAD_lr_tlhg_freq <- renderText({
    tlhg <- req(reac_selected_tlhg())
    d_tlhg_dist <- req(gnomAD_reac_get_selected_tlhg_dist())
    
    n_tlhg <- d_tlhg_dist |> filter(TLHG == tlhg) |> pull(N)
    N_TLHG_all <- d_tlhg_dist |> pull(N) |> sum()
    
    isolate(paste0("TLHG ", tlhg, " was observed ", fmt(n_tlhg), " out of a total of ", fmt(N_TLHG_all), "."))
  })
  
  output$gnomAD_lr_snv <- renderText({
    d_SNV <- req(gnomAD_rare_SNV())
    isolate(d_SNV |> mutate(Var = paste0(Position, Profile)) |> pull(Var))
  })
  
  output$gnomAD_lr_snv_freq <- renderText({
    lr_res <- req(gnomAD_lr_ingridients())
    d_SNV <- req(gnomAD_rare_SNV())
    tlhg <- req(reac_selected_tlhg())
    var <- d_SNV |> mutate(Var = paste0(Position, Profile)) |> pull(Var)
    
    isolate(paste0(var, " was observed ", fmt(lr_res$snv_num), " out of a total of ", fmt(lr_res$snv_den), " in TLHG ", tlhg, "."))
  })
  
  output$gnomAD_lr_popfreq <- renderText({
    lr_res <- req(gnomAD_lr_ingridients())
    
    isolate(paste0("(", 
                   fmt(lr_res$tlhg_num), " / ", fmt(lr_res$tlhg_den), ") x (", 
                   fmt(lr_res$snv_num), " / ", fmt(lr_res$snv_den), ") = 10^(", fmt(log10(lr_res$popfreq), digits = 4), ") = ",
                   "1 : ", fmt(1 / lr_res$popfreq)))
  })
  
  output$gnomAD_lr <- renderText({
    lr_res <- req(gnomAD_lr_ingridients())
    
    isolate(paste0("1 / 10^(", fmt(log10(lr_res$popfreq), digits = 4), ") = ", fmt(1 / lr_res$popfreq)))
  })
  
  ##############################################################################
  # GNOMAD END
  ##############################################################################
  
}

# Run the application 
shinyApp(ui = ui, server = server)
