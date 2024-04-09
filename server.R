# to do:
# future tense
# ne pronouns
# transitive object/subject
# perfective tense plural fem
# irregular verbs

# libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

# source
source("data.R")
source("helpers/scraper.R")
source("helpers/tables.R")

# server starts here ----
function(input, output, session) {
  
  # create empty reactive data frame of input ----
  current_input <- reactiveVal(
    data.frame(
      input_id = character(0),
      devanagari = character(0),
      roman = character(0),
      stringsAsFactors = F
    )
  )
  current_dev <- reactiveVal("")
  current_rom <- reactiveVal("")
  
  # function to add a row based on character mappings (uses reactive values) ----
  addRow <- function(btn_id) {
    mapping <- character_mappings[character_mappings$input_id == btn_id, ]
    new_row <- data.frame(
      input_id = btn_id,
      devanagari = mapping$devanagari,
      roman = mapping$roman,
      stringsAsFactors = F
    )
    current_df <- current_input()
    new_df <- rbind(current_df, new_row)
    current_input(new_df)
  }
  
  # observe keyboard button clicks and add rows accordingly ----
  observe({
    lapply(character_mappings$input_id, function(btn_id) {
      observeEvent(input[[btn_id]], {
        addRow(btn_id)
      })
    })
  })
  
  # output message if input is empty ----
  output$emptyInputMessage <- renderUI({
    if (nrow(current_input()) == 0) {
      h5(
        style = "color: #F8766D",
        "It looks like there's nothing in the input yet.
        Click on Devanagari letters on the right to make a verb stem.")
    }
  })
  
  # create reactive strings from input table ----
  current_dev <- reactive({
    
    if (nrow(current_input()) == 0) {
      return("-")
    } else if (nrow(current_input()) > 0) {
      return(paste(current_input()$devanagari, collapse = ""))
    }
    
  })
  current_rom <- reactive({
    
    if (nrow(current_input()) == 0) {
      return("-")
    } else if (nrow(current_input()) > 0) {
      return(paste(current_input()$roman, collapse = ""))
    }
    
  })
  
  # check if last input of verb stem is a vowel ----
  vowel_ending <- reactive({
    input_data <- current_input()
    
    if (is.null(input_data) || nrow(input_data) == 0) {
      return(FALSE)
    } else {
      last_dev <- input_data$devanagari[nrow(input_data)]
      ends_with_vowel <- last_dev %in% dev_vowels
      ends_with_vowel
    }
  })

  
  # create infinitive strings from current verb stems ----
  inf_dev <- reactive({
    paste0(current_dev(), "ना")
  })
  inf_rom <- reactive({
    paste0(current_rom(), "nā")
  })
  
  # infinitive side panel output ----
  output$inf_dev <- renderText({
    inf_dev()
  })
  output$inf_rom <- renderText({
    inf_rom()
  })
  
  # backspace button ----
  observeEvent(input$backspace_button, {
    current_df <- current_input()
    if (nrow(current_df) > 0) {
      new_df <- current_df[-nrow(current_df), ]
      current_input(new_df)
    }
  })
    
  # clear input button ----
  observeEvent(input$clear_input, {
    current_input(data.frame(
      input_id = character(0),
      devanagari = character(0),
      roman = character(0),
      stringsAsFactors = F
    ))
  })
  
  # change available tenses based on button ----
  observe({
    if (input$buttons_tense == "Future") {
      updateRadioGroupButtons(session, "buttons_aspect", choices = c("Indefinite", "Perfect", "Continuous"), selected = "Indefinite", status = "primary")
    } else if (input$buttons_tense == "Present") {
      updateRadioGroupButtons(session, "buttons_aspect", choices = c("Perfect", "Imperfect", "Continuous"), selected = "Imperfect", status = "primary")
    } else {
      updateRadioGroupButtons(session, "buttons_aspect", choices = c("Indefinite", "Perfect", "Imperfect", "Continuous"), selected = "Imperfect", status = "primary")
    }
  })
  
  # change tense tab shown based on button ----
  observe({
    tense_tab <- paste(input$buttons_tense, input$buttons_aspect)
    updateTabsetPanel(session, "current_tab", selected = tense_tab)
  })
  
  # tense plot data ----
  plot_data <- reactive({
    df_chart %>%
      filter(Tense == input$buttons_tense, Aspect == input$buttons_aspect)
  })
  
  # tense plot ----
  output$tense_plot <- renderPlot({
    ggplot(plot_data(), aes(x = x_value, colour = name, y = name)) +
      geom_vline(xintercept = 0) +
      geom_label(aes(label = letter_label), label.size = 0.5) +
      theme_classic() +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold")
      ) +
      scale_x_continuous(breaks = seq(-1.5, 1.5, 1.5), labels = c("Past", "Present", "Future")) +
      coord_cartesian(xlim = c(-2.1, 2.1))
  })
  
  # definition panel ----
  output$definition_title <- renderText({
    paste0("Wiktionary definition of ", inf_dev(), ":")
  })
  output$output_def <- renderUI({
    
    if (input$definition_switch == T & nrow(current_input()) > 0){
      matching_row <- existing_verbs[existing_verbs$Stem == current_dev(), ]
      if (nrow(matching_row) > 0) {
        return(HTML(matching_row$Definition))
      } else {
        word <- inf_dev()
        wiki_definition <- HTML(scrape_wiktionary(word))
        return(wiki_definition)
      }
    } else if (nrow(current_input()) == 0){
      return("Error: Input is empty. Use the Devanagari keyboard to input a verb stem and the definition of the verb will appear here.")
    }
  })
  
  # ----- present ----
  output$tbl_present_imperfect <- renderUI({
    HTML(createTable1(current_dev(), current_rom(), "present", "imperfect", vowel_ending()))
  })
  output$tbl_present_continuous <- renderUI({
    HTML(createTable1(current_dev(), current_rom(), "present", "continuous", vowel_ending()))
  })
  output$tbl_present_perfect <- renderUI({
    if (input$buttons_transitive == "Transitive") {
      HTML(createTable3(current_dev(), current_rom(), "present", "perfect", vowel_ending()))
    } else {
      HTML(createTable1(current_dev(), current_rom(), "present", "perfect", vowel_ending()))
    }
  })
  output$tbl_present_perfect_chuk <- renderUI({
    HTML(createTable1(paste0(current_dev(), " चुक"), paste0(current_rom(), " chuk"), "present", "perfect", FALSE))
  })
  
  # ----- past ----
  output$tbl_past_imperfect <- renderUI({
    HTML(createTable2(current_dev(), current_rom(), "past", "imperfect", vowel_ending()))
  })
  output$tbl_past_continuous <- renderUI({
    HTML(createTable2(current_dev(), current_rom(), "past", "continuous", vowel_ending()))
  })
  output$tbl_past_perfect <- renderUI({
    if (input$buttons_transitive == "Transitive") {
      HTML(createTable3(current_dev(), current_rom(), "past", "perfect", vowel_ending()))
    } else {
      HTML(createTable2(current_dev(), current_rom(), "past", "perfect", vowel_ending()))
    }
  })
  output$tbl_past_perfect_chuk <- renderUI({
    HTML(createTable2(paste0(current_dev(), " चुक"), paste0(current_rom(), " chuk"), "past", "perfect", FALSE))
  })
  output$tbl_past_indefinite <- renderUI({
    # special case: past indefinite
    if (input$buttons_transitive == "Transitive") {
      HTML(createTable3(current_dev(), current_rom(), "indefinite", "perfect", vowel_ending())) # if transitive & needs ne
    } else {
      HTML(createTable2(current_dev(), current_rom(), "indefinite", "perfect", vowel_ending())) # if intransitive & doesn't need ne
    }
  })
  output$tbl_past_indefinite_chuk <- renderUI({
    HTML(createTable2(paste0(current_dev(), " चुक"), paste0(current_rom(), " chuk"), "indefinite", "perfect", FALSE))
  })
  
  # ----- future ----
  output$tbl_future_continuous <- renderUI({
    HTML(createTable1(current_dev(), current_rom(), "future", "continuous", vowel_ending()))
  })
  output$tbl_future_perfect <- renderUI({
    if (input$buttons_transitive == "Transitive") {
      HTML(createTable3(current_dev(), current_rom(), "future", "perfect", vowel_ending()))
    } else {
      HTML(createTable1(current_dev(), current_rom(), "future", "perfect", vowel_ending()))
    }
  })
  output$tbl_future_perfect_chuk <- renderUI({
    HTML(createTable1(paste0(current_dev(), " चुक"), paste0(current_rom(), " chuk"), "future", "perfect", FALSE))
  })
  output$tbl_future_indefinite <- renderUI({
    HTML(createTable1(current_dev(), current_rom(), "indefinite", "future", vowel_ending()))
  })
  
}
