library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyBS)

# source
source("data.R")
source("helpers/scraper.R")
source("helpers/tables.R")

ui <- fluidPage(
  theme = shinytheme("lumen"),
  
  # ---- tags & footer ----
  tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "style.css"
  ),
  tags$link(
    rel = "stylesheet",
    href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css",
    crossorigin = "anonymous"
  ),
  tags$footer(
    class = "footer",
    HTML("to be corrected: irregulars, transitivity | "),
    HTML("last updated: 21 mar '24 | built in R shiny by cleo tay:&nbsp;"),
    a(
      href = "https://github.com/cleotayzq/",
      target = "_blank",
      HTML('<i class="fab fa-github"></i>')
    ),
    HTML('| licensed under <a href="https://creativecommons.org/licenses/by-sa/4.0/" target="_blank">CC-BY-SA 4.0</a>')
  ),
  
  # ------ title panel ------
  titlePanel(
    windowTitle = "Cleo's Hindi Verb Conjugator",
    div(
      tags$strong("Cleo's Hindi Verb Conjugator"),
      br(),
      tags$small('Click on the Devanagari letters to input a verb stem, select a tense and aspect, and have fun conjugating!'),
      hr()
    )
  ),
  
  # ---- sidebar layout ----
  sidebarLayout(
    # ------ sidebar panel ------
    sidebarPanel(
      id = "ctrl-panel",
      width = 4,
      # ---- infinitive form ----
      div(
        style = "padding-bottom: 5px;",
        h5("Infinitive:")
      ),
      wellPanel(
        style = "background-color: #ffffff; padding-left: .5em; padding-right: .5em;",
        fluidRow(
          column(
            width = 6,
            h5("Devanagari"),
            textOutput("inf_dev")
            ),
          column(
            width = 6,
            h5("Romanagari"),
            textOutput("inf_rom")
          )
        )
      ),
      # ---- keyboard switch ----
      prettySwitch(
        inputId = "keyboard_switch",
        value = T,
        inline = T,
        label = tags$strong("Devanagari Keyboard"),
        status = "success",
        fill = T
      ),
      uiOutput("emptyInputMessage"),
      hr(),
      # ---- tense plot ----
      fluidRow(
        column(
          class = "ctrl-panel-bttn",
          width = 6,
          radioGroupButtons(
            "buttons_tense",
            label = tags$strong("Tense:"),
            choices = c("Past", "Present", "Future"),
            selected = "Present",
            direction = "vertical",
            justified = T,
            width = "100%",
            status = "success"
          )
        ),
        column(
          class = "ctrl-panel-bttn",
          width = 6,
          radioGroupButtons(
            "buttons_aspect",
            label = tags$strong("Aspect:"),
            choices = c("Indefinite", "Perfect", "Imperfect", "Continuous"),
            selected = "Imperfect",
            direction = "vertical",
            justified = T,
            width = "100%",
            status = "primary"
          )
        )
      ),
      conditionalPanel(
        condition = 'input.buttons_tense == "Past" && (input.buttons_aspect == "Indefinite" || input.buttons_aspect == "Perfect") || input.buttons_aspect == "Perfect"',
        radioGroupButtons(
          "buttons_transitive",
          label = tags$strong("Transitivity:"),
          choices = c("Transitive", "Intransitive"),
          selected = "Transitive",
          justified = T,
          width = "100%",
          status = "warning"
        )
      ),
      hr(),
      h5("Reichenbach's (1975) Tense Framework:"),
      wellPanel(
        class = "plot-well",
        fluidRow(
          class = "plot-legend",
          span(
            style = "color: #619CFF;",
            span("E", class = "plot-letter"),
            "Event"
          ),
          span(
            style = "color: #00BA38;",
            span("R", class = "plot-letter"),
            "Reference"
          ),
          span(
            style = "color: #F8766D;",
            span("S", class = "plot-letter"),
            "Speech"
          ),
        ),
        plotOutput("tense_plot", height = 150)
      ),
      # ---- define switch ----
      hr(),
      prettySwitch(
        inputId = "definition_switch",
        value = T,
        inline = T,
        label = tags$strong("Wiktionary Definition"),
        status = "success",
        fill = T
      )
    ), # end sidebar panel
    # ------ main panel ------
    mainPanel(
      width = 8,
      # ------ keyboard panel ------
      conditionalPanel(
        condition = "input.keyboard_switch == true",
        h4("Devanagari Keyboard:"),
        fluidRow(
          
          # ---- vowels ----
          column(
            width = 4,
            h5("Vowels:"),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("vowel1a", "अ"),
                bsTooltip("vowel1a", "a: <strong><u>a</u></strong>live", "top")
              ),
              div(
                actionButton("vowel2a", "आ"),
                bsTooltip("vowel2a", "ā: p<strong><u>a</u></strong>lm", "top")
              ),
              div(
                actionButton("vowel2b", "ा"),
                bsTooltip("vowel2b", "ā: p<strong><u>a</u></strong>lm", "top")
              )
            ),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("vowel3a", "इ"),
                bsTooltip("vowel3a", "i: h<strong><u>i</u></strong>t", "top")
              ),
              div(
                actionButton("vowel3b", "ि"),
                bsTooltip("vowel3b", "i: h<strong><u>i</u></strong>t", "top")
              ),
              div(
                actionButton("vowel4a", "ई"),
                bsTooltip("vowel4a", "ī: h<strong><u>ea</u></strong>t", "top")
              ),
              div(
                actionButton("vowel4b", "ी"),
                bsTooltip("vowel4b", "ī: h<strong><u>ea</u></strong>t", "top")
              )
            ),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("vowel5a", "उ"),
                bsTooltip("vowel5a", "u: f<strong><u>oo</u></strong>t", "top")
              ),
              div(
                actionButton("vowel5b", "ु"),
                bsTooltip("vowel5b", "u: f<strong><u>oo</u></strong>t", "top")
              ),
              div(
                actionButton("vowel6a", "ऊ"),
                bsTooltip("vowel6a", "ū: f<strong><u>oo</u></strong>l", "top")
              ),
              div(
                actionButton("vowel6b", "ू"),
                bsTooltip("vowel6b", "ū: f<strong><u>oo</u></strong>l", "top")
              )
            ),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("vowel7a", "ए"),
                bsTooltip("vowel7a", "e: march<strong><u>é</u></strong>", "top")
              ),
              div(
                actionButton("vowel7b", "े"),
                bsTooltip("vowel7b", "e: march<strong><u>é</u></strong>", "top")
              ),
              div(
                actionButton("vowel8a", "ऐ"),
                bsTooltip("vowel8a", "ai: h<strong><u>e</u></strong>n", "top")
              ),
              div(
                actionButton("vowel8b", "ै"),
                bsTooltip("vowel8b", "ai: h<strong><u>e</u></strong>n", "top")
              )
            ),
            fixedRow(
              class = "keyboard-row",
              div(
                actionButton("vowel9a", "ओ"),
                bsTooltip("vowel9a", "ai: h<strong><u>o</u></strong>tel", "top")
              ),
              div(
                actionButton("vowel9b", "ो"),
                bsTooltip("vowel9b", "ai: h<strong><u>o</u></strong>tel", "top")
              ),
              div(
                actionButton("vowel10a", "औ"),
                bsTooltip("vowel10a", "au: <strong><u>o</u></strong>ff", "top")
              ),
              div(
                actionButton("vowel10b", "ौ"),
                bsTooltip("vowel10b", "au: <strong><u>o</u></strong>ff", "top")
              )
            ),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("vowel11a", "ऋ"),
                bsTooltip("vowel11a", "r̥: c<strong><u>ri</u></strong>tic", "top")
              ),
              div(
                actionButton("vowel11b", "ृ"),
                bsTooltip("vowel11b", "r̥: c<strong><u>ri</u></strong>tic", "top")
              )
            )
          ),
          
          # ---- consonants ----
          column(
            width = 4,
            h5("Consonants:"),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("velar1", "क"),
                bsTooltip("velar1", "k: s<strong><u>k</u></strong>it", "top")
              ),
              div(
                actionButton("velar2", "ख"),
                bsTooltip("velar2", "kh: <strong><u>k</u></strong>it (but more aspirated)", "top")
              ),
              div(
                actionButton("velar3", "ग"),
                bsTooltip("velar3", "g: <strong><u>g</u></strong>ift", "top")
              ),
              div(
                actionButton("velar4", "घ"),
                bsTooltip("velar4", "gh: do<strong><u>g-h</u></strong>ouse", "top")
              ),
              div(
                actionButton("velar5", "ङ"),
                bsTooltip("velar5", "ṅ: i<strong><u>n</u></strong>k", "top")
              )
            ),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("palatal1", "च"),
                bsTooltip("palatal1", "ch: <strong><u>ch</u></strong>eese (but with less release of breath)", "top")
              ),
              div(
                actionButton("palatal2", "छ"),
                bsTooltip("palatal2", "chh: pit<strong><u>ch-h</u></strong>ook", "top")
              ),
              div(
                actionButton("palatal3", "ज"),
                bsTooltip("palatal3", "j: <strong><u>j</u></strong>eer", "top")
              ),
              div(
                actionButton("palatal4", "झ"),
                bsTooltip("palatal4", "jh: lar<strong><u>ge h</u></strong>ouse", "top")
              ),
              div(
                actionButton("palatal5", "ञ"),
                bsTooltip("palatal5", "ñ: i<strong><u>n</u></strong>ch", "top")
              )
            ),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("retroflex1", "ट"),
                bsTooltip("retroflex1", "ṭ: <strong><u>t</u></strong>rain (but harder)", "top")
              ),
              div(
                actionButton("retroflex2", "ठ"),
                bsTooltip("retroflex2", "ṭh: a<strong><u>t h</u></strong>ome", "top")
              ),
              div(
                actionButton("retroflex3", "ड"),
                bsTooltip("retroflex3", "ḍ: <strong><u>d</u></strong>rum (but harder)", "top")
              ),
              div(
                actionButton("retroflex4", "ढ"),
                bsTooltip("retroflex4", "ḍh: aspirated form of ड", "top")
              ),
              div(
                actionButton("retroflex5", "ण"),
                bsTooltip("retroflex5", "ṇ: retroflex hard n", "top")
              )
            ),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("dental1", "त"),
                bsTooltip("dental1", "t: a<strong><u>t t</u></strong>he", "top")
              ),
              div(
                actionButton("dental2", "थ"),
                bsTooltip("dental2", "th: aspirated form of त", "top")
              ),
              div(
                actionButton("dental3", "द"),
                bsTooltip("dental3", "d: brea<strong><u>d</u></strong>th", "top")
              ),
              div(
                actionButton("dental4", "ध"),
                bsTooltip("dental4", "dh: aspirated form of द", "top")
              ),
              div(
                actionButton("dental5", "न"),
                bsTooltip("dental5", "n: a<strong><u>n</u></strong>them", "top")
              )
            ),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("labial1", "प"),
                bsTooltip("labial1", "p: s<strong><u>p</u></strong>in (minimal release of breath)", "top")
              ),
              div(
                actionButton("labial2", "फ"),
                bsTooltip("labial2", "ph: to<strong><u>p-h</u></strong>at", "top")
              ),
              div(
                actionButton("labial3", "ब"),
                bsTooltip("labial3", "b: <strong><u>b</u></strong>in", "top")
              ),
              div(
                actionButton("labial4", "भ"),
                bsTooltip("labial4", "bh: clu<strong><u>b-h</u></strong>ouse", "top")
              ),
              div(
                actionButton("labial5", "म"),
                bsTooltip("labial5", "m: <strong><u>m</u></strong>other", "top")
              )
            ),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("appro1", "य"),
                bsTooltip("appro1", "y: <strong><u>y</u></strong>et (but less tense)", "top")
              ),
              div(
                actionButton("appro2", "र"),
                bsTooltip("appro2", "r: <strong><u>r</u></strong>oll", "top")
              ),
              div(
                actionButton("appro3", "ल"),
                bsTooltip("appro3", "l: <strong><u>l</u></strong>abel (but softer)", "top")
              ),
              div(
                actionButton("appro4", "व"),
                bsTooltip("appro4", "v: between v and w", "top")
              )
            ),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("fricat1", "श"),
                bsTooltip("fricat1", "ś: <strong><u>sh</u></strong>ip", "top")
              ),
              div(
                actionButton("fricat2", "ष"),
                bsTooltip("fricat2", "ṣ: usually pronounced like श", "top")
              ),
              div(
                actionButton("fricat3", "स"),
                bsTooltip("fricat3", "s: <strong><u>s</u></strong>ip", "top")
              ),
              div(
                actionButton("fricat4", "ह"),
                bsTooltip("fricat4", "h: a<strong><u>h</u></strong>ead", "top")
              )
            )
          ),
          column(
            width = 4,
            h5("Borrowed:"),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("spec1", "क़"),
                bsTooltip("spec1", "q: k sound (further back than क)", "top")
              ),
              div(
                actionButton("spec2", "ख़"),
                bsTooltip("spec2", "x: lo<strong><u>ch</u></strong>", "top")
              ),
              div(
                actionButton("spec3", "ग़"),
                bsTooltip("spec3", "ġ: g sound (further back than ग)", "top")
              ),
              div(
                actionButton("spec4", "ज़"),
                bsTooltip("spec4", "z: <strong><u>z</u></strong>ip", "top")
              )
            ),
            fluidRow(
              class = "keyboard-row",
              div(
                actionButton("spec5", "झ़"),
                bsTooltip("spec5", "zh: [ʒ]", "top")
              ),
              div(
                actionButton("spec6", "फ़"),
                bsTooltip("spec6", "f: [f]", "top")
              ),
              div(
                actionButton("spec7", "ड़"),
                bsTooltip("spec7", "ṛ: flapped hard r with a ड sound", "top")
              ),
              div(
                actionButton("spec8", "ढ़"),
                bsTooltip("spec8", "ṛh: flapped equivalent of ढ", "top")
              )
            ),
            br(),
            h5("Special Characters:"),
            fixedRow(
              class = "keyboard-row",
              div(
                actionButton("halant", "्"),
                bsTooltip("halant", "halant: reduces previous consonant to half-form", "top")
              ),
              div(
                actionButton("vowel13a", "ं"),
                bsTooltip("vowel13a", "bindu: nasalisation", "top")
              ),
              div(
                actionButton("vowel13b", "ँ"),
                bsTooltip("vowel13b", "chandrabindu: nasalisation", "top")
              ),
              div(
                actionButton("vowel13c", "ः"),
                bsTooltip("vowel13c", "visarg: pronounced ह", "top")
              )
            ),
            hr(),
            actionButton("backspace_button", "Backspace", class = "btn-warning"),
            actionButton("clear_input", "Clear input", class = "btn-warning")
          ),
          
        ),
        hr()
      ),
      # ------ tabset panel ------
      tabsetPanel(
        id = "current_tab",
        type = "hidden",
        # ------ present tense menu ------
        navbarMenu(
          "Present Tense",
          # ------ present perfect tab ------
          tabPanel(
            "Present Perfect",
            div(
              h4("Present Perfect:"),
              em("e.g., I have gone; I have eaten."),
            ),
            br(),
            htmlOutput("tbl_present_perfect"),
            div(
              em("e.g., I have already gone; I have already eaten.")
            ),
            br(),
            htmlOutput("tbl_present_perfect_chuk")
          ),
          # ------ present imperfect tab ------
          tabPanel(
            "Present Imperfect",
            div(
              h4("Present Imperfect:"),
              em("e.g., I (habitually) go; I (habitually) eat."),
            ),
            br(),
            htmlOutput("tbl_present_imperfect")
          ),
          # ------ present continuous tab ------
          tabPanel(
            "Present Continuous",
            div(
              h4("Present Continuous:"),
              em("e.g., I am going; I am eating."),
            ),
            br(),
            htmlOutput("tbl_present_continuous")
          )
        ), # end present tense menu
        
        # ------ past tense menu ------
        navbarMenu(
          "Past Tense",
          # ------ past indefinite tab ------
          tabPanel(
            "Past Indefinite",
            div(
              h4("Past Indefinite:"),
              em("e.g., I went; I ate."),
            ),
            br(),
            htmlOutput("tbl_past_indefinite"),
            div(
              em("e.g., I already went; I already ate.")
            ),
            br(),
            htmlOutput("tbl_past_indefinite_chuk")
          ),
          # ------ past perfect tab ------
          tabPanel(
            "Past Perfect",
            div(
              h4("Past Perfect:"),
              em("e.g., I had gone; I had eaten."),
            ),
            br(),
            htmlOutput("tbl_past_perfect"),
            div(
              em("e.g., I had already gone; I had already eaten.")
            ),
            br(),
            htmlOutput("tbl_past_perfect_chuk")
          ),
          # ------ past imperfect tab ------
          tabPanel(
            "Past Imperfect",
            div(
              h4("Past Imperfect:"),
              em("e.g., I used to go; I used to eat."),
            ),
            br(),
            htmlOutput("tbl_past_imperfect")
          ),
          # ------ past continuous tab ------
          tabPanel(
            "Past Continuous",
            div(
              h4("Past Continuous:"),
              em("e.g., I was going; I was eating."),
            ),
            br(),
            htmlOutput("tbl_past_continuous")
          ),
        ), # end past tense menu
        
        # ------ future tense menu ------
        navbarMenu(
          "Future Tense",
          # ------ future indefinite tab ------
          tabPanel(
            "Future Indefinite",
            div(
              h4("Future Indefinite:"),
              em("e.g., I will go; I will eat"),
            ),
            br(),
            htmlOutput("tbl_future_indefinite")
          ),
          # ------ future perfect tab ------
          tabPanel(
            "Future Perfect",
            div(
              h4("Future Perfect:"),
              em("e.g., I will have gone; I will have eaten."),
            ),
            br(),
            htmlOutput("tbl_future_perfect"),
            div(
              em("e.g., I will have already gone; I will have already eaten.")
            ),
            br(),
            htmlOutput("tbl_future_perfect_chuk")
          ),
          # ------ future continuous tab ------
          tabPanel(
            "Future Continuous",
            div(
              h4("Future Continuous:"),
              em("e.g., I will be going; I will be eating."),
            ),
            br(),
            htmlOutput("tbl_future_continuous")
          ),
        ) # end future tense menu
      ), # end tabsetPanel
      # ----- definition -----
      conditionalPanel(
        condition = "input.definition_switch == true",
        hr(),
        h4(textOutput("definition_title")),
        div(
          class = 'definition',
          htmlOutput("output_def")
        ),
        hr()
      ) # end conditional panel definition
    ) # end mainPanel
  ) # end sidebarLayout
) # end fluidPage

shinyUI(ui)