library(rvest)

existing_verbs <- read.csv("helpers/verblist_2024-03-31.csv")

scrape_wiktionary <- function(word) {
  url <- paste0("https://en.wiktionary.org/wiki/", URLencode(word))
  
  wrong_langs <- c("Marathi", "Nepali", "Sanskrit", "Mewari")
  
  tryCatch(
    {
      page <- read_html(url)
      
      langugage_elements <- html_elements(page, 'h2 .mw-headline') %>%
        html_text()
      
      hindi_element <- "Hindi" %in% langugage_elements
      
      verb_element <- html_elements(page, '[id^="Verb"]')
      
      if (hindi_element) { # if page has hindi
        
        if (length(verb_element) > 0) { # if page has a verb
          ol_elements <- page %>%
            html_elements("p + ol > li") %>%
            # html_elements('h2:has("#Hindi") ~ :is("h3, h4"):has("[id^="Verb"]") + p + ol > li') %>%
            html_text()
        
          formatted_element <- "<ul>"
          
          for (ol_element in ol_elements) {
            lines <- strsplit(ol_element, "\n")[[1]]
            
            line_def <- lines[1]
            
            if (length(lines) == 1) {
              formatted_element <- paste0(formatted_element,
                                          "<li>", line_def, "</li>")
              
            } else if (length(lines) > 1){
              formatted_element <- paste0(formatted_element,
                                          "<li>", line_def,
                                          "<ul>")
              for (line_index in 2:length(lines)){
                line <- lines[line_index]
                if (!grepl("\\d", line)) {
                  formatted_element <- paste0(formatted_element,
                                              "<li>", line, "</li>")
                }
              }
              formatted_element <- paste0(formatted_element, "</ul></li>")
            }
          }
          
          formatted_element <- paste0(formatted_element, "</ul>")
          return(formatted_element)
          
        } else {
          return("Error: Not a verb.\n")
        }
      } else {
        return("Error: Does not have a Hindi Definition.\n")
      }
    },
    error = function(e) {
      if (grepl("error 404", e$message)) {
        return("Error: Word does not exist.")
      } else {
        return(e$message)
      }
    })
}

# verbs <- read.csv("helpers/transitivity.csv")
# 
# for (i in 1:nrow(verbs)) {
#   verb_inf <- paste0(verbs$Stem[i],"ना")
#   verbs$Definition[i] <- scrape_wiktionary(verb_inf)
# }
# 
# write.csv(verbs, "verblist_2024-03-31.csv", row.names = F)