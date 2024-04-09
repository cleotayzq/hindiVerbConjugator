library(dplyr)
library(tidyr)

dev_vowels <- c("अ","आ", "इ", "ई",
                "ा", "ि", "ी", 
                "उ", "ऊ", "ए", "ऐ",
                "ु", "ू", "े", "ै",
                "ओ","औ","ऋ","ॠ",
                "ो", "ौ", "ृ", "ॄ")

# Data frame to store character mappings ----
character_mappings <- data.frame(
  input_id = c(
    "velar1", "velar2", "velar3", "velar4", "velar5",
    "palatal1", "palatal2", "palatal3", "palatal4", "palatal5",
    "retroflex1", "retroflex2", "retroflex3", "retroflex4", "retroflex5",
    "dental1", "dental2", "dental3", "dental4", "dental5",
    "labial1", "labial2", "labial3", "labial4", "labial5",
    "appro1", "appro2", "appro3", "appro4",
    "fricat1", "fricat2", "fricat3", "fricat4",
    "spec1", "spec2", "spec3", "spec4",
    "spec5", "spec6", "spec7", "spec8", "halant",
    "vowel1a", "vowel2a", "vowel3a", "vowel4a",
    "vowel2b", "vowel3b", "vowel4b",
    "vowel5a", "vowel6a", "vowel7a", "vowel8a",
    "vowel5b", "vowel6b", "vowel7b", "vowel8b",
    "vowel9a", "vowel10a", "vowel11a", "vowel12a",
    "vowel9b", "vowel10b", "vowel11b", "vowel12b",
    "vowel13a", "vowel13b", "vowel13c"
  ),
  devanagari = c(
    "क", "ख", "ग", "घ", "ङ", # velars
    "च", "छ", "ज", "झ", "ञ", # palatal
    "ट", "ठ", "ड", "ढ", "ण", # retroflex
    "त", "थ", "द", "ध", "न", # dental
    "प", "फ", "ब", "भ", "म", # labial
    "य", "र", "ल", "व", # appro
    "श", "ष", "स", "ह", # fricative
    "क़", "ख़","ग़", "ज़", # specials 
    "झ़", "फ़़", "ड़", "ढ़", "्", # specials
    # vowels
    "अ","आ", "इ", "ई",
    "ा", "ि", "ी", 
    "उ", "ऊ", "ए", "ऐ",
    "ु", "ू", "े", "ै",
    "ओ","औ","ऋ","ॠ",
    "ो", "ौ", "ृ", "ॄ",
    "ं", "ँ", "ः"
  ),
  roman = c(
    "k", "kh", "g", "gh", "ṅ", # velars
    "ch", "chh", "j", "jh", "ñ", # palatal
    "ṭ", "ṭh", "ḍ", "ḍh", "ṇ", # retroflex
    "t", "th", "d", "dh", "n", # dental
    "p", "ph", "b", "bh", "m", # labial
    "y", "r", "l", "v", # appro
    "ś", "ṣ", "s", "h", # fricative
    "q", "x", "ġ", "z", # specials
    "zh", "f", "ṛ", "ṛh", "", # specials
    # vowels
    "a", "ā", "i", "ī",
    "ā", "i", "ī",
    "u", "ū", "e", "ai",
    "u", "ū", "e", "ai",
    "o", "au", "r̥", "r̥̄",
    "o", "au", "r̥", "r̥̄",
    "n", "n", "ḥ"
  )
)

# Data frame for tense visualization ----
df_chart <- data.frame(
  Tense = c(
    "Future", "Future", "Future",
    "Past", "Past", "Past", "Past", "Past", "Past",
    "Present", "Present", "Present", "Present", "Present", "Present",
    "Future", "Future", "Future"),
  Aspect = c(
    "Continuous", "Perfect", "Indefinite",
    "Continuous", "Imperfect", "Imperfect", "Imperfect", "Perfect", "Indefinite",
    "Continuous", "Imperfect", "Imperfect", "Imperfect", "Perfect", "Indefinite",
    "Imperfect", "Imperfect", "Imperfect"),
  English = c(
    "will be coming", "will have gone", "will be coming",
    "was coming", "used to come", "used to come", "used to come", "had come", "came",
    "is coming", "comes", "comes", "comes", "has come", "is",
    "", "", ""),
  Hindi = c(
    "आ रहा होगा", "आया होगा", "आएग",
    "आ रहा था", "आता था", "आता था", "आता था", "आया था", "आया",
    "आ रहा है", "आता है", "आता है", "आता है", "आया है", "है", 
    "", "", ""),
  Event = c(1, 1, 1, -1, -1.5, -1, -0.5, -2, -1, 0, -0.5, 0, 0.5, -1, 0, 1, 1.5, 0.5),
  Reference = c(1, 2, 1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 1, 1.5, 0.5),
  Speech = 0
)

df_chart <- df_chart %>%
  mutate(Tense = ordered(Tense, levels = c("Past", "Present", "Future")),
         Tense_Aspect = paste(Tense, Aspect)) %>%
  pivot_longer(5:7, values_to = "x_value") %>%
  mutate(name = factor(name, levels = c("Speech", "Reference", "Event")),
         letter_label = ifelse(name == "Event" & Aspect == "Continuous", "~~E~~>",
                               ifelse(name == "Event", "E",
                                      ifelse(name == "Reference", "R", "S"))))
