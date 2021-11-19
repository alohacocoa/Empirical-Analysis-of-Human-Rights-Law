
#this clears the environment
rm(list = ls())

#this sets the wd to the directory of this R file. might not be necessary, but better be safe than sorry
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#sets R to english because I don't like german warnings
Sys.setenv(LANG = "en")


library(haven)
library(tidyverse)
library(margins)
library(magrittr)
library(ggeffects)
library(lme4)
library(splines)
library(stargazer)
library(dplyr)
library(tidytext)
library(strex)
library(tm)
library(stringr)


## Data Preparation

#Import the dataset. Filter for variables that we need in the end, rename the variables to facilitate subsequent coding, and remove duplicates.
hudoc <- read.csv("hudoc.csv")
head(hudoc)
hudoc <- hudoc %>% rename(case = Document.Title  ,case_id = Application.Number, 
                          concdate = Date, 
                          conclusion = Conclusion)%>% select(-3, -4) %>% distinct(case_id, .keep_all = TRUE)

#Recode and create variables

#convert everything to lower case to facilitate recoding
hudoc$conclusion <- sapply(hudoc$conclusion, tolower)
hudoc$case <- sapply(hudoc$case, tolower)

#create country variable
hudoc$case <- gsub("v. |c. ", "vs.", hudoc$case)
hudoc$country <- gsub(".*vs.", "", hudoc$case)
hudoc$country <- word(hudoc$country, 1)
table(hudoc$country)
hudoc$country <- gsub("suisse", "switzerland", hudoc$country)

#create violation variable (first filter for the text and number of article separately, then merge together. Afterwards remove junk/summarize)
#create the text variable for the extraction of violations
#hudoc$text <- str_extract(hudoc$conclusion, "^\\D+")
#hudoc$text <- rm()
hudoc$text <- hudoc$conclusion


removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
hudoc$text <- sapply(hudoc$text, removeSpecialChars)

#this makes it so that the french is the same as english
hudoc$text <- gsub("  |   ", " ", hudoc$text)
hudoc$text <- gsub(" de l ", " of ", hudoc$text)
hudoc$text <- gsub("article ", "art ", hudoc$text)
hudoc$text <- gsub("non ", "no ", hudoc$text)


#this looks for the phrase "violation of" followed by any number of non-digit characters and ends with one or more digits. the whole phrase must not be preceded by "no ". the matches are then written into the new varialbe "violations_with_text". the variable is a list
hudoc$violations_with_text <-  stringr::str_extract_all(hudoc$text, "(?<!no\\s)violation\\sof\\D{0,20}\\d+") 


#this creates a new variable called "violations". it gets rid of the text and leaves only the numbers. I couldn't do it with only regex because for some reason, using negative and positive lookbehind does not work (in R). anyway, the variable is also a list
for (l in 1:length(hudoc$violations_with_text)) {
  hudoc$violations[l] <- stringr::str_extract_all(hudoc$violations_with_text[l], "\\d+")
} 


#still to do before data visualization:
# - find and get rid of duplicates
# - use hudoc$violations variable to create a new row in every case where there is more than one violation, copying everything except for the value of hudoc$violation. don't know how to do that yet



















#junk
# hudoc$text <- gsub("preliminary objection joined to merits and dismissed victim remainder inadmissible violation of |preliminary objection joined to merits and dismissed victim violation of |preliminary objection joined to merits and dismissed ","preliminary objection joined to merits and dismissed ", hudoc$text)
# hudoc$text <- gsub("exception pr liminaire rejet e |exceptions pr liminaires rejet es |preliminary objection dismissed ", "preliminary objections dismissed ", hudoc$text)
# hudoc$text <- gsub("partiellement irrecevable ", "partially irrecevable ", hudoc$text)
# hudoc$text <- gsub("remainder inadmissible violation  |remainder inadmissible struck out the list  ", "remainder inadmissible  ", hudoc$text)
# hudoc$text <- gsub("preliminary objection joined to merits and dismissed victim remainder inadmissible |preliminary objection joined to merits and dismissed victim violation ", "preliminary objection joined to merits and dismissed ", hudoc$text)
# hudoc$text <- gsub("partially irrecevable violation", "partially irrecevable ", hudoc$text)

#merge text and article no.
#hudoc$violation_art <- paste(hudoc$text, hudoc$article, sep = "art ")

#create categorical variable
#hudoc$catvar <- as.character(as.numeric(as.factor(hudoc$text)))
#table(hudoc$catvar)
#hudoc$catvar <- as.factor(ifelse(hudoc$catvar == 9, 1, 0))

#create dummy variable
#hudoc$dummy <- as.character(as.numeric(as.factor(hudoc$text)))
#hudoc$dummy <- car::recode(hudoc$dummy, "1= '0'; 9 = '1'; else = NA")














