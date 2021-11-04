
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

#Import the dataset. Filter for variables that we need in the end as well as rename the variables to facilitate subsequent coding.
hudoc <- read.csv("hudoc.csv")
head(hudoc)
hudoc <- hudoc %>% rename(case = Document.Title,
                          case_id = Application.Number, 
                          concdate = Date, 
                          conclusion = Conclusion) %>% select(-3, -4)

#Recode and create variables

#convert everything to lower case to facilitate recoding
hudoc$conclusion <- sapply(hudoc$conclusion, tolower)
hudoc$case <- sapply(hudoc$case, tolower)

#create country variable
hudoc$case <- gsub("v. |c. ", "vs.", hudoc$case)
hudoc$country <- gsub(".*vs.", "", hudoc$case)
hudoc$country <- word(hudoc$country, 1)
table(hudoc$country)
hudoc$country <- gsub("slovaquie", "slovakia", hudoc$country)
hudoc$country <- gsub("suisse", "switzerland", hudoc$country)

#create violation variable (first filter for the text and number of article separately, then merge together. Afterwards remove junk/summarize)

#David: the regex should target the following: match digit which must be preceded by one of these three expressions: "violation of art. ", "violation of article ", or "violation de l'article". in addition, do not match results which are preceded by "no". the results could be saved in a matrix easily by using str_extract_all(hudoc$conclusion, regex, simplify = TRUE)
#hudoc$article2 <- str_extract_all(hudoc$conclusion, "(?<="violation of article ")

hudoc$text <- str_extract(hudoc$conclusion, "^\\D+")
hudoc$article <- str_first_number(hudoc$conclusion)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
hudoc$text <- sapply(hudoc$text, removeSpecialChars)

# #junk
# hudoc$text <- gsub("  |   ", " ", hudoc$text)
# hudoc$text <- gsub(" de l |of ", " ", hudoc$text)
# hudoc$text <- gsub("article |art ", " ", hudoc$text)
# hudoc$text <- gsub("non ", "no ", hudoc$text)
# hudoc$text <- gsub("preliminary objection joined to merits and dismissed victim remainder inadmissible violation of |preliminary objection joined to merits and dismissed victim violation of |preliminary objection joined to merits and dismissed ","preliminary objection joined to merits and dismissed ", hudoc$text)
# hudoc$text <- gsub("exception pr liminaire rejet e |exceptions pr liminaires rejet es |preliminary objection dismissed ", "preliminary objections dismissed ", hudoc$text)
# hudoc$text <- gsub("partiellement irrecevable ", "partially irrecevable ", hudoc$text)
# hudoc$text <- gsub("remainder inadmissible violation  |remainder inadmissible struck out the list  ", "remainder inadmissible  ", hudoc$text)
# hudoc$text <- gsub("preliminary objection joined to merits and dismissed victim remainder inadmissible |preliminary objection joined to merits and dismissed victim violation ", "preliminary objection joined to merits and dismissed ", hudoc$text)
# hudoc$text <- gsub("partially irrecevable violation", "partially irrecevable ", hudoc$text)

#merge text and article no.
hudoc$violation_art <- paste(hudoc$text, hudoc$article, sep = "art ")

#create categorical variable
hudoc$catvar <- as.character(as.numeric(as.factor(hudoc$text)))
table(hudoc$catvar)
hudoc$catvar <- as.factor(ifelse(hudoc$catvar == 9, 1, 0))

#create dummy variable
hudoc$dummy <- as.character(as.numeric(as.factor(hudoc$text)))
hudoc$dummy <- car::recode(hudoc$dummy, "1= '0'; 9 = '1'; else = NA")

#TO DO: change encoding of date var to date-time (currently it's factor) 


## Including Plots
















