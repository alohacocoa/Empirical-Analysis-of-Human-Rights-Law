
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


#removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9\\+ ]", " ", x)
#hudoc$text <- sapply(hudoc$text, removeSpecialChars)

#this makes it so that the french is the same as english
hudoc$text <- gsub("  |   ", " ", hudoc$text)
hudoc$text <- gsub(" de l'", " of ", hudoc$text)
hudoc$text <- gsub("article ", "art ", hudoc$text)
hudoc$text <- gsub("non ", "no ", hudoc$text)
hudoc$text <- gsub("non-violation", "no violation", hudoc$text)





#this looks for the phrase "violation of" followed by any number of non-digit characters and ends with one or more digits. the whole phrase must not be preceded by "no ". the matches are then written into the new varialbe "violations_with_text". the variable is a list
hudoc$violations_with_text <-  stringr::str_extract_all(hudoc$text, "(?<!no\\s)violation\\sof\\D{0,20}\\d+") 

#this looks for the phrase "NUMBER+NUMBER" the matches are  written into the new varialbe "violations_with_plus". the variable is a list
hudoc$violations_with_plus <-  stringr::str_extract(hudoc$text, "\\d+\\+\\d+(?!\\-)") 



#this creates a new variable called "violations_with_text". it gets rid of the text and leaves only the numbers. I couldn't do it with only regex because for some reason, using negative and positive lookbehind does not work (in R). anyway, the variable is also a list
for (l in 1:length(hudoc$violations_with_text)) {
  hudoc$violations_with_text_numbers[l] <- stringr::str_extract_all(hudoc$violations_with_text[l], "\\d+")
} 

## [We don't need this when using the appraoch below] this creates a new variable that contains all the instances of "NUMBER+NUMBER". I manually checked and none of these are cases of non-violations
for (l in 1:length(hudoc$violations_with_text)) {
  hudoc$violations_with_plus_numbers[l] <- stringr::str_extract_all(hudoc$violations_with_plus[l], "\\d+")
} 

#this combines the two variables into one
hudoc$violation_numbers_combined <- mapply(c, hudoc$violations_with_plus_numbers, hudoc$violation_with_text_numbers, SIMPLIFY = FALSE)

#this loop below gets rid of missing values that were present in the "violations_with_plus_numbers" variable
for (l in 1:length(hudoc$violation_numbers_combined)) {
  hudoc$violation_numbers_combined[[l]] <- hudoc$violation_numbers_combined[[l]][!is.na(hudoc$violation_numbers_combined[[l]])]
}

#this loop below gets rid of duplicate values from situations like "violation of art. 14+3" where 14 was matched twice
for (l in 1:length(hudoc$violation_numbers_combined)) {
  hudoc$violation_numbers_combined[[l]] <- unique(hudoc$violation_numbers_combined[[l]])
}

#this creates a new variable "violations" based on the previous variable
hudoc$violations <- hudoc$violation_numbers_combined

hudoc$violations

#this gets rid of all the intermediary variables
glimpse(hudoc)






#still to do before data visualization:
# - find and get rid of duplicates (is included in the first lines of code)
# - use hudoc$violations variable to create a new row in every case where there is more than one violation, copying everything except for the value of hudoc$violation. don't know how to do that yet
# => I was a bit lazy and chose another approach, I groupd the violations (violations_with_text) by country, aggregated all violations per country, got rid of the "violations of" part, and extracted the numbers

#create new dataobject and group new dataset "bycountry" by country, remove list values by applying unlist
bycountry <- apply(hudoc, 2, function(y) sapply(y, function(x) paste(unlist(x), collapse=" ")))

#aggregate data by country, concatenate text of violations
agg <- aggregate(data=bycountry, unlist(violations_with_text)~country, paste0, collapse=' ')

#rename variable name to concat_violations
agg$concat_violations <-agg$`unlist(violations_with_text)`

#get number of violations for slovakia (since the values are displayed as a list, we need to apply unlist; in a second step we remove the spaces)
slovakia <- agg$concat_violations[1]
slovakia <- str_split(slovakia, 'violation of art ')
slovakia <- unlist(slovakia)
slovakia <- gsub(" |   ", "", slovakia)

#not necessary, but facilitates the identification of articles (see levels)
slovakia <- as.factor(slovakia)
slovakia

#art 6 is the most violated article for Slovakia
table(slovakia)

#get number of violations for switzerland (since the values are displayed as a list, we need to apply unlist; in a second step we remove the spaces)
switz <- agg$concat_violations[2]
switz <- str_split(switz, 'violation of art ')
switz <- unlist(switz)
switz <- gsub(" |   ", "", switz)

#not necessary, but facilitates the identification of articles (see levels)
switz <- as.factor(switz)
switz

#Interestingly, art 6 is the most violated article for Switzerland 
table(switz)



















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














