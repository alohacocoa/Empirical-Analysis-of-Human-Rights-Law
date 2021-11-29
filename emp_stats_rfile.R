
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

#convert everything to lower case to facilitate recoding
hudoc$conclusion <- sapply(hudoc$conclusion, tolower)
hudoc$case <- sapply(hudoc$case, tolower)

#create country variable
hudoc$case <- gsub("v. |c. ", "vs.", hudoc$case)
hudoc$country <- gsub(".*vs.", "", hudoc$case)
hudoc$country <- word(hudoc$country, 1)
table(hudoc$country)
hudoc$country <- gsub("suisse", "switzerland", hudoc$country)

#create text variable as a first step to retract violated articles
hudoc$text <- hudoc$conclusion

#this makes it so that the french is the same as english
hudoc$text <- gsub("  |   ", " ", hudoc$text)
hudoc$text <- gsub(" de l'", " of ", hudoc$text)
hudoc$text <- gsub("article ", "art. ", hudoc$text)
hudoc$text <- gsub("non ", "no ", hudoc$text)
hudoc$text <- gsub("non-violation", "no violation", hudoc$text)
hudoc$text <- gsub("\\+", " violation of art. ", hudoc$text)

#this looks for the phrase "violation of" followed by any number of non-digit characters and ends with one or more digits. the whole phrase must not be preceded by "no ". the matches are then written into the new varialbe "violations_with_text". the variable is a list
hudoc$violations_with_text <-  stringr::str_extract_all(hudoc$text, "(?<!no\\s)violation\\sof\\sart\\D{0,20}\\d+(?!\\sof\\sprotocol)") 

#this loop below gets rid of duplicate values from situations like "violation of art. 14+3" where 14 was matched twice
for (l in 1:length(hudoc$violations_with_text)) {
  hudoc$violations_with_text[[l]] <- unique(hudoc$violations_with_text[[l]])
}

#create new dataobject and group new dataset "bycountry" by country, remove list values by applying unlist
bycountry <- apply(hudoc, 2, function(y) sapply(y, function(x) paste(unlist(x), collapse=" ")))

#aggregate data by country, concatenate text of violations
agg <- aggregate(data=bycountry, violations_with_text~country, paste0, collapse=' ')


#get number of violations for slovakia (since the values are displayed as a list, we need to apply unlist; in a second step we remove the spaces)
slovakia <- agg$violations_with_text[1]
slovakia <- str_split(slovakia, 'violation of art. ')
slovakia <- unlist(slovakia)
slovakia <- gsub(" |   ", "", slovakia)
slovakia <- as.factor(slovakia)
slovakia
table(slovakia)


#get number of violations for switzerland (since the values are displayed as a list, we need to apply unlist; in a second step we remove the spaces)
switz <- agg$violations_with_text[2]
switz <- str_split(switz, 'violation of art. ')
switz <- unlist(switz)
switz <- gsub(" |   ", "", switz)
switz <- as.factor(switz)
switz
table(switz)

#data frame for Switzerland
x1 <- "Switzerland"
x2 <- switz
switz.table <- list(country = x1, violations = x2)
switz.table <- as.data.frame(switz.table)
switz.table <- switz.table[-1,] 
#plot
sw <- ggplot(switz.table, aes(x=violations)) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) ) + xlab("Violated ECHR Aritcles by Switzerland")
sw

#data frame for Slovakia
x3 <- "Slovakia"
x4 <- slovakia
slov.table <- list(country = x3, violations = x4)
slov.table <- as.data.frame(slov.table)
slov.table <- slov.table[-1,] 
#plot
sl <- ggplot(slov.table, aes(x=violations)) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) ) + xlab("Violated ECHR Aritcles by Slovakia")
sl
                                                











