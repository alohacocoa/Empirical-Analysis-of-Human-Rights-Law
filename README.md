# Empirical-Analysis-of-Human-Rights-Law
This is a student research project for the course "Empirical Analysis of International Human Rights Law" taught at the University of St. Gallen.

# About the project
We inspect and compare the types of human rights violations committed by Switzerland and Slovakia in the last ten years. We make use of data provided by https://hudoc.echr.coe.int/eng.

Our sample contains 135 cases for Slovakia and 104 cases for Switzerland. There are no double counts.

# How to reproduce this project
To download the .csv data we used from hudoc, apply the following search parameters: case law > judgements; state > Switzerland, Slovakia; date > last ten years; violations > select all

Alternatively, this is a direct link to the search results: https://hudoc.echr.coe.int/eng#{%22languageisocode%22:[%22ENG%22,%22GER%22,%22FRE%22],%22respondent%22:[%22SVK%22,%22CHE%22],%22documentcollectionid2%22:[%22JUDGMENTS%22],%22kpdate%22:[%222011-10-31T00:00:00.0Z%22,%222021-10-31T00:00:00.0Z%22],%22violation%22:[%222%22,%222-1%22,%223%22,%225%22,%225+5-4%22,%225-1%22,%225-1-a%22,%225-1-b%22,%225-1-c%22,%225-1-f%22,%225-2%22,%225-3%22,%225-4%22,%225-5%22,%226%22,%226+6-3-c%22,%226+13%22,%226-1%22,%226-2%22,%226-3-c%22,%228%22,%228-1%22,%2210%22,%2210-1%22,%2213%22,%2213+3%22,%2213+6%22,%2213+6-1%22,%2213+8%22,%2214%22,%2214+2%22,%2214+3%22,%2214+8%22,%2214+8-1%22,%2234%22,%22P1-1%22,%22P1-1-1%22,%22P1-1-2%22],%22isplaceholder%22:[%22False%22]}

# Data Preparation

#Data Preparation

```{r setup, include=FALSE}
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
```

## Data Preparation

Import the dataset. Filter for variables that we need in the end as well as rename the variables to facilitate subsequent coding.

```{r}
hudoc <- read.csv("hudoc.csv")
head(hudoc)
hudoc <- hudoc %>% rename(case = Document.Title  ,case_id = Application.Number, 
                          concdate = Date, 
                          conclusion = Conclusion)%>% select(-3, -4)
```

Recode and create variables
```{r}
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
hudoc$text <- str_extract(hudoc$conclusion, "^\\D+")
hudoc$article <- str_first_number(hudoc$conclusion)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
hudoc$text <- sapply(hudoc$text, removeSpecialChars)

#junk
hudoc$text <- gsub("  |   ", " ", hudoc$text)
hudoc$text <- gsub(" de l |of ", " ", hudoc$text)
hudoc$text <- gsub("article |art ", " ", hudoc$text)
hudoc$text <- gsub("non ", "no ", hudoc$text)
hudoc$text <- gsub("preliminary objection joined to merits and dismissed victim remainder inadmissible violation of |preliminary objection joined to merits and dismissed victim violation of |preliminary objection joined to merits and dismissed ","preliminary objection joined to merits and dismissed ", hudoc$text)
hudoc$text <- gsub("exception pr liminaire rejet e |exceptions pr liminaires rejet es |preliminary objection dismissed ", "preliminary objections dismissed ", hudoc$text)
hudoc$text <- gsub("partiellement irrecevable ", "partially irrecevable ", hudoc$text)
hudoc$text <- gsub("remainder inadmissible violation  |remainder inadmissible struck out the list  ", "remainder inadmissible  ", hudoc$text)
hudoc$text <- gsub("preliminary objection joined to merits and dismissed victim remainder inadmissible |preliminary objection joined to merits and dismissed victim violation ", "preliminary objection joined to merits and dismissed ", hudoc$text)
hudoc$text <- gsub("partially irrecevable violation", "partially irrecevable ", hudoc$text)

#merge text and article no.
hudoc$violation_art <- paste(hudoc$text, hudoc$article, sep = "art ")

#create categorical variable
hudoc$catvar <- as.character(as.numeric(as.factor(hudoc$text)))
table(hudoc$catvar)
hudoc$catvar <- as.factor(ifelse(hudoc$catvar == 9, 1, 0))

#create dummy variable
hudoc$dummy <- as.character(as.numeric(as.factor(hudoc$text)))
hudoc$dummy <- car::recode(hudoc$dummy, "1= '0'; 9 = '1'; else = NA")

