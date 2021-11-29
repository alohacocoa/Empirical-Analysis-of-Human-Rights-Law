# Empirical-Analysis-of-Human-Rights-Law
This is a student research project for the course "Empirical Analysis of International Human Rights Law" taught at the University of St. Gallen.

# About the project
We inspect and compare the types of human rights violations committed by Switzerland and Slovakia in the last ten years. We make use of data provided by https://hudoc.echr.coe.int/eng.

Our sample contains 135 cases for Slovakia and 104 cases for Switzerland. There are no double counts.

# How to reproduce this project
To download the .csv data we used from hudoc, apply the following search parameters: case law > judgements; state > Switzerland, Slovakia; date > last ten years; violations > select all

Alternatively, this is a direct link to the search results: https://hudoc.echr.coe.int/eng#{%22languageisocode%22:[%22ENG%22,%22GER%22,%22FRE%22],%22respondent%22:[%22SVK%22,%22CHE%22],%22documentcollectionid2%22:[%22JUDGMENTS%22],%22kpdate%22:[%222011-10-31T00:00:00.0Z%22,%222021-10-31T00:00:00.0Z%22],%22violation%22:[%222%22,%222-1%22,%223%22,%225%22,%225+5-4%22,%225-1%22,%225-1-a%22,%225-1-b%22,%225-1-c%22,%225-1-f%22,%225-2%22,%225-3%22,%225-4%22,%225-5%22,%226%22,%226+6-3-c%22,%226+13%22,%226-1%22,%226-2%22,%226-3-c%22,%228%22,%228-1%22,%2210%22,%2210-1%22,%2213%22,%2213+3%22,%2213+6%22,%2213+6-1%22,%2213+8%22,%2214%22,%2214+2%22,%2214+3%22,%2214+8%22,%2214+8-1%22,%2234%22,%22P1-1%22,%22P1-1-1%22,%22P1-1-2%22],%22isplaceholder%22:[%22False%22]}

# Data manipulation
## R set-up
First, we clear the R environment and set the system language to English. In addition, we activate libraries which we need for the subsequent coding.
```{r}
#this clears the environment
rm(list = ls())

#this sets the wd to the directory of this R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#sets R to English
Sys.setenv(LANG = "en")
```


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
library(ggplot2)
library(forcats)
```

## Import

We import the raw data, filter for variables of interest, rename the variables to facilitate subsequent coding and exclude duplicates. The latter are in the dataset because for some cases we have a German as well as French translation. 
```{r}
hudoc <- read.csv("hudoc.csv")
head(hudoc)
hudoc <- hudoc %>% rename(case = Document.Title  ,case_id = Application.Number, 
                          concdate = Date, 
                          conclusion = Conclusion)%>% select(-3, -4) %>% distinct(case_id, .keep_all = TRUE)
```

## Step 1
Now, we start with the recoding. First we convert every variable that contains text and which we need to answer our research questions to lower case. 
```{r}
hudoc$conclusion <- sapply(hudoc$conclusion, tolower)
hudoc$case <- sapply(hudoc$case, tolower)
```

## Step 2
Since our dataset does not contain a country variable, we create one.
```{r}
hudoc$case <- gsub("v. |c. ", "vs.", hudoc$case)
hudoc$country <- gsub(".*vs.", "", hudoc$case)
hudoc$country <- word(hudoc$country, 1)
table(hudoc$country)
hudoc$country <- gsub("suisse", "switzerland", hudoc$country)
table(hudoc$country)
```

## Step 3
In a second step, we create a copy of the conclusion variable which will be our baseline for the following coding steps. In addition, we translate certain words from French to English to facilitate the extraction. 
```{r}
hudoc$text <- hudoc$conclusion
hudoc$text <- gsub("  |   ", " ", hudoc$text)
hudoc$text <- gsub(" de l'", " of ", hudoc$text)
hudoc$text <- gsub("article ", "art. ", hudoc$text)
hudoc$text <- gsub("non ", "no ", hudoc$text)
hudoc$text <- gsub("non-violation", "no violation", hudoc$text)
hudoc$text <- gsub("\\+", " violation of art. ", hudoc$text)
```

## Step 4
Starting from the text variable, we create another one. The new variable "violations_with_text" looks for the following structure: phrase "violation of" followed by any number of non-digit characters and ends with one or more digits while the whole phrase must not be preceded by "no". This results in a list variable.
```{r}
hudoc$violations_with_text <-  stringr::str_extract_all(hudoc$text, "(?<!no\\s)violation\\sof\\sart\\D{0,20}\\d+(?!\\sof\\sprotocol)") 
```

## Step 5
We build a loop to get rid of duplicate values in the "violations_with_text" variable as now we have situations where the conclusion lists a certain article multiple times in a phrase, e.g. Insert case example.
```{r}
for (l in 1:length(hudoc$violations_with_text)) {
  hudoc$violations_with_text[[l]] <- unique(hudoc$violations_with_text[[l]])
}
```

## Step 6
In order to extract the number of violations per country, we create a new data object and group the new dataset called "bycountry" by country and since our variable of interest "violations_with_text" is a list variable we unlist it.
```{r}
bycountry <- apply(hudoc, 2, function(y) sapply(y, function(x) paste(unlist(x), collapse=" ")))
```

## Step 7

We change the structure of our dataset by aggregating all violations by country so that we only have two rows: one for violations by Switzerland and one for violations by Slovakia. 
```{r}
agg <- aggregate(data=bycountry, violations_with_text~country, paste0, collapse=' ')
```

## Step 8

Now, we extract the number of violations per Country. First, we extract the whole row for Slovakia and unlist the object. We get rid of the "violations of art." component as well as the resulting spaces in order to be able to count the article number. Although not necessary, we transform the information as a factor variable to be able to see which articles are generally included for Slovakia in our data (when running the command, see Levels at the bottom of the output).
```{r}
slovakia <- agg$violations_with_text[1]
slovakia <- str_split(slovakia, 'violation of art. ')
slovakia <- unlist(slovakia)
slovakia <- gsub(" |   ", "", slovakia)
slovakia <- as.factor(slovakia)
slovakia
table(slovakia)
```
We can see, article 6 is the most violated article of 72 violations.

##Step 9

We repeat the above steps for Switzerland. 
```{r}
switz <- agg$violations_with_text[2]
switz <- str_split(switz, 'violation of art. ')
switz <- unlist(switz)
switz <- gsub(" |   ", "", switz)
switz <- as.factor(switz)
switz
table(switz)
```
In contrast, we see article 8 is the most violated article by Switzerland.


## Step 10

We plot our results by first creating datasets of our results.

```{r}
#data frame for Switzerland
x1 <- "Switzerland"
x2 <- switz
switz.table <- list(country = x1, violations = x2)
switz.table <- as.data.frame(switz.table)
switz.table <- switz.table[-1,] 

sw <- ggplot(switz.table, aes(x=violations)) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) ) + xlab("Violated ECHR Aritcles by Switzerland")
sw

#data frame for Slovakia
x3 <- "Slovakia"
x4 <- slovakia
slov.table <- list(country = x3, violations = x4)
slov.table <- as.data.frame(slov.table)
slov.table <- slov.table[-1,] 

sl <- ggplot(slov.table, aes(x=violations)) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) ) + xlab("Violated ECHR Aritcles by Slovakia")
sl
