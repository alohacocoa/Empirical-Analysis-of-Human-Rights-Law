# Preparing the environment ----
#==========================#


#this clears the environment
rm(list = ls())

#this sets the wd to the directory of this R file. might not be necessary, but better be safe than sorry
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#sets R to English
Sys.setenv(LANG = "en")

#this loads packages
library(haven)
library(tidyverse)
library(magrittr)
library(ggtext)
library(strex)



# CSV import ----
#==========================#



#Import the dataset. Filter for variables that we need in the end, rename the variables to facilitate subsequent coding, and remove duplicates.
hudoc <- read.csv("hudoc.csv")
head(hudoc)
hudoc <- hudoc %>% rename(case = Document.Title  ,case_id = Application.Number, 
                          concdate = Date, 
                          conclusion = Conclusion)%>% select(-3, -4) %>% distinct(case_id, .keep_all = TRUE)




# Data cleaning and variable extraction ----
#==========================#



#convert everything to lowercase to facilitate recoding
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

#this makes it so that the French is the same as English
hudoc$text <- gsub("  |   ", " ", hudoc$text)
hudoc$text <- gsub(" de l'", " of ", hudoc$text)
hudoc$text <- gsub("article ", "art. ", hudoc$text)
hudoc$text <- gsub("non ", "no ", hudoc$text)
hudoc$text <- gsub("non-violation", "no violation", hudoc$text)
hudoc$text <- gsub("\\+", " violation of art. ", hudoc$text)

#this looks for the phrase "violation of" followed by any number of non-digit characters and ends with one or more digits. the whole phrase must not be preceded by "no ". the matches are then written into the new variable "violations_with_text". the variable is a list. protocols are excluded
hudoc$violations_with_text <-  stringr::str_extract_all(hudoc$text, "(?<!no\\s)violation\\sof\\sart\\D{0,20}\\d+(?!\\sof\\sprotocol)") 

#this loop below gets rid of duplicate values from situations like "violation of art. 14+3" where 14 was matched twice
for (l in 1:length(hudoc$violations_with_text)) {
  hudoc$violations_with_text[[l]] <- unique(hudoc$violations_with_text[[l]])
}



# Creating new data objects for analysis ----
#==========================#


#create new dataobject and group new dataset "bycountry" by country, remove list values by applying unlist
bycountry <- apply(hudoc, 2, function(y) sapply(y, function(x) paste(unlist(x), collapse=" ")))

#aggregate data by country, concatenate text of violations
agg <- aggregate(data=bycountry, violations_with_text~country, paste0, collapse=' ')


#get number of violations for Slovakia (since the values are displayed as a list, we need to apply unlist; in a second step we remove the spaces)
slovakia <- agg$violations_with_text[1]
slovakia <- str_split(slovakia, 'violation of art. ')
slovakia <- unlist(slovakia)
slovakia <- gsub(" |   ", "", slovakia)
slovakia <- as.factor(slovakia)
slovakia
table(slovakia)


#get number of violations for Switzerland (since the values are displayed as a list, we need to apply unlist; in a second step we remove the spaces)
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
switz.table <- switz.table %>% group_by(violations) %>% summarise(count = n())
switz.table$country <- "Switzerland"


#data frame for Slovakia
x3 <- "Slovakia"
x4 <- slovakia
slov.table <- list(country = x3, violations = x4)
slov.table <- as.data.frame(slov.table)
slov.table <- slov.table[-1,]
slov.table <- slov.table %>% group_by(violations) %>% summarise(count = n())
slov.table$country <- "Slovakia"

#combined data frame    
comb.table <- rbind(slov.table, switz.table) %>%
  group_by(country) %>% 
  mutate(total_country = sum(count), proportion = count/total_country) %>%
  ungroup()
comb.table$violations <- factor(comb.table$violations, ordered = TRUE, c("", 2, 3, 5, 6, 8, 10, 13, 14, 34)) #for some reason "" was listed as a level too



# Plots ----
#==========================#


#plotting both countries separately
bar <- function(data, country){
  ggplot(data, aes(x=reorder(violations, count), y=count)) +
    geom_bar(color="blue", fill=rgb(0.2,0.2,0.2,0.7), stat = "identity") +
    geom_text(aes(label=count), vjust=-0.5) +
    labs(title = paste("Violated ECHR articles by", country),
         subtitle = "last 10 years, since 2021") +
    xlab("article nr.") +
    ylab("violation count")
  }

sw <- bar(data = switz.table, country = "Switzerland")
sl <- bar(data = slov.table, country = "Slovakia")
sw
sl

#plotting both countries combined
comb1 <- ggplot(data=comb.table, aes(x=country, y=total_country, fill=country)) +
  geom_bar(color="blue", stat = "identity", position="dodge", show.legend = FALSE) +
  geom_text(aes(label=total_country), position=position_dodge(width=0.9), vjust=-0.5) +
  labs(title = "Violated ECHR articles",
       subtitle = "total number, last 10 years, since 2021") +
  ylab("violation count") +
  scale_fill_manual(aesthetics="fill", values=c(rgb(0.1,0.4,0.5,0.7), rgb(0.5,0.4,0.1,0.7)))
comb1

comb2 <- ggplot(data=comb.table, aes(x=violations, y=proportion, fill=country)) +
  geom_bar(color="blue", stat = "identity", position="dodge") +
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.5) +
  labs(title = "Violated ECHR articles",
       subtitle = "proportional to country total, absolute numbers above bars, last 10 years, since 2021") +
  ylab("proportion of country's total violations") +
  xlab("article nr.") +
  scale_fill_manual(aesthetics="fill", values=c(rgb(0.1,0.4,0.5,0.7), rgb(0.5,0.4,0.1,0.7)))
comb2


#saving plots
dir.create('./plots')

ggsave('violated_art_switzerland.png', path = "./plots", plot = sw, width = 7.5, height = 5, device = "png")
ggsave('violated_art_slovakia.png', path = "./plots", plot = sl, width = 7.5, height = 5, device = "png")
ggsave('violated_art_total.png', path = "./plots", plot = comb1, width = 4.5, height = 5, device = "png")
ggsave('violated_art_prop.png', path = "./plots", plot = comb2, width = 7.5, height = 5, device = "png")









