### Beginning stages of Topic Analysis for AskCFIA project
### Anna Gow - Summer 2018


# Import relevant libraries

require("qdap")
require("tm")
require("class")
require("SnowballC")
require("ggplot2")
require("caret")
require("dplyr")
require("stringr")
require("tidyverse")
require("tidytext")
require("ggplot2")
require("broom")
require("plyr")
require("data.table")
require("RWeka")
require("enc")

# This option is a hacky fix to get around some computing power issues
options(expressions = 5e5)

# Read csv, rename columns
# TODO: Change the read.csv function for your setup
# TODO: Encoding may need to be changed (a headache to figure out)
df <- read.csv("FullAskCFIA.csv", encoding = "ANSI", stringsAsFactors = FALSE)
names(df) <- c("ID", "Description", "Response", "Type", "Level", "Country", "Province", "Specialist", "Start.Date", "Additional.Info.Date", "End.Date", 
               "Service.Date", "Status", "Target.OGE", "Consultations", "Contributor", "Language", "Item.Type", "Path")

# Subset first to most useful columns, rename
# This dataframe has 2739 rows and 8 variables
info_df <- subset(df, select = c("ID", "Description", "Type", "Province", "Country", "Start.Date", "Language", "Level"))
names(info_df)[names(info_df) == "Start.Date"] <- "Date"

# Format "Date" column
info_df$Date <- as.Date(info_df$Date, "%m/%d/%Y")
info_df <- subset(info_df, Date >= as.Date("2016-07-30"))
info_df$Date <- format(info_df$Date, "%m")

# Rewrite the levels as integers 
# This makes them an ordered factor, helpful later on
info_df$Level[info_df$Level == "AskCFIA_DemandezACIA;#174"] <- 1
info_df$Level[info_df$Level == "AskCFIATechLiaison_DemandezACIALiaisonTech;#213"] <- 2
info_df$Level[info_df$Level == "AskCFIATech_DemandezACIATech;#175"] <- 3


ddply(info_df, c("Type"), summarise, N = length(Type))

# Re-name types
info_df$Type[info_df$Type == "askCFIA - CFIA Fish List Application"] <- "Fish_List"
info_df$Type[info_df$Type == "askCFIA - Dairy"] <- "Dairy" 
info_df$Type[info_df$Type == "askCFIA - Eggs and Egg Products"] <- "Egg"  
info_df$Type[info_df$Type == "askCFIA - Fish and Seafood"] <- "Seafood"  
info_df$Type[info_df$Type == "askCFIA - Fresh Fruits and Vegetables"] <- "Fruit" 
info_df$Type[info_df$Type == "askCFIA - Honey"] <- "Honey"
info_df$Type[info_df$Type == "askCFIA - Maple"] <- "Maple"
info_df$Type[info_df$Type == "askCFIA - Processed Products (Fruits and Vegetables)"] <- "Processed" 
info_df$Type[info_df$Type == "askCFIA - Food other (IMFP)"] <- "Other"


ddply(info_df, c("Type"), summarise, N = length(Type))


ddply(info_df, c("Country"), summarise, N = length(Country))

# TODO: get rid of "USA", "Other -", "Autre", etc.
# Rename USA by state or maybe by regino
# Combine Other/Autre counts when same country


# US Regions
# TODO: Geographical analysis within US Regions
Northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania")
Midwest <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")
South <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "District of Columbia", "West Virginia", 
           "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas")
West <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington")

# Check which questions came from the USA

for (i in 1:nrow(info_df)) {
  info_df$USA[i] <- ifelse(grepl(" USA| États-Unis", info_df$Country[i], ignore.case = TRUE), 1, 0)
}

index_USA <- which(info_df$USA == 1)

for (int in index_USA) {
  info_df$Country[int] <- gsub(" - USA| - États-Unis", "", info_df$Country[int])
  info_df$Province[int] <- info_df$Country[int]
}

index_Californie <- which(info_df$Country == "Californie")
info_df$Country[index_Californie] <- gsub("Californie", "California", info_df$Country[index_Californie])

info_df$Country <- gsub("Autre - |Other - ", "", info_df$Country, ignore.case = TRUE)
info_df$Country <- gsub("\\s*\\([^\\)]+\\)","",as.character(info_df$Country))
info_df$Country <- gsub("United Kingdom of Great Britain and Northern Ireland|United Kingdom of Great Britain and Northern Ireland ", "UK", as.character(info_df$Country))


## and the better, more sophisticated version:
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
info_df$Country <- capwords(info_df$Country, strict = TRUE)

info_df$Country <- gsub("Uk$", "UK", as.character(info_df$Country))


# Translate country names for easier analysis
country_translate <- function(v) {
  v <- gsub("Algérie", "Algeria", v, ignore.case = TRUE)
  v <- gsub("Cote D'ivoire", "Ivory Coast", v, ignore.case = TRUE)
  v <- gsub("Espagne", "Spain", v, ignore.case = TRUE)
  v <- gsub("Maroc", "Morocco", v, ignore.case = TRUE)
  v <- gsub("Mexique", "Mexico", v, ignore.case = TRUE)
  v <- gsub("Ouzbékistan", "Uzbekistan", v, ignore.case = TRUE)
  v <- gsub("Suisse", "Switzerland", v, ignore.case = TRUE)
  v <- gsub("Tunisie", "Tunisia", v, ignore.case = TRUE)
  return(v)
}

country_fix <- function(v) {
  v <- gsub("United Arab Emirates", "UAE", v, ignore.case = TRUE)
  v <- gsub("Australia  And France", "Australia", v, ignore.case = TRUE)
  v <- gsub("Austria/germany", "Germany", v, ignore.case = TRUE)
  v <- gsub("Lao People's Democratic Republic", "Laos", v, ignore.case = TRUE)
  v <- gsub("Russian Federation", "Russia", v, ignore.case = TRUE)
  v <- gsub("Trinidad and Tobago", "Trinidad", v, ignore.case = TRUE)
  v <- gsub("Viet Nam", "Vietnam", v, ignore.case = TRUE)
  
  return(v)
}

info_df$Country <- country_translate(info_df$Country)
info_df$Country <- country_fix (info_df$Country)

for (int in index_USA) {
  info_df$Country[int] <- "USA"
}


for (i in 1:nrow(info_df)) {
  info_df$International[i] <- ifelse(grepl("Canada", info_df$Country[i], ignore.case = TRUE), 0, 1)
}

ddply(info_df, c("Country"), summarise, N = length(Country))
ddply(info_df, c("International"), summarise, N = sum(International))

# Remove accents 
accent_corp <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "l'|j'|d'", replace = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "é|è|ê|ë", replace = "e")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "à|â|ä", replace = "a")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "ç", replace = "c")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "ñ", replace = "n")
  return(corpus)
}

# Clean the corpus
# NOTE: No stemming if computing power allows for it! Otherwise stem.

clean_corp <- function(corpus) {
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- accent_corp(corpus)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = ".*https.*", replace = "websiteurl")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = ".*http.*", replace = "websiteurl")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = ".*www.*", replace = "websiteurl")
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c("websiteurl", "helloi", Top200Words))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[^[:alnum:] ]", replace = "")
  #corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}

# AskCFIA Team asked me to remove "Fish_List" questions 
info_noFish <- subset(info_df, Type != "Fish_List")

# Limit to English questions
en_info <- subset(info_noFish, Language == "English")
en_info$Total <- str_count(en_info$Description, '\\s+')+1 



# Unfortunately, upon inspection, it turns out that some are still french
# Find & remove entries starting with "Bonjour"
for (i in 1:nrow(en_info)) {
  en_info$French[i] <- ifelse(grepl("Bonjour.*", en_info$Description[i], ignore.case = TRUE), 1, 0)
}

en_info <- subset(en_info, French == 0)

# Again there are still some in French
# Remove more common French works

for (i in 1:nrow(en_info)) {
  en_info$French[i] <- ifelse(grepl("français.*", en_info$Description[i], ignore.case = TRUE), 1, 0)
}
en_info <- subset(en_info, French == 0)

for (i in 1:nrow(en_info)) {
  en_info$French[i] <- ifelse(grepl("j'ai.*", en_info$Description[i], ignore.case = TRUE), 1, 0)
}
en_info <- subset(en_info, French == 0)

for (i in 1:nrow(en_info)) {
  en_info$French[i] <- ifelse(grepl(" nous.*", en_info$Description[i], ignore.case = TRUE), 1, 0)
}
en_info <- subset(en_info, French == 0)
rownames(en_info) <- seq(length=nrow(en_info)) 

for (i in 1:nrow(en_info)) {
  en_info$French[i] <- ifelse(grepl(" fruits et légumes ", en_info$Description[i], ignore.case = TRUE), 1, 0)
}
en_info <- subset(en_info, French == 0)
rownames(en_info) <- seq(length=nrow(en_info)) 

for (i in 1:nrow(en_info)) {
  en_info$French[i] <- ifelse(grepl(" produira ", en_info$Description[i], ignore.case = TRUE), 1, 0)
}
en_info <- subset(en_info, French == 0)
rownames(en_info) <- seq(length=nrow(en_info)) 

for (i in 1:nrow(en_info)) {
  en_info$French[i] <- ifelse(grepl(" poisson ", en_info$Description[i], ignore.case = TRUE), 1, 0)
}
en_info <- subset(en_info, French == 0)
rownames(en_info) <- seq(length=nrow(en_info)) 


# This has eliminated all French entries until May 2018, new data may cause issues 
# NOTE: If doing this analysis with new data set, manually verify!



# Now isolate the description to start text analysis
en_words <- en_info$Description
en_words <- tolower(en_words)
en_words <- removeNumbers(en_words)

# Amplification and Deamplification words
amp_deamp_words <- c(qdapDictionaries::amplification.words, qdapDictionaries::deamplification.words)


stopwords_df <- data.frame(words = stopwords("en"), stringsAsFactors = FALSE)
amp_deamp_df <- data.frame(words = amp_deamp_words, stringsAsFactors = FALSE)

# Join stopwords with amp/deamp words because we will still compute polarity
# Need amp/deamp words to do this but they overlap with some stopwords
# NOTE: There's probably a better way to do this!

stopwords_amp <- anti_join(stopwords_df, amp_deamp_df)
stopwords_new <- stopwords_amp$words

en_words <- removeWords(en_words, c(stopwords_new, "like", "[\n]"))
en_words <- utf8(en_words)

polarity <- vector()


for (i in 1:nrow(en_info)) {
  pol_list <- polarity(en_words[i])
  pol_df <- as.data.frame(pol_list)
  polarity <- c(polarity, pol_df$all.polarity)
}

en_info$Polarity <- polarity

en_info$Polarity[is.na(en_info$Polarity)] <- 0

en_corp <- VCorpus(VectorSource(en_words))

all_clean <- clean_corp(en_corp)

# NOTE: Using TfIdf weighting allows for faster computation
# Regular weighting may give better results, was unable to compute on my device

#all_dtm = DocumentTermMatrix(all_clean, control = list(weighting = weightTfIdf))
all_dtm = DocumentTermMatrix(all_clean) 
all_mat = as.matrix(all_dtm)

all_df = as.data.frame(all_mat)


sum_vector <- vector()
for (i in 1:ncol(all_df)) {
  sum_vector[i] <- sum(all_df[,i])
}

max(sum_vector)
max_10p <- floor(0.1*max(sum_vector))
big_idx <- which(sum_vector >= max_10p)


# Most common words
colnames(all_df)[big_idx]
common_words <- colnames(all_df)[big_idx]

#TODO: Create maps showing related bigrams + trigrams
english_df <- data.frame(ID = seq(length = nrow(en_info)), Question = (en_info$Description), Level = (en_info$Level), Type = (en_info$Type))
new_corp2 <- VCorpus(VectorSource(english_df$Question))


clean_corp2 <- function(corpus) {
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "please"))
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = ".*https.*", replace = "websiteurl")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = ".*http.*", replace = "websiteurl")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = ".*www.*", replace = "websiteurl")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "labelling", replace = "labeling")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "labelled", replace = "labeled")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "license", replace = "licence")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "licensed", replace = "licenced")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "licensing", replace = "licencing")
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c("websiteurl", "helloi", Top200Words, "thank", "thanks", "please", "hello", "regards", "dear", "eat", "hearing"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[^[:alnum:] ]", replace = "")
  #corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}

new_corp2 <- clean_corp2(new_corp2)

library(RWeka)

# Define bigram tokenizer
tokenizer2 <- function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}

# Make a bigram TDM
bigram_tdm <- TermDocumentMatrix(new_corp2, control = list(tokenize = tokenizer2))

bigram_m <- as.matrix(bigram_tdm)

term_frequency <- rowSums(bigram_m)
term_frequency <- sort(term_frequency, decreasing = TRUE)
term_freq_df <- data.frame(bigram = names(term_frequency), count = term_frequency, row.names = NULL)
top_terms_df <- subset(term_freq_df, count > 19)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

bigram_df <- data.frame(bigram = "test", count = 1, level = 2, type = "Other", stringsAsFactors = FALSE)

for (top_bigrams in top_terms_df$bigram) {
  #add row to empty df with bigram, count, median level, mode type
  entry_df <- english_df[grep(top_bigrams, english_df$Question, ignore.case = TRUE),]
  entry_df$Level <- as.numeric(entry_df$Level)
  median_level <- median(entry_df$Level)
  mode_type <- Mode(entry_df$Type)
  
  newEntry_df <- data.frame(bigram = top_bigrams, count = as.numeric(top_terms_df$count[top_terms_df$bigram == top_bigrams]), level = median_level, type = mode_type)
  
  bigram_df <- rbind(bigram_df, newEntry_df)
}

View(bigram_df)



##TODO: Trigrams

english_df <- data.frame(ID = seq(length = nrow(en_info)), Question = (en_info$Description), Level = (en_info$Level), Type = (en_info$Type))
new_corp3 <- VCorpus(VectorSource(english_df$Question))


# Leave in certain preopostions
library(dplyr)

stopwords_df <- data.frame(words = stopwords("en"), stringsAsFactors = FALSE)
prep_df <- data.frame(words = c("of", "to", "from", "and"), stringsAsFactors = FALSE)
stopwords <- as_tibble(stopwords_df)
prep <- as_tibble(prep_df)


stopwords_prep <- anti_join(stopwords, prep)
stopwords_new <- stopwords_prep$words


clean_corp3 <- function(corpus) {
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords_new))
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = ".*https.*", replace = "websiteurl")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = ".*http.*", replace = "websiteurl")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = ".*www.*", replace = "websiteurl")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "labelling", replace = "labeling")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "labelled", replace = "labeled")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "license", replace = "licence")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "licensed", replace = "licenced")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "licensing", replace = "licencing")
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, c("please", "websiteurl", "helloi", "thank", "thanks", "please", 
                                          "hello", "regards", "dear", "eat", "nicholas", "question", "you",
                                          "may", "us", "want", "we", "i", "like", "information received"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "[^[:alnum:] ]", replace = "")
  corpus <- tm_map(corpus, stemDocument)
  return(corpus)
}

new_corp3 <- clean_corp3(new_corp3)

# Define trigram tokenizer
tokenizer3 <- function(x) {NGramTokenizer(x, Weka_control(min = 3, max = 3))}

# Make trigram tdm
trigram_tdm <- TermDocumentMatrix(new_corp3, control = list(tokenize = tokenizer3))

trigram_m <- as.matrix(trigram_tdm)

term_frequency <- rowSums(trigram_m)
term_frequency <- sort(term_frequency, decreasing = TRUE)
term_freq_df <- data.frame(trigram = names(term_frequency), count = term_frequency, row.names = NULL)
View(term_freq_df)


top_terms_df <- subset(term_freq_df, count > 3)

trigram_df <- data.frame(trigram = "test", count = 1, level = 2, type = "Other", stringsAsFactors = FALSE)

for (top_trigrams in top_terms_df$trigram) {
  #add row to empty df with bigram, count, median level, mode type
  entry_df <- english_df[grep(top_trigrams, english_df$Question, ignore.case = TRUE),]
  entry_df$Level <- as.numeric(entry_df$Level)
  median_level <- median(entry_df$Level)
  mode_type <- Mode(entry_df$Type)
  
  newEntry_df <- data.frame(trigram = top_trigrams, count = as.numeric(top_terms_df$count[top_terms_df$trigram == top_trigrams]), level = median_level, type = mode_type)
  
  trigram_df <- rbind(trigram_df, newEntry_df)
}

View(trigram_df)
