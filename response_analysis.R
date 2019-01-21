# Import relevant libraries

library("qdap")
library("class")
library("SnowballC")
library("ggplot2")
library("caret")
library("dplyr")
library("stringr")
library("tidyverse")
library("tidytext")
library("ggplot2")
library("broom")
library("plyr")
library("data.table")
library("RWeka")
options(expressions = 5e5)

# Read csv, rename columns
df <- read.csv("FullAskCFIA.csv", encoding = "ANSI", stringsAsFactors = FALSE)
names(df) <- c("ID", "Description", "Response", "Type", "Level", "Country", "Province", "Specialist", "Start.Date", "Additional.Info.Date", "End.Date", 
               "Service.Date", "Status", "Target.OGE", "Consultations", "Contributor", "Language", "Item.Type", "Path")

response_df <- subset(df, select = c("ID", "Response", "Type", "Level"))

# Rewrite the levels as integers 
# This makes them an ordered factor, helpful later on
response_df$Level[response_df$Level == "AskCFIA_DemandezACIA;#174"] <- 1
response_df$Level[response_df$Level == "AskCFIATechLiaison_DemandezACIALiaisonTech;#213"] <- 2
response_df$Level[response_df$Level == "AskCFIATech_DemandezACIATech;#175"] <- 3




en_info_df <- read.csv("en_info.csv", encoding = "ANSI", stringsAsFactors = FALSE)
names(en_info_df) <- c("Row", "ID")
en_info_ID <- en_info_df$ID


eng_response_df <- response_df[response_df$ID %in% en_info_ID, ]
rownames(eng_response_df) <- seq(length = nrow(eng_response_df))
eng_response_df <- subset(eng_response_df, Response != "")

library(quanteda)
library(PerformanceAnalytics)
description <- data.frame(id = eng_response_df$ID, text = eng_response_df$Response, stringsAsFactors = FALSE)
description_corp <- corpus(description, text_field = "text")

# Compute readability
readability_df <- textstat_readability(description_corp, measure = c("Coleman.Liau.grade", "Flesch.Kincaid", "FOG"))
readability_df <- readability_df[,2:4]


# readability_rounded_df <- data.frame(Coleman.Liau.grade = floor(readability_df$Coleman.Liau.grade), 
#                                      Flesch.Kincaid = floor(readability_df$Flesch.Kincaid),
#                                      FOG = floor(readability_df$FOG))

eng_response_df <- cbind(eng_response_df, readability_df)
eng_response_df$Level <- as.numeric(eng_response_df$Level)
eng_response_df$Coleman.Liau.grade <- as.numeric(eng_response_df$Coleman.Liau.grade)
eng_response_df$Flesch.Kincaid <- as.numeric(eng_response_df$Flesch.Kincaid)
eng_response_df$FOG <- as.numeric(eng_response_df$FOG)


my_data <- eng_response_df[, 4:7]
chart.Correlation(my_data, histogram = TRUE, method = "spearman")

readLevel_df <- subset(eng_response_df, select = c("ID", "Coleman.Liau.grade", "Flesch.Kincaid", "FOG"))


eng_new <- right_join(df, readLevel_df, by = "ID")

write.csv(eng_new, "askCFIA_RL.csv", row.names = FALSE)
