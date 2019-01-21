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

question_df <- subset(df, select = c("ID", "Description", "Type", "Level"))

# Rewrite the levels as integers 
# This makes them an ordered factor, helpful later on
question_df$Level[question_df$Level == "AskCFIA_DemandezACIA;#174"] <- 1
question_df$Level[question_df$Level == "AskCFIATechLiaison_DemandezACIALiaisonTech;#213"] <- 2
question_df$Level[question_df$Level == "AskCFIATech_DemandezACIATech;#175"] <- 3




en_info_df <- read.csv("en_info.csv", encoding = "ANSI", stringsAsFactors = FALSE)
names(en_info_df) <- c("Row", "ID")
en_info_ID <- en_info_df$ID


eng_question_df <- question_df[question_df$ID %in% en_info_ID, ]
rownames(eng_question_df) <- seq(length = nrow(eng_question_df))
eng_question_df <- subset(eng_question_df, Description != "")

library(quanteda)
library(PerformanceAnalytics)
description <- data.frame(id = eng_question_df$ID, text = eng_question_df$Description, stringsAsFactors = FALSE)
description_corp <- corpus(description, text_field = "text")

# Compute readability
readability_df <- textstat_readability(description_corp, measure = c("Coleman.Liau.grade", "Flesch.Kincaid", "FOG"))
readability_df <- readability_df[,2:4]


# readability_rounded_df <- data.frame(Coleman.Liau.grade = floor(readability_df$Coleman.Liau.grade), 
#                                      Flesch.Kincaid = floor(readability_df$Flesch.Kincaid),
#                                      FOG = floor(readability_df$FOG))

eng_question_df <- cbind(eng_question_df, readability_df)
eng_question_df$Level <- as.numeric(eng_question_df$Level)
eng_question_df$Coleman.Liau.grade <- as.numeric(eng_question_df$Coleman.Liau.grade)
eng_question_df$Flesch.Kincaid <- as.numeric(eng_question_df$Flesch.Kincaid)
eng_question_df$FOG <- as.numeric(eng_question_df$FOG)


my_data <- eng_question_df[, 4:7]
chart.Correlation(my_data, histogram = TRUE, method = "spearman")

readLevel_df <- subset(eng_question_df, select = c("ID", "Coleman.Liau.grade", "Flesch.Kincaid", "FOG"))


eng_new <- right_join(df, readLevel_df, by = "ID")

write.csv(eng_new, "askCFIA_Q_RL.csv", row.names = FALSE)
