rm(list =ls())
library(dplyr)
library(tidyr)
library(stringr)
library(RTextTools)

# define paths
in_path <- "~/Drobox/GitHub/Semantica/Semantic-Search/data/Qualtrics/Study 1/Raw/"
out_path <- "~/Drobox/GitHub/Semantica/Semantic-Search/data/Qualtrics/Study 1/Processed/"

# define pre-processing options (in addition to lower-casing and removing punctuation)
PLURALS <- TRUE
COLLOCATIONS <- TRUE
STEM <- FALSE

# load data
Study1 <- read.csv(paste0(in_path, "SurveyAgg.csv"), stringsAsFactors = FALSE, sep=",")

# add obs id
Study1$pid <- paste0("pid", 1:nrow(Study1))
# add study id
Study1$study <- "study1"

# Fluency component ------------------------------

# keep variables of interest
list.cols <- colnames(Study1)[grep("^W[[:digit:]]_", colnames(Study1))]
Study1Fluency <- Study1[,c("pid", list.cols)]

# change associations column names
cues <- c("government", "american.values", "democrat", "republican", "welfare")
for(i in 2:(length(cues)+1)){
  colnames(Study1Fluency)[grep(paste("^W", i, sep = ""), colnames(Study1Fluency))] <- c(paste(cues[i-1], 1:20, sep = ""))
}

# add cue to fluency list
for(i in cues){Study1Fluency[,paste0(i, "0")] <- i}

# convert from wide to long format
Study1Fluency <- Study1Fluency %>% gather(cue, fluency, government1:welfare0)
Study1Fluency <- Study1Fluency %>% mutate(token_order = as.integer(gsub("[^[:digit:]]", " ", Study1Fluency$cue)))
Study1Fluency$cue <- gsub("[[:digit:]]+$", replacement = "", x = Study1Fluency$cue)
Study1Fluency <- Study1Fluency %>% arrange(pid, cue, token_order) %>% select(pid, cue, fluency)

# Survey component ------------------------------

# recode gender
names(Study1)[which(names(Study1) == "D2")] <- "gender"
Study1$gender[Study1$gender %in% c("Other")] <- NA
Study1$gender <- tolower(Study1$gender)

# recode party
names(Study1)[which(names(Study1) == "P2")] <- "party2"
Study1$party2[Study1$party2 %in% c("Other party (please specify)", "No preference")] <- NA
Study1$party2 <- tolower(Study1$party2)

# recode ideology
names(Study1)[which(names(Study1) == "P1")] <- "ideology"
Study1$ideology[Study1$ideology %in% c("Haven't thought much about this")] <- NA
Study1$ideology2  <- ifelse(Study1$ideology %in% c("1. Extremely liberal", "2. Liberal", "3. Slightly liberal"), "any.liberal", ifelse(Study1$ideology %in% c("7. Extremely conservative", "6. Conservative", "5. Slightly conservative"), "any.conservative", ifelse(Study1$ideology %in% c("4. Moderate; middle of the road"), "any.independent", NA)))
Study1$ideology <- recode(Study1$ideology, "1. Extremely liberal" = "extremely.liberal", "2. Liberal" = "liberal", "3. Slightly liberal" = "slightly.liberal", "4. Moderate; middle of the road" = "moderate", "5. Slightly conservative" = "slightly.conservative", "6. Conservative" = "conservative", "7. Extremely conservative" = "extremely.conservative", .default = NA_character_)

# recode govt services attitudes
names(Study1)[which(names(Study1) == "P3")] <- "services"
Study1$services[Study1$services == "1. Government should provide many fewer services"] <- "1"
Study1$services[Study1$services == "7. Government should provide many more services"] <- "7"
Study1$services[Study1$services == "Haven't thought much about this"] <- NA
Study1$services <- as.numeric(Study1$services)

# recode welfare attitudes
names(Study1)[which(names(Study1) == "P4")] <- "welfare"
Study1$welfare[Study1$welfare == "1. Government should see to jobs and standard of living"] <- "1"
Study1$welfare[Study1$welfare == "7. Government should let each person get ahead on their own"] <- "7"
Study1$welfare[Study1$welfare == "Haven't thought much about this"] <- NA
Study1$welfare <- as.numeric(Study1$welfare)
Study1$welfare <- recode(Study1$welfare , `1` = 7L, `2` = 6L, `3` = 5L, `4` = 4L,  `5` = 3L, `6` = 2L, `7` = 1L)

# recode other vars
names(Study1)[which(names(Study1) == "D6")] <- "religion"
names(Study1)[which(names(Study1) == "D7")] <- "education"
names(Study1)[which(names(Study1) == "D8")] <- "hispanic"
names(Study1)[which(names(Study1) == "D9")] <- "race"

# create minority variable
Study1$minority <- ifelse(Study1$race %in% c("White"), "white", "minority")
Study1$minority[Study1$hispanic == "Yes"] <- "minority"
Study1$minority[is.na(Study1$race)] <- NA

# --------------------------
# PRE-PROCESSING            ----
# --------------------------
Study1Fluency$fluency <- tolower(Study1Fluency$fluency)
Study1Fluency$fluency <- gsub("'", "", Study1Fluency$fluency)  # remove apostrophes
Study1Fluency$fluency <- gsub("\\.", "", Study1Fluency$fluency)  # remove full stops
Study1Fluency$fluency <- gsub("[^[:alnum:]]", " ", Study1Fluency$fluency) # remove all non-alpha characters
Study1Fluency$fluency <- str_replace_all(Study1Fluency$fluency, "^ +| +$|( ) +", "\\1")  # remove excess white space
Study1Fluency$fluency <- gsub("^us$", "united states", Study1Fluency$fluency) # combine multiple word entries
Study1Fluency$fluency <- gsub(" ", "_", Study1Fluency$fluency) # combine multiple word entries
Study1Fluency <- Study1Fluency %>% filter(fluency!="")

if(PLURALS){
vocab_plurals <- Study1Fluency$fluency
vocab_plurals <- vocab_plurals[!grepl("_", vocab_plurals)]
vocab_plurals <- vocab_plurals[grepl("s$", vocab_plurals)]
vocab_plurals <- tibble(pattern = unique(vocab_plurals), replacement = gsub("s$", "", unique(vocab_plurals)))
vocab_plurals <- vocab_plurals %>% filter(replacement %in% Study1Fluency$fluency)
for(i in 1:nrow(vocab_plurals)){Study1Fluency$fluency <- gsub(paste0("\\<", vocab_plurals$pattern[i],"\\>"), vocab_plurals$replacement[i], Study1Fluency$fluency)} # slower but safer
}

# collocations
if(COLLOCATIONS){
vocab_collocs <- Study1Fluency$fluency
vocab_collocs <- vocab_collocs[grepl("_", vocab_collocs)]
vocab_collocs <- tibble(pattern = unique(vocab_collocs), replacement = gsub("_", "", unique(vocab_collocs)))
vocab_collocs <- vocab_collocs %>% filter(replacement %in% Study1Fluency$fluency)
for(i in 1:nrow(vocab_collocs)){Study1Fluency$fluency <- gsub(paste0("\\<", vocab_collocs$pattern[i],"\\>"), vocab_collocs$replacement[i], Study1Fluency$fluency)} # slower but safer
}

if(STEM){
  Study1Fluency$fluency <- wordStem(Study1Fluency$fluency, language = "en", warnTested = FALSE)
}

# drop repetitions in fluency lists (technically not allowed in SFT)
fluency_list <- Study1Fluency %>% group_by(pid, cue) %>% distinct(fluency, .keep_all = TRUE) %>% ungroup() # model will not work w/o deleting duplicates

# --------------------------
# CONVERT TO SS CORPUS      ----
# --------------------------
# keep only relevant survey data
survey <- Study1 %>% select(pid, ideology, ideology2, party2, gender, religion, education, hispanic, race, minority, services, welfare) %>% unique() %>% as_tibble()
# add random grouping
set.seed(1984L)
random_pid <- sample(survey$pid, length(survey$pid)/2, replace = FALSE)
survey$gang <- ifelse(survey$pid %in% random_pid, "jets", "sharks")

# tags
tags <- survey
tags$tags <- apply(tags[ , c("ideology", "ideology2", "party2", "gender", "minority", "gang")] , 1 , paste , collapse = " ")
tags <- tags[,c("pid", "tags")]

# ss corpus
sscorpus <- list("fluency" = fluency_list,
                #"valence" = NA,
                #"behavioral" = NA,
                "tags" = tags,
                "survey" = survey)


# save data
saveRDS(sscorpus, paste0(out_path, "sscorpus.rds"))


