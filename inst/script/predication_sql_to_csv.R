## This script turns the SQL dump file available from the 
## National Library of Medicine site (see below)
## to a CSV file.
## See graph_semmed.R to generate the graph representation
## from the CSV file generated here.

library(stringr)
library(dplyr)
library(readr)

## Set global options
options(stringsAsFactors = FALSE)

## Read in SQL dump file
## You can download the gzipped SQL dump file from here:
## https://ii.nlm.nih.gov/SemRep_SemMedDB_SKR/SemMedDB/SemMedDB_download.shtml
semmed <- readLines("../data/semmedVER31_R_PREDICATION_06302018.sql")

## Get variable names
var_names <- semmed[26:37]
var_names <- str_extract(var_names, "`.+`") %>% str_replace_all("`", "")

## Restrict to lines that contain data
line_start_tables <- 52
line_end_tables <- length(semmed)-13
semmed <- semmed[line_start_tables:line_end_tables]

## Split on ),(
sp <- str_split(semmed, "\\),\\(")
sp <- unlist(sp)
sp <- str_replace_all(sp, "INSERT INTO `PREDICATION` VALUES ", "")
sp <- str_replace_all(sp, "(^\\()|(\\);$)", "")

rm(semmed)
gc()

## Count the number of ,'
## There should be 8
seps <- str_extract_all(sp, ",'")
len_seps <- lengths(seps)
table(len_seps)
sp[len_seps==9]

## But sometimes there are 9.
## All the problem cases are with 'Transport Media,'
## which has a trailing comma
sp[len_seps==9] <- str_replace(sp[len_seps==9],
    "Transport Media,",
    "Transport Media"
)
stopifnot(all(str_count(sp, ",'")==8))
rm(seps, len_seps)
gc()

## Now formally split on ,'
sp <- str_split(sp, ",'")

## There should be 9 resulting sections
stopifnot(all(lengths(sp)==9))

## For memory issues break the number of database entries up
## into smaller parts
breaks <- seq(1, length(sp), by = round(length(sp)/10))
breaks[length(breaks)] <- length(sp)+1
for (i in seq_len(length(breaks)-1)) {
    ## Within the subsections created by splitting, split again on commas
    span <- breaks[i]:(breaks[i+1]-1)
    list_pred <- lapply(sp[span], function(x) {
        x <- str_replace_all(x, "'", "")
        stopifnot(
            str_count(x[1], ",")==1,
            str_count(x[6], ",")==1,
            str_count(x[9], ",")==1
        )
        dat <- c(
            str_split(x[1], ",")[[1]],
            x[2:5],
            str_split(x[6], ",")[[1]],
            x[7:8],
            str_split(x[9], ",")[[1]]
        )
        dat <- dat %>% as.list() %>% as.data.frame()
        colnames(dat) <- var_names
        dat
    })
    df_pred <- do.call(rbind, list_pred)
    df_pred <- df_pred %>%
        mutate(
            PREDICATION_ID = as.integer(PREDICATION_ID),
            SENTENCE_ID = as.integer(SENTENCE_ID),
            SUBJECT_NOVELTY = as.integer(SUBJECT_NOVELTY),
            OBJECT_NOVELTY = as.integer(OBJECT_NOVELTY)
        )
    write_csv(df_pred, path = paste0("../data/PREDICATION_", i,".csv"))
}

df_pred_list <- vector("list", length(breaks)-1)
for (i in seq_len(length(breaks)-1)) {
    df_pred <- read_csv(df_pred,
        path = paste0("../data/PREDICATION_", i,".csv"))
    df_pred_list[[i]] <- df_pred
}
df_total <- do.call(rbind, df_pred_list)
write_csv(df_total, path = "../data/PREDICATION.csv.gz")
