rm(list=ls())
#########################################################
### A) Installing and loading required packages
#########################################################
if (!require("tidyverse")) {
    install.packages("tidyverse", dependencies = TRUE)
    library(tidyverse)
}
if (!require("lubridate")) {
    install.packages("lubridate", dependencies = TRUE)
    library(lubridate)
}
if (!require("ggplot2")) {
    install.packages("ggplot2", dependencies = TRUE)
    library(ggplot2)
}
if (!require("fiftystater")) {
    install.packages("fiftystater", dependencies = TRUE)
    library(fiftystater)
}
if (!require("maps")) {
    install.packages("maps", dependencies = TRUE)
    library(maps)
}
if (!require("reshape2")) {
    install.packages("reshape2", dependencies = TRUE)
    library(maps)
}



#########################################################
# Read the Iraq casualties database
#########################################################
executions.df <- as.tibble(read.csv("/Users/amkhosla/Desktop/Statistics/Projects/US_Executions/US_Executions.csv"))
#executions.df <- executions.df %>%
#    "victim_Asian", "victim_Black", "victim_Latino", "victim_Native_American", "victim_Other", "victim_White"    
#executions.df <- executions.df[order(executions.df$Race, executions.df$Sex),]
    
race.type <- c("Asian", "Black", "Latino", "Native_American", "Other", "White")
race.index <- function(anrace) { return(which(race.type == anrace))}
#race.index("Native American")

compute_victim_race_vector <- function(aRow) {
    race.vector <- c("Asian"=0, "Black"=0, "Latino"=0, "Native_American"=0, "Other"=0, "White"=0)
    # To get the single element of a dataframe (MAJOR PITA)
    # bar <- as.vector(theXthRow$Colname[1])
    if((aRow$v1_number>0) & (!is.na(aRow$v1_race))) {
        race.vector[aRow$v1_race] <- race.vector[aRow$v1_race] + aRow$v1_number
    }
    if((aRow$v2_number>0) & (!is.na(aRow$v2_race))) {
        race.vector[aRow$v2_race] <- race.vector[aRow$v2_race] + aRow$v2_number
    }
    if((aRow$v3_number>0) & (!is.na(aRow$v3_race))) {
        race.vector[aRow$v3_race] <- race.vector[aRow$v3_race] + aRow$v3_number
    }
    return(race.vector)
}
row_kills <- sapply(1:1469, function(aRowIndex) { compute_victim_race_vector(executions.df[aRowIndex,]) })

victim_race_column <- function(aRace) {
    return(sapply(1:1469, function(aRowIndex) { as.numeric(unlist(row_kills[aRowIndex])[aRace]) }))
}
executions.df$victim_Asian           <- victim_race_column("Asian")
executions.df$victim_Black           <- victim_race_column("Black")
executions.df$victim_Latino          <- victim_race_column("Latino")
executions.df$victim_Native_American <- victim_race_column("Native_American")
executions.df$victim_Other           <- victim_race_column("Other")
executions.df$victim_White           <- victim_race_column("White")
#View(executions.df)
# 
# race.percent <- c(55.3, 12.6, 4.8, 17.1, 0.9, 9.3)
# executed.compute_percent <- function(x) { return(x/1469) }

# GET READY TO PLOT!!!
race_victims.df <- select(executions.df, c("Race", "victim_Asian", "victim_Black", "victim_Latino", "victim_Native_American", "victim_Other", "victim_White"))
race_victims.melt <- melt(race_victims.df, id=c("Race"))
compute_killed_races <- function(aRace) {
    race.vector <- c("Race"=aRace, "Asian"=0, "Black"=0, "Latino"=0, "Native_American"=0, "Other"=0, "White"=0)
    dummy_race.df <- race_victims.df %>% filter(Race==aRace)
    race.vector["Asian"] = sum(dummy_race.df$victim_Asian)
    race.vector["Black"] = sum(dummy_race.df$victim_Black)
    race.vector["Latino"] = sum(dummy_race.df$victim_Latino)
    race.vector["Native_American"] = sum(dummy_race.df$victim_Native_American)
    race.vector["Other"] = sum(dummy_race.df$victim_Other)
    race.vector["White"] = sum(dummy_race.df$victim_White)
    return(race.vector)
}

summary_table <- c( compute_killed_races("Asian"),
                    compute_killed_races("Black"),
                    compute_killed_races("Latino"),
                    compute_killed_races("Native_American"),
                    compute_killed_races("Other"),
                    compute_killed_races("White"))

ggplot(data = race_victims.melt, aes(x=Race, y=value)) + 
    geom_point(aes(color=variable)) +
    coord_flip()
