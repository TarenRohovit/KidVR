rm(list=ls())
library(tidyr)
library(dplyr)
library(ez)
#sets working directory 
setwd("/Users/tarenrohovit/Desktop/KidVRData")

#the number of data files to be read
snumbers <- c(2:4)
longformat <- c()
for (val in snumbers){
  currentData <- read.csv(paste(val,".csv", sep = ""))
  
  #currentData <- read.csv("2.csv")
  #renames to remove spaces
  currentData <- currentData %>%
    rename(IntDist = Initial.Distance, AdjDist = Adjusted.Distance )
  #converts from factors to numbers
  currentData$AdjDist <- as.numeric(as.character(currentData$AdjDist))
  currentData$IntDist <- as.numeric(as.character(currentData$IntDist))
  currentData$TrialNum <- as.numeric(as.character(currentData$TrialNum))
  #Removes Feedback trials
  currentData <- filter(currentData, Phase == "Adjustment")

  
  #correct trial numbers that were offset due to counterblancing
  currentData <- currentData %>%
    select(-Percentage, -Response) %>%
    mutate(NewTrialNum = case_when((Affordance == "Reach Out" & TrialNum < 21) ~ TrialNum,
      (Affordance == "Reach Out" & TrialNum > 20) ~ TrialNum - 20,
      (Affordance == "Reach Up" & TrialNum > 20) ~ TrialNum,
      (Affordance == "Reach Up" & TrialNum < 21) ~ TrialNum + 20))


  #creates one dataframe containing all of the data
  longformat <- rbind(longformat, currentData)
}


#creates a new column to hold the category of each trial

longformat <- longformat %>%
  mutate(BaselineByCategory = case_when(Affordance == "Reach Out" ~ "out_baseline",
                                        Affordance == "Reach Up" ~ "up_baseline"),
         TrialCategory = case_when(((NewTrialNum == 1 | NewTrialNum == 2) & IntDist < 1) ~ "out_baseAdj_below",
                                   ((NewTrialNum == 1 | NewTrialNum == 2) & IntDist > 1) ~ "out_baseAdj_above",

                                   ((NewTrialNum == 5 | NewTrialNum == 6) & IntDist < 1) ~ "out_30_below",
                                   ((NewTrialNum == 5 | NewTrialNum == 6) & IntDist > 1) ~ "out_30_above",

                                   ((NewTrialNum == 9 | NewTrialNum == 10) & IntDist < 1) ~ "out_20_below",
                                   ((NewTrialNum == 9 | NewTrialNum == 10) & IntDist > 1) ~ "out_20_above",

                                   ((NewTrialNum == 13 | NewTrialNum == 14) & IntDist < 1) ~ "out_10_below",
                                   ((NewTrialNum == 13 | NewTrialNum == 14) & IntDist > 1) ~ "out_10_above",

                                   ((NewTrialNum == 17 | NewTrialNum == 18) & IntDist < 1) ~ "out_5_below",
                                   ((NewTrialNum == 17 | NewTrialNum == 18) & IntDist > 1) ~ "out_5_above",

                                   ((NewTrialNum == 21 | NewTrialNum == 22) & IntDist < 1) ~ "up_baseline_below",
                                   ((NewTrialNum == 21 | NewTrialNum == 22) & IntDist > 1) ~ "up_baseline_above",

                                   ((NewTrialNum == 25 | NewTrialNum == 26) & IntDist < 1) ~ "up_30_below",
                                   ((NewTrialNum == 25 | NewTrialNum == 26) & IntDist > 1) ~ "up_30_above",

                                   ((NewTrialNum == 29 | NewTrialNum == 30) & IntDist < 1) ~ "up_20_below",
                                   ((NewTrialNum == 29 | NewTrialNum == 30) & IntDist > 1) ~ "up_20_above",

                                   ((NewTrialNum == 33 | NewTrialNum == 34) & IntDist < 1) ~ "up_10_below",
                                   ((NewTrialNum == 33 | NewTrialNum == 34) & IntDist > 1) ~ "up_10_above",

                                   ((NewTrialNum == 37 | NewTrialNum == 38) & IntDist < 1) ~ "up_5_below",
                                   ((NewTrialNum == 37 | NewTrialNum == 38) & IntDist > 1) ~ "up_5_above"))


#convert to wide format
wideBaseOnly <- longformat %>%
  select(Participant, Age, EyeHeight, Baseline, BaselineByCategory) %>%
  group_by(Participant, Baseline) %>%
  distinct() %>%
  pivot_wider(names_from = BaselineByCategory, values_from = Baseline)

wideNoBase <- longformat %>%
  select(-Date, -Phase, -TrialNum, -NewTrialNum, -IntDist, -Affordance, -Baseline, -BaselineByCategory) %>%
  pivot_wider(names_from = TrialCategory, values_from = AdjDist)

wideformat <- merge(wideBaseOnly, wideNoBase)

#write.csv(wideformat, "wideData.csv", row.names = FALSE)


#
#ANOVA
#

longNovRep <- gather(wideformat, affordance , adjustment, c(out_baseAdj_below, out_baseAdj_above, out_30_below,
                                                            out_30_above, out_20_below, out_20_above,
                                                            out_10_below, out_10_above, out_5_below,
                                                            out_5_above, up_baseline_below, up_baseline_above, up_30_below,
                                                            up_30_above, up_20_below, up_20_above,
                                                            up_10_below, up_10_above, up_5_below,
                                                            up_5_above), factor_key=TRUE)

anova <- ezANOVA(data = longNovRep, dv = adjustment, wid = Participant, within = affordance, return_aov = FALSE)


