######## ---------- LOAD PACKAGES ----------------------------------------######
library(dplyr)

######## ---------- LOAD DATA --------------------------------------------######
#questionnaire data
questionnaire <- read.csv("data/questionnaire_en_anom.csv", sep = ";",
                  na.strings = c("","NA"))
keyfeatures <- read.csv("data/keystrokes_feat_30mins.csv",
                        stringsAsFactors = FALSE)


####-----------------  PREPROCESS DATA  ------------------------------------####
#change questionnaire data to numerics
quest <- questionnaire %>%
  mutate_at(vars(matches("P\\d\\d|R\\d\\d|F\\d\\d")),
            funs(as.numeric(factor(., levels = c("Strongly Disagree",
                                                 "Disagree", "Neutral",
                                                 "Agree", "Strongly agree")))))
# reverse coding questionnaire
quest2 <- quest %>%
  mutate_at(vars(P05, P07, P09, P10, P12, R02, R09, R10, R11),
            funs(6-.))
# compute means for revision/planning
quest2$plan <- rowMeans(select(quest2, matches("P\\d\\d")))
quest2$revise <- rowMeans(select(quest2, matches("R\\d\\d")))


####-----------------  CHECK DEMOGRAPHICS  ---------------------------------####

questsum <- quest %>%
  # only who did both parts
  filter(!pid %in% c(2,13,19,31,35,36,39,69,86,115,130)) %>%
  summarize(N = n(),
            mean = mean(Age),
            sd = sd(Age),
            Female = sum(Gender == "Female"))
