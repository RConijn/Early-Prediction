######## ---------- LOAD PACKAGES ----------------------------------------######
library(dplyr)
library(stringr)
library(tidyr)

######## ---------- LOAD DATA --------------------------------------------######
#keystroke data
raw_keylog <- read.csv("data/baselinelog_anom_en.csv",
                         stringsAsFactors = FALSE)

######## ---------- REMOVE BASELINE -------------------------------------#######

keylog_rem_baseline <- raw_keylog %>%
  # only writing summary task
  filter(baseline == 0,
         # remove baseline for pid 80
         !(baseline == 0 & pid == 80 & id < 1084),
         # remove some wrongly grouped keystrokes
         id != 0,
         # rmeove pid who typed in other word document
         !pid %in%  c(27,55)
  ) %>%
  group_by(pid) %>%
  mutate(positionFull = positionFull - first(positionFull),
         doclengthFull = doclengthFull - first(doclengthFull),
         charProduction = charProduction - first(charProduction),
         startTime = startTime - first(startTime),
         endTime = endTime - first(endTime),
         focus = ifelse(type == "focus", output, NA)) %>%
  select(-startClock, -endClock, -RawStart, -RawEnd) %>%
  # only 30 mins
  filter(endTime <= 60*1000*30,
         positionFull >= 0) %>%
  ungroup()

######## ---------- GET PAUSE LENGTH ------------------------------------#######

baseline <- raw_keylog %>%
  filter(baseline == 1, type == "keyboard") %>%
  group_by(pid) %>%
  summarize(
    pause = mean(pauseTime, na.rm =T) + 2*sd(pauseTime, na.rm = T)
  )

  # no copy task for pid 7 -- take average pause time
baseline <- baseline %>%
  rbind(c(7, mean(baseline$pause)))

######## ---------- ADD INFO ----------------------------------------------#####


# clean data for keystrokes only
keylogkey <- keylog_rem_baseline %>%
  left_join(baseline, by = "pid") %>%
  filter(type == "keyboard") %>%
  arrange(pid, startTime) %>%
  group_by(pid) %>%
  mutate(iki = pauseTime,
         iki = ifelse(row_number() == 1 | iki == 0, NA, iki),
         iki_inword = ifelse(pauseLocationFull == "WITHIN WORDS", iki, NA),
         iki_betwword = ifelse(pauseLocationFull %in% c("BEFORE WORDS",
                                                        "AFTER WORDS"), iki, NA),
         start_word = (row_number() == 1 | pauseLocationFull == "BEFORE WORDS"),
         start_sentence = (pauseLocationFull == "BEFORE SENTENCES"),
         keystroke_no = 1: n(),
         typo = ifelse(pauseLocationFull == "REVISION" &
                         iki <= pause &
                         lead(iki) <= pause &
                         (lag(pauseLocationFull) == "WITHIN WORDS"  |
                          (lag(pauseLocationFull, 4)  == "WITHIN WORDS" &
                             lag(pauseLocationFull) == "REVISION"
                           & lag(iki) <= pause) |
                            (lag(pauseLocationFull, 6)  == "WITHIN WORDS" &
                               lag(pauseLocationFull) == "REVISION" &
                               lag(pauseLocationFull,2) == "REVISION") &
                            lag(iki) <= pause &
                            lag(iki, 2) <= pause), 1, 0),
         correct = ifelse(pauseLocationFull == "REVISION", 1, 0),
         correct = ifelse(correct == 1, cumsum(diff(correct) > 0 |
                                                  row_number() == 1), 0),
         leadingedge = positionFull == doclengthFull |
                                    positionFull + 1 == doclengthFull,
         revision = ifelse(pauseLocationFull %in% c("REVISION", "CHANGE") |
                             (!leadingedge & !pauseLocationFull %in%
                                c("UNKNOWN", "COMBINATION KEY", "END")), 1, 0),
         revision = ifelse((pauseLocationFull == "REVISION" & !is.na(iki) &
                             (iki >= pause | lag(pauseLocationFull) == "CHANGE")) |
                             (pauseLocationFull == "CHANGE" &
                             (iki >= pause |lag(pauseLocationFull) == "REVISION")),
                           lag(revision) + 1, revision),
         revision_no = ifelse(revision >= 1,
                              cumsum(revision-lag(revision) > 0 |
                                       row_number() == 1),
                              0),
         #only betweenwords if after words is within 2 keystrokes distance
         betweenwords = ifelse(start_word &
                                 lag(pauseLocationFull) == "AFTER WORDS",
                               startTime - lag(startTime, 2),
                        ifelse(start_word &
                                lag(pauseLocationFull,2) == "AFTER WORDS",
                                      startTime - lag(startTime, 3), NA)),
         betweensentence = ifelse(start_sentence &
                                    lag(pauseLocationFull) == "AFTER SENTENCES",
                                  startTime - lag(startTime, 2),
                           ifelse(start_sentence &
                                    lag(pauseLocationFull,2) == "AFTER SENTENCES",
                                  startTime - lag(startTime, 3), NA)),
         char_pasted = ifelse(type == "insert" & pid == 78,
                             positionFull - lag(positionFull),
                      ifelse(type == "insert", nchar(output) - 2, 0))
  )


######## ---------- INSERT SOURCE USAGE ---------------------------------#######

## check source usage
focus1 <- keylog_rem_baseline %>%
  filter(type == "focus")
focus <- as.data.frame(table(focus1$output))
write.csv(focus, "data/source_usage.csv", row.names = FALSE)


otherkeyevents <- c("DOWN", "UP", "LEFT", "RIGHT", "DELETE", "BACK", "RALT",
                    "ESCAPE", "END", "INSERT", "HOME", "RCTRL", "LALT", "LWIN",
                    "LCTRL", "F11", "F12", "APPS", "NUMLOCK", "OEM_5",
                    "OEM_PLUS")


## group source usage
keylog_rem_baseline2 <- keylog_rem_baseline %>%
  filter(type != "keyboard") %>%
  bind_rows(keylogkey) %>%
  arrange(pid, id) %>%
  group_by(pid) %>%
  mutate(focus =
           ifelse(str_detect(focus, regex("Wordlog", ignore_case = TRUE)),
                  "Word document",
           ifelse(str_detect(focus, regex("WritingTask", ignore_case = TRUE)),
                  "Writing task",
           ifelse(str_detect(focus, regex("Translate|LEO.org|vertaling|engels|Synonym|Dictionary|Translate|Language",
                                       ignore_case = TRUE)), "Translate",
           ifelse(str_detect(focus, regex("Woong|Selective")),
                  "Search original paper",
           ifelse(str_detect(focus, regex("Document")),"Second word document",
                  NA))))),
         other_events = ifelse(type != "keyboard" |
                                 (type == "keyboard" &
                                    output %in% otherkeyevents) &
                                 typo != 1 ,1,0),
         # remove typos from 'other events'
         other_events = ifelse(other_events == 1,
                                cumsum(diff(other_events) > 0 |
                                         row_number() == 1), 0),
         burst_sd = ifelse(iki >  pause , 0, 1),
         burst_sd = ifelse(is.na(burst_sd), 0, burst_sd),
         burst_sd = ifelse(burst_sd == 1, cumsum(diff(burst_sd) > 0 |
                                                   row_number() == 1), 0)
         )  %>%
  fill(focus)

######## ---------- WRITE DATA ------------------------------------------#######
write.csv(keylog_rem_baseline2, "data/extendedlog.csv", row.names = FALSE)

################################################################################








