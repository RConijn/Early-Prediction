######## ---------- LOAD PACKAGES ----------------------------------------######
library(dplyr)
library(tidyr)
library(stringr)
library(entropy)
library(philentropy)
options(scipen = 1000)


######## ---------- LOAD DATA --------------------------------------------######
#keystroke data
keylog <- read.csv("data/extendedlog.csv", stringsAsFactors = FALSE)

grades <- read.csv("data/grades.csv",
                   stringsAsFactors = FALSE) %>%
  mutate(char_final = nchar(full_text)) %>%
  rename(pid = `ï..id`)

######## ---------- TRIM DATA --------------------------------------------######
keylog_trim <- keylog %>%
  mutate(iki_trim = ifelse(iki > quantile(iki, 0.95, na.rm = TRUE), NA, iki),
         iki_inword_trim = ifelse(iki_inword >
                                    quantile(iki_inword, 0.95, na.rm = TRUE), NA,
                                  iki_inword),
         iki_betwword_trim = ifelse(iki_betwword >
                                      quantile(iki_betwword, 0.95, na.rm = TRUE), NA,
                                    iki_betwword),
         betweenwords_trim = ifelse(betweenwords >
                                      quantile(betweenwords, 0.95, na.rm = TRUE), NA,
                                    betweenwords),
         betweensentence_trim = ifelse(betweensentence >
                                         quantile(betweensentence, 0.95,
                                                  na.rm = T), NA, betweensentence)
  )


# fill missing data with mean (only for m/sd betweensentence)
NA2mean <- function(x) {
  replace(x, is.na(x), mean(x, na.rm = TRUE))
}

######## ---------- FEATURE EXTRACTION PER PID ---------------------------######
group_data <- function(timeframe){
  timeframe_start <- as.numeric(substr(timeframe, 1,2))
  timeframe_end <- as.numeric(substr(timeframe, 3,4))

  keylog_trim <- keylog_trim %>%
    filter(endTime <= timeframe_end * 60 * 1000,
           startTime >= timeframe_start * 60 * 1000)

  #extract features related to cut-paste-jump events
  cutpastejump <- keylog_trim %>%
    group_by(pid) %>%
    mutate(cut_paste_jump = ifelse((type == "replacement"
                                    & lead(type) %in% c("insert", "keyboard")) |
                                     (type == "insert") |
                                     (type == "mouse" & focus == "Word document" &
                                        output %in% c("Double LEFT Click",
                                                      "LEFT Click")), 1, 0),
           cut_paste_jump2 = ifelse(cut_paste_jump == 1,
                                    cumsum(diff(cut_paste_jump) > 0 |
                                             row_number() == 1), 0),
           cut_paste_jump2 = ifelse(lag(type) == "replacement",
                                    lag(cut_paste_jump2), cut_paste_jump2)) %>%
    filter(cut_paste_jump2 > 0) %>%
    group_by(pid, cut_paste_jump2) %>%
    summarise(
      n_cut_paste_jump = n(),
      time_cut_paste_jump = max(endTime) - min(startTime),
      time_cut_paste_jump =  ifelse(time_cut_paste_jump >
                                      quantile(time_cut_paste_jump, 0.95,
                                               na.rm = TRUE), NA,
                                    time_cut_paste_jump)
    ) %>%
    group_by(pid) %>%
    summarise(
      cut_paste_jump_m = mean(log(time_cut_paste_jump), na.rm = TRUE),
      cut_paste_jump_sd = sd(log(time_cut_paste_jump), na.rm = TRUE)
    )

  #other events

  otherevents <- keylog_trim %>%
    group_by(pid) %>%
    mutate(total_time = max(endTime) - min(startTime)) %>%
    filter(other_events > 0) %>%
    group_by(pid, other_events) %>%
    summarise(
      time_other_events2 = max(endTime) - min(startTime),
      total_time = first(total_time)) %>%
    group_by(pid) %>%
    summarise(
      perc_other_events = sum(time_other_events2, na.rm = TRUE) /
        first(total_time))

  otherevents2 <- keylog_trim %>%
    group_by(pid) %>%
    mutate(between_bursts = ifelse(burst_sd == 0, 1, 0),
           between_bursts = ifelse(between_bursts == 1,
                                   cumsum(diff(between_bursts) > 0 |
                                            row_number() == 1), 0),
           n_words = sum(start_word, na.rm = TRUE)) %>%
    filter(between_bursts > 0) %>%
    group_by(pid, between_bursts) %>%
    summarise(n_other_events = sum(other_events, na.rm = TRUE),
              n_words = first(n_words)) %>%
    group_by(pid) %>%
    summarise(n_prod_cycle = sum(n_other_events > 0) / first(n_words) )


  #linear transitions
  linear <- keylog_trim %>%
    group_by(pid) %>%
    filter(pauseLocationFull != "COMBINATION KEY") %>%
    mutate(linear_word =
             ifelse(pauseLocationFull=="BEFORE WORDS" &
                      (lag(pauseLocationFull) == "AFTER WORDS" |
                         lag(pauseLocationFull) == "AFTER SENTENCES") &
                      (lead(pauseLocationFull) == "AFTER WORDS" |
                         lead(pauseLocationFull) == "WITHIN WORDS" ) &
                      (lag(pauseLocationFull,2) == "WITHIN WORDS" |
                         lag(pauseLocationFull,2) == "BEFORE WORDS" |
                         lag(pauseLocationFull,2) == "BEFORE SENTENCES" &
                         (doclengthFull == positionFull |
                            doclengthFull == positionFull + 1)), 1, 0),
           linear_sentence =
             ifelse((pauseLocationFull=="BEFORE SENTENCES" &
                       (lead(pauseLocationFull) == "AFTER WORDS" |
                          lead(pauseLocationFull) == "WITHIN WORDS")) &
                      (lag(pauseLocationFull) == "BEFORE PARAGRAPHS" |
                         (lag(pauseLocationFull) == "BEFORE SENTENCES" &
                            lag(pauseLocationFull,2) == "AFTER SENTENCES")  &
                         (doclengthFull == positionFull |
                            doclengthFull == positionFull + 1)), 1, 0)) %>%
    summarise(
      perc_linear_words = sum(linear_word, na.rm = TRUE) /
        sum(start_word, na.rm = TRUE),
      perc_linear_sentences = sum(linear_sentence, na.rm = TRUE) /
        sum(start_sentence, na.rm = TRUE)
    )


  #extract features related to non-keystrokes
  nonkeys <- keylog_trim %>%
    group_by(pid) %>%
    summarise(n_keystrokes = sum(type == "keyboard", na.rm = TRUE),
              n_focus_task = sum(type == "focus" &
                                   str_detect(output, regex("WritingTask",
                                                            ignore_case = TRUE)), na.rm = TRUE),
              n_focus_translate = sum(type == "focus" &
                                        str_detect(focus, regex("Translate|LEO.org|vertaling|engels|Synonym|Dictionary|Translate|Language",
                                                                ignore_case = TRUE)), na.rm = TRUE)) %>%
    select(pid, n_focus_task, n_focus_translate)


  #extract features related to corrections
  correct <- keylog_trim %>%
    group_by(pid,correct) %>%
    filter(correct != 0) %>%
    summarise(n_backspaces = n(),
              time_backspaces = max(endTime) - min(startTime),
              time_backspaces = ifelse(time_backspaces < 0, 0,
                                       time_backspaces),
              time_backspaces = ifelse(time_backspaces >
                                         quantile(time_backspaces, 0.95,
                                                  na.rm = TRUE), NA,
                                       time_backspaces)) %>%
    group_by(pid) %>%
    summarise(single_backspacetime_m = mean(log(time_backspaces[n_backspaces == 1]),
                                            na.rm = TRUE),
              single_backspacetime_sd = sd(log(time_backspaces[n_backspaces == 1]),
                                           na.rm = TRUE),
              multi_backspacetime_m = mean(log(time_backspaces[n_backspaces > 1]),
                                           na.rm = TRUE),
              multi_backspacetime_sd = sd(log(time_backspaces[n_backspaces > 1]),
                                          na.rm = TRUE)
    )

  #extract features related to corrections
  revision <- keylog_trim %>%
    group_by(pid, revision_no) %>%
    filter(revision_no != 0) %>%
    summarise(leadingedge = first(leadingedge)) %>%
    group_by(pid) %>%
    summarise(
      n_revisions = n(),
      n_leadedge_rev = sum(leadingedge),
      n_intext_rev = sum(!leadingedge)
    )



  #extract features related to bursts (no pause >= mean + 2*sd_iki in copy task)
  burst_sd <- keylog_trim %>%
    group_by(pid) %>%
    mutate(bursttype = ifelse(burst_sd !=0 & lead(burst_sd) == 0
                              & lead(pauseLocationFull) == "REVISION"
                              & (lead(doclengthFull) == lead(positionFull) |
                                   lead(positionFull) + 1 == lead(doclengthFull)),
                              "R-burst",
                              ifelse((burst_sd !=0 & lead(burst_sd) == 0
                                      & (lead(doclengthFull) == lead(positionFull) |
                                           lead(positionFull) + 1 == lead(doclengthFull))) | (
                                             burst_sd !=0 & lag(burst_sd) == 0 &
                                               lag(pauseLocationFull) != "REVISION" ) |
                                       (burst_sd != 0 & row_number() == 1),
                                     "P-burst",
                                     ifelse((burst_sd !=0 & lead(burst_sd) == 0 &
                                               doclengthFull != positionFull &
                                               doclengthFull != positionFull + 1) |
                                              (burst_sd !=0 & lag(burst_sd) == 0 &
                                                 doclengthFull != positionFull &
                                                 doclengthFull != positionFull + 1)
                                            ,"I-burst", NA))),
           total_words = sum(start_word, na.rm = TRUE)) %>%
    group_by(pid,burst_sd) %>%
    filter(burst_sd != 0) %>%
    summarise(keystrokes_burst = n(),
              burst_words = sum(start_word, na.rm = TRUE),
              total_words = first(total_words),
              time_burst = max(endTime) - min(startTime),
              start_burst = first(bursttype),
              end_burst = last(bursttype)) %>%
    group_by(pid) %>%
    summarise(num_bursts = n(),
              burstsd_keystrokes_m = mean(keystrokes_burst, na.rm = TRUE),
              burstsd_keystrokes_sd = sd(keystrokes_burst, na.rm = TRUE),
              burstsd_keystrokes_max = max(keystrokes_burst, na.rm = TRUE),
              r_bursts = sum(end_burst == "R-burst", na.rm = TRUE),
              p_bursts = sum(start_burst == "P-burst" &  end_burst == "P-burst",
                             na.rm = TRUE),
              i_bursts = sum(start_burst == "I-burst" &  end_burst == "I-burst",
                             na.rm = TRUE),
              words_p_bursts = sum(burst_words[start_burst == "P-burst" &
                                                 end_burst == "P-burst"],
                                   na.rm = TRUE),
              perc_rbursts = r_bursts / num_bursts,
              perc_words_pbursts = words_p_bursts / first(total_words),
              perc_ibursts = i_bursts / num_bursts
    ) %>% select(-c(r_bursts, p_bursts, i_bursts, words_p_bursts))

  #extract features related to window-based keystrokes
  windows <- keylog_trim  %>%
    filter(type == "keyboard") %>%
    group_by(pid) %>%
    #create 30s windows
    mutate(interval = 30 * ceiling(startTime/30000),
           interval = factor(interval, levels = seq(30,1800,30))) %>%
    group_by(pid, interval) %>%
    summarise(n_keystrokes = n()) %>%
    complete(pid, interval, fill = list(n_keystrokes = 0)) %>%
    mutate(n_keystrokescum = cumsum(n_keystrokes),
           interval_num = as.numeric(interval),
           diff_n_keystrokes = sign(c(NA, diff(n_keystrokes))))


  windowfeat <- windows %>%
    group_by(pid) %>%
    summarise(n_keystrokes_30s_sd = sd(n_keystrokes, na.rm = TRUE),
              n_keystrokes_30s_slope =
                lm(n_keystrokes ~ interval_num)$coefficients[["interval_num"]],
              n_keystrokes_30s_entropy = entropy(n_keystrokes, method = "ML") /
                sum(n_keystrokes),
              n_keystrokes_30s_uniformity =
                JSD(rbind(rep(max(n_keystrokescum)/60,60),n_keystrokes),
                    unit = "log2"),
              n_keystrokes_30s_extremes = sum(diff_n_keystrokes != 0, na.rm = TRUE)
    )

  windowfeat2 <- windows %>%
    group_by(pid) %>%
    mutate(row_number = row_number()) %>%
    filter(n_keystrokes != 0) %>%
    mutate(dist_windows = c(NA, diff(row_number))) %>%
    summarise(dist_windows_1key_m = mean(dist_windows, na.rm = TRUE),
              dist_windows_1key_sd = sd(dist_windows, na.rm = TRUE))

  #extract features related to keystrokes in general
  keyfeatures <- keylog_trim  %>%
    filter(type == "keyboard") %>%
    group_by(pid) %>%
    # merge with grades
    left_join(grades, by = "pid") %>%
    select(-full_text) %>%
    summarise(
      #frequency-based features
      n_keystrokes = n(),
      n_words = sum(start_word),
      n_backspaces = sum(output %in% c("BACK", "DELETE")),
      iki_05_10 = sum(iki %in%  500:1000),
      iki_10_15 = sum(iki %in% 1000:1500),
      iki_15_20 = sum(iki %in% 1500:2000),
      iki_20_30 = sum(iki %in% 2000:3000),
      iki_30 = sum(iki > 3000, na.rm = TRUE),
      #time-based features with one summary value
      initial_pause = min(startTime),
      total_time = max(endTime,  na.rm = TRUE),
      iki_median = median(log(iki_trim),  na.rm = TRUE),
      iki_largest = max(iki,  na.rm = TRUE),
      #time-based features
      iki_m = mean(log(iki_trim), na.rm = TRUE),
      iki_sd = sd(log(iki_trim), na.rm = TRUE),
      iki_inword_m = mean(log(iki_inword_trim), na.rm = TRUE),
      iki_inword_sd = sd(log(iki_inword_trim), na.rm = TRUE),
      iki_betwword_m = mean(log(iki_betwword_trim), na.rm = TRUE),
      iki_betwword_sd = sd(log(iki_betwword_trim), na.rm = TRUE),
      betweenwords_m = mean(log(betweenwords_trim), na.rm = TRUE),
      betweenwords_sd = sd(log(betweenwords_trim), na.rm = TRUE),
      betweensentence_m = mean(log(betweensentence_trim), na.rm = TRUE),
      betweensentence_sd= sd(log(betweensentence_trim), na.rm = TRUE),
      # ratio-based values
      ratio_keystrokes_final_prod = max(char_final) / n_keystrokes,
      ratio_keystrokes_lead_edge = (sum(positionFull == doclengthFull |
                                          positionFull + 1 == doclengthFull ))
      / n_keystrokes,
      ratio_large_iwi = sum(betweenwords >= pause, na.rm = TRUE) /
        sum(!is.na(betweenwords))
    ) %>%
    left_join(correct, by = "pid") %>%
    left_join(revision, by = "pid") %>%
    left_join(cutpastejump, by = "pid") %>%
    left_join(otherevents, by = "pid") %>%
    left_join(otherevents2, by = "pid") %>%
    left_join(windowfeat, by = "pid") %>%
    left_join(nonkeys, by = "pid") %>%
    left_join(windowfeat2, by = "pid") %>%
    left_join(burst_sd, by = "pid") %>%
    left_join(linear, by = "pid") %>%
    full_join(select(grades, pid, c.total), by = "pid") %>%
    # remove copy-paster (pid ==82), typed in other word doc (pid == 27,55) and
    # person who did not do the task (pid == 91)
    filter(!pid %in% c(82, 27, 55, 91))


  # set missing data to zeros
  keyfeatures <- do.call(data.frame,lapply(keyfeatures, function(x) replace(x, !is.finite(x),0)))

  # fix times if nothing happened
  keyfeatures <- keyfeatures %>%
    mutate(initial_pause = ifelse(initial_pause == 0, timeframe_end*60*1000,
                                  initial_pause),
           total_time = ifelse(total_time == 0, timeframe_end*60*1000,
                               total_time))

  write.csv(keyfeatures,
            paste0("data/time/keystrokes_from_", timeframe_start, "_to_",
                   timeframe_end, "_min.csv"),
            row.names = FALSE)
}



######## ---------- WRITE FEATURE DATA -----------------------------------######
# write cleaned data upto every 5 minutes
lapply(c("0005", "0010", "0015", "0020","0025", "0030",
         "0510", "1015","1520", "2025", "2530"), group_data)


################################################################################
