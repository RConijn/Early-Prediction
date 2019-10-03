library(dplyr)
library(xtable)
options(scipen = 1000)


######## ---------- LOAD DATA --------------------------------------------######
keyfeatures <- read.csv("data/keystrokes_feat_30mins.csv",
                        stringsAsFactors = FALSE)
keyfeatures_log <- read.csv("data/keystrokes_upto_30min.csv",
                        stringsAsFactors = FALSE)
keyfeatures_0_5 <- read.csv("data/keystrokes_from_0_to_5_min.csv",
                            stringsAsFactors = FALSE)
keyfeatures_5_10 <- read.csv("data/keystrokes_from_5_to_10_min.csv",
                            stringsAsFactors = FALSE)
keyfeatures_10_15 <- read.csv("data/keystrokes_from_10_to_15_min.csv",
                             stringsAsFactors = FALSE)
keyfeatures_15_20 <- read.csv("data/keystrokes_from_15_to_20_min.csv",
                             stringsAsFactors = FALSE)
keyfeatures_20_25 <- read.csv("data/keystrokes_from_20_to_25_min.csv",
                             stringsAsFactors = FALSE)
keyfeatures_25_30 <- read.csv("data/keystrokes_from_25_to_30_min.csv",
                              stringsAsFactors = FALSE)
keyfeatures_0_10 <- read.csv("data/keystrokes_from_0_to_10min.csv",
                              stringsAsFactors = FALSE)
keyfeatures_10_20 <- read.csv("data/keystrokes_from_10_to_20min.csv",
                              stringsAsFactors = FALSE)
keyfeatures_20_30 <- read.csv("data/keystrokes_from_20_to_30min.csv",
                              stringsAsFactors = FALSE)

features <-
  c("initial_pause", "total_time","iki_m", "iki_sd", "iki_median",
    "iki_largest", "iki_inword_m", "iki_inword_sd", "iki_betwword_m",
    "iki_betwword_sd", "betweenwords_m", "betweenwords_sd", "betweensentence_m",
    "betweensentence_sd", "iki_05_10", "iki_10_15", "iki_15_20", "iki_20_30",
    "iki_30", "ratio_large_iwi", "n_revisions", "n_leadedge_rev",
    "n_intext_rev", "n_backspaces", "single_backspacetime_m",
    "single_backspacetime_sd", "multi_backspacetime_m",
    "multi_backspacetime_sd",
    "ratio_keystrokes_final_prod",  "ratio_keystrokes_lead_edge",
    "burstsd_keystrokes_m", "burstsd_keystrokes_sd",
    "burstsd_keystrokes_max", "num_bursts","perc_rbursts", "perc_ibursts",
    "perc_words_pbursts",  "n_prod_cycle", "perc_linear_words",
    "perc_linear_sentences", "n_keystrokes", "n_words", "n_keystrokes_30s_sd",
    "n_keystrokes_30s_slope", "n_keystrokes_30s_entropy",
    "n_keystrokes_30s_uniformity", "n_keystrokes_30s_extremes",
    "dist_windows_1key_m", "dist_windows_1key_sd", "n_focus_translate",
    "n_focus_task", "cut_paste_jump_m", "cut_paste_jump_sd",
    "perc_other_events")

feature_labs <-
  c("Initial pause time (min)","Total time (min)", "Mean IKI", "SD IKI",
    "Median IKI", "Largest IKI (min)", "Mean IKI within word",
    "SD IKI within word", "Mean IKI between words", "SD IKI between words",
    "Mean time between words", "SD time between words",
    "Mean time between sentences", "SD time between sentences",
    "Number of IKI 0.5-1sec", "Number of IKI 1-1.5sec",
    "Number of IKI 1.5-2sec", "Number of IKI 2-3sec",
    "Number of IKI larger than 3 sec",
    "Percentage of long pauses between words",
    "Number of revisions", "Number of leading-edge revisions",
    "Number of in-text revisions", "Number of backspaces",
    "Mean time in single backspacing", "SD time in single backspacing",
    "Mean time in multiple backspacing", "SD time in multiple backspacing",
    "Percentage of characters final text",
    "Percentage of characters at leading edge",
    "Mean number of keystrokes per burst", "SD number of keystrokes per burst",
    "Largest number of keystrokes per burst", "Number of bursts",
    "Percentage of R-bursts", "Percentage of I-bursts",
    "Percentage of words in P-bursts", "Number of production cycles",
    "Percentage of linear transitions sentences",
    "Percentage of linear transitions words", "Number of keystrokes",
    "Number of words", "SD number of keystrokes per 30s",
    "Slope number of keystrokes per 30s",
    "Entropy number of keystrokes per 30s",
    "Uniformity number of keystrokes per 30s",
    "Local extreme number of keystrokes per 30s",
    "Mean distance 30s windows $>$1 keystroke",
    "SD distance 30s windows $>$1 keystroke",
    "Number of focus shift to translation", "Number of focus shift to task",
    "Mean time cut/paste/jump events", "SD time cut/paste/jump events",
    "Percentage of time spent on other events")


######## ---------- GET DESCRIPTIVES OF FEATURES -------------------------######

# make stars function
corstar <-function(x,y){
  #Compute correlation matrix
  R <- cor(x,y)
  p <- cor.test(x,y)$p.value

  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "*** ",
             ifelse(p < .01,  "**  ",
             ifelse(p < .05,  "*   ", "    ")))

  ## trunctuate the correlation matrix to two decimal
  out <- paste( sprintf("%.2f", round(R, 2)), mystars, sep = "")
  out
}

#### get mean and sds
keysum <- keyfeatures %>%
  summarise_all(funs(xamean = mean, xasd = sd)) %>%
  gather %>%
  separate(key, c("key","stat"), sep = "_x") %>%
  spread(stat, value) %>%
  mutate(amean = round(as.numeric(amean), 2),
         asd = round(as.numeric(asd),2))

## log correlations
keysum2 <- keyfeatures_log %>%
  summarise_all(funs(xcor_total = corstar(., c.total))) %>%
  gather %>%
  separate(key, c("key","stat"), sep = "_x") %>%
  spread(stat, value)

keysum05 <- keyfeatures_0_5 %>%
  summarise_all(funs(xcor_05 = corstar(., c.total))) %>%
  gather %>%
  separate(key, c("key","stat"), sep = "_x") %>%
  spread(stat, value)

keysum010 <- keyfeatures_0_10 %>%
  summarise_all(funs(xcor_010 = corstar(., c.total))) %>%
  gather %>%
  separate(key, c("key","stat"), sep = "_x") %>%
  spread(stat, value)

keysum510 <- keyfeatures_5_10 %>%
  summarise_all(funs(xcor_510 = corstar(., c.total))) %>%
  gather %>%
  separate(key, c("key","stat"), sep = "_x") %>%
  spread(stat, value)

keysum1015 <- keyfeatures_10_15 %>%
  summarise_all(funs(xcor_1015 = corstar(., c.total))) %>%
  gather %>%
  separate(key, c("key","stat"), sep = "_x") %>%
  spread(stat, value)

keysum1020 <- keyfeatures_10_20 %>%
  summarise_all(funs(xcor_1020 = corstar(., c.total))) %>%
  gather %>%
  separate(key, c("key","stat"), sep = "_x") %>%
  spread(stat, value)

keysum1520 <- keyfeatures_15_20 %>%
  summarise_all(funs(xcor_1520 = corstar(., c.total))) %>%
  gather %>%
  separate(key, c("key","stat"), sep = "_x") %>%
  spread(stat, value)

keysum2025 <- keyfeatures_20_25 %>%
  summarise_all(funs(xcor_2025 = corstar(., c.total))) %>%
  gather %>%
  separate(key, c("key","stat"), sep = "_x") %>%
  spread(stat, value)

keysum2030 <- keyfeatures_20_30 %>%
  summarise_all(funs(xcor_2030 = corstar(., c.total))) %>%
  gather %>%
  separate(key, c("key","stat"), sep = "_x") %>%
  spread(stat, value)

keysum2530 <- keyfeatures_25_30 %>%
  summarise_all(funs(xcor_2530 = corstar(., c.total))) %>%
  gather %>%
  separate(key, c("key","stat"), sep = "_x") %>%
  spread(stat, value)


######## ---------- GET NICE NAMES ---------------------------------------######
keysum3 <- keysum %>%
  full_join(keysum2, by = "key") %>%
  full_join(keysum05, by = "key") %>%
  full_join(keysum510, by = "key") %>%
  full_join(keysum1015, by = "key") %>%
  full_join(keysum1520, by = "key") %>%
  full_join(keysum2025, by = "key") %>%
  full_join(keysum2530, by = "key") %>%

  full_join(keysum010, by = "key") %>%
  full_join(keysum1020, by = "key") %>%
  full_join(keysum2030, by = "key") %>%
  filter(!key %in%  c("pid", "c.total")) %>%
  mutate(amean = ifelse(key %in% c("initial_pause", "total_time", "iki_largest"),
                  round(amean/1000/60,2), round(amean,2)),
    asd = ifelse(key %in% c("initial_pause", "total_time", "iki_largest"),
                  round(asd/1000/60,2), round(asd,2)),
    key= factor(key, ordered = TRUE, levels = features,
                labels = feature_labs)) %>%
  arrange(key)

# write
write.csv(keysum3,
          paste0("data/descriptives.csv"))









