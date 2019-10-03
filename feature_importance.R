library(dplyr)
library(tidyr)
library(ggplot2)
library(qdap)

results_time <- read.csv("data/results_time_auc", stringsAsFactors = F)

results_keys <- read.csv("data/results_keys_auc", stringsAsFactors = F)

# extract top 5 features per model
features_time <- results_time %>%
  filter(trainmethod != "Baseline (majority class)") %>%
  mutate(features =  beg2char(bestFeat, ",", 5)) %>%
  separate(features, c("f1", "f2", "f3", "f4", "f5"), ",") %>%
  gather(key = "variable_imp", value = "variable", f1:f5) %>%
  mutate(variable_imp = gsub("f", "", variable_imp)) %>%
  select(trainmethod, timeframe, variable_imp, variable)

features_time2 <- features_time %>%
  mutate(id = row_number()) %>%
  spread(key = variable, value = variable_imp, fill = 0) %>%
  select(-`<NA>`)

features_time3 <- features_time2 %>%
  select(-id) %>%
  mutate_at(vars(`0_to_10_.cut_paste_jump_m`:`5_to_10_.perc_words_pbursts`),
            as.numeric) %>%
  group_by(trainmethod, timeframe) %>%
  summarize_each(funs(sum))


write.csv(features_time3, "data/feature_importance.csv", row.names = F)


features_time4 <- as.data.frame(t(features_time3[-c(1:2)]))

features_time4[features_time4 == 0] <- NA

#order on basis of most important feature
order <- apply(features_time4, 2, function(x) 6 - as.numeric(x))
order <- rowSums(order, na.rm=TRUE)


features_time5 <- as.data.frame(cbind(variable = rownames(features_time4),
                                      features_time4)) %>%
  mutate(Time = gsub("\\..*$", "", variable),
         Variable = gsub("^.*\\.", "", variable),
         order = as.numeric(order),
         Variable = ifelse(Variable == "betweensentence_sd", "SD time between sentences",
                    ifelse(Variable == "betweenwords_sd", "SD time between words",
                    ifelse(Variable == "cut_paste_jump_m", "Mean time cut/paste/jump events",
                    ifelse(Variable == "cut_paste_jump_sd", "SD time cut/paste/jump events",
                    ifelse(Variable == "dist_windows_1key_m", "Mean distance 30s windows >1 keystroke",
                    ifelse(Variable == "iki_betwword_sd", "SD IKI between words",
                    ifelse(Variable == "iki_inword_m", "Mean IKI within words",
                    ifelse(Variable == "iki_inword_sd", "SD IKI within words",
                    ifelse(Variable == "iki_largest", "Largest IKI",
                    ifelse(Variable == "iki_sd", "SD IKI",
                    ifelse(Variable == "initial_pause", "Initial pause time",
                    ifelse(Variable == "multi_backspacetime_m", "Mean time in multiple backspacing",
                    ifelse(Variable == "n_focus_task", "Number of focus shifts to task",
                    ifelse(Variable == "n_prod_cycle", "Number of production cycles",
                    ifelse(Variable == "num_bursts", "Number of bursts",
                    ifelse(Variable == "perc_other_events", "Percentage of other events",
                    ifelse(Variable == "perc_rbursts", "Percentage of R-bursts",
                    ifelse(Variable == "perc_words_pbursts", "Percentage of words in P-bursts",
                    Variable)))))))))))))))))),
         Time = ifelse(Time == "0_to_5_m", "(0-5 mins)",
                ifelse(Time == "0_to_10_", "(0-10 mins)",
                ifelse(Time == "0_to_15_", "(0-15 mins)",
                ifelse(Time == "0_to_20_", "(0-20 mins)",
                ifelse(Time == "0_to_25_", "(0-25 mins)",
                ifelse(Time == "0_to_30_", "(0-30 mins)",
                ifelse(Time == "5_to_10_", "(5-10 mins)",
                ifelse(Time == "10_to_15", "(10-15 mins)",
                ifelse(Time == "15_to_20", "(15-20 mins)",
                ifelse(Time == "20_to_25", "(20-25 mins)", Time))))))))))) %>%
  arrange(order) %>%
  mutate(variable = factor(variable, unique(variable),
                           labels = paste(Variable, Time))) %>%
  select(-order, -Time, -Variable)

features_time6 <- features_time5 %>%
  gather(key = "method", value = "Importance", -variable) %>%
  mutate(Classifier = ifelse(method %in% c("V1", "V2", "V3", "V4", "V5", "V6"),
                              "Naive Bayes",
                       ifelse(method %in% c("V7", "V8", "V9", "V10", "V11", "V12"),
                              "Random forest", "Support vector machine")),
         timeframe = factor(ifelse(method %in% c("V1", "V7", "V13"), "0-05",
                     ifelse(method %in% c("V2", "V8", "V14"), "0-10",
                     ifelse(method %in% c("V3", "V9", "V15"), "0-15",
                     ifelse(method %in% c("V4", "V10", "V16"), "0-20",
                     ifelse(method %in% c("V5", "V11", "V17"), "0-25","0-30")))))))

# plot feature importance

features_time6 %>%
  filter(!is.na(Importance)) %>%
  mutate(Importance = factor(Importance)) %>%
  ggplot() +
  geom_tile(aes(x = Classifier, y = variable, fill = Importance),
            stat = "identity", position = "identity") +
  facet_grid(. ~ timeframe) +
  ylab("Variable") +
  theme_bw() +
  theme(#text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(direction = -1)


#########################################################

# extract top 5 features per model
features_keys <- results_keys %>%
  filter(trainmethod != "Baseline (majority class)") %>%
  mutate(features =  beg2char(bestFeat, ",", 5)) %>%
  separate(features, c("f1", "f2", "f3", "f4", "f5"), ",") %>%
  gather(key = "variable_imp", value = "variable", f1:f5) %>%
  mutate(variable_imp = gsub("f", "", variable_imp)) %>%
  select(trainmethod, timeframe, variable_imp, variable)

features_keys2 <- features_keys %>%
  mutate(id = row_number()) %>%
  spread(key = variable, value = variable_imp, fill = 0) %>%
  select(-`<NA>`)

features_keys3 <- features_keys2 %>%
  select(-id) %>%
  mutate_at(vars(`0_to_20_.iki_15_20`:`5_to_10_.ratio_keystrokes_final_prod`),
            as.numeric) %>%
  group_by(trainmethod, timeframe) %>%
  summarize_each(funs(sum))


write.csv(features_keys3, "data/feature_importance_keys.csv", row.names = F)


features_keys4 <- as.data.frame(t(features_keys3[-c(1:2)]))

features_keys4[features_keys4 == 0] <- NA

#order on basis of most important feature
order <- apply(features_keys4, 2, function(x) 6 - as.numeric(x))
order <- rowSums(order, na.rm=TRUE)


features_keys5 <- as.data.frame(cbind(variable = rownames(features_keys4),
                                      features_keys4)) %>%
  mutate(Time = gsub("\\..*$", "", variable),
         Variable = gsub("^.*\\.", "", variable),
         order = as.numeric(order),
         Variable = ifelse(Variable == "betweensentence_m", "Mean time between sentences",
                    ifelse(Variable == "cut_paste_jump_m", "Mean time cut/paste/jump events",
                    ifelse(Variable == "iki_05_10", "Number of IKI 0.5-1sec",
                    ifelse(Variable == "iki_15_20", "Number of IKI 1.5-2sec",
                    ifelse(Variable == "iki_20_30", "Number of IKI 2-3sec",
                    ifelse(Variable == "iki_m", "Mean IKI",
                    ifelse(Variable == "iki_sd", "SD IKI",
                    ifelse(Variable == "initial_pause", "Initial pause time",
                    ifelse(Variable == "multi_backspacetime_m", "Mean time in multiple backspacing",
                    ifelse(Variable == "n_intext_rev", "Number of in-text revisions",
                    ifelse(Variable == "num_bursts", "Number of bursts",
                    ifelse(Variable == "perc_other_events", "Percentage of other events",
                    ifelse(Variable == "perc_rbursts", "Percentage of R-bursts",
                    ifelse(Variable == "perc_words_pbursts", "Percentage of words in P-bursts",
                    ifelse(Variable == "ratio_keystrokes_final_prod", "Percentage of characters in final text",
                    ifelse(Variable == "single_backspacetime_sd", "SD time in single backspacing",
                                      Variable)))))))))))))))),
         Time = ifelse(Time == "0_to_5_k", "(0-5/30th keys)",
                ifelse(Time == "0_to_10_", "(0-10/30th keys)",
                ifelse(Time == "0_to_15_", "(0-15/30th keys)",
                ifelse(Time == "0_to_20_", "(0-20/30th keys)",
                ifelse(Time == "0_to_25_", "(0-25/30th keys)",
                ifelse(Time == "0_to_30_", "(0-30/30th keys)",
                ifelse(Time == "5_to_10_", "(5-10/30th keys)",
                ifelse(Time == "10_to_15", "(10-15/30th keys)",
                ifelse(Time == "15_to_20", "(15-20/30th keys)",
                ifelse(Time == "20_to_25", "(20-25/30th keys)", Time))))))))))) %>%
  arrange(order) %>%
  mutate(variable = factor(variable, unique(variable),
                           labels = paste(Variable, Time))) %>%
  select(-order, -Time, -Variable)

features_keys6 <- features_keys5 %>%
  gather(key = "method", value = "Importance", -variable) %>%
  mutate(Classifier = ifelse(method %in% c("V1", "V2", "V3", "V4", "V5", "V6"),
                             "Naive Bayes",
                             ifelse(method %in% c("V7", "V8", "V9", "V10", "V11", "V12"),
                                    "Random forest", "Support vector machine")),
         timeframe = factor(ifelse(method %in% c("V1", "V7", "V13"), "0-05",
                                   ifelse(method %in% c("V2", "V8", "V14"), "0-10",
                                          ifelse(method %in% c("V3", "V9", "V15"), "0-15",
                                                 ifelse(method %in% c("V4", "V10", "V16"), "0-20",
                                                        ifelse(method %in% c("V5", "V11", "V17"), "0-25","0-30")))))))

# plot feature importance
features_keys6 %>%
  filter(!is.na(Importance)) %>%
  mutate(Importance = factor(Importance)) %>%
  ggplot() +
  geom_tile(aes(x = Classifier, y = variable, fill = Importance),
            stat = "identity", position = "identity") +
  facet_grid(. ~ timeframe) +
  ylab("Variable") +
  theme_bw() +
  theme(#text = element_text(size = 20),
    axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(direction = -1)

