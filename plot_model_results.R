library(dplyr)
library(ggplot2)

results_time <- read.csv("data/results_time_auc", stringsAsFactors = F)

results_keys <- read.csv("data/results_keys_auc", stringsAsFactors = F)

results_keys_f <- read.csv("data/results_keys_F", stringsAsFactors = F)

#############----------PLOT RESULTS----------------------------------###########
results_time %>%
  ggplot(aes(x=timeframe, y=AUC, group=trainmethod)) +
  geom_line(aes(color=trainmethod))+
  geom_point(aes(color=trainmethod))+
  theme_bw() +
  scale_x_continuous(breaks = seq(0,30,5), minor_breaks = seq(0 , 30, 1)) +
  scale_color_discrete(name = "Classifier") +
  xlab("Timeframe (mins)") +
  theme(legend.position="bottom")

results_keys %>%
  ggplot(aes(x=timeframe, y=AUC, group=trainmethod)) +
  geom_line(aes(color=trainmethod))+
  geom_point(aes(color=trainmethod))+
  theme_bw() +
  scale_x_continuous(breaks = seq(0,30,5), minor_breaks = seq(0 , 30, 1)) +
  scale_color_discrete(name = "Classifier:") +
  xlab("Timeframe (x/30th keystrokes)") +
  theme(legend.position="bottom")


##########


#plot results precision, recall, Fscore, accuracy for RF only
plot_class_eval <- function(depvar,trainmethod, metrics){
  results <- readRDS(paste0("models/results_",depvar,"_",trainmethod, ".rda"))

  results %>%
    gather(metric = metrics) %>%
    ggplot(aes(x=timeframe, y=value)) +
    geom_line(aes(color=key))+
    geom_point(aes(color=key))+
    theme_bw() +
    scale_x_continuous(breaks = seq(0,30,5), minor_breaks = seq(0 , 30, 1)) +
    xlab("Timeframe (mins)") +
    theme(legend.position="bottom") +
    scale_color_discrete(name = "Metric:") +
    ylim(-0.02, .8)
}
