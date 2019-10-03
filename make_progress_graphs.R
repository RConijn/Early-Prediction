######## ---------- LOAD PACKAGES ----------------------------------------######
library(dplyr)
library(ggplot2)
library(tidyverse)
library(egg)

######## ---------- LOAD DATA --------------------------------------------######
#keystroke data
keylog <- read.csv("data/extendedlog.csv") %>%
  left_join(read.csv("data/planningstage3.csv"), by = "pid")


######## ---------- MAKE PLOT --------------------------------------------######

make_progress_graph <-function(student){
dataset <- keylog %>%
  filter(pid == student)
print(student)
## make source usage plot
plot2 <-  ggplot(dataset %>% filter(!is.na(focus))) +
  geom_point(aes(x=startTime/1000/60, y = focus, colour=focus)) +
  labs(x = "Time (min)", y = "Source opened") +
  theme_bw() +
  theme(legend.position = "none") +
  #geom_vline(xintercept=dataset$plan_end/60) +
  scale_x_continuous(minor_breaks = seq(0 , 30, 2), breaks = seq(0 , 30, 4))

## make characters typed plot
plot1 <-  ggplot(dataset %>% mutate(char_pasted =
                                      ifelse(char_pasted == 0, -1, char_pasted))) +
  geom_line(aes(x=startTime/1000/60, y = positionFull,colour="Position in document")) +
  geom_line(aes(x=startTime/1000/60, y = doclengthFull,colour="Document length")) +
  geom_line(aes(x=startTime/1000/60, y = charProduction,colour="Total characters produced")) +
  #geom_point(aes(x=startTime/1000/60, y = char_pasted,colour="Total characters pasted")) +
  labs( y = "Number of characters") +
  ylim(0,NA) +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_bw() +
  theme(legend.title= element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "lines"),
        legend.position = c(0.2, 0.8)) +
  #geom_vline(xintercept=dataset$plan_end/60) +
  scale_x_continuous(minor_breaks = seq(0 , 30, 2), breaks = seq(0 , 30, 4),
                     limits = c(0,30))   # +
  #ggtitle(student)

## combine plots
p <- ggarrange(plot1, plot2, ncol=1, heights = c(80,20))

pdf(paste0("plots/progress_graphs/pg_pid_", student, ".pdf"),
    width = 8, height = 6, bg = "transparent", family = "Times")
print(p)
dev.off()
}

## call function for a particular student
make_progress_graph(45)

##print all graphs
students <- unique(keylog$pid)
lapply(students, make_progress_graph)

################################################################################
