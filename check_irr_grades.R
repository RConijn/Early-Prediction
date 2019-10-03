######## ---------- LOAD PACKAGES ----------------------------------------######
library("dplyr")
library(irr)
library(psych)

######## ---------- LOAD DATA --------------------------------------------######
grades1 <- read.csv("data/grades.csv")
grades2 <- read.csv("data/grades_rianne.csv")

######## ---------- CHECK CORRELATIONS -----------------------------------######
cor.test(grades1$c.total, grades2$r.total)

cor.test(grades1$c.structure, grades2$r.structure)
cor.test(grades1$c.grammar, grades2$r.grammar)
cor.test(grades1$c.language, grades2$r.language)
cor.test(grades1$c.content, grades2$r.content)
cor.test(grades1$c.idea, grades2$r.idea)


######## ---------- CHECK IRR -------------------------------------------######
grades <- left_join(grades1, grades2) %>%
  mutate(diff = abs(c.total - r.total),
         c.pass = c.total >= 5.5,
         r.pass = r.total >= 5.5,
         r.total2 = r.total *2,
         c.total2 = c.total*2,
         pass_fail = (c.pass != r.pass)) %>%
  # no keystroke data / copy paster
  filter(!ï..id %in%  c(27,55,82))


kappa2(grades[,c("r.total","c.total")], "squared")
kappa2(grades[,c("c.grammar","r.grammar")], "squared")
kappa2(grades[,c("c.language","r.language")], "squared")
kappa2(grades[,c("c.content","r.content")], "squared")
kappa2(grades[,c("c.idea","r.idea")], "squared")
kappa2(grades[,c("c.structure","r.structure")], "squared")

flag <- filter(grades, (c.total - r.total) > 1|
                 (c.total - r.total) < -1)
#6.3% more than 1.0 score difference
flag <- filter(grades, (c.total - r.total) > 2|
                 (c.total - r.total) < -2)
#0% more than 2.0 score difference

mean(grades$c.idea)
mean(grades$c.structure)
mean(grades$c.content)
mean(grades$c.language)
mean(grades$c.grammar)
mean(grades$c.total)

sd(grades$c.idea)
sd(grades$c.structure)
sd(grades$c.content)
sd(grades$c.language)
sd(grades$c.grammar)
sd(grades$c.total)

sum(grades$c.idea <= 2)
sum(grades$c.structure <= 2)
sum(grades$c.content <= 2)
sum(grades$c.language <= 2)
sum(grades$c.grammar <= 2)
sum(grades$c.total < 5.5)
