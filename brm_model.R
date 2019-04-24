library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(fitdistrplus)
library(brms)

# Region 4 ####
# Region 4 is the critical region (the question)
# Read in first pass data
data <- read_csv("FPs.csv")

# participant 36 is actually the second half of 35 so rename that
data[data$Participant == 36,]$Participant <- 35

# condition labels, 1 = prediction facilitated, 2 = prediction unfacilitated 
data$Condition <- recode(data$Condition, "1" = "facilitated", "2" = "unfacilitated")

data$Condition <- as.factor(data$Condition)

# Throw away zeroes
data <- data %>% filter(R4 != 0)

# Visualise
data %>% 
  ggplot(aes(x = Condition, y = R4, colour = Condition)) +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  guides(colour = FALSE)

data %>% 
  group_by(Condition) %>%
  summarise(mean(R4), sd(R4))

model <- brm(R4 ~ Condition + (1 | Participant) + (1 + Condition | Item), data = data)
summary(model)
