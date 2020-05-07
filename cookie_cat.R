setwd("C:/Users/anura/OneDrive/Desktop/Subjects/5510-Web Analytics/Project")
df = read.csv("cookie_cats.csv")
View(df)

df[ df == "TRUE" ] <- 1
df[ df == "FALSE" ] <- 0

View(df)

model1 = glm(formula = retention_1 ~ sum_gamerounds + as.factor(version),
             family = binomial("logit"), data = df)
summary(model1)

model2 = glm(formula = retention_7 ~ sum_gamerounds + as.factor(version) + retention_1,
             family = binomial("logit"), data = df)
summary(model2)

install.packages('tidyverse')
library(tidyverse)
install.packages('tidyquant')
library(tidyquant)

df %>%
  select(version) %>%
  group_by(version) %>%
  tally()

aggregate(df$retention_1, by=list(df$version), FUN=mean)

aggregate(df$retention_7, by=list(df$version), FUN=mean)


new <- matrix(nrow = 0, ncol = 2)

colnames(new) <- c("gate_30", "gate_40")

bootstrp_1d <- as_tibble(new)

for(i in 1:1000){
  bootstrp_1d[i, ]  <- df[sample(1:nrow(df), nrow(df), replace = TRUE), ] %>% 
    group_by(version) %>% 
    summarise(mean_gate = mean(retention_1)) %>% 
    ungroup() %>% 
    spread(version, mean_gate)
}

bootstrp_1d %>%
  gather(key = "version", value = "mean", ... = c(gate_30, gate_40)) %>%
  ggplot(aes(x = mean, color = version, fill = version, alpha = 0.4)) +
  geom_density() +
  theme_tq()

bootstrp_1d <- bootstrp_1d %>%
  mutate(diff = ((gate_30 - gate_40)/gate_40)*100)

View(bootstrp_1d)

bootstrp_1d %>%
  ggplot(aes(x = diff, color = 'red', fill = 'red', alpha = 0.2)) +
  geom_density() +
  xlab(label = "% diff") +
  theme_tq()

# Calculating the probability that 1-day retention is greater when the gate is at level 30
bootstrp_1d %>%
  mutate(gate_30_hgr = ifelse(diff > 0, 1, 0)) %>%
  summarise_all(mean) %>%
  select(gate_30_hgr)



#for retention 7

emp_mtrx <- matrix(nrow = 0, ncol = 2)

colnames(emp_mtrx) <- c("gate_30", "gate_40")

boot_7d <- as_tibble(emp_mtrx)

for(i in 1:1000){
  boot_7d[i, ]  <- df[sample(1:nrow(df), nrow(df), replace = TRUE), ] %>% group_by(version) %>% summarise(mean_gate = mean(retention_7)) %>% ungroup() %>% spread(version, mean_gate)
}

boot_7d %>%
  gather(key = "version", value = "mean", ... = c(gate_30, gate_40)) %>%
  ggplot(aes(x = mean, color = version, fill = version, alpha = 0.4)) +
  geom_density() +
  theme_tq()


boot_7d <- boot_7d %>%
  mutate(diff = ((gate_30 - gate_40)/gate_40)*100)

boot_7d %>%
  ggplot(aes(x = diff, color = 'red', fill = 'red', alpha = 0.2)) +
  geom_density() +
  xlab(label = "% diff") +
  theme_tq()


boot_7d %>%
  mutate(gate_30_hgr = ifelse(diff > 0, 1, 0)) %>%
  summarise_all(mean) %>%
  select(gate_30_hgr)



