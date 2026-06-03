#load packages 
library(ggplot2)
library(survival)
library(survminer)

#load data
haberman <- read.csv("Downloads/haberman.csv")

#fix datasets
colnames(haberman) <- c(
  "age",
  "operation_year",
  "positive_nodes",
  "survival_status"
)

haberman$survival_status <- ifelse(
  haberman$survival_status == 1,
  "Survived",
  "Died"
)

haberman$survival_status <- factor(haberman$survival_status, levels = c("Survived", "Died"))

#make data plots
ggplot(haberman, aes(x = age, fill = survival_status)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity")

ggplot(haberman, aes(x = survival_status, y = positive_nodes, fill = survival_status)) +
  geom_boxplot()

ggplot(haberman, aes(x = survival_status, y = positive_nodes)) +
  geom_boxplot() +
  labs(
    x = "Survival Outcome",
    y = "Number of Positive Lymph Nodes",
    title = "Lymph Node Count by Survival Status"
  ) + 
  theme_minimal()

haberman$survival_status <- factor(haberman$survival_status,
                                   levels = c("Survived", "Died"))
ggplot(haberman, aes(x = survival_status, y = positive_nodes, fill = survival_status)) +
  geom_boxplot() +
  labs(
    x = "Survival Outcome",
    y = "Number of Positive Lymph Nodes",
    title = "Lymph Node Count by Survival Status"
  ) +
  theme_minimal()

#survival curve 

lung <- survival::lung

lung$status <- ifelse(lung$status == 2, 1, 0)

fit <- survfit(Surv(time, status) ~ sex, data = lung)

ggsurvplot(
  fit,
  data = lung,
  pval = TRUE,
  risk.table = TRUE,
  legend.labs = c("Male", "Female"),
  title = "Survival Curve by Sex (Lung Cancer Dataset)",
  xlab = "Days",
  ylab = "Survival Probability"
)
