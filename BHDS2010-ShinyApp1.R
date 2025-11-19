library(readr)
library(car)
df <- read_csv("healthcare_dataset.csv")
#View(df)
df_small <- df[1:1000, ]
View(df_small)


df_small$Gender <- as.factor(df_small$Gender)
df_small$`Blood Type` <- as.factor(df_small$`Blood Type`)
df_small$`Medical Condition` <- as.factor(df_small$`Medical Condition`)
df_small$Doctor <- as.factor(df_small$Doctor)
df_small$Hospital <- as.factor(df_small$Hospital)
df_small$`Insurance Provider` <- as.factor(df_small$`Insurance Provider`)




model_small <- lm(`Billing Amount` ~ Age + Medication + Gender +`Medical Condition` + `Insurance Provider` + `Admission Type`, data = df_small)
summary(model_small)
Anova(model_small)
car::Anova(model_small, type = 2)


library(ggplot2)
library(dplyr)
library(lubridate)
df_small$month_bin <- floor_date(df_small$`Date of Admission`, "month")
ggplot(df_small, aes(x = month_bin)) +
  geom_histogram(stat = "count") + geom_bar(width = 2) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  xlab("Month") +
  ylab("Number of Admissions") +
  ggtitle("Admissions by Month") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df, aes(x = `Billing Amount`)) +
  geom_histogram(binwidth = 500, color = "black", fill = "lightblue") +
  theme_bw() +
  ylab("Count") +
  xlab("Billing Amount") +
  ggtitle("Distribution of Billing Amounts")

