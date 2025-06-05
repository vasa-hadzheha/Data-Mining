library(dplyr)
library(ggplot2)
library(crayon)
library(scales)
library(lubridate)
library(tidyverse)
library(writexl)

df <- read_csv("travel-times.csv")
#дивимось на структуру даних
str(df)
#кількість вимірів
dim(df)
#дивимося на вигляд даних
head(df,10)
#Фічі датасету
names(df)
#Отримуємо першу загальну інформацію про датасет
summary(df)
#Подивимось на формат та значення кожного стовпця
glimpse(df)
#Кількість пропущених значень
colSums(is.na(df))
# Видаляємо нерелевантні дані зі стовпця
df$FuelEconomy <- as.numeric(ifelse(df$FuelEconomy == "-", NA, df$FuelEconomy))
# Перевіряємо кількість порожніх значень
sum(is.na(df$FuelEconomy))
#Заміняємо порожні комірки середнім значенням стовпця
df$FuelEconomy[is.na(df$FuelEconomy)] <- mean(df$FuelEconomy, na.rm = TRUE)
#оскільки в останньому стовпці забагато пропусків 88% порожні тому видалимо його
df <- subset(df, select = -Comments)
#Перейдемо до перекодування категоріальних ознак
df %>%
  count(Take407All) %>%
  arrange(desc(n))
#Перекодуємо категоріальні значення стовпця
df$Take407All <- ifelse(df$Take407All == "Yes", 1, 0)
#Те саме зробимо для інших категоріальних стовпців
df %>%
  count(GoingTo) %>%
  arrange(desc(n))
df$GoingTo <- ifelse(df$GoingTo == "GSK", 2, 1)
df %>%
  count(DayOfWeek) %>%
  arrange(desc(n))
#В залежності від задачі можна перекодувати наступну колонку різними методами
#я вирішив використати one hot encoded
df_onehot <- model.matrix(~ DayOfWeek - 1, data = df)
df_onehot <- as.data.frame(df_onehot)
#Вирішуємо проблему мультиколінеарності
df_onehot <- df_onehot[, !colnames(df_onehot) %in% "DayOfWeekMonday"]
# вставимо перекодований стовпець у головний датасет
df <- cbind(df, df_onehot)
# Видаляємо оригінальний стовпець із днями тижня
df$DayOfWeek <- NULL
