install.packages("randomForest")
install.packages("caret")
install.packages("ranger")
install.packages("rpart")
install.packages("rpart.plot")    # Для красивої візуалізації дерева
install.packages("readr")  # Встановлення (якщо ще не встановлено)
install.packages("party")
library(party)
library(readr)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)  # для крос-валідації
library(ranger)
library(tidyr)

df <- read_csv("Life_Expectancy_Data.csv")
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

df$Hepatitis_B <- as.numeric(ifelse(df$Hepatitis_B == "N/A", NA, df$Hepatitis_B))
df %>%
  count(Hepatitis_B) %>%
  arrange(desc(n))
df$Polio <- as.numeric(ifelse(df$Polio == "N/A", NA, df$Polio))
df %>%
  count(Polio) %>%
  arrange(desc(n))
df$Total_expenditure <- as.numeric(ifelse(df$Total_expenditure == "N/A", NA, df$Total_expenditure))
df %>%
  count(Total_expenditure) %>%
  arrange(desc(n))
df$Diphtheria <- as.numeric(ifelse(df$Diphtheria == "N/A", NA, df$Diphtheria))
df %>%
  count(Diphtheria) %>%
  arrange(desc(n))
df$Population <- as.numeric(ifelse(df$Population == "Unknown", NA, df$Population))
df %>%
  count(Population) %>%
  arrange(desc(n))
#Видалимо певні рядки де дані пропущено (у деяких стовпцях пропущено не так багато даних, тому їх можна видалити)
df <- df %>% filter(!is.na(Polio) & !is.na(Diphtheria) & !is.na(Income_composition_of_resources) & !is.na(Schooling) & !is.na(Total_expenditure))
#Заміняємо пропущені значення у стовпцях Hepatitis_B та Population на середні
#Середні значення вираховуються для кожного континента окремо
# Обчислюємо середні значення для кожного континенту
averages <- df %>%
  group_by(Continent) %>%
  summarise(
    mean_Hepatitis_B = mean(Hepatitis_B, na.rm = TRUE),
    mean_Population  = mean(Population, na.rm = TRUE)
  )

print(averages)
#Заповнюємо порожні комірки у стовпцях
df <- df %>%
  group_by(Continent) %>%
  mutate(
    Hepatitis_B = if_else(is.na(Hepatitis_B), mean(Hepatitis_B, na.rm = TRUE), Hepatitis_B),
    Population  = if_else(is.na(Population),  mean(Population,  na.rm = TRUE), Population)
  ) %>%
  ungroup()
#Кількість пропущених значень
colSums(is.na(df))

#Метод випадкових дерев
# Перетворення назв стовпців у коректний формат (без пробілів, спецсимволів)
colnames(df) <- make.names(colnames(df))

# Перетворення цільової змінної Continent у фактор
df$Continent <- as.factor(df$Continent)

# Розділення даних на навчальну (80%) та тестову (20%) вибірки
set.seed(123)
trainIndex <- createDataPartition(df$Continent, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test  <- df[-trainIndex, ]
str(train)  # Структура даних
summary(train)  # Описова статистика

# Навчання моделі Random Forest
classifier_rf <- ranger(
  formula = Continent ~ .,  # Використовуємо всі ознаки для передбачення
  data = train,             # Навчальна вибірка
  num.trees = 100,          # Кількість дерев (n_estimators = 100 у sklearn)
  max.depth = 20,
  min.node.size = 5,# Максимальна глибина дерев (аналог max_depth = 5 у sklearn)
  mtry = floor(sqrt(ncol(train) - 1)),  # Аналог max_features="sqrt" у sklearn
  importance = "impurity",  # Аналог criterion="gini" (оцінка важливості змінних)
  oob.error = TRUE,         # Використання OOB (out-of-bag) оцінки
  seed = 42                 # Для відтворюваності результатів (аналог random_state=42)
)

# Перевірка OOB-оцінки (аналог classifier_rf.oob_score_ у Python)
oob_error <- classifier_rf$prediction.error
oob_accuracy <- 1 - oob_error

# Виведення результатів
print(paste("OOB Error:", round(oob_error, 4)))
print(paste("OOB Accuracy:", round(oob_accuracy, 4)))

#Підбір найкращих гіперпараметрів
param_grid <- expand.grid(
  max.depth = c(2, 3, 5, 10, 20),       # Аналог max_depth
  min.node.size = c(5, 10, 20, 50, 100, 200), # Аналог min_samples_leaf
  num.trees = c(5, 10, 25, 30, 50, 100) # Аналог n_estimators
)
set.seed(42)
trainIndex <- createDataPartition(df$Continent, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test  <- df[-trainIndex, ]
results <- data.frame()

for (i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  
  model <- ranger(
    formula = Continent ~ .,
    data = train,
    num.trees = params$num.trees,
    max.depth = params$max.depth,
    min.node.size = params$min.node.size,
    oob.error = TRUE,
    seed = 42
  )
  
  oob_error <- model$prediction.error
  
  results <- rbind(results, data.frame(
    num.trees = params$num.trees,
    max.depth = params$max.depth,
    min.node.size = params$min.node.size,
    oob_error = oob_error
  ))
}
best_params <- results %>% arrange(oob_error) %>% head(1)
print(best_params)


# Візуалізація важливості змінних
importance_rf <- classifier_rf$variable.importance
importance_rf_df <- data.frame(
  Feature = names(importance_rf),
  Importance = importance_rf
)

# Сортуємо за важливістю та будуємо графік
importance_rf_df <- importance_rf_df %>%
  arrange(desc(Importance))

ggplot(importance_rf_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable Importance in Random Forest", x = "Features", y = "Importance")
# Навчання моделі за допомогою randomForest
rf_model <- randomForest(Continent ~ ., data = train, ntree = 100)

# Візуалізація одного з дерев
library(rpart.plot)
rpart.plot(rf_model$forest[[1]])  # Вивести перше дерево з лісу
# Вивести структуру одного дерева в лісі
tree <- randomForest::getTree(rf_model, k = 1, labelVar = TRUE)

# Переглянути структуру дерева
print(tree)
