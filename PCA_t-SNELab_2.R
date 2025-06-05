install.packages("reshape2")
install.packages("rgl")
install.packages("Rtsne")
install.packages("plotly")
library(plotly)
library(Rtsne)
library(dplyr)
library(ggplot2)
library(crayon)
library(scales)
library(lubridate)
library(tidyverse)
library(writexl)
library(reshape2)
library(rgl)

df <- read_csv("Country-data.csv")
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
#Кількість пропущених значень
colSums(is.na(df))
#Переглянемо чи повторюються певні країни
table(df$country)
#Переглянемо кореляцію
cor_matrix <- cor(df[, -1])  # Виключаємо стовпець з назвами країн
melted_cor <- melt(cor_matrix)
ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(mid = "white", high = "red", low = "blue") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Додаємо значення в клітинки
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap")
#Оскільки life_expec добре корелює з багатьма змінними оберемо її за цільову функцію
#поки стовпець із країнами нам не потрібний
Y <- data.frame(life_expec = df$life_expec)
X <- df[, !(names(df) %in% c("country", "life_expec"))]
melted_cor <- melt(cor(X))
ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(mid = "white", high = "red", low = "blue") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Додаємо значення в клітинки
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap")
Y %>%
  count(life_expec) %>%
  arrange(desc(n))
#Стандартизуємо дані
data_scaled <- scale(X)
# Виконання PCA
pca_model <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
summary(pca_model)  # Показує, скільки варіації пояснюють кожна компонента
screeplot(pca_model)  # Графік, що показує, скільки варіації пояснюється кожною компонентою
#За умовою на потрібно залишити тільки дві варіації
pca_model <- prcomp(data_scaled, center = TRUE, scale. = TRUE, rank. = 2)
summary(pca_model)
# Пояснювальна дисперсія для кожної компоненти
explained_variance <- pca_model$sdev^2
explained_variance_ratio <- explained_variance / sum(explained_variance)  # Відсоток варіації для кожної компоненти

# Виведемо покомпонентну дисперсію
cat("Покомпонентна дисперсія (власні значення):\n")
print(explained_variance)

# Виведемо покомпонентну пояснювальну дисперсію
cat("Покомпонентна пояснювальна дисперсія (у відсотках):\n")
print(explained_variance_ratio)

# Сумарна пояснювальна дисперсія
cat("Сумарна пояснювальна дисперсія:\n")
print(sum(explained_variance_ratio))

#Сумарна пояснювальна дисперсія для перших 2 компонент
print(sum(explained_variance_ratio[1:4]))
#На наступному графіку можна побачити що тільки перші 4 компоненти
#містять більшу частину інформації, (>0.8) тому всі інші можна відкинути
screeplot(pca_model, main="Scree Plot")
# Отримуємо коефіцієнти головних компонент
#Пробуємо аналізувати кореляційні зв"язки між головними компонентами та ознаками.
#Так іноді намагаються пояснити зміст головних компонент
components <- as.data.frame(pca_model$rotation)
#Побудуємо графік
# Отримання перетворених даних (T)
T <- pca_model$x[, 1:2]  # Перша і друга головні компоненти
# Візуалізація
plot(T[, 1], T[, 2], col = Y$life_expec, pch = 19, xlab = "First Principal Component", ylab = "Second Principal Component")
#Ми визначили що найкраща кількість компонент для даної вибірки 4 яка має 
#пояснювальну дисперсію 0.8766732
#Спробуємо побудувати графік
pca_model <- prcomp(data_scaled, center = TRUE, scale. = TRUE, rank. = 4)
summary(pca_model)
components <- as.data.frame(pca_model$rotation)
T <- pca_model$x[, 1:4]  # Перша і друга головні компоненти
# Візуалізація
# Створюємо 3D графік для перших трьох компонент
plot3d(T[, 1], T[, 2], T[, 3], col = Y$life_expec, size = 5, xlab = "PC1", ylab = "PC2", zlab = "PC3")

#Метод t-SNE
set.seed(42)  # Фіксуємо генератор випадкових чисел для відтворюваності

tsne_model <- Rtsne(data_scaled, dims = 2, perplexity = 50, verbose = TRUE, max_iter = 1000)
# Створюємо датафрейм для візуалізації
tsne_df <- data.frame(TSNE1 = tsne_model$Y[, 1], 
                      TSNE2 = tsne_model$Y[, 2], 
                      Life_Expectancy = Y$life_expec)

# Будуємо графік
ggplot(tsne_df, aes(x = TSNE1, y = TSNE2, color = Life_Expectancy)) +
  geom_point(size = 2) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "t-SNE Visualization", x = "t-SNE Component 1", y = "t-SNE Component 2") +
  theme_minimal()

#Пробуємо змінювати перплексію
# Вибираємо розумні значення perplexity
perplexities <- c(5, 10, 30, 50)
max_perplexity <- floor(nrow(data_scaled) / 3)
perplexities <- perplexities[perplexities < max_perplexity]  

results <- list()

for (p in perplexities) {
  tsne_test <- Rtsne(data_scaled, dims = 2, perplexity = p, max_iter = 1000)
  results[[as.character(p)]] <- data.frame(
    TSNE1 = tsne_test$Y[, 1], 
    TSNE2 = tsne_test$Y[, 2], 
    Life_Expectancy = Y$life_expec,  # Додаємо цільову змінну
    Perplexity = as.factor(p)
  )
}

tsne_all <- do.call(rbind, results)

# Візуалізація t-SNE
tsne_df$Life_Expectancy_Cat <- cut(tsne_df$Life_Expectancy, breaks = 4, labels = c("Low", "Medium", "High", "Very High"))

ggplot(tsne_df, aes(x = TSNE1, y = TSNE2, color = Life_Expectancy_Cat)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("blue", "green", "orange", "red")) +
  labs(title = "t-SNE Visualization", x = "t-SNE Component 1", y = "t-SNE Component 2") +
  theme_minimal()

# Створюємо категорії для Life_Expectancy (4 категорії)
tsne_3d_df$Life_Expectancy_Cat <- cut(tsne_3d_df$Life_Expectancy, 
                                      breaks = quantile(tsne_3d_df$Life_Expectancy, probs = seq(0, 1, 0.25), na.rm = TRUE),
                                      include.lowest = TRUE,
                                      labels = c("Low", "Medium", "High", "Very High"))

# 3D візуалізація з дискретними кольорами
plot_ly(tsne_3d_df, x = ~TSNE1, y = ~TSNE2, z = ~TSNE3, 
        color = ~Life_Expectancy_Cat, 
        colors = c("blue", "green", "orange", "red"), 
        type = "scatter3d", mode = "markers")
