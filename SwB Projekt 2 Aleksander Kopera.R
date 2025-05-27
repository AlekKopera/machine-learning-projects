################################################################################
##                    SwB - Projekt 2 - segmentacja                           ##
################################################################################

# 1. Przygotowanie środowiska pracy i wczytanie danych
# 2. Przygotowanie danych i standaryzacja
# 3. Klastrowanie hierarchiczne
# 4. Klastrowanie metodą k-means

#===============================================================================
# 1. Przygotowanie środowiska pracy i wczytanie danych
#===============================================================================

if (!requireNamespace("tidyverse",quietly=T)) install.packages("tidyverse"); library(tidyverse)
if (!requireNamespace("NbClust",quietly=T)) install.packages("NbClust"); library(NbClust)

rm(list=ls())
getwd()
# setwd()

data = read.csv("dane_projekt2.csv",sep=";",header=T)
View(data)

#===============================================================================
# 2. Przygotowanie danych, standaryzacja, analiza pareto
#===============================================================================

row.names(data) = data[,1]
data = select(data,-PSD)

sum(is.na(data)) # 0 brakujących wartości
for (col in names(data)) {
  col_val = data[[col]]
  
  empty = all(col_val != "")
  dash = all(col_val != "-")
  non_neg = all(col_val >= 0)
  
  cat("Kolumna:", col, "\n")
  cat("  Brak pustych wartości (''): ", empty, "\n")
  cat("  Brak wartości '-': ", dash, "\n")
  cat("  Brak ujemnych wart.: ", non_neg, "\n")
}

summary(data) # SKU3 i SKU6 się nigdzie nie sprzedają
data = select(data, -SKU6)
data = select(data, -SKU3)

n = nrow(data)
k = ncol(data)

data$total_val <- rowSums(data[, 1:k])
summary(data$total_val) # W każdym punkcie sprzedaży sprzedaje się towar za 2000
data = select(data, -total_val)

data_scaled = scale(data)
data_scaled = as.data.frame(data_scaled)
summary(data_scaled)
anyNA(data_scaled)
#rm(data)

#=== Analiza pareto ze względu na produkty
sku_sales = colSums(data) # Obliczenie sumarycznej sprzedaży dla każdego SKU
sku_sales_sorted = sort(sku_sales, decreasing = T) # Posortowanie produktów według sprzedaży malejąco
sku_cumulative = cumsum(sku_sales_sorted) / sum(sku_sales_sorted) * 100 # Obliczenie skumulowanego udziału procentowego

# Tworzenie ramki danych
SKU = names(sku_sales_sorted)
Sales = sku_sales_sorted
Cumulative = sku_cumulative
pareto_df <- data.frame(SKU, Sales, Cumulative)

# Znalezienie liczby produktów, które generują X% sprzedaży (np. 70%)
target_percent <- 0.70  # Można zmienić
pareto_cutoff <- which(pareto_df$Cumulative >= target_percent * 100)[1]  # Pierwszy punkt spełniający warunek

# Wykres Pareto z dynamicznym progiem
ggplot(pareto_df, aes(x = reorder(SKU, -Sales), y = Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Słupki sprzedaży dla SKU
  geom_line(aes(y = Cumulative * max(Sales) / 100, group = 1), color = "red", size = 1) +  # Skumulowany udział procentowy
  geom_point(aes(y = Cumulative * max(Sales) / 100), color = "red", size = 2) +  # Punkty na linii Pareto
  geom_hline(yintercept = max(Sales) * target_percent, linetype = "dashed", color = "black") +  # Linia X% sprzedaży
  geom_vline(xintercept = pareto_cutoff, linetype = "dashed", color = "black") +  # Nowa pionowa linia
#  labs(title = paste0("Analiza Pareto dla sprzedaży produktów (SKU) - ", target_percent * 100, "% sprzedaży"),
#       x = "Produkt (SKU)",
#       y = "Łączna sprzedaż") +
  labs(x = "Produkt (SKU)",
       y = "Łączna sprzedaż") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Obrót etykiet SKU
  scale_y_continuous(labels = scales::comma)
# Źródło: Opracowanie własne na podstawie danych transakcyjnych
# Wykres 1: Analiza Pareto dla sprzedaży produktów (SKU)

#===============================================================================
# 3. Klastrowanie hierarchiczne
#===============================================================================

# Uwaga niektóre obliczenia w tej sekcji są bardzo obciążające dla komputera

#=== Podejscie 1
determine.nclust1 = NbClust(data_scaled,
                             distance="euclidean", # Sposób liczenia odległości
                             min.nc=3, max.nc=10,  # Rozważamy od 3 do 10 klastrów
                             method="average",     # Łaczenie klastrów
                             index="silhouette")   # Index według którego wybieramy liczbe klastrów

determine.nclust1$Best.nc[1] # 3 klastry

dist_matrix1 = dist(data_scaled,"euclidean") # Metoda euclidaen
hierch1 = hclust(dist_matrix1,"average")
dend1 = as.dendrogram(hierch1)
plot(dend1)

cluster.labs1 <- cutree(hierch1,3)
table(cluster.labs1) # Bardzo nierównomierne rozłożenie obserwacji między różnymi klastrami

#=== Podejście 2
determine.nclust2 = NbClust(data_scaled,
                            distance="euclidean", # Sposób liczenia odległości
                            min.nc=3, max.nc=15,  # Rozważamy od 3 do 15 klastrów
                            method="ward.D2",     # Łaczenie klastrów
                            index="silhouette")   # Index według którego wybieramy liczbe klastrów

determine.nclust2$Best.nc[1] # 13 klastrów

dist_matrix2 = dist(data_scaled,"euclidean") # Metoda euclidaen
hierch2 = hclust(dist_matrix2,"ward.D2")
dend2 = as.dendrogram(hierch2)
plot(dend1)

cluster.labs2 <- cutree(hierch2,13)
table(cluster.labs2) # dalej nierówno rozłożenie

#=== Podejście 2
determine.nclust2 = NbClust(data_scaled,
                            distance="euclidean", # Sposób liczenia odległości
                            min.nc=3, max.nc=15,  # Rozważamy od 3 do 15 klastrów
                            method="ward.D2",     # Łaczenie klastrów
                            index="silhouette")   # Index według którego wybieramy liczbe klastrów

determine.nclust2$Best.nc[1] # 13 klastrów

dist_matrix2 = dist(data_scaled,"euclidean") # Metoda euclidaen
hierch2 = hclust(dist_matrix2,"ward.D2")
dend2 = as.dendrogram(hierch2)
plot(dend1)

cluster.labs2 <- cutree(hierch2,13)
table(cluster.labs2) # dalej nierówne rozłożenie

#=== Podejście 3
determine.nclust3 = NbClust(data_scaled,
                            distance="manhattan", # Sposób liczenia odległości
                            min.nc=3, max.nc=10,  # Rozważamy od 3 do 15 klastrów
                            method="average",     # Łaczenie klastrów
                            index="silhouette")   # Index według którego wybieramy liczbe klastrów

determine.nclust3$Best.nc[1] # 5 klastrów

dist_matrix3 = dist(data_scaled,"euclidean") # Metoda euclidaen
hierch3 = hclust(dist_matrix3,"average")
dend3 = as.dendrogram(hierch3)
plot(dend3)

cluster.labs3 <- cutree(hierch3,5)
table(cluster.labs3) # dalej nierówne rozłożenie


#===============================================================================
# 4. Klastrowanie metodą k-means
#===============================================================================
kmeans3 = kmeans(data_scaled,3,nstart=25)
table(kmeans3$cluster) # dużo bardziej równomierne rozłożenie obserwacji

#=== Metoda łokcia
kmeans1 <- kmeans(data_scaled,1,nstart=25)
kmeans2 <- kmeans(data_scaled,2,nstart=25)
kmeans3 <- kmeans(data_scaled,3,nstart=25)
kmeans4 <- kmeans(data_scaled,4,nstart=25)
kmeans5 <- kmeans(data_scaled,5,nstart=25)
kmeans6 <- kmeans(data_scaled,6,nstart=25)
kmeans7 <- kmeans(data_scaled,7,nstart=25)
kmeans8 <- kmeans(data_scaled,8,nstart=25)
kmeans9 <- kmeans(data_scaled,9,nstart=25)
kmeans10 <- kmeans(data_scaled,10,nstart=25)
kmeans11 <- kmeans(data_scaled,11,nstart=25)
kmeans12 <- kmeans(data_scaled,12,nstart=25)
kmeans13 <- kmeans(data_scaled,13,nstart=25)
kmeans14 <- kmeans(data_scaled,14,nstart=25)
kmeans15 <- kmeans(data_scaled,15,nstart=25)

# tot.withinss - całkowita suma kwadratów odległości wewnątrz klastra
var.within <- c(kmeans1$tot.withinss,kmeans2$tot.withinss,kmeans3$tot.withinss,kmeans4$tot.withinss,
                kmeans5$tot.withinss,kmeans6$tot.withinss,kmeans7$tot.withinss,kmeans8$tot.withinss,
                kmeans9$tot.withinss,kmeans10$tot.withinss,kmeans11$tot.withinss,kmeans12$tot.withinss,
                kmeans13$tot.withinss,kmeans14$tot.withinss,kmeans15$tot.withinss)
nclusters <- seq(1:15)
plot(nclusters,var.within,type="l") # Prawdopodobnie widzimy łokieć przy 10 - wybieramy 10 klastrów

# Wykres z pakietem ggplot
elbow_data <- data.frame(
  nclusters = 1:15,
  var_within = c(kmeans1$tot.withinss, kmeans2$tot.withinss, kmeans3$tot.withinss, 
                 kmeans4$tot.withinss, kmeans5$tot.withinss, kmeans6$tot.withinss, 
                 kmeans7$tot.withinss, kmeans8$tot.withinss, kmeans9$tot.withinss, 
                 kmeans10$tot.withinss, kmeans11$tot.withinss, kmeans12$tot.withinss, 
                 kmeans13$tot.withinss, kmeans14$tot.withinss, kmeans15$tot.withinss)
)

# Tworzenie wykresu łokciowego
ggplot(elbow_data, aes(x = nclusters, y = var_within)) +
  geom_line(color = "blue", size = 1) +  # Linia wykresu
  geom_point(color = "red", size = 3) +  # Punkty na wykresie
  geom_vline(xintercept = 9, linetype = "dashed", color = "black") + # Zaznaczenie optymalnej liczby klastrów
  labs(x = "Liczba klastrów",
       y = "Wariancja wewnątrzklastrowa (tot.withinss)") +
  theme_minimal() +  # Minimalistyczny styl wykresu
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
# Źródło: Opracowanie własne na podstaiwe danych transakcyjnych
# Wykres 2: Wykres łokciowy (wybór optymalnej liczby klastrów)




var.between = c(kmeans1$betweenss,kmeans2$betweenss, kmeans3$betweenss,
                 kmeans4$betweenss, kmeans5$betweenss, kmeans6$betweenss,
                 kmeans7$betweenss, kmeans8$betweenss, kmeans9$betweenss,
                 kmeans10$betweenss, kmeans11$betweenss, kmeans12$betweenss,
                 kmeans13$betweenss, kmeans14$betweenss, kmeans15$betweenss)

plot(nclusters, var.between, type="l") # Na tym wykresie ciężko jest mówić o łokciu


table(kmeans9$cluster)
data_scaled$Cluster = kmeans9$cluster

#=== Dominujące produkty w każdym klastrze
# Obliczenie średnich dla każdej zmiennej w podziale na klastery
cluster_means = aggregate(. ~ Cluster, data = data_scaled, mean)
print(cluster_means)

# Funkcja wybierająca top N produktów w każdym klastrze
top_products = apply(cluster_means[, -1], 1, function(row) {
  # Sortowanie wartości w wierszu w malejącej kolejności i wybór top 3
  top <- names(sort(row, decreasing = TRUE)[1:3])
  paste(top, collapse = ", ") # Łączenie wyników w jedną linię
})

# Tworzenie poziomej tabeli z wynikami
top_products_df = data.frame(
  Cluster = cluster_means$Cluster,
  Top_Products = top_products
)

# Wyświetlenie wyniku
print(top_products_df)

#=== Udział procentowych produktów w klastrach
data$Cluster = kmeans9$cluster # Powracamy do danych niewystandaryzowanych
cluster_sums = aggregate(. ~ Cluster, data = data, sum)

# Udział procentowy produktów w sumarycznej sprzedaży klastra
cluster_percent <- cluster_sums
cluster_percent[, -1] <- sweep(cluster_sums[, -1], 1, rowSums(cluster_sums[, -1]), FUN = "/") * 100

# Wyświetlenie poprawnych wyników
print(cluster_percent)

# Funkcja wybierająca top 3 produkty na podstawie udziału procentowego
top_products_percent <- apply(cluster_percent[, -1], 1, function(row) {
  top <- names(sort(row, decreasing = TRUE)[1:3])  # Sortowanie wiersza i wybór top 3
  paste(top, collapse = ", ")  # Łączenie nazw produktów w jedną linię
})

# Tworzenie tabeli z wynikami
top_products_percent_df <- data.frame(
  Cluster = cluster_percent$Cluster,  # Kolumna z numerami klastrów
  Top_Products_Percent = top_products_percent  # Top 3 produkty w klastrze
)

# Wyświetlenie wyników
print(top_products_percent_df)



