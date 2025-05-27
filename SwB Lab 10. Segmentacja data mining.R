################################################################################
##                        Data Mining 04.12.2024                              ##
################################################################################

#===============================================================================
# 0. Spis treści
#===============================================================================
# 1. Przygotowanie środowiska pracy
# 2. Wczytanie i przygotowanie danych
# 3. Standaryzacja
# 4. Klastrowanie - hierarchiczne
# 5. Metoda k-średnich

#===============================================================================
# 1. Przygotowanie środowsika pracy
#===============================================================================
if(!requireNamespace("NbClust",quietly=T)) install.packages("NbClust")
if(!requireNamespace("dplyr",quietly=T)) install.packages("dplyr")
library(NbClust)
library(dplyr)
rm(list=ls())


#===============================================================================
# 2. Wczytanie i przygotowanie danych
#===============================================================================
daneRFM <- read.csv("dane_RFM.csv", sep=";", header=TRUE)
summary(daneRFM$X2016.07) #ta kolumna jest podejrzana, nie ma tu żadnych wartości
which(daneRFM$X2016.07!="")
View(daneRFM[which(daneRFM$X2016.07!=""),]) #to nie są wartości do analizy
daneRFM <- dplyr::select(daneRFM, -X2016.07) #usuwamy ostatnią kolumnę

k <- ncol(daneRFM) #tak oznaczymy numer ostatniej kolumny z miesiącami

#==== Frequency
daneRFM$Frequency <- rowSums(!is.na(daneRFM[,2:k]))
summary(daneRFM$Frequency)
View(daneRFM[daneRFM$Frequency==0,])
daneRFM <- dplyr::filter(daneRFM, Frequency!=0) #usuwamy klientów, którzy nic nie kupili

#==== Recency
daneRFM$last.purchase <- ifelse(!is.na(daneRFM[,k]),1,NA)
for(i in 1:(k-2)){ #iterujemy od 1 do k-2=29
  daneRFM$last.purchase <- ifelse(!is.na(daneRFM[,k-i]) & is.na(daneRFM$last.purchase),i+1,daneRFM$last.purchase)
} # ta pętla zastępuje 30 linijek kodu 

#===== monetary value
daneRFM$Total.value <- rowSums(daneRFM[,2:k], na.rm = TRUE)
daneRFM$Mean.value <- daneRFM$Total.value / daneRFM$Frequency
summary(daneRFM$Total.value)
summary(daneRFM$Mean.value)

#===============================================================================
# 3. Standaryzacja - sprowadzamy zmienne do porównywalnej skali - scale()
#===============================================================================
daneRFM$FREQ <- scale(daneRFM$Frequency) # scale() - służy do standaryzacji
daneRFM$REC <- scale(daneRFM$last.purchase) # Nie muisimy odwracać skali
daneRFM$MON <- scale(daneRFM$Mean.value)

summary(daneRFM[31:38])
# Srednia 0 - gites
# W mon.v1 max to 18, wartość ekstremalna w przypadku wystandaryzowanego rozkładu
# Może trzeba usunąć tego klienta, albo monetary value obłożyć log
daneRFM <- daneRFM %>% filter(MON<=18)
# Po usunięciu outlierów dane trzeba ponownie wystandaryzować

# Zapisujemy dane do segmentacji w oddzielnym dataframe
dane.segmentacja <- daneRFM [,36:38]
colnames(dane.segmentacja) <- c("FREQ","REC","MON")
colnames(dane.segmentacja)

#===============================================================================
# 4. Klastrowanie hierarchiczne
#===============================================================================
# 4.1. Wyzaczenie macierzy odległości między obserwacjami, dostępne trzy metody:
#  a) Euklidesowa
#  b) Manhatan
#  c) Maximum (Czebyszewa)
#  Funkcja dist(dane,metoda)
# 4.2. Klastrowanie, dostępne 5 meotd:
#  a) najbliższego sąsiada (single linkage)
#  b) nadalszego sąsiada (complete linkage)
#  c) średniej (average linkage)
#  d) centroidalna (centroid linkage)
#  e) Warda (Ward's linkage)
#  Funkcja hclust(macierz odl., metoda)
# 4.3 Wyznaczenie optymalnej liczby klastrów na podstawie danego indeksu:
#  a) silhouette - sylwetki
#  b) all - większość indeksów
#  Funkcja NbClust(dane,
#                  distance,       - metoda liczenia odległości między obserwacjami
#                  min.nc, max.nc, - minimalna i maksymalna liczba klastrów
#                  method,         - metoda liczenia pdobieństwa między klastrami
#                  index          - index według którego wybieramy liczbe klastrów
# 4.4 Określanie liczby klastrów i tworzenie rzeczywistych grup
#  Funkcja cutree(hclust(macierz odl.,metoda),liczba klastrów)

#===============================================================================
#=== 4.1 wyznaczanie macierzy odległosci między obserwacjami Macierz symetryczna (odelegość x od y równa y od x)
# Na początek macierz dla 4 pierwszych obserwacji 
dist(dane.segmentacja[1:4,]) # Największa odległość między 1 a 2 obserwacją
?dist # Metoda deafultowa - ekulidesowa
dist(dane.segmentacja[1:4,], method="manhattan") # Ciekawostka
dist(dane.segmentacja[1:4,],"maximum")

dist1 <- dist(dane.segmentacja,method="euclidean") # Wyznaczanie calej macierzy odległości

#=== 4.2 Klastrowanie - funckja hclust(macierz odległości, metoda)
hierch1 <- hclust(dist1,method="average") # Klastrowanie metodą average - srednia odległość puktów w klastrach
dend1 <- as.dendrogram(hierch1) # Tworzymy obiekt typu dendogram
plot(dend1) # Nie mamy super ładnego wyniku
abline(col="red",h=5)


hierch2 <- hclust(dist1, method="ward.D") # Inna metoda
dend2 <- as.dendrogram(hierch2)
plot(dend2)
abline(col="red",h=100)

#=== 4.3 Znajdywanie optymalnej liczby klastrów - funkcja NbClust
determine.nclust1 <- NbClust(dane.segmentacja,
                             distance="euclidean", # Sposób liczenia odległości
                             min.nc=3, max.nc=20,  # Rozważamy od 3 do 20 klastrów
                             method="average",     # Łaczenie klastrów
                             index="silhouette")   # Index według którego wybieramy liczbe klastrów
?NbClust()
determine.nclust1$Best.nc[1]
nclusters <- determine.nclust1$Best.nc[1]
nclusters # optymalna liczba klastrów - 3

#=== 4.4 Określanie liczby klastrów
?cutree # Tworzenie rzeczywistych grup na podstawie wyników klastrowania hierarchicznego
cluster.labs1 <- cutree(hierch1,3)
table(cluster.labs1) # Liczba obserwacji przypisanych do poszczególnych klastrów

cluster.labs2 <- cutree(hierch1,6)
table(cluster.labs2) # Nadal słabo, może inna metoda nić "average"


#===============================================================================
# 5. Metoda k-średnich
#===============================================================================
# Klasteryzacja - kmeans(data,           - zbiór danych
#                        centers,        - liczba klastrów
#                        iter.max = ..., - maksymalna liczba iteracji (domyślnie 10)
#                        nstart = ...,   - liczba losowych inicjalizacji centroidów
#)

kmeans5 <- kmeans(dane.segmentacja,5) # Od razu określamy liczbę klastrów
# Uwaga dane już zostały wystandaryzowane
table(kmeans5$cluster) # Wynik się zmienia, po każdym puszczeniu kodu
# Liczebności w klastrach wygladają dużo lepiej niż w poprzedniej metodzie
kmeans5 <- kmeans(dane.segmentacja,5,nstart=25) # Powtórz 25 razy, wybierz najlepszy

#=== Wykres łokciowy
# Szukamy optymalnej liczby klastów
kmeans1 <- kmeans(dane.segmentacja,1,nstart=25)
kmeans2 <- kmeans(dane.segmentacja,2,nstart=25)
kmeans3 <- kmeans(dane.segmentacja,3,nstart=25)
kmeans4 <- kmeans(dane.segmentacja,4,nstart=25)
kmeans5 <- kmeans(dane.segmentacja,5,nstart=25)
kmeans6 <- kmeans(dane.segmentacja,6,nstart=25)
kmeans7 <- kmeans(dane.segmentacja,7,nstart=25)
kmeans8 <- kmeans(dane.segmentacja,8,nstart=25)
kmeans9 <- kmeans(dane.segmentacja,9,nstart=25)
kmeans10 <- kmeans(dane.segmentacja,10,nstart=25)

# tot.withinss - całkowita suma kwadratów odległości wewnątrz klastra
var.within <- c(kmeans1$tot.withinss,kmeans2$tot.withinss,kmeans3$tot.withinss,kmeans4$tot.withinss,
                kmeans5$tot.withinss,kmeans6$tot.withinss,kmeans7$tot.withinss,kmeans8$tot.withinss,
                kmeans9$tot.withinss,kmeans10$tot.withinss)
nclusters <- seq(1:10)
plot(nclusters,var.within,type="l") # Prawdopodobnie widzimy łokieć przy 4
# Można też patrzeć na wariancje between

# Wybieramy 4 klastry
daneRFM$Klaster <- kmeans4$cluster

# Sprawdzamy charakterystyki poszczególnych klastów
daneRFM %>%
  group_by(Klaster) %>%
  summarise(R.ave=mean(last.purchase),R.med=median(last.purchase),R.sd=sd(last.purchase),
            F.ave=mean(Frequency),F.med=median(Frequency),F.sd=sd(Frequency),
            M.ave=mean(Mean.value),M.med=median(Mean.value),M.sd=sd(Mean.value),
            Count=n())

#=== Wykres lokciowy z betwenss - suma kwadratów odległości między klastrami
var.between <- c(kmeans1$betweenss,kmeans2$betweenss, kmeans3$betweenss,
                 kmeans4$betweenss, kmeans5$betweenss, kmeans6$betweenss,
                 kmeans7$betweenss, kmeans8$betweenss, kmeans9$betweenss,
                 kmeans10$betweenss)

plot(nclusters, var.between, type="l") # Łokieć też przy 4 klastrze
points(nclusters, var.between)


#=== Wyznaczanie optymalnej liczby klastrów NbClust()
optimal.clusters <- NbClust(dane.segmentacja, min.nc = 2, max.nc = 10,
                            method="kmeans",index = "all")
optimal.clusters$Best.nc # 4 klastry najbardziej optymalne

optimal.clusters1 <- NbClust(dane.segmentacja, min.nc = 2, max.nc = 10,
                            method="kmeans",index = "silhouette")
optimal.clusters1$Best.nc # 4 klastry najbardziej optymalne
