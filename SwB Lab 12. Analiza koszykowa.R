#===============================================================================
# Analiza koszykowa
#===============================================================================
# 1. Przygotowanie środowiska pracy
# 2. Zbiór danych, przygotowanie do analizy
# 3. Znajdywanie reguł
# 4. Tabela z wynikami analizy
# 5. Tworzenie rekomendacji

#===============================================================================
# 1. Przygotowanie środowiska pracy
if(!requireNamespace("reshape2",quietly=T)) install.packages("reshape2")
if(!requireNamespace("arules",quietly=T)) install.packages("arules")
if(!requireNamespace("arulesViz",quietly=T)) install.packages("arulesViz")
if(!requireNamespace("dplyr",quietly=T)) install.packages("dplyr")
library(reshape2)
library(arules)
library(arulesViz)
library(dplyr)

rm(list=ls())
getwd()
#setwd("C:/Users/user/Desktop/SwB")
my.data = read.table("Data_classes.txt",header=T,sep="\t",dec=",",
                     stringsAsFactor=F,colClasses=c("Client_ID"="character")) ###### 80477 obs. of 11 variables
str(my.data)
# W Clinent_ID zostały uciete zera, wczytamy jako tekst
# Dodano stringsAsFacotr = F - nie konwertuj kolumn klasy tekstowej na factor
# Dodano colClasses=c("a"="b") - wczytujemy kolumne a jako klase b

#===============================================================================
# 2. Zbiór danych, przygotowanie do analizy
#===============================================================================
#   2.1 Czyszczenie danych
#   2.2 Zagregowanie sales value po klientach, po product family
#   2.3 Przekształacamy dataframe na sparse matrix
#   2.4 Zastępujemy wartosci != 0 jedynkami 

#====== 2.1 Czyścimy dane
#=== Pozbywamy się ujemne quantity i sales value
summary(my.data$SalesValue)
summary(my.data$SalesQuantity)
my.data = dplyr::filter(my.data,SalesValue>0) # 77963 obs. of 11 variables
#my.data = my.data %>% filter(SalesValue>0)


#=== Usuwamy recordy bez id klienta
sum(my.data$Client_ID=="") # 1966 recordów bez id klienta
my.data = my.data %>% filter(Client_ID!="" & !is.na(Client_ID) & 
                               Client_ID!="UNKNOWN") # nie dodawać my.data jeszcze raz do argumentów
my.data = dplyr::filter(my.data, Client_ID!="" & !is.na(Client_ID) & 
                          Client_ID!="UNKNOWN") ###### 75997 obs. of 11 variables

#=== usuwamy myślniki z product family, wartosi 999 i 998
sum(my.data$Product_family=="-") # 8
sum(is.na(my.data$Product_family)) # 0
sum(my.data$Product_family==999)
sum(my.data$Product_family==998)
sum(my.data$Product_family=="UNKNOWN")
my.data = dplyr::filter(my.data,Product_family!="-" & !is.na(Product_family) &
                          my.data$Product_family!=999 & my.data$Product_family!=998 &
                          Product_family!="UNKNOWN"
                        ) ###### 75976 obs. of 11 variables
#my.data = my.data %>% filter(Product_family!="-")

#====== 2.2 Zagregowanie sales value po klientach, po product family - ułatwia analizę wzorców zakupowych
my.data = aggregate(my.data$SalesValue, by=list(my.data$Client_ID,
                    my.data$Product_family),sum) ###### 21195 obs. of 3 variables
# Zostaly nam Client_ID, Product_family, salesValue"

# Nazwiemy kolumny
colnames(my.data) = c("Client_ID","Product_family","SalesValue")

#====== 2.3 Przekształacamy dataframe na sparse matrix
# Funkcja dcast() - przekształcanie z formatu długiego na krótki
# dcast(data,
#       formula            - jak mają być przekształcone dane
#       fun.aggregate      - funkcja agregująca wartości w przypadku, 
#                            gdy instieje więcej niż jedna wartośc dla danej kombinacji wierszy i kolumn
#       value.var = ".."   - nazwa kolumny, która zawiera wartości do wypełnienia komórek w nowym formacie
#       margin = F         - Jeśli = T to dadaej margiensy sumujące dla wierszy i kolumn
#)
pivot = dcast(my.data,                   # dane
              Client_ID~Product_family,  # Kazdy wiersz będzie reprezentować Client_ID, a kolumna Product_ID
              sum,                       # Funkcja agregująca
              value.var="SalesValue",    # Które zmienne dają wartość do macierzy
              margin=F)                  # Bez marginesów sumujących

######### 11854 obs. of 98 variables

# Nazywamy wiersze i usuwamy niepotrzebną kolumne Client_ID
rownames(pivot) = pivot[,"Client_ID"]
pivot = dplyr::select(pivot,-Client_ID) ####### 11854 obs. of 97 variables

#====== 2.4 Zastępujemy wartosci != 0 jedynkami 
pivot.binary = ifelse(pivot<20,0,1) ####### Large matrix 1149838 elements
# <20 bo nie interesuja nas male wartosci

#===============================================================================
# 3. Znajdywanie reguł
#===============================================================================
# 3.1 Stworzenie obiektu typu transactions ze sparse matrix
# 3.2 Znalezienie reguł
# 3.3 Inspekcja reguł i interpretacja parametrów
# 3.4 Wizualizacja reguł

#====== 3.1 Stworzenie obiektu typu transactions ze sparse matrix (macierz w formacie długim)
pivot.tr = as(pivot.binary,"transactions") # Potrzebny typ obiektu transactions
itemFrequency(pivot.tr,type="absolute")

#====== 3.2 Znalezienie regół 
# Na początek regułydługości 2
rules1 = apriori(pivot.tr,
                 parameter=list(supp=0.005),conf=0.4, # Potrzebny argument minimum support
                 minlen=2,maxlen=2) # Rozpatrujemy reguły z 2 produktami: jezeli A to B
# 59 traksacji, wierszy macierzy
# 14 regul

#
inspect(rules1)
# w polowie przypadkow ludzie ktorzy zakupili B11 zakupili B02 (14 reguła)
# Confidence max to 89 % - 2 reguła
# Zadanie na kolosa - ocenic jakosc regul
# confidence 100% - produkty i tak zawsze kupowane razem, male pole do rekomendacji

# Wizualizacja reguł
plot(rules1)
# Kropka to regula

plot(rules1,method="graph")


# Inne reguly od 2 do 3 lementow
rules2 = apriori(pivot.tr,
                 parameter=list(supp=0.005),conf=0.4, # Potrzebny argument minimum support
                 minlen=2,maxlen=3)
inspect(rules2)
# Jak się mają parametry do długości reguł

#===============================================================================
# 4. Ramka z wynikami analizy
#===============================================================================
lhs_i <- rules1@lhs@data@i
# rules1 - obiekt klasy rules, zawierający reguły
# rules1@lhs - lewy człon reguł
# rules@lhs@data - obiekt klasy sparse matrix, przechowujący informacje o tym jakie elementy w lhs każdej reguły
# @i indeks wierszy (elementów) w sparse matrix. Każdy index odpowiada pozycji elementu w zbiorze ItemInfo
# lhs_i - wektor indeksów elementów będących w lhs każdej reguły

lhs <- rules1@lhs@itemInfo$labels
# rules@lhs@itemInfo - ramka danych zawierająca info o lhs każdej reguły
# rules@lhs@itemInfo@labels - nazwy etykiet każdego produktu z lhs każdej reguły
# lhs - wektor nazw wszystkich elementów w zbiorze danych które mogą się pojawić w lhs

rhs_i <- rules1@rhs@data@i
rhs <- rules1@rhs@itemInfo$labels

rhs_i[1] # 0
# UWAGA
# Macierz sparse (np. rules1@lhs@data, rules1@rhs@data) jest indeksowany od 0, ponieważ
#   jest to typowe dla macierzy sparse
# Wektor rhs zawierający etykiety produkktów jest indeksowany w sposób typowy dla R;
#   czyli od 1
# rhs_[1] = 0 - indeks pierwszego elementu w macirzy sparse
# rhs[1] - etykieta dla pierwszego elementu

#rhs_i[1]=0 - pierwszy element ma indeks zero
#(ale nazwa tego 0 jest zapisana jako pierwszy element w wektorze rhs)
# stąd do wartości rhs_i trzeba dodać 1, podobnie do lhs_i
rhs_i <- rhs_i+1
lhs_i <- lhs_i+1

LHS <- lhs[lhs_i] # Wykorzystujemy etykiety produktów i ich indeksy do stworzenia wektorów
RHS <- rhs[rhs_i] # reprezentujących reguły

# Dokładamy informacje o parametrach:
reg.tech <- as.data.frame(rules1@quality)

# Łączymy LHS, RHS oraz info o parametrach
reg1 = data.frame(LHS,RHS,reg.tech)

#===============================================================================
# 5. Tworzenie rekomendacji
#===============================================================================
# Rekomendacje tworzymy dla klientów którym pasuje LHS, ale nie pasuje RHS danej reguły

# Zacznijmy od manulanego tworzenia rekomendacji na podstawie 1 reguły
print(reg1[1,])
# Wracamy do pivot.binary - sparse matrix zawierającej informacje o tym jaki klient kupił jaki produkt
a = pivot.binary[,"E01"] # LHS
b = pivot.binary[,"A01"] # RHS
c = data.frame(a,b)
c = c %>% filter(a==1) %>% filter(b==0)
# c = c[which(a==1 & b!=1),]

rekomendacja_A01 = data.frame(Client_ID = row.names(c),
                              prod.bought = "E01",
                              prod.recom = "A01",
                              Explanation = paste("Because this customer bought E01 and",
                                                   paste(100*round(reg1[1,4],2),'%',sep=''),
                                                   "of customers who bought E01 bought as well A01"
                              ))
print(rekomendacja_A01[1,])


# Automatyzacja tworzenia rekomendacji
# Będziemy iterować po każdym wierszu z regułami, sprawdzając dla każdej reguły, czy zbudujemy rekomendacje
# Wyniki będziemy dodawac do nowej tabeli, którą musimy zainicjować
output = data.frame(Client_ID=character(),prod.bought=character(),
                    Recommendation=character(),
                    Probability=numeric(), Explanation=character())

for (i in 1:nrow(reg1)) {
  a = pivot.binary[,as.character(reg1[i,1])] # LHS
  b = pivot.binary[,as.character(reg1[i,2])] # RHS
  c = cbind(a,b)
  c = as.data.frame(c)
  # Wybieramy klientów, którzy kupili LHS, a nie kupili RHS
  c = c %>% filter(a==1,b==0)
  
  prod.bought = as.character(reg1[i,1]) # Wyciągamy nazwę zakupionego produktu
  prod.recomm = as.character(reg1[i,2]) # Wyciągamy nazwę rekomendowanego produktu
  
  new_df = data.frame(CustomerID = rownames(c),prod.bought=prod.bought,
                      Recommendation = prod.recomm, Probability = reg1[i,4],
                      Explanation = paste("Because this customer bought",reg1[i,1], "and",
                                          paste(round(100*reg1[i,4],2),'%',sep=''),
                                          "of customers who bought", reg1[i,1],
                                          "bought as well",reg1[i,2]))
  output = rbind(output,new_df)
}

# Z 14 reguł utworzono ponad 2000 rekomendacji
# Wyniki można wyeksportować do Excela, ładnie sformatować i wysłać sprzdawcom,
## lub współpracować z działem IT i zaimplementować je na stronie, w aplikacji