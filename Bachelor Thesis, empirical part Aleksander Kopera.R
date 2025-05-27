################################################################################
##            IRD - Projekt - Akceptacja wniosków kredytowych                 ##
################################################################################

# Autor: Aleksander Kopera

#===============================================================================
# Spis treści
# 1. Pyrzygotowanie środowiska pracy
# 2. Wczytanie zbioru danych
# 3. Eksploracyjna analiza danych
# 4. Budowa modeli

#===============================================================================
# 1. Przygotowanie środowiska pracy
rm(list=ls())
# dev.off()

getwd()
# setwd("C:/Users/user/Desktop/Indukowane Reguły Decyzyjne - projekt")

if (!requireNamespace("ggplot2",quietly=T)) install.packages("ggplot2")
if (!requireNamespace("dplyr",quietly=T)) install.packages("dplyr")
if (!requireNamespace("scorecard",quietly=T)) install.packages("scorecard")
if (!requireNamespace("corrplot", quietly = T)) install.packages("corrplot")
if (!requireNamespace("rpart",quietly=T)) install.packages("rpart")
if (!requireNamespace("rpart.plot",quietly=T)) install.packages("rpart.plot")
if (!requireNamespace("caret",quietly=T)) install.packages("caret")
if (!requireNamespace("pROC",quietly=T)) install.packages("pROC")
if (!requireNamespace("randomForest", quietly = T)) install.packages("randomForest")
if (!requireNamespace("cvAUC", quietly = T)) install.packages("cvAUC")
if (!requireNamespace("xgboost", quietly = T)) install.packages("xgboost")
library(readxl)         # Wczytywanie pliku Excel
library(ggplot2)        # Wizualizacja danych
library(dplyr)          # Manipulacja danych
library(scorecard)      # Obliczanie wskaźnika IV
library(corrplot)       # Wizualizacja macierzy korelacji
library(rpart)          # Trenowanie drzewa decyzyjnego
library(rpart.plot)     # Wizualizacja drzwa
library(caret)          # Trenowanie modeli klasyfikacyjnych
library(pROC)           # Wyznaczanie krzywej ROC
library(randomForest)   # Trenowanie lasu losowego
library(cvAUC)          # Wizualizacja średniej krzywej ROC z CV
library(xgboost)        # Extreme Gradient Boosting

#===============================================================================
# 2. Wczytanie zbioru danych
URL = "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
credit_data = read.table(URL, header=F ,sep="", stringsAsFactors=F)
# credit_data = read_excel("credit_data_original.xlsx"); credit_data %>% select(-'...1')
# credit_data = read.table("C:/Users/user/Desktop/Praca Licencjacka/german.data", 
#                          header = FALSE, sep = " ", 
#                          col.names = column_names, stringsAsFactors = FALSE,
#                          strip.white = TRUE)
View(credit_data)


# Nadanie nazw zmiennym zgodnie z dokumentacją
colnames(credit_data) = c("Status", "Duration", "CreditHistory", "Purpose", "CreditAmount",
                          "Savings", "Employment", "InstallmentRate", "PersonalStatusSex",
                          "OtherDebtors", "ResidenceSince", "Property", "Age", "OtherInstallment",
                          "Housing", "ExistingCredits", "Job", "NumPeopleLiable", "Telephone", 
                          "ForeignWorker", "CreditRisk")

# column_names = c("Status", "Duration", "CreditHistory", "Purpose", "CreditAmount",
#                  "Savings", "Employment", "InstallmentRate", "PersonalStatusSex",
#                  "OtherDebtors", "ResidenceSince", "Property", "Age", "OtherInstallment",
#                  "Housing", "ExistingCredits", "Job", "NumPeopleLiable", "Telephone", 
#                  "ForeignWorker", "CreditRisk")

# Konwersja zmiennych kategorycznych na typ factor
factor_vars = c("Status", "CreditHistory", "Purpose", "Savings", "Employment",
                "PersonalStatusSex", "OtherDebtors", "Property", "OtherInstallment", 
                "Housing", "Job", "Telephone", "ForeignWorker", "CreditRisk")

credit_data[factor_vars] = lapply(credit_data[factor_vars], as.factor)

credit_data$CreditRisk = ifelse(credit_data$CreditRisk == 2, 0, 1)
credit_data = credit_data %>%
  mutate(CreditRisk = factor(CreditRisk,
                             levels=c(0,1),
                             labels=c("Niewiarygodny", "Wiarygodny")))

credit_data_original = credit_data

# Zmienne objaśniające:
#   1. Status               - Status rachunku klienta
#                           - kategoryczna: A11 - brak rachunku, 
#                                           A12 - saldo < 200 DM, 
#                                           A13 - saldo >= 200, 
#                                           A14 - nieznany
#
#   2. Duration             - Czas trwania kredytu (miesiące), ciągła 
#
#   3. CreditHistory        - Historia kredytowa klienta
#                           - Kategoryczna: A30 - brak kredytów / wszystkie spłacone terminowo,
#                                           A31 - wszystkie kredyty w tym banku spłacone terminowo,
#                                           A32 - obecne kredyty spłacane terminowo,
#                                           A33 - opóźnienia w spłatach w przeszłości,
#                                           A34 - konto krytyczne / inne kredyty poza bankiem
# 
#   4. Purpose              - Cel kredytu
#                           - kategoryczna: A40 - samochód (nowy), 
#                                           A41 – samochód (używany),
#                                           A42 – meble/wyposażenie, 
#                                           A43 – radio/telewizor,
#                                           A44 – sprzęt AGD, 
#                                           A45 – naprawy,
#                                           A46 – edukacja, 
#                                           A47 – wakacje (brak danych),
#                                           A48 – przekwalifikowanie, 
#                                           A49 – działalność gospodarcza,
#                                           A410 – inne
#
#   5. CreditAmount         - kwota kredytu w DM, ciągła
#
#   6. Savings              - poziom oszczędności klienta
#                           - kategoryczna: A61 - brak oszczędności (< 100 DM)
#                                           A62 = "oszczędności 100–499 DM
#                                           A63 = "oszczędności 500–999 DM
#                                           A64 = oszczędności ≥ 1000 DM
#                                           A65 = "brak inf. / brak konta oszczędnościowego
#
#   7. Employment           - staż zatrudnienia klienta
#                           - kategoryczna: A71 - bezrobotny
#                                           A72 - < 1 rok
#                                           A73 - 1-4 lata
#                                           A74 - 4-7 lat
#                                           A75 - >= 7 lat
#
#   8. InstallmentRate      - wysokość raty jako procent dochodu klienta, ciągła
#
#   9. PersonalStatusSex    - status cywilny i płeć klienta
#                           - kategoryczna: A91 - mężczyzna, rozwiedziony,
#                                           A92 - kobieta, rozwiedziona/zamężna,
#                                           A93 - mężczyzna, kawaler
#                                           A94 - mężczyzna, żonaty/wdowiec
#                                           A95 - kobieta, panna
#
#   10. OtherDebtors        - czy klient ma innych dłużników/poręczycieli
#                           - kategoryczna: A101 - brak
#                                           A102 - współwnioskodawca
#                                           A103 - poręczyciel
#
#   11. ResidenceSince      - liczba lat zamieszkania w obecnym miejscu, ciągła
#
#   12. Property            - Rodzaj własności klienta
#                           - kategoryczna: A121 - nieruchomość
#                                           A122 - umowa oszczędnościowa / ubez. na życie
#                                           A123 - samochód lub inne
#                                           A124 - brak informacji / brak własności
#
#   13. Age                 - wiek klienta, ciągła
#
#   14. OtherInstallment    - Inne zobowiązania ratalne klienta
#                           - Kategoryczna: A141 – bank, 
#                                           A142 – sklep, 
#                                           A143 – brak
#
#   15. Housing             - Rodzaj zamieszkania klienta
#                           - kategoryczna: A151 – najem ,
#                                           A152 – własne mieszkanie,
#                                           A153 – bez opłat
#
#   16. ExistingCredits     - liczba aktualnych kredytów w banku, ciągła
#
#   17. Job                 - kategoria zatrudnienia klienta
#                           - kategoryczna: A171 - bezrobotny / niewykwalifikowany (nierezydent)
#                                           A172 - niewykwalifikowany (rezydent)
#                                           A173 - wykwalifikowany / urzędnik
#                                           A174 - kadra zarz./ samozatrudniony / wysoko wykwalifikowany
#
#   18. NumPeopleLiable     - liczba osób na utrzymaniu klienta, ciągła 
#
#   19. Telephone           - czy klient posiada telefon
#                           - kategoryczna: A191 - brak, 
#                                           A192 - zarejestrowany na nazwisko klienta
#
#   20. ForeignWorker       - czy klient jest cudzoziemcem
#                           - kategoryczna: A201 - tak, 
#                                           A202 - nie
# 
# Zmienna objaśniana:
#   CreditRisk              - klasyfikacja klienta jako dobrego lub złego
#                           - kategoryczna: 1 - wiarygondy klient, 
#                                           2 / 0 - niewiarygodny klient

#===============================================================================
# 3. Eksploracyjna analiza danych (EDA)

#   3.1 Rozkład Zmiennej objasnianej
liczba_poztywnych_ocen = sum(credit_data$CreditRisk=="Wiarygodny")   # 700
liczba_negatywnych_ocen = sum(credit_data$CreditRisk=="Niewiarygodny")  # 300

data_pie = credit_data %>%
  group_by(CreditRisk) %>%
  summarise(n=n()) %>%
  mutate(Percent_share = round(n / sum(n) * 100, 1),
         Label=paste0(Percent_share, "%"))

ggplot(data = data_pie, aes(x="", y = Percent_share, fill=CreditRisk)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  coord_polar(theta = 'y') +
  theme_void() +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5) +
  theme(legend.position = 'right', 
        plot.title = element_text(hjust = 0.85,face='bold',size=14)) +
  labs(title = "Rozkład zmiennej objaśnianej", fill = "Ryzyko kredytowe")
rm(data_pie)

# Rozkład nie jest równomierny: 1 - 30%, 2 - 70% możeb być konieczne nadanie wag obserwacjom,
# lub wygenerowanie dodatkowych (symulowanych) obserwacji z klasą 2

#   3.2 Wskaźnik IV dla zmiennnych objaśnianych
iv_table = iv(credit_data, y = "CreditRisk", positive='Wiarygodny')
iv_table$info_value = round(iv_table$info_value, 4)
iv_table

# 1:            Status      0.6660
# 2:          Duration      0.3345
# 3:     CreditHistory      0.2932
# 4:               Age      0.2597 / 0.2612
# 5:           Savings      0.1960
# 6:           Purpose      0.1692
# 7:          Property      0.1126
# 8:        Employment      0.0864
# 9:           Housing      0.0833 / 0.0854
# 10:  OtherInstallment     0.0576
# 11: PersonalStatusSex     0.0447
# 12:     ForeignWorker     0.0439 
# 13:      CreditAmount     0.0390 / 0.0376
# 14:      OtherDebtors     0.0320
# 15:   InstallmentRate     0.0263 
# 16:   ExistingCredits     0.0133  
# 17:               Job     0.0088 
# 18:         Telephone     0.0064 
# 19:    ResidenceSince     0.0036 
# 20:   NumPeopleLiable     0.0000 

#   3.3 Rozkłady zmiennych

#     3.3.1 Zmienne ilościowe
numeric_vars = credit_data %>% select(where(is.numeric))
# numeric_vars = numeric_vars %>% select(-CreditRisk)

# Duration
hist(credit_data$Duration, col='steelblue', freq=T, breaks=8,
     main='2.1 Czas trwania kredytów', xlab="Czas trwania kredytu (miesiące)", ylab="Częstość")
summary(credit_data$Duration)
# Min.   1st Qu. Median  Mean    3rd Qu. Max. 
# 4.0    12.0    18.0    20.9    24.0    72.0 

# CreditAmount
hist(credit_data$CreditAmount, col='steelblue', alpha=0.7, freq=T, breaks=15,
     main='2.2 Kwoty kredytów', xlab="Wysokość kredytu (marki niemieckie)", ylab="Częstość")
summary(credit_data$CreditAmount)
# Min.   1st Qu. Median  Mean    3rd Qu.Max. 
# 250    1366    2320    3271    3972   18424 

# InstallmentRate
# Porządkowanie danych
data_IR = credit_data %>% 
  count(InstallmentRate) %>% 
  mutate(InstallmentRate = factor(InstallmentRate, levels = c(1, 2, 3, 4)))

ggplot(data=data_IR ,aes(x=InstallmentRate, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), vjust=-0.5, size=4) +
  labs(title="2.3 Wysoskość raty jako procent dochodu klienta",x="", y="Liczba wystąpień") +
  scale_x_discrete(labels=c("1" = "1%",
                            "2" = "2%",
                            "3" = "3%",
                            "4" = "4%")) +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14, face='bold', hjust=0.5))
rm(data_IR)
summary(credit_data$InstallmentRate)
# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 1.000   2.000   3.000   2.973   4.000   4.000 

# ResidenceSince
data_RS = credit_data %>%
  count(ResidenceSince) %>%
  mutate(ResidenceSince = factor(ResidenceSince, levels = c(1, 2, 3, 4)))

ggplot(data=data_RS, aes(x=ResidenceSince, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), vjust=-0.5, size=4) +
  labs(title="2.4 Liczba lat zamieszkania w obecnym miejscu",x="", y="Liczba wystąpień") +
  scale_x_discrete(labels=c("1" = "1 rok",
                            "2" = "2 lata",
                            "3" = "3 lata",
                            "4" = "4 lata")) +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.5))
rm(data_RS)
summary(credit_data$ResidenceSince)
# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 1.000   2.000   3.000   2.845   4.000   4.000 
# table(credit_data$ResidenceSince)
# 1   2   3   4 
# 130 308 149 413 

# Age
hist(credit_data$Age, col='steelblue', freq=T,
     main='2.5 Rozkład wieku klientów', xlab="Wiek", ylab="Częstość")
summary(credit_data$Age)
# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 19.00   27.00   33.00   35.55   42.00   75.00 


# ExistingCredits
data_EC = credit_data %>%
  count(ExistingCredits)

ggplot(data=data_EC,aes(x=ExistingCredits, y =n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), vjust=-0.5, size=4) +
  labs(title="2.6 Liczba aktualnych kredytów w banku",
       x="", y="Liczba wystąpień") +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.5))
rm(data_EC)
summary(credit_data$ExistingCredits)
# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 1.000   1.000   1.000   1.407   2.000   4.000


# NumPeopleLiable
data_NPL = credit_data %>%
  count(NumPeopleLiable) %>%
  mutate(NumPeopleLiable = reorder(NumPeopleLiable, -n))

ggplot(data=data_NPL, aes(x=NumPeopleLiable, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), vjust=-0.5, size=4) +
  labs(title="2.7 Liczba osób na utrzymaniu u klientów",x="", y="Liczba wystąpień") +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.45))
rm(data_NPL)
summary(credit_data$NumPeopleLiable)
# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 1.000   1.000   1.000   1.155   1.000   2.000

#     3.3.2 Zmienne jakościowe
qual_vars = credit_data %>% select(!where(is.numeric))

# Status
data_S = credit_data %>%
  count(Status) %>%
  mutate(Status = reorder(Status, n))

ggplot(data=data_S, aes(x=Status, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), vjust=-0.5, size=4) +
  labs(title="3.1 Status rachunku bankowego",
       x="", y="Liczba wystąpnień") +
  scale_x_discrete(labels=c("A11" = "Brak rachunku",
                            "A12" = "Saldo < 200",
                            "A13" = "Saldo ≥ 200",
                            "A14" = "Nieznany")) +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.5))
rm(data_S)
summary(credit_data$Status)
# A11 A12 A13 A14 
# 274 269  63 394

# CreditHistory
data_CH = credit_data %>%
  count(CreditHistory) %>%
  mutate(CreditHistory = reorder(CreditHistory,n))

ggplot(data=data_CH, aes(x=CreditHistory, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), hjust=-0.15, size=4) +
  labs(title='3.2 Historia kredytowa',
       x="", y='Liczba wystąpnień') +
  scale_x_discrete(labels=c("A30" = "brak kredytów / spłacone terminowo", 
                            "A31" = "kredyty w banku spłacone terminowo",
                            "A32" = "akt. kredyty spłacane na czas",
                            "A33" = "opóźnienia w spłatach w przeszłości",
                            "A34" = "inne kredyty poza bankiem")) +
  coord_flip() +        # Poziomy układ
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.25))
rm(data_CH)
summary(credit_data$CreditHistory)
# A30 A31 A32 A33 A34 
# 40  49  530 88  293

# Purpose
data_P = credit_data %>%
  count(Purpose) %>%
  mutate(Purpose = reorder(Purpose,n))

ggplot(data=data_P, aes(x=Purpose, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), hjust=-0.15, size=4) + 
  labs(title='3.3 Zadeklarowany cel kredytu',
       x='', y='Liczba wystąpień') + 
  scale_x_discrete(labels=c("A40"  = "samochód (nowy)",
                            "A41"  = "samochód (używany)",
                            "A42"  = "meble / wyposażenie",
                            "A43"  = "radio / telewizor",
                            "A44"  = "sprzęt AGD",
                            "A45"  = "naprawy",
                            "A46"  = "edukacja",
                            "A47"  = "wakacje (brak danych)",
                            "A48"  = "przekwalifikowanie",
                            "A49"  = "działalność gospodarcza",
                            "A410" = "inne")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.32))
rm(data_P)
summary(credit_data$Purpose)
# A40  A41 A410  A42  A43  A44  A45  A46  A48  A49 
# 234  103 12    181  280  12   22   50   9    97 

# Savings
data_Sv = credit_data %>%
  count(Savings) %>%
  mutate(Savings = reorder(Savings,n))

ggplot(data=data_Sv, aes(x=Savings, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), hjust=-0.15, size=4) +
  labs(title='3.4 Poziom oszczędności klientów',
       x='', y='Liczba wystąpień') +
  scale_x_discrete(labels=c("A61" = "brak oszczędności (< 100 DM)",
                            "A62" = "oszczędności 100–499 DM",
                            "A63" = "oszczędności 500–999 DM",
                            "A64" = "oszczędności ≥ 1000 DM",
                            "A65" = "brak inf. / brak konta oszczędnościowego")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.15))
rm(data_Sv)
summary(credit_data$Savings)
# A61 A62 A63 A64 A65 
# 603 103  63  48 183


# Employment
data_E = credit_data %>%
  count(Employment) %>%
  mutate(Employment = reorder(Employment,n))

ggplot(data=data_E, aes(x=Employment, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), hjust=-0.15, size=4) +
  labs(title='3.5 Staż zatrudnienia',
       x='', y='Liczba wystąpień') +
  scale_x_discrete(labels=c("A71" = "bezrobotny",
                            "A72" = "zatrudniony < 1 rok",
                            "A73" = "zatrudniony 1–4 lata",
                            "A74" = "zatrudniony 4–7 lat",
                            "A75" = "zatrudniony ≥ 7 lat")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.38))
rm(data_E)
summary(credit_data$Employment)
# A71 A72 A73 A74 A75 
#  62 172 339 174 253


# PersonalStatusSex
data_PS = credit_data %>%
  count(PersonalStatusSex) %>%
  mutate(PersonalStatusSex = reorder(PersonalStatusSex,n))

ggplot(data=data_PS, aes(x=PersonalStatusSex, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), hjust=-0.15, size=4) +
  labs(title='3.6 Status cywilny, płeć',
       x='', y='Liczba wystąpień') +
  scale_x_discrete(labels=c("A91" = "mężczyzna, rozwiedziony",
                            "A92" = "kobieta, rozwiedziona/zamężna",
                            "A93" = "mężczyzna, kawaler",
                            "A94" = "mężczyzna, żonaty/wdowiec",
                            "A95" = "kobieta, panna")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.33))
rm(data_PS)
summary(credit_data$PersonalStatusSex)
# A91 A92 A93 A94 
# 50  310 548  92


# OtherDebtors
data_OD = credit_data %>%
  count(OtherDebtors) %>%
  mutate(OtherDebtors = reorder(OtherDebtors,n))

ggplot(data=data_OD, aes(x=OtherDebtors, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), hjust=-0.15, size=4) +
  labs(title='3.7 Występowane innych dłużników / poręczycieli', 
       x='', y='Liczba wystąpień') +
  scale_x_discrete(labels=c("A101" = "brak",
                            "A102" = "współwnioskodawca",
                            "A103" = "poręczyciel")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.3))
rm(data_OD)
summary(credit_data$OtherDebtors)
# A101 A102 A103 
# 907   41   52


# Property
data_Pr = credit_data %>%
  count(Property) %>%
  mutate(Property = reorder(Property,n))

ggplot(data=data_Pr, aes(x=Property, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), hjust=-0.05, size=4) +
  labs(title='3.8 Kategorie własności', x='', y='Liczba wystąpień') +
  scale_x_discrete(labels=c("A121" = "nieruchomość",
                            "A122" = "umowa oszczędnościowa / ubez. na życie",
                            "A123" = "samochód lub inne",
                            "A124" = "brak informacji / brak własności")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.30))
rm(data_Pr)
summary(credit_data$Property)
# A121 A122 A123 A124 
# 282  232  332  154 


# OtherInstallment
data_OI = credit_data %>%
  count(OtherInstallment) %>%
  mutate(OtherInstallment = reorder(OtherInstallment,n))

ggplot(data=data_OI, aes(x=OtherInstallment, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), hjust=-0.15, size=4) +
  labs(title='3.9 Inne zobowiązania ratalne', x='', y='Liczba wystąpień') +
  scale_x_discrete(labels=c("A141" = "bank",
                            "A142" = "sklep",
                            "A143" = "brak")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.45))
rm(data_OI)
summary(credit_data$OtherInstallment)
# A141 A142 A143 
# 139   47  814 


# Housing
data_H = credit_data %>%
  count(Housing) %>%
  mutate(Housing = reorder(Housing,n))

ggplot(data=data_H, aes(x=Housing, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), hjust=-0.15, size=4) +
  labs(title='3.10 Kategorie zamieszkania', x='', y='Liczba wystąpień') +
  scale_x_discrete(labels=c("A151" = "najem",
                            "A152" = "własność",
                            "A153" = "bez opłat")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.45))
rm(data_H)
summary(credit_data$Housing)
# A151 A152 A153 
# 179  713  108


# Job
data_J = credit_data %>%
  count(Job) %>%
  mutate(Job = reorder(Job,n))

ggplot(data=data_J, aes(x=Job, y=n)) +
  geom_bar(stat='identity', fill='steelblue', alpha=1, color='black') +
  geom_text(aes(y=n, label=n), hjust=-0.15, size=4) +
  labs(title='3.11 Kategoria zatrudnienia', x='', y='Liczba wystąpień') +
  scale_x_discrete(labels=c("A171" = "bezrobotny / niewykw. (nierezydent)",
                            "A172" = "niewykwalifikowany (rezydent)",
                            "A173" = "wykwalifikowany / urzędnik",
                            "A174" = "kadra zarz./ samozatr. / wysoko wykw.")) +
  coord_flip() +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(size=14,face='bold',hjust=0.27))
rm(data_J)
summary(credit_data$Job)
# A171 A172 A173 A174 
# 22   200   630   148


# Telephone
data_T = credit_data %>%
  count(Telephone) %>%
  mutate(Telephone = dplyr::recode(Telephone, 
                            "A191" = "Brak telefonu",
                            "A192" = "Telefon na nazwisko klienta"),
         Label = paste0(n))

ggplot(data_T, aes(x='', y=n, fill=Telephone)) +
  geom_bar(stat='identity', width=1, color='white') +
  scale_fill_manual(values = c("Brak telefonu" = "#F8766D", 
                               "Telefon na nazwisko klienta" = "steelblue")) +
  coord_polar(theta='y') +
  geom_text(aes(label=Label), position=position_stack(vjust=0.5), size=5) +
  theme_void() +
  labs(title='3.12 Dostęp do telefonu', fill="Dostępność") +
  theme(legend.title=element_text(face='bold'),
        plot.title=element_text(size=14,face='bold',hjust=0.7))
rm(data_T)
summary(credit_data$Telephone)
# A191 A192 
# 596  404 


# ForeignWorker
data_FW = credit_data %>%
  count(ForeignWorker) %>%
  mutate(ForeignWorker = dplyr::recode(ForeignWorker,
                                "A201" = 'Tak',
                                "A202" = 'Nie'),
         Label = paste0(n))

ggplot(data_FW, aes(x='', y=n, fill=ForeignWorker)) +
  geom_bar(stat='identity', width=1, color='white') +
  scale_fill_manual(values = c("Nie" = "#F8766D", 
                               "Tak" = "steelblue")) +
  coord_polar(theta='y') +
  geom_text(aes(label=Label), position=position_stack(vjust=0.5), size=5) +
  theme_void() +
  labs(title='3.13 Pochodzenie klientów', fill="Obcokrajowiec") +
  theme(plot.title=element_text(size=14,hjust=0.7, face='bold'),
        legend.title=element_text(face='bold'))
rm(data_FW)
summary(credit_data$ForeignWorker)
# A201 A202 
# 963   37


#   3.4 Analiza rozkładu zmiennej objasnianej w zależności od cech

#     3.4.1 Zmienne ilościowe

# Duration
ggplot(data=credit_data, aes(x=CreditRisk, y=Duration, fill=CreditRisk)) +
  geom_boxplot(alpha=1, outlier.shape=7) +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  labs(title="", x='Ryzyko kredytowe',y='Czas trwania kredytu', fill="Ryzyko kredytowe") +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face='bold'),
        panel.grid=element_blank())
kruskal.test(Duration~CreditRisk, data=credit_data)
# Kruskal-Wallis chi-squared = 42.264, df = 1, p-value = 7.975e-11
# Rozkłady długości kredytu są różne, mediana czasu trwania kredytów jest większa
# w przypadku niewiarygodnych klientów


# CreditAmount
ggplot(data=credit_data, aes(x=CreditRisk, y=CreditAmount, fill=CreditRisk)) +
  geom_boxplot(alpha=1, outlier.shape=7) +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  labs(title='', x="Ryzyko kredytowe", y='Kwota kredytu (marki niemeckie)', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face='bold'),
        panel.grid=element_blank())
kruskal.test(CreditAmount~CreditRisk, data=credit_data)
# Kruskal-Wallis chi-squared = 7.5759, df = 1, p-value = 0.005915
# Rozkłady kwoty kredytu w zależności od ryzyka kredytowego są różne,
# mediana kwoty w przypadku niewarygondych klientów jest większa


# InstallmentRate
ggplot(credit_data, aes(x=as.factor(InstallmentRate), fill=as.factor(CreditRisk))) +
  geom_bar(position = "dodge") +
  labs(x = "Rata kredytu jako % dochodu", y="Liczba obserwacji", fill="Ryzyko kredytowe") +
  theme_minimal() +
  theme(axis.title=element_text(size=12, face='bold')# ,
        #panel.grid=element_blank()
        )

ggplot(data=credit_data, aes(x=CreditRisk, y=InstallmentRate, fill=CreditRisk)) +
  geom_boxplot(alpha=1, outlier.shape=7) +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  labs(title='', x="Ryzyko kredytowe", y='Wysokośc raty jako procent dochodu', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face='bold'),
        panel.grid=element_blank())

ggplot(credit_data, aes(x=InstallmentRate, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  labs(title='', x='Wysokość raty jako procent dochodu', y='Udział procentowy', fill='Ryzyko kredytowe')  +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank())

kruskal.test(InstallmentRate~CreditRisk, data=credit_data)
# Kruskal-Wallis chi-squared = 5.4248, df = 1, p-value = 0.01985
# Wyoskość raty jako precent dochodu róznicuje udziały klas ryzyka
# Test może dawać nieprawidłowy wynik bo zmienna ma mało unikalnych wartości

table_I = table(credit_data$InstallmentRate, credit_data$CreditRisk)
chisq.test(table_I)
# X-squared = 5.4768, df = 3, p-value = 0.14
# Wysokość raty jako % dochodu nie różnicuje udziału klas ryzyka
rm(table_I)


# ResidenceSince
ggplot(credit_data, aes(x=as.factor(ResidenceSince), fill=as.factor(CreditRisk))) +
  geom_bar(position='dodge') +
  labs(x="Liczba lat zamieszkania w obecnym miejscu", y="Liczba obserwacji", fill="Ryzyko kredytowe") +
  theme_minimal() +
  theme(axis.title=element_text(size=12, face='bold') #,
        #panel.grid=element_blank()
        )
kruskal.test(ResidenceSince~CreditRisk, data=credit_data)
# Kruskal-Wallis chi-squared = 0.0065122, df = 1, p-value = 0.9357
# Rozkłady są bardzo podobne, liczba lat zamieszkania w obecnym miejscu nie różnicuje
# ryzyka kredytowego

table_RS = table(credit_data$ResidenceSince, credit_data$CreditRisk)
chisq.test(table_RS)
# X-squared = 0.7493, df = 3, p-value = 0.8616
# Brak związku między liczbą lat zamieszkania a ryzykiem kredytowym
rm(table_RS)

# Age
ggplot(data=credit_data, aes(x=Age, fill=CreditRisk)) +
  geom_histogram(alpha=0.7, position='dodge', binwidth=5) +
  labs(title='', x='Wiek', y='Liczba wystąpień') +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold') #, 
        # panel.grid=element_blank()
        )

ggplot(data=credit_data, aes(x=CreditRisk, y=Age, fill=CreditRisk)) +
  geom_boxplot(alpha=1, outlier.shape=7) +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  labs(title='', x="Ryzyko kredytowe", y='Kwota kredytu (marki niemeckie)', fill='Ryzyko kredytowe') +
  labs(title='Rozkład wieku w zależności od ryzyka', x='Ryzyko kredytowe', y='Wiek', fill="Ryzyko kredytowe") +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5,face='bold',size=14))
kruskal.test(Age~CreditRisk, data=credit_data)
# Kruskal-Wallis chi-squared = 12.574, df = 1, p-value = 0.0003911
# Rozkłady są różne, ocena ryzyka kredytowego  rożnicuje rozkładu wieku


# ExistingCredits
ggplot(credit_data, aes(x=as.factor(ExistingCredits), fill=as.factor(CreditRisk))) +
  geom_bar(position='dodge') +
  labs(title='', x="Liczba akutalnych kredytów", y='Liczba wystąpnień', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold'))

ggplot(data=credit_data, aes(x=CreditRisk, y=ExistingCredits, fill=CreditRisk)) +
  geom_boxplot(alpha=0.7, outlier.shape=7) +
  labs(title='', x='Ryzyko kredytowe', y='Liczba aktualnych kredytow', fill="Ryzyko Kredytowe") +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold') #,
        #panel.grid=element_blank()
  )

kruskal.test(data=credit_data, ExistingCredits~CreditRisk)
# Kruskal-Wallis chi-squared = 2.2365, df = 1, p-value = 0.1348
# Rozkłady są zbliżone, ocena ryzyka nie różnicuje rozkładu liczby aktualnych kredtów

table_EC = table(credit_data$ExistingCredits, credit_data$CreditRisk)
fisher.test(table_EC)
# p-value = 0.4358
# Liczba aktualnie spłacanych kredytów nie różnicuje udziału klas ryzyka
rm(table_EC)

# NumPeopleLiable
ggplot(credit_data, aes(x=as.factor(NumPeopleLiable), fill=as.factor(CreditRisk))) +
  geom_bar(position='dodge') +
  labs(title='', x='Liczba osób na utrzymaniu', y='Liczba wystąpień', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold'))
kruskal.test(data=credit_data, NumPeopleLiable~CreditRisk)
# Kruskal-Wallis chi-squared = 0.0090802, df = 1, p-value = 0.9241
# Ryzyko kredytowe nie różnicuje rozkładu liczby osób na utrzymaniu

table_NPL = table(credit_data$NumPeopleLiable, credit_data$CreditRisk)
chisq.test(table_NPL)
# X-squared = 0, df = 1, p-value = 1
# Liczba osób na utrzymaniu nie różnicuje rozkładu udziału klas ryzyka

#     3.4.2 Zmienne jakościowe

#       3.4.2.1 Status
table(credit_data$Status)
table_S = table(credit_data$Status, credit_data$CreditRisk)
prop.table(table_S, margin=1)
#     Niewiarygodny Wiarygodny
# A11     0.4927007  0.5072993
# A12     0.3903346  0.6096654
# A13     0.2222222  0.7777778
# A14     0.1167513  0.8832487
mosaicplot(table_S, color=T, main='', xlab='Status rachunku', ylab='Ryzyko kredytowe')

ggplot(credit_data, aes(x=Status, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A11" = "Brak rachunku",
                            "A12" = "Saldo < 200",
                            "A13" = "Saldo ≥ 200",
                            "A14" = "Nieznany")) +
  labs(title='Rozkład klas ryzyka w zależności od statsu rachunku bankowego',
       x='Status rachunku', y='Udział procentowy', fill='Ryzyko kredytowe')  +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title=element_text(hjust=0.5,face='bold',size=14))
chisq.test(table_S)
# X-squared = 123.72, df = 3, p-value < 2.2e-16
# Rozkłady znacznie się różnią - status rachunku bankowego wyraźnie różnicuje udział
# procentowy klas ryzyka kredytowe
rm(table_S)


#       3.4.2.2 CreditHistory
table_CH = table(credit_data$CreditHistory, credit_data$CreditRisk)
prop.table(table_CH, margin=1)
#     Niewiarygodny Wiarygodny
# A30     0.6250000  0.3750000
# A31     0.5714286  0.4285714
# A32     0.3188679  0.6811321
# A33     0.3181818  0.6818182
# A34     0.1706485  0.8293515

ggplot(credit_data, aes(x=CreditHistory, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A30" = "brak kredytów / spł. terminowo", 
                            "A31" = "kredyty w banku spł. terminowo",
                            "A32" = "akt. kredyty spłacane na czas",
                            "A33" = "opóźnienia w spłatach w przesz.",
                            "A34" = "inne kredyty poza bankiem")) +
  labs(title='', x='Historia kredytowa', y='Udział procentowy', fill='Ryzyko kredytowe') + 
  theme_bw() + 
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())

# Z uwagi na podobny rozkład klas i małą liczność kategorii łącze kategorie:
# kredyt spłacany na czas (A32) oraz opóźnienia w spłatach w przeszłości (A33)
credit_data$CreditHistory2 = as.character(credit_data$CreditHistory)
credit_data$CreditHistory2[credit_data$CreditHistory %in% c("A32", "A33")] <- 'A35'
credit_data$CreditHistory2 = as.factor(credit_data$CreditHistory2)

table_CH2 = table(credit_data$CreditHistory2, credit_data$CreditRisk)
prop.table(table_CH2, margin=1)
#     Niewiarygodny Wiarygodny
# A30     0.6250000  0.3750000
# A31     0.5714286  0.4285714
# A34     0.1706485  0.8293515
# A35     0.3187702  0.6812298

ggplot(credit_data, aes(x=CreditHistory2, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A30" = "brak kredytów / spł. terminowo", 
                            "A31" = "kredyty w banku spł. terminowo",
                            "A32" = "kredyt spłacany na czas",
                            "A33" = "opóźnienia w spłatach w przesz.",
                            "A34" = "inne kredyty poza bankiem",
                            "A35" = "kredyt spł. na czas / opóź. w spł. w przesz."
                              )) +
  labs(title='', x='Historia kredytowa', y='Udział procentowy', fill='Ryzyko kredytowe') + 
  theme_bw() + 
  theme(axis.title=element_text(size=12, face='bold'),
        axis.text.x=element_text(angle=45, hjust=1))

chisq.test(table_CH2)
# X-squared = 61.691, df = 3, p-value = 2.558e-13
# Widzoczna zależność między ryzykiem a historią kredytową - Udziały procentowe 
# klas ryzyka znacznie się różnią w zależności od histori kredytowej
rm(table_CH)

#     3.4.2.3 Purpose
table_P = table(credit_data$Purpose, credit_data$CreditRisk)
prop.table(table_P, margin=1)
#      Niewiarygodny Wiarygodny
# A40      0.3803419  0.6196581
# A41      0.1650485  0.8349515
# A410     0.4166667  0.5833333
# A42      0.3204420  0.6795580
# A43      0.2214286  0.7785714
# A44      0.3333333  0.6666667
# A45      0.3636364  0.6363636
# A46      0.4400000  0.5600000
# A48      0.1111111  0.8888889
# A49      0.3505155  0.6494845

ggplot(credit_data, aes(x=Purpose, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A40"  = "samochód (nowy)",
                            "A41"  = "samochód (używany)",
                            "A42"  = "meble/wyposażenie",
                            "A43"  = "radio/telewizor",
                            "A44"  = "sprzęt AGD",
                            "A45"  = "naprawy",
                            "A46"  = "edukacja",
                            "A47"  = "wakacje (brak danych)",
                            "A48"  = "przekwalifikowanie",
                            "A49"  = "działalność gospodarcza",
                            "A410" = "inne")) +
  labs(title='', x='Cel kredytu', y='Udział procentowy', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())
chisq.test(table_P) # Wyniki mogą nie być wiarygodne

# Z uwagi na podobny rozkład klas i małą liczność kategorii łącze kategorie:
# A410 (inne), A40(samchód(nowy)), A42 (meble/wyposażenie), A44 (sprzęt AGD), 
# A45 (naprawy), (A46) edukacja, A49 (działalność gospodarcza)
credit_data$Purpose2 = as.character(credit_data$Purpose)
credit_data$Purpose2[credit_data$Purpose %in% c("A40", "A410", "A42", "A44", "A45", "A46", "A49")] <- "A411"
credit_data$Purpose2 = as.factor(credit_data$Purpose2)

table_P2 = table(credit_data$Purpose2, credit_data$CreditRisk)
prop.table(table_P2, margin=1)
#      Niewiarygodny Wiarygodny
# A41      0.1650485  0.8349515
# A411     0.3618421  0.6381579
# A43      0.2214286  0.7785714
# A48      0.1111111  0.8888889

ggplot(credit_data, aes(x=Purpose2, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A41"  = "samochód (używany)",
                            "A43"  = "radio/telewizor",
                            "A47"  = "wakacje (brak danych)",
                            "A48"  = "przekwalifikowanie",
                            "A411" = "inne")) +
  labs(title='', x='Cel kredytu', y='Udział procentowy', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold'),
        axis.text.x=element_text(angle=45, hjust=1))
fisher.test(table_P2)
# p-value = 7.927e-07
# Występuje zależność między zmiennymi
rm(table_P)
rm(table_P2)


#       3.4.2.4 Savings
table_Sv = table(credit_data$Savings, credit_data$CreditRisk)
table_Sv
prop.table(table_Sv, margin=1)
#     Niewiarygodny Wiarygodny
# A61     0.3598673  0.6401327
# A62     0.3300971  0.6699029
# A63     0.1746032  0.8253968
# A64     0.1250000  0.8750000
# A65     0.1748634  0.8251366

ggplot(credit_data, aes(x=Savings, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A61" = "brak oszczędności (< 100 DM)",
                            "A62" = "oszczędności 100–499 DM",
                            "A63" = "oszczędności 500–999 DM",
                            "A64" = "oszczędności ≥ 1000 DM",
                            "A65" = "brak inf. / brak konta oszczędnościowego")) +
  labs(title='', x='Poziom oszczędności', y='Udział Procentowy', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())

# Z uwagi na pdobny udział klas ryzyka i sens merytoryczny grupuje zmienną Savings
# A61, A62 - niskie oszczędności
# A63, A64, A65 - wyższe oszczędności / nieznane
credit_data$Savings2 = as.character(credit_data$Savings)
credit_data$Savings2[credit_data$Savings %in% c("A61", "A62")] <- "niskie"
credit_data$Savings2[credit_data$Savings %in% c("A63", "A64", "A65")] <- "wysokie / nieznane"
credit_data$Savings2 = as.factor(credit_data$Savings2)

table_Sv2 = table(credit_data$Savings2, credit_data$CreditRisk)
table_Sv2
prop.table(table_Sv2, margin=1)
#                    Niewiarygodny Wiarygodny
# niskie                 0.3555241  0.6444759
# wysokie / nieznane     0.1666667  0.8333333

ggplot(credit_data, aes(x=Savings2, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels=scales::percent) +
  #scale_x_discrete(labels=c())
  labs(title='', x='Poziom oszczędności', y='Udział procentowy', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold'),
        axis.text.x=element_text(angle=45, hjust=1))

chisq.test(table_Sv2)
# X-squared = 34.36, df = 1, p-value = 4.581e-09
# Widoczna zależność między poziomem oszczędności a ryzykiem kredytowym
rm(table_Sv)
rm(table_Sv2)

#         3.4.2.5 Employment
table_E = table(credit_data$Employment, credit_data$CreditRisk)
prop.table(table_E, margin=1)
#     Niewiarygodny Wiarygodny
# A71     0.3709677  0.6290323
# A72     0.4069767  0.5930233
# A73     0.3067847  0.6932153
# A74     0.2241379  0.7758621
# A75     0.2529644  0.7470356

ggplot(credit_data, aes(x=Employment, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A71" = "bezrobotny",
                            "A72" = "zatrudniony < 1 rok",
                            "A73" = "zatrudniony 1–4 lata",
                            "A74" = "zatrudniony 4–7 lat",
                            "A75" = "zatrudniony ≥ 7 lat")) +
  labs(title='', x='Staż zatrudnenia', y='Udział procentowy', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())

chisq.test(table_E)
# X-squared = 18.368, df = 4, p-value = 0.001045
# Staż zatrudnienia istotnie różnicuje ryzyko kredytowe


# PersonalStatusSex
table_PSS = table(credit_data$PersonalStatusSex, credit_data$CreditRisk)
table_PSS
prop.table(table_PSS, margin=1)
#     Niewiarygodny Wiarygodny
# A91     0.4000000  0.6000000
# A92     0.3516129  0.6483871
# A93     0.2664234  0.7335766
# A94     0.2717391  0.7282609

ggplot(credit_data, aes(x=PersonalStatusSex, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A91" = "mężczyzna, rozwiedziony",
                            "A92" = "kobieta, rozwiedziona / zamężna",
                            "A93" = "mężczyzna, kawaler",
                            "A94" = "mężczyzna, żonaty / wdowiec",
                            "A95" = "kobieta, panna")) +
  labs(title='', x='Stan cywilny i płeć', y='Udział procentowy', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())

credit_data$PersonalStatusSex2 = as.character(credit_data$PersonalStatusSex)
credit_data$PersonalStatusSex2[credit_data$PersonalStatusSex %in% c("A93","A94")] <- "A96"
credit_data$PersonalStatusSex2 = as.factor(credit_data$PersonalStatusSex2)

table_PSS2 = table(credit_data$PersonalStatusSex2, credit_data$CreditRisk)
prop.table(table_PSS2, margin=1)
#     Niewiarygodny Wiarygodny
# A91     0.4000000  0.6000000
# A92     0.3516129  0.6483871
# A96     0.2671875  0.7328125

ggplot(credit_data, aes(x=PersonalStatusSex2, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A91" = "mężczyzna, rozwiedziony",
                            "A92" = "kobieta, rozwiedziona/zamężna",
                            "A95" = "kobieta, panna",
                            "A96" = "mężczyzna kawaler/żonaty/wdowiec")) +
  labs(title='', x="Stan cywilny i płeć", y="Udział procentowy", fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold'),
        axis.text.x=element_text(angle=45, hjust=1))

chisq.test(table_PSS2)
# X-squared = 9.5946, df = 2, p-value = 0.008252
# Status cywilny różnicuje ryzyko kredytowe 
rm(table_PSS)
rm(table_PSS2)


# OtherDebtors
table_OD = table(credit_data$OtherDebtors, credit_data$CreditRisk)
prop.table(table_OD, margin=1)
#      Niewiarygodny Wiarygodny
# A101     0.2998897  0.7001103
# A102     0.4390244  0.5609756
# A103     0.1923077  0.8076923

ggplot(credit_data, aes(x=OtherDebtors, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A101" = "brak",
                            "A102" = "współwnioskodawca",
                            "A103" = "poręczyciel")) +
  labs(title='', x='Inni dłużnicy/poręczyciele klienta', y='Udział procentowy', fill="Ryzyko kredytowe") +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())
chisq.test(table_OD)
# X-squared = 6.6454, df = 2, p-value = 0.03606
fisher.test(table_OD)
# p-value = 0.03989
# Związek między wartością zmiennej pokazującą innych dłużników poręczycieli klienta i
# ryzykiem kredytowym jest statystycznie istotny 
rm(table_OD)


# Property
table_Pr = table(credit_data$Property, credit_data$CreditRisk)
table_Pr
prop.table(table_Pr, margin=1)
#      Niewiarygodny Wiarygodny
# A121     0.2127660  0.7872340
# A122     0.3060345  0.6939655
# A123     0.3072289  0.6927711
# A124     0.4350649  0.5649351

ggplot(credit_data, aes(x=Property, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A121" = "nieruchomość",
                            "A122" = "umowa oszczędnościowa / ubez. na życie",
                            "A123" = "samochód lub inne",
                            "A124" = "brak informacji / brak własności")) +
  labs(title='', x='Rodzaj własności', y='Udział procentowy', fill="Ryzyko kredytowe") +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())

credit_data$Property2 = as.character(credit_data$Property)
credit_data$Property2[credit_data$Property %in% c("A122", "A123")] <- 'inne'
credit_data$Property2 = as.factor(credit_data$Property2)

table_Pr2 = table(credit_data$Property2, credit_data$CreditRisk)
table_Pr2
prop.table(table_Pr2, margin=1)
#      Niewiarygodny Wiarygodny
# A121     0.2127660  0.7872340
# A124     0.4350649  0.5649351
# inne     0.3067376  0.6932624

ggplot(credit_data, aes(x=Property2, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A121" = "nieruchomość",
                            "A124" = "brak informacji / brak własności")) +
  labs(title='', x='Rodzaj własności', y='Udział procentowy', fill="Ryzyko kredytowe") +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold'),
        axis.text.x=element_text(angle=45, hjust=1))

chisq.test(table_Pr2)
# X-squared = 23.719, df = 2, p-value = 7.072e-06
# Rodzaj włansości klienta wyraźnie różnicuje klasę ryzyka kredytowego
rm(table_Pr)
rm(table_Pr2)


# OtherInstallment
table_OI = table(credit_data$OtherInstallment, credit_data$CreditRisk)
table_OI
prop.table(table_OI, margin=1)
# Niewiarygodny Wiarygodny
# A141     0.4100719  0.5899281
# A142     0.4042553  0.5957447
# A143     0.2751843  0.7248157

ggplot(credit_data, aes(x=OtherInstallment, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A141" = "bank",
                            "A142" = "sklep",
                            "A143" = "brak")) +
  labs(title='', x='Inne zobowiązania ratalne klienta', y='Udział procentowy', fill="Ryzyko Kredytowe") +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())

credit_data$OtherInstallment2 = factor(ifelse(credit_data$OtherInstallment %in% c("A141","A42"), 'Tak', 'Nie'))

table_OI2 = table(credit_data$OtherInstallment2, credit_data$CreditRisk)
table_OI2
prop.table(table_OI2, margin=1)
#     Niewiarygodny Wiarygodny
# Nie     0.2822300  0.7177700
# Tak     0.4100719  0.5899281

ggplot(credit_data, aes(x=OtherInstallment2, fill=CreditRisk)) +
  geom_bar(position='fill')+
  scale_y_continuous(labels=scales::percent) +
  labs(title='', x='Inne zobowiązania ratalne klienta', y='Udział procentowy', fill="Ryzyko Kredytowe") +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold'),
        axis.text.x=element_text(angle=45, hjust=1))
chisq.test(table_OI)
# X-squared = 12.839, df = 2, p-value = 0.001629
# Zależność między występowaniem innych ratalnych zobowiązan klienta a oceną ryzyka
# występuje
rm(table_OI)
rm(table_OI2)


# Housing 
table_H = table(credit_data$Housing, credit_data$CreditRisk)
table_H
prop.table(table_H, margin=1)
#      Niewiarygodny Wiarygodny
# A151     0.3910615  0.6089385
# A152     0.2608696  0.7391304
# A153     0.4074074  0.5925926


ggplot(credit_data, aes(x=Housing, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A151" = "najem",
                            "A152" = "własność",
                            "A153" = "bez opłat")) +
  labs(title='', x='Kategoria zamieszkania klienta', y='Udział procetowy', fill='Ryzyko Kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())

credit_data$Housing2 = factor(ifelse(credit_data$Housing %in% c("A151", "A153"), 
                              "najem / bez opłat", "własność"))

table_H2 = table(credit_data$Housing2, credit_data$CreditRisk)
table_H2
prop.table(table_H2, margin=1)
#                   Niewiarygodny Wiarygodny
# najem / bez opłat     0.3972125  0.6027875
# własność              0.2608696  0.7391304

ggplot(credit_data, aes(x=Housing2, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels=scales::percent) +
  labs(title='', x='Rodzaj zamieszkania klienta', y='Udział procetowy', fill='Ryzyko Kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold'),
        axis.text.x=element_text(angle=45, hjust=1))
chisq.test(table_H2)
# X-squared = 17.471, df = 1, p-value = 2.918e-05
# Wyraźna zależność między ryzkiem kredytowym a rodzej zamiekszania klienta
rm(table_H)
rm(table_H2)


# Job
table_J = table(credit_data$Job, credit_data$CreditRisk) 
table_J
prop.table(table_J, margin=1)
#      Niewiarygodny Wiarygodny
# A171     0.3181818  0.6818182
# A172     0.2800000  0.7200000
# A173     0.2952381  0.7047619
# A174     0.3445946  0.6554054

ggplot(credit_data, aes(x=Job, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A171" = "bezrobotny / niewykw. (nierezydent)",
                            "A172" = "niewykwalifikowany (rezydent)",
                            "A173" = "wykwalifikowany / urzędnik",
                            "A174" = "kadra zarz./ samozatr. / wysoko wykw.")) +
  labs(title='', x='Kategoria zatrudnienia klienta', y='Udział procentowy', fill="Ryzyko kredytowe") +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold'),
        axis.text.x=element_text(angle=45, hjust=1))
chisq.test(table_J)
# X-squared = 1.8852, df = 3, p-value = 0.5966
# Kategoria zatrudnienia klienta nie różnicuje oceny ryzyka kredytowego
rm(table_J)

# Telephone
table_T = table(credit_data$Telephone, credit_data$CreditRisk)
table_T
prop.table(table_T, margin=1)
#      Niewiarygodny Wiarygodny
# A191     0.3137584  0.6862416
# A192     0.2797030  0.7202970

ggplot(credit_data, aes(x=Telephone, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A191" = "Brak telefonu",
                   "A192" = "Telefon na nazwisko klienta")) +
  labs(title='', x='Dostęp do telefonu', y='Udział procentowy', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12, face='bold'),
        axis.text.x=element_text(angle=45, hjust=1))
chisq.test(table_T)
# X-squared = 1.1726, df = 1, p-value = 0.2789
# Dostęp do telefonu nie różnicuje ryzyka kredytowego
rm(table_T)


# ForeignWorker
table_F = table(credit_data$ForeignWorker, credit_data$CreditRisk)
table_F
prop.table(table_F, margin=1)
#      Niewiarygodny Wiarygodny
# A201     0.3073728  0.6926272
# A202     0.1081081  0.8918919

ggplot(credit_data, aes(x=ForeignWorker, fill=CreditRisk)) +
  geom_bar(position='fill') +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A201" = 'Tak',
                   "A202" = 'Nie')) +
  labs(title='', x='Czy klient jest obcokrajowcem', y='Udział procentowy', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        panel.grid=element_blank())
chisq.test(table_F)
fisher.test(table_F)
# X-squared = 5.8216, df = 1, p-value = 0.01583
# Występuje zależność między ryzkiem kredytowym a pochodzeniem klienta
rm(table_F)



#     3.4.3 Wskaźnik IV dla starych oraz nowych zmiennych, z pogrupowanymi wartościami
#           o zbliżonych udziałach klas zmiennej objasnianej

credit_data_grouped = credit_data

iv_table2 = iv(credit_data, y = "CreditRisk", positive='Wiarygodny')
iv_table2$info_value = round(iv_table2$info_value, 4)
iv_table2

# 1:             Status      0.6660
# 2:           Duration      0.3345
# 3:      CreditHistory      0.2932
# 4:                Age      0.2597
# 5:            Savings      0.1960
# 6:           Savings2      0.1894
# 7:            Purpose      0.1692
# 8:           Purpose2      0.1543
# 9:           Property      0.1126
# 10:          Property2     0.1126
# 11:         Employment     0.0864
# 12:            Housing     0.0833
# 13:           Housing2     0.0830
# 14:   OtherInstallment     0.0576
# 15:  PersonalStatusSex     0.0447
# 16: PersonalStatusSex2     0.0446
# 17:      ForeignWorker     0.0439
# 18:  OtherInstallment2     0.0415
# 19:       CreditAmount     0.0390
# 20:       OtherDebtors     0.0320
# 21:    InstallmentRate     0.0263
# 22:    ExistingCredits     0.0133
# 23:                Job     0.0088
# 24:          Telephone     0.0064
# 25:     ResidenceSince     0.0036
# 26:    NumPeopleLiable     0.0000

#   3.5 Analiza współlinniowości oraz korelacji między zmiennymi
cor_matrix = cor(numeric_vars)
#                    Duration CreditAmount InstallmentRate ResidenceSince         Age ExistingCredits NumPeopleLiable
# Duration         1.00000000   0.62498420      0.07474882     0.03406720 -0.03613637     -0.01128360     -0.02383448
# CreditAmount     0.62498420   1.00000000     -0.27131570     0.02892632  0.03271642      0.02079455      0.01714215
# InstallmentRate  0.07474882  -0.27131570      1.00000000     0.04930237  0.05826568      0.02166874     -0.07120694
# ResidenceSince   0.03406720   0.02892632      0.04930237     1.00000000  0.26641918      0.08962523      0.04264343
# Age             -0.03613637   0.03271642      0.05826568     0.26641918  1.00000000      0.14925358      0.11820083
# ExistingCredits -0.01128360   0.02079455      0.02166874     0.08962523  0.14925358      1.00000000      0.10966670
# NumPeopleLiable -0.02383448   0.01714215     -0.07120694     0.04264343  0.11820083      0.10966670      1.00000000
corrplot(cor_matrix, method='color',addCoef.col = "black", tl.cex=0.8)
# Silna korelacja między Duration a CreditAmount - 0.62
library(car)
vif(lm(CreditAmount~., numeric_vars))
#    variable        gvif
# 1:        Duration 1.009406
# 2: InstallmentRate 1.016974
# 3:  ResidenceSince 1.082776
# 4:             Age 1.111386
# 5: ExistingCredits 1.035020
# 6: NumPeopleLiable 1.030093
# Brak problemu wspólinniowości zmiennych Duration, CreditAmount

#   3.6 Analiza interakcji zmiennych

#     3.6.1 CreditAmount/Duration
credit_data$MonthlyRate = credit_data$CreditAmount / credit_data$Duration

ggplot(data=credit_data, aes(x=CreditRisk, y=MonthlyRate, fill=CreditRisk)) +
  geom_boxplot(alpha=0.7, outlier.shape=7) +
  labs(title="", x='Ryzyko kredytowe',y='Miesięczne obiążenie klienta') +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face='bold'),
        panel.grid=element_blank())

ggplot(data=credit_data, aes(x=CreditRisk, y=MonthlyRate, fill=CreditRisk)) +
  geom_boxplot(alpha=1, outlier.shape=7) +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  labs(title="", x='Ryzyko kredytowe',y='Miesięczne obiążenie klienta', fill='Ryzyko kredytowe',
       caption = 'Uwaga: jedna odstająca obserwacje dla klasy "Wiarygodny" oraz dwie 
       dla klasy "Niewiargydny" zostały pominięte dla czytelności wykresu.') +
  coord_cartesian(ylim = c(0, 900)) +
  theme_bw() +
  theme(#plot.title=element_text(hjust=0.5, face='bold'),
        panel.grid=element_blank(),
        plot.caption = element_text(hjust = 0.5))

ggplot(data=credit_data, aes(x=CreditRisk, y=MonthlyRate, fill=CreditRisk)) +
  geom_boxplot(alpha=0.7, outlier.shape=7) +
  labs(title="", x='Ryzyko kredytowe',y='Miesięczne obiążenie klienta', fill="Ryzyko Kredytowe") +
  theme_bw() +
  coord_flip() +
  theme(plot.title=element_text(hjust=0.5, face='bold'),
        panel.grid=element_blank())

kruskal.test(MonthlyRate ~ CreditRisk, data = credit_data)
# Kruskal-Wallis chi-squared = 8.8129, df = 1, p-value = 0.002991

#     3.6.2 Purpose, Job
credit_data$Purpose_Job = interaction(credit_data$Purpose, credit_data$Job)
table_interakcja_pj = table(credit_data$Purpose_Job, credit_data$CreditRisk)
prop.table(table_interakcja_pj, margin=1)
fisher.test(table_interakcja_pj, simulate.p.value=T) # p-value = 0.01599

ggplot(credit_data, aes(x = Purpose, fill = CreditRisk)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  facet_wrap(~ Job, labeller = as_labeller(c("A171" = "bezrobotny / niewykw. (nierezydent)",
                                             "A172" = "niewykwalifikowany (rezydent)",
                                             "A173" = "wykwalifikowany / urzędnik",
                                             "A174" = "kadra zarz./ samozatr. / wysoko wykw."
  ))) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A40"  = "samochód (nowy)",
                            "A41"  = "samochód (używany)",
                            "A42"  = "meble/wyposażenie",
                            "A43"  = "radio/telewizor",
                            "A44"  = "sprzęt AGD",
                            "A45"  = "naprawy",
                            "A46"  = "edukacja",
                            "A47"  = "wakacje (brak danych)",
                            "A48"  = "przekwalifikowanie",
                            "A49"  = "działalność gospodarcza",
                            "A410" = "inne")) +
  labs(title='', x='Cel kredytu', y='Udział procentowy', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())

#     3.6.3 Status, Savings
credit_data$Status_Savings = interaction(credit_data$Status, credit_data$Savings)
table_interakcja_ss <- table(credit_data$Status_Savings, credit_data$CreditRisk)
fisher.test(table_interakcja_ss, simulate.p.value=T) # p-value = 0.0004998

ggplot(credit_data, aes(x = Status, fill = CreditRisk)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  facet_wrap(~ Savings, labeller = as_labeller(c("A61" = "brak oszczędności (< 100 DM)",
                                                 "A62" = "oszczędności 100–499 DM",
                                                 "A63" = "oszczędności 500–999 DM",
                                                 "A64" = "oszczędności ≥ 1000 DM",
                                                 "A65" = "brak inf. / brak konta oszcz."
  ))) +
  labs(title="Rozkład klas ryzyka wzależności od interakcji statusu rachunku i oszczędności",
       x="Status rachunku klienta", y="Udziały klas", fill="Ryzyko kredytowe") +
  scale_x_discrete(labels=c("A11" = "Brak",
                            "A12" = "Saldo < 200",
                            "A13" = "Saldo ≥ 200",
                            "A14" = "Nieznany")) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.title=element_text(size=12),
        panel.grid=element_blank(),
        plot.title.position = "plot",
        plot.title=element_text(hjust=0.5,face='bold',size=14))

#     3.6.4 Credit Amount / Age
credit_data$AmountPerAge = credit_data$CreditAmount / credit_data$Age
kruskal.test(AmountPerAge ~ CreditRisk, data = credit_data)
# Kruskal-Wallis chi-squared = 15.164, df = 1, p-value = 9.855e-05


ggplot(data=credit_data, aes(x=CreditRisk, y=AmountPerAge, fill=CreditRisk)) +
  geom_boxplot(alpha=1, outlier.shape=7) +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  labs(title="", x='Ryzyko kredytowe',y='Wskaźnik obciążenia kredytowego', fill="Ryzyko Kredytowe") +
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.title = element_text(size=12))

#     3.6.5 Employment, InstallmentRate
credit_data$Emp_Install = interaction(credit_data$Employment, credit_data$InstallmentRate)
table_emp_inst = table(credit_data$Emp_Install, credit_data$CreditRisk)
fisher.test(table_emp_inst, simulate.p.value = TRUE)
# X-squared = 33.72, df = NA, p-value = 0.01949

ggplot(credit_data, aes(x = Employment, fill = CreditRisk)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  facet_wrap(~ InstallmentRate, labeller = as_labeller(c("1" = "1%",
                                                 "2" = "2%",
                                                 "3" = "3%",
                                                 "4" = "4%"
  ))) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A71" = "bezrobotny",
                            "A72" = "zatrudniony < 1 rok",
                            "A73" = "zatrudniony 1–4 lata",
                            "A74" = "zatrudniony 4–7 lat",
                            "A75" = "zatrudniony ≥ 7 lat")) +
  labs(title='', x='Kategoria zatrudnenia', y='Udział procentowy', fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())


#     3.6.6 Housing, ResidenceSince
credit_data$Housing_Res = interaction(credit_data$Housing, credit_data$ResidenceSince)
table_housing_res = table(credit_data$Housing_Res, credit_data$CreditRisk)
chisq.test(table_housing_res, simulate.p.value = TRUE)
# X-squared = 33.829, df = NA, p-value = 0.0004998
# Występuje zależność między interakcją a ryzykiem kredytowym

ggplot(credit_data, aes(x = Housing, fill = CreditRisk)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Niewiarygodny" = "#F8766D", 
                               "Wiarygodny" = "steelblue")) +
  facet_wrap(~ ResidenceSince, labeller = as_labeller(c("1" = "1 rok",
                                                         "2" = "2 lata",
                                                         "3" = "3 lata",
                                                         "4" = "4 lata"
  ))) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("A151" = "najem",
                            "A152" = "własność",
                            "A153" = "bez opłat")) +
  labs(title='', x='Rodzaj zamieszkania klienta', y='Udział procentowy',
       fill='Ryzyko kredytowe') +
  theme_bw() +
  theme(axis.title=element_text(size=12),
        axis.text.x=element_text(angle=45, hjust=1),
        panel.grid=element_blank())


#   3.7 Wskaźnik IV dla zbioru z interakcjami
iv_table3 = iv(credit_data, y = "CreditRisk", positive='Wiarygodny')
iv_table3$info_value = round(iv_table3$info_value, 4)
iv_table3

# 1:     Status_Savings      0.7972 X
# 2:             Status      0.6660
# 3:           Duration      0.3345
# 4:      CreditHistory      0.2932
# 5:      CreditHistory2     0.2932
# 6:                Age      0.2597
# 7:        Purpose_Job      0.2067 X
# 8:            Savings      0.1960
# 9:           Savings2      0.1894
# 10:           Purpose      0.1692
# 11:        Emp_Install     0.1686 X
# 12:        Housing_Res     0.1602 X
# 13:           Purpose2     0.1543
# 14:           Property     0.1126
# 15:          Property2     0.1126
# 16:         Employment     0.0864
# 17:            Housing     0.0833
# 18:           Housing2     0.0830
# 19:   OtherInstallment     0.0576
# 20:  PersonalStatusSex     0.0447
# 21: PersonalStatusSex2     0.0446
# 22:      ForeignWorker     0.0439
# 23:  OtherInstallment2     0.0415
# 24:       CreditAmount     0.0390
# 25:       OtherDebtors     0.0320
# 26:    InstallmentRate     0.0263
# 27:        MonthlyRate     0.0185
# 28:    ExistingCredits     0.0133
# 29:                Job     0.0088
# 30:          Telephone     0.0064
# 31:       AmountPerAge     0.0036
# 32:     ResidenceSince     0.0036
# 33:    NumPeopleLiable     0.0000

iv_table3 = iv_table3 %>%
  arrange(desc(info_value)) %>%
  mutate(variable = reorder(variable, info_value))

ggplot(iv_table3, aes(x = variable, y = info_value)) +
  geom_col(fill = "steelblue", alpha = 0.8, color = "black") +
  geom_text(aes(label = info_value), hjust = -0.1, size = 3.5, color = "black") +
  coord_flip() +
  labs(x = "Zmienna",y = "Wartość IV") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(axis.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank())


#   3.8 czyszczony zbiór danych ze zmiennymi o IV >= 0.02

credit_data_clean = credit_data %>% select(CreditRisk,Status_Savings, Status, Duration,
                                             CreditHistory, CreditHistory2, Age,
                                             Purpose_Job, Savings, Savings2, Purpose,
                                             Emp_Install, Housing_Res, Purpose2,
                                             Property,  Property2, Employment, Housing,
                                             Housing2, OtherInstallment, PersonalStatusSex,
                                             PersonalStatusSex2, ForeignWorker,
                                             OtherInstallment2, CreditAmount, OtherDebtors,
                                             InstallmentRate)


print("Błąd")
print(a)va
#===============================================================================
# 4. Budowa modeli klasyfikacyjnych

#   4.1 Drzewo decyzyjne bazowe

set.seed(123456)

#       1. Podział z zbiór treningowy i testowy
train_index_base = sample(1:nrow(credit_data_original), size=0.7*nrow(credit_data_original))
train_base = credit_data_original[train_index_base,]
test_base = credit_data_original[-train_index_base,]

#       2. Budowa drzewa base
model_tree_base = rpart(CreditRisk ~ ., data=train_base, method='class', cp=0.01) # Ręcznie

#       3. Wizualizacja drzewa
rpart.plot(model_tree_base, type=4, extra=104, fallen.leaves=T)

#       4. Predykcja na zbiorze testowym
pred_base = predict(model_tree_base, test_base, type='class')

#       5. Macierz pomyłek
confusionMatrix(pred_base, test_base$CreditRisk)

# Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            41         30
# Wiarygodny               69        160
# 
# Accuracy : 0.67           
# 95% CI : (0.6136, 0.723)
# No Information Rate : 0.6333         
# P-Value [Acc > NIR] : 0.1035809      
# 
# Kappa : 0.2322         
# 
# Mcnemar's Test P-Value : 0.0001339      
#                                          
#             Sensitivity : 0.3727         
#             Specificity : 0.8421         
#          Pos Pred Value : 0.5775         
#          Neg Pred Value : 0.6987         
#              Prevalence : 0.3667         
#          Detection Rate : 0.1367         
#    Detection Prevalence : 0.2367         
#       Balanced Accuracy : 0.6074         
#                                          
#        'Positive' Class : Niewiarygodny   
                                          
#       6. Predykcja prawdopodobieństw dla klasy "Wiarygodny"
pred_probs_base = predict(model_tree_base, test_base, type='prob')

#       7. Krzywa ROC + wartość AUC
#roc_obj_base = roc(response = test_base$CreditRisk,
#                   predictor = pred_probs_base[, "Wiarygodny"],
#                   levels = c("Wiarygodny", "Niewiarygodny"),
#                   direction = ">")

roc_obj_base = roc(response = test_base$CreditRisk,
                   predictor = pred_probs_base[, "Niewiarygodny"],
                   levels = c("Niewiarygodny", "Wiarygodny"),
                   direction = ">")

plot(roc_obj_base, col='steelblue', legacy.axes=T) # Krzywa ROC - Drzewo bazowe
auc(roc_obj_base)     # Area under the curve: 0.6602

#       8. Istotność zmiennych
model_tree_base$variable.importance
# Status        CreditHistory     Purpose       CreditAmount        Duration            Savings 
# 33.1854740    25.1031861        24.2000773    23.3954857          17.9116318          17.5970874 

# Property      Age               Employment    PersonalStatusSex   ExistingCredits     InstallmentRate 
# 9.7514208     8.8305373         7.1094042     5.2750559           4.1761041           4.0046715 

# Housing       OtherDebtors      Job           ResidenceSince      OtherInstallment    NumPeopleLiable 
# 3.5031769     3.2800199         2.4119958     1.7247063           0.9993486           0.1754659  

barplot(model_tree_base$variable.importance, las=2, col='steelblue', cex.names=0.8,
        ylab="Spadek indeksu Giniego")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#   Przycinanie drzewa base

# Wybór optymalnego complexity parameter
printcp(model_tree_base)
#         CP nsplit rel error  xerror     xstd
# 1 0.057895      0   1.00000 1.00000 0.061924
# 2 0.052632      3   0.82632 0.97895 0.061509
# 3 0.019298      4   0.77368 0.85789 0.058854
# 4 0.018421      7   0.71579 0.90000 0.059831
# 5 0.015789     11   0.64211 0.88947 0.059592
# 6 0.013158     15   0.57895 0.88421 0.059471
# 7 0.010000     19   0.52632 0.90000 0.059831

best_cp_base = model_tree_base$cptable[which.min(model_tree_base$cptable[,'xerror']), 'CP']
model_tree_base_pruned = prune(model_tree_base, cp=best_cp_base)

# Wizualizacja drzewa
rpart.plot(model_tree_base_pruned, type=4, extra=104, fallen.leaves=T)

# Predykcja na zbiorze testowym
pred_base_pruned = predict(model_tree_base_pruned, test_base, type='class')

# Macierz pomyłek
confusionMatrix(pred_base_pruned, test_base$CreditRisk)
# Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            37         19
# Wiarygodny               73        171
# 
# Accuracy : 0.6933         
# 95% CI : (0.6378, 0.745)
# No Information Rate : 0.6333         
# P-Value [Acc > NIR] : 0.01711        
# 
# Kappa : 0.2636         
# 
# Mcnemar's Test P-Value : 3.283e-08      
#                                          
#             Sensitivity : 0.3364         
#             Specificity : 0.9000         
#          Pos Pred Value : 0.6607         
#          Neg Pred Value : 0.7008         
#              Prevalence : 0.3667         
#          Detection Rate : 0.1233         
#    Detection Prevalence : 0.1867         
#       Balanced Accuracy : 0.6182         
#                                          
#        'Positive' Class : Niewiarygodny   

# Predykcja prawdopodobieństw dla klasy "Wiarygodny"
pred_probs_base_pruned = predict(model_tree_base_pruned, test_base, type='prob')

# Krzywa ROC + wartość AUC
roc_obj_base_pruned = roc(response = test_base$CreditRisk,
                          predictor = pred_probs_base_pruned[, "Wiarygodny"],
                          levels = c("Wiarygodny", "Niewiarygodny"),
                          direction = ">")
plot(roc_obj_base_pruned, col='steelblue', legacy.axes=T) # Krzywa ROC - Drzewo bazowe
auc(roc_obj_base_pruned)     # Area under the curve: 0.718

# Istotność zmiennych - taka sama jak w poprzedniej wersji


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Walidacja krzyżowa drzewa base

set.seed(123456)

# Konntroler CV
cv_control_tree_base = trainControl(method = "cv",
                                    number = 5,
                                    classProbs = T,
                                    summaryFunction = twoClassSummary,
                                    savePredictions = "final")

# Budowa drzewa z CV
model_tree_base_cv = train(CreditRisk ~ ., 
                           data = credit_data_original,
                           method = "rpart",
                           metric = "ROC",
                           trControl = cv_control_tree_base,
                           tuneLength = 10)

summary(model_tree_base_cv)


# Wizualizacja drzewa model_tree_base_cv
best_cp_base_cv = model_tree_base_cv$bestTune$cp

model_tree_base_final = rpart(CreditRisk ~ ., 
                               data = credit_data_original,
                               method = "class",
                               cp = best_cp_base_cv)

summary(model_tree_base_final)
sum(model_tree_base_final$frame$var == "<leaf>")  # 12 liści
nrow(model_tree_base_final$frame)                 # 23 węzły
depths_tree_base_final = rpart:::tree.depth(as.numeric(rownames(model_tree_base_final$frame)))
max(depths_tree_base_final)                       # maksymalna głębokość - 8


#png("drzewo_base_final.png", width = 2000, height = 1500, res = 300)
rpart.plot(model_tree_base_final, type = 4, extra = 104, fallen.leaves = TRUE)
#dev.off()
barplot(model_tree_base_final$variable.importance, las=2, col='steelblue', cex.names=0.8,
        ylab="Spadek indeksu Giniego")

varimp_df = data.frame(
  Zmienna = names(model_tree_base$variable.importance),
  Waznosc = model_tree_base$variable.importance
)

ggplot(varimp_df, aes(x = reorder(Zmienna, Waznosc), y = Waznosc)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # poziomy wykres 
  labs(x='', y = "Ważność zmiennych") +
  theme_bw() +
  theme(axis.title = element_text(size=12),
        panel.grid=element_blank())
rm(varimp_df)

# Reguły
rpart.rules(model_tree_base_final)

# Wyniki modelu
print(model_tree_base_cv)
model_tree_base_cv$results
model_tree_base_cv$bestTune

# Macierz pomyłek
confusionMatrix(data=model_tree_base_cv$pred$pred[model_tree_base_cv$pred$cp == model_tree_base_cv$bestTune$cp],
                reference = model_tree_base_cv$pred$obs[model_tree_base_cv$pred$cp == model_tree_base_cv$bestTune$cp])
#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           125         95
# Wiarygodny              175        605
# 
# Accuracy : 0.73            
# 95% CI : (0.7013, 0.7573)
# No Information Rate : 0.7             
# P-Value [Acc > NIR] : 0.02013         
# 
# Kappa : 0.3041          
# 
# Mcnemar's Test P-Value : 1.526e-06       
#                                           
#             Sensitivity : 0.4167          
#             Specificity : 0.8643          
#          Pos Pred Value : 0.5682          
#          Neg Pred Value : 0.7756          
#              Prevalence : 0.3000          
#          Detection Rate : 0.1250          
#    Detection Prevalence : 0.2200          
#       Balanced Accuracy : 0.6405          
#                                           
#        'Positive' Class : Niewiarygodny

roc_obj_tree_base_cv = roc(response  = model_tree_base_cv$pred$obs,
                           predictor = model_tree_base_cv$pred$Wiarygodny,
                           levels    = c("Wiarygodny", "Niewiarygodny"),
                           direction = ">")

plot(roc_obj_tree_base_cv, col='steelblue', legacy.axes=T)
auc(roc_obj_tree_base_cv) # Area under the curve: 0.7206


#-------------------------------------------------------------------------------
#   4.2 Drzewo decyzyjne bazowe z wagami

set.seed(123456)

#       1. Ustalenie wag
class_weights_base = ifelse(train_base$CreditRisk == "Niewiarygodny", 1/0.3, 1/0.7)

#       2. Budowa drzewa base
model_tree_base_weighted = rpart(CreditRisk ~ ., 
                        data=train_base, 
                        method='class', 
                        weights = class_weights_base,
                        cp=0.01)

#       3. Wizualizacja drzewa
rpart.plot(model_tree_base_weighted, type=4, extra=104, fallen.leaves=T)

#       4. Predykcja na zbiorze testowym
pred_base_weighted = predict(model_tree_base_weighted, test_base, type='class')

#       5. Macierz pomyłek
confusionMatrix(pred_base_weighted, test_base$CreditRisk)
#               Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            74         59
# Wiarygodny               36        131
# 
# Accuracy : 0.6833          
# 95% CI : (0.6274, 0.7356)
# No Information Rate : 0.6333          
# P-Value [Acc > NIR] : 0.04012         
# 
# Kappa : 0.3469          
# 
# Mcnemar's Test P-Value : 0.02400         
#                                           
#              Sensitivity : 0.6727          
#             Specificity : 0.6895          
#          Pos Pred Value : 0.5564          
#          Neg Pred Value : 0.7844          
#              Prevalence : 0.3667          
#          Detection Rate : 0.2467          
#    Detection Prevalence : 0.4433          
#       Balanced Accuracy : 0.6811          
#                                           
#        'Positive' Class : Niewiarygodny   

#       6. Predykcja prawdopodobieństw dla klasy "Wiarygodny"
pred_probs_base_weighted = predict(model_tree_base_weighted, test_base, type='prob')

#       7. Krzywa ROC + wartość AUC
roc_obj_base_weighted = roc(response = test_base$CreditRisk,
                            predictor = pred_probs_base_weighted[, "Wiarygodny"],
                            levels = c("Wiarygodny", "Niewiarygodny"),
                   direction = ">")
plot(roc_obj_base_weighted, col='steelblue', legacy.axes=T) # Krzywa ROC - Drzewo bazowe
auc(roc_obj_base_weighted)     # Area under the curve: 0.7085

#       8. Istotność zmiennych
model_tree_base$variable.importance
# Status        CreditHistory     Purpose       CreditAmount        Duration            Savings 
# 33.1854740    25.1031861        24.2000773    23.3954857          17.9116318          17.5970874 

# Property      Age               Employment    ersonalStatusSex    ExistingCredits     InstallmentRate 
# 9.7514208     8.8305373         7.1094042     5.2750559           4.1761041           4.0046715 

# Housing       OtherDebtors      Job           ResidenceSince      OtherInstallment    NumPeopleLiable 
# 3.5031769     3.2800199         2.4119958     1.7247063           0.9993486           0.1754659 

barplot(model_tree_base$variable.importance, las=2, col='steelblue', cex.names=0.8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
#   Przycinanie drzewa base z wagami

# Wybór optymalnego complexity parameter
printcp(model_tree_base_weighted)
#         CP nsplit rel error  xerror     xstd
# 1 0.315789      0   1.00000 1.00000 0.029063
# 2 0.041729      1   0.68421 0.72556 0.027551
# 3 0.020301      4   0.55789 0.66992 0.026986
# 4 0.018045      5   0.53759 0.67368 0.027027
# 5 0.015038      8   0.48346 0.68647 0.027164
# 6 0.010150      9   0.46842 0.63233 0.026549
# 7 0.010150     13   0.41955 0.60977 0.026264
# 8 0.010000     15   0.39925 0.60977 0.026264

best_cp_base_weighted = model_tree_base_weighted$cptable[which.min(model_tree_base_weighted$cptable[,'xerror']), 'CP']
model_tree_base_weighted_pruned = prune(model_tree_base_weighted, cp=best_cp_base_weighted)

# Wizualizacja drzewa
rpart.plot(model_tree_base_weighted_pruned, type=4, extra=104, fallen.leaves=T)

# Predykcja na zbiorze testowym
pred_base_weighted_pruned = predict(model_tree_base_weighted_pruned, test_base, type='class')

# Macierz pomyłek
confusionMatrix(pred_base_weighted_pruned, test_base$CreditRisk)
#               Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            73         52
# Wiarygodny               37        138
#
# Accuracy : 0.7033          
# 95% CI : (0.6481, 0.7545)
# No Information Rate : 0.6333          
# P-Value [Acc > NIR] : 0.006429        
#
# Kappa : 0.3791          
# 
# Mcnemar's Test P-Value : 0.137810        
#                                           
#             Sensitivity : 0.6636          
#             Specificity : 0.7263          
#          Pos Pred Value : 0.5840          
#          Neg Pred Value : 0.7886          
#               Prevalence : 0.3667          
#          Detection Rate : 0.2433          
#    Detection Prevalence : 0.4167          
#       Balanced Accuracy : 0.6950          
#                                           
#        'Positive' Class : Niewiarygodny   

# Predykcja prawdopodobieństw dla klasy "Wiarygodny"
pred_probs_base_weighted_pruned = predict(model_tree_base_weighted_pruned, test_base, type='prob')

# Krzywa ROC + wartość AUC
roc_obj_base_weighted_pruned = roc(response = test_base$CreditRisk,
                                   predictor = pred_probs_base_weighted_pruned[, "Wiarygodny"],
                                   levels = c("Wiarygodny", "Niewiarygodny"),
                          direction = ">")
plot(roc_obj_base_weighted_pruned, col='steelblue', legacy.axes=T) # Krzywa ROC - Drzewo bazowe z waga,o
auc(roc_obj_base_weighted_pruned)     # Area under the curve: 0.7226

# Istotność zmiennych - taka sama jak w poprzedniej wersji


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Walidacja krzyżowa drzewa base z wagami

set.seed(123456)

# Ustalenie wag
class_weights_base_weighted = ifelse(credit_data_original$CreditRisk == "Niewiarygodny", 1/0.3, 1/0.7)

# Budowa drzewa z CV z wagami
model_tree_base_weighted_cv = train(CreditRisk ~ ., 
                           data = credit_data_original,
                           method = "rpart",
                           metric = "ROC",
                           trControl = cv_control_tree_base,
                           tuneLength = 10,
                           weights = class_weights_base_weighted)


# Wyniki modelu
print(model_tree_base_weighted_cv)
model_tree_base_weighted_cv$results
model_tree_base_weighted_cv$bestTune

# Macierz pomyłek
confusionMatrix(data=model_tree_base_weighted_cv$pred$pred[model_tree_base_weighted_cv$pred$cp == model_tree_base_weighted_cv$bestTune$cp],
                 reference = model_tree_base_weighted_cv$pred$obs[model_tree_base_weighted_cv$pred$cp == model_tree_base_weighted_cv$bestTune$cp])
#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           198        217
# Wiarygodny              102        483
# 
# Accuracy : 0.681           
# 95% CI : (0.6511, 0.7098)
# No Information Rate : 0.7             
# P-Value [Acc > NIR] : 0.9102          
# 
# Kappa : 0.3155          
# 
# Mcnemar's Test P-Value : 1.739e-10       
#                                           
#             Sensitivity : 0.6600          
#             Specificity : 0.6900          
#          Pos Pred Value : 0.4771          
#          Neg Pred Value : 0.8256          
#              Prevalence : 0.3000          
#          Detection Rate : 0.1980          
#    Detection Prevalence : 0.4150          
#       Balanced Accuracy : 0.6750          
#                                           
#        'Positive' Class : Niewiarygodny

roc_obj_tree_base_weighted_cv = roc(response  = model_tree_base_weighted_cv$pred$obs,
                                    predictor = model_tree_base_weighted_cv$pred$Wiarygodny,
                                    levels    = c("Wiarygodny", "Niewiarygodny"),
                                    direction = ">")

plot(roc_obj_tree_base_weighted_cv, col='steelblue', legacy.axes=T)
auc(roc_obj_tree_base_weighted_cv) # Area under the curve: 0.721

#-------------------------------------------------------------------------------
#   4.3 Drzewo z pogrupowanymi zmiennymi

set.seed(123456)

#       1. Zbiór treningowy i testowy
train_index_grouped = sample(1:nrow(credit_data_grouped), size=0.7*nrow(credit_data_grouped))
train_grouped = credit_data_grouped[train_index_grouped,]
test_grouped = credit_data_grouped[-train_index_grouped,]

#       2. Budowa drzewa ze zmeinnymi o pogrupowanych wartościach
model_tree_grouped = rpart(CreditRisk ~ ., data=train_grouped, method='class', cp=0.01)

#       3. Wizualizacja
rpart.plot(model_tree_grouped, type=4, extra=104, fallen.leaves=T)

#       4. Predykcja na zbiorze testowym
pred_grouped = predict(model_tree_grouped, test_grouped, type='class')

#       5. Macierz pomyłek
confusionMatrix(pred_grouped, test_grouped$CreditRisk)
# Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            41         30
# Wiarygodny               69        160
# 
# Accuracy : 0.67           
# 95% CI : (0.6136, 0.723)
# No Information Rate : 0.6333         
# P-Value [Acc > NIR] : 0.1035809      
# 
# Kappa : 0.2322         
# 
# Mcnemar's Test P-Value : 0.0001339      
#                                          
#             Sensitivity : 0.3727         
#             Specificity : 0.8421         
#          Pos Pred Value : 0.5775         
#          Neg Pred Value : 0.6987         
#              Prevalence : 0.3667         
#          Detection Rate : 0.1367         
#    Detection Prevalence : 0.2367         
#       Balanced Accuracy : 0.6074         
#                                          
#        'Positive' Class : Niewiarygodny

#       6. Predykcja prawdopodobieństw dla klasy "Wiarygodny"
pred_probs_grouped = predict(model_tree_grouped, test_grouped, type='prob')

#       7. Krzywa ROC + wartość AUC
roc_obj_grouped = roc(response = test_grouped$CreditRisk,
                   predictor = pred_probs_grouped[, "Wiarygodny"],
                   levels = c("Wiarygodny", "Niewiarygodny"),
                   direction = ">")
plot(roc_obj_grouped, col='steelblue', legacy.axes=T) # Krzywa ROC - Drzewo grouped
auc(roc_obj_grouped)     # Area under the curve: 0.6602

#       8. Istotność zmiennych
model_tree_grouped$variable.importance
# Status                Purpose             CreditHistory       CreditAmount      Savings             Duration 
# 33.1854740            24.2000773          23.8412561          23.3954857        17.5970874          17.0807762 

# Savings2              Property            Age                 Purpose2          PersonalStatusSex   Employment 
# 9.8156259             9.7514208           8.8305373           5.8961489         5.2750559           5.0349459 

# Property2             ExistingCredits     Housing             InstallmentRate   OtherDebtors        ResidenceSince 
# 4.6624299             4.1761041           3.5031769           3.1366159         2.7080966           1.7247063 

# PersonalStatusSex2    Job                 OtherInstallment    NumPeopleLiable 
# 1.5625000             1.3703292           0.5239002           0.1754659 

barplot(model_tree_grouped$variable.importance, col='steelblue', las=2, cex.names=0.8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Przycinanie drzewa grouped

# Znalezienie optymalnego complexity parameter
printcp(model_tree_grouped)
#         CP nsplit rel error  xerror     xstd
# 1 0.057895      0   1.00000 1.00000 0.061924
# 2 0.052632      3   0.82632 0.97895 0.061509
# 3 0.019298      4   0.77368 0.85789 0.058854
# 4 0.018421      7   0.71579 0.90000 0.059831
# 5 0.015789     11   0.64211 0.88947 0.059592
# 6 0.013158     15   0.57895 0.88421 0.059471
# 7 0.010000     19   0.52632 0.90000 0.059831

best_cp_grouped = model_tree_grouped$cptable[which.min(model_tree_grouped$cptable[,'xerror']), 'CP']
model_tree_grouped_pruned = prune(model_tree_grouped, cp=best_cp_grouped)

# Wizualizacja drzewa
rpart.plot(model_tree_grouped_pruned, type=4, extra=104, fallen.leaves=T)

# Predykcja na zbiorze testowym
pred_grouped_pruned = predict(model_tree_grouped_pruned, test_grouped, type='class')

# Macierz pomyłek
confusionMatrix(pred_grouped_pruned, test_grouped$CreditRisk)
#               Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            37         19
# Wiarygodny               73        171
# 
# Accuracy : 0.6933         
# 95% CI : (0.6378, 0.745)
# No Information Rate : 0.6333         
# P-Value [Acc > NIR] : 0.01711        
# 
# Kappa : 0.2636         
# 
# Mcnemar's Test P-Value : 3.283e-08      
#                                          
#             Sensitivity : 0.3364         
#             Specificity : 0.9000         
#          Pos Pred Value : 0.6607         
#          Neg Pred Value : 0.7008         
#              Prevalence : 0.3667         
#          Detection Rate : 0.1233         
#    Detection Prevalence : 0.1867         
#       Balanced Accuracy : 0.6182         
#                                          
#        'Positive' Class : Niewiarygodny    

# Predykcja prawdopodobieństw dla klasy 'Wiarygodny'
pred_probs_grouped_pruned = predict(model_tree_grouped_pruned, test_grouped, type='prob')                                      

# Krzywa ROC + AUC
roc_obj_grouped_pruned = roc(response = test_grouped$CreditRisk ,
                             predictor = pred_probs_grouped_pruned[, "Wiarygodny"],
                             levels = c("Wiarygodny", "Niewiarygodny"),
                             direction =">")
plot(roc_obj_grouped_pruned, col='steelblue', legacy.axes=T) # Krzywa ROC - drzewo grouped pruned
auc(roc_obj_grouped_pruned)  # Area under the curve: 0.718
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Walidacja krzyżowa drzewa grouped

set.seed(123456)

# Konntroler CV
cv_control_tree_grouped = trainControl(method = "cv",
                                    number = 5,
                                    classProbs = T,
                                    summaryFunction = twoClassSummary,
                                    savePredictions = "final")

# Budowa drzewa z CV
model_tree_grouped_cv = train(CreditRisk ~ ., 
                           data = credit_data_grouped,
                           method = "rpart",
                           metric = "ROC",
                           trControl = cv_control_tree_grouped,
                           tuneLength = 10)

print(model_tree_grouped_cv)
model_tree_grouped_cv$results
model_tree_grouped_cv$bestTune

# Macierz pomyłek
confusionMatrix(data = model_tree_grouped_cv$pred$pred,
                reference = model_tree_grouped_cv$pred$obs)
#               Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           120         81
# Wiarygodny              180        619
# 
# Accuracy : 0.739          
# 95% CI : (0.7106, 0.766)
# No Information Rate : 0.7            
# P-Value [Acc > NIR] : 0.003604       
# 
# Kappa : 0.3139         
# 
# Mcnemar's Test P-Value : 1.311e-09      
#                                          
#             Sensitivity : 0.4000         
#             Specificity : 0.8843         
#          Pos Pred Value : 0.5970         
#          Neg Pred Value : 0.7747         
#              Prevalence : 0.3000         
#          Detection Rate : 0.1200         
#    Detection Prevalence : 0.2010         
#       Balanced Accuracy : 0.6421         
#                                          
#        'Positive' Class : Niewiarygodny


roc_obj_tree_grouped_cv = roc(response  = model_tree_grouped_cv$pred$obs,
                           predictor = model_tree_grouped_cv$pred$Wiarygodny,
                           levels    = c("Wiarygodny", "Niewiarygodny"),
                           direction = ">")

plot(roc_obj_tree_grouped_cv, col='steelblue', legacy.axes=T)
auc(roc_obj_tree_grouped_cv) # Area under the curve: 0.7253


#-------------------------------------------------------------------------------
#   4.4 Drzewo z pogrupowanymi zmiennymi i wagami

set.seed(123456)

#       1. Ustalenie wag
class_weights_grouped = ifelse(train_grouped$CreditRisk == "Niewiarygodny", 1/0.3, 1/0.7)

#       2. Budowa drzewa ze zmeinnymi o pogrupowanych wartościach
model_tree_grouped_weighted = rpart(CreditRisk ~ .,
                                    data=train_grouped,
                                    method='class',
                                    weights = class_weights_grouped,
                                    cp=0.01)

#       3. Wizualizacja
rpart.plot(model_tree_grouped_weighted, type=4, extra=104, fallen.leaves=T)

#       4. Predykcja na zbiorze testowym
pred_grouped_weighted = predict(model_tree_grouped_weighted, test_grouped, type='class')

#       5. Macierz pomyłek
confusionMatrix(pred_grouped_weighted, test_grouped$CreditRisk)
#               Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            74         59
# Wiarygodny               36        131
# 
# Accuracy : 0.6833          
# 95% CI : (0.6274, 0.7356)
# No Information Rate : 0.6333          
# P-Value [Acc > NIR] : 0.04012         
# 
# Kappa : 0.3469          
# 
# Mcnemar's Test P-Value : 0.02400         
#                                           
#             Sensitivity : 0.6727          
#             Specificity : 0.6895          
#          Pos Pred Value : 0.5564          
#          Neg Pred Value : 0.7844          
#              Prevalence : 0.3667          
#          Detection Rate : 0.2467          
#    Detection Prevalence : 0.4433          
#       Balanced Accuracy : 0.6811          
#                                           
#        'Positive' Class : Niewiarygodny

#       6. Predykcja prawdopodobieństw dla klasy "Wiarygodny"
pred_probs_grouped_weighted = predict(model_tree_grouped_weighted, test_grouped, type='prob')

#       7. Krzywa ROC + wartość AUC
roc_obj_grouped_weighted = roc(response = test_grouped$CreditRisk,
                               predictor = pred_probs_grouped_weighted[, "Wiarygodny"],
                               levels = c("Wiarygodny", "Niewiarygodny"),
                      direction = ">")
plot(roc_obj_grouped_weighted, col='steelblue', legacy.axes=T) # Krzywa ROC - Drzewo grouped z wagami
auc(roc_obj_grouped_weighted)     # Area under the curve: 0.7085

#       8. Istotność zmiennych
model_tree_grouped_weighted$variable.importance
# Status              Duration              Savings       CreditAmount    Savings2          CreditHistory 
# 123.9264304         45.9795336            38.9156671    32.9495389      31.6415996        30.2476570 

# OtherDebtors        Employment            Purpose       Property        NumPeopleLiable   ExistingCredits 
# 24.9670270          23.2946648            19.3616076    16.6239085      8.6606867         7.2910344 

# ResidenceSince      Property2             Job           Telephone       Age               InstallmentRate 
# 6.1833837           4.2607648             4.2053207     3.4635915       3.2021266         2.4909008 

# OtherInstallment    OtherInstallment2     Purpose2      ForeignWorker 
# 0.9686836           0.9686836             0.7543866     0.6971551 

barplot(model_tree_grouped_weighted$variable.importance, col='steelblue', las=2, cex.names=0.8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Przycinanie drzewa grouped z wagami

# Znalezienie optymalnego complexity parameter
printcp(model_tree_grouped_weighted)
#         CP nsplit rel error  xerror     xstd
# 1 0.315789      0   1.00000 1.00000 0.029063
# 2 0.041729      1   0.68421 0.72556 0.027551
# 3 0.020301      4   0.55789 0.70226 0.027326
# 4 0.018045      5   0.53759 0.64887 0.026747
# 5 0.015038      8   0.48346 0.64586 0.026711
# 6 0.010150      9   0.46842 0.66692 0.026953
# 7 0.010150     13   0.41955 0.68421 0.027140
# 8 0.010000     15   0.39925 0.68421 0.027140

best_cp_grouped_weighted = model_tree_grouped_weighted$cptable[which.min(model_tree_grouped_weighted$cptable[,'xerror']), 'CP']
model_tree_grouped_weighted_pruned = prune(model_tree_grouped_weighted, cp=best_cp_grouped_weighted)

# Wizualizacja drzewa
rpart.plot(model_tree_grouped_weighted_pruned, type=4, extra=104, fallen.leaves=T)

# Predykcja na zbiorze testowym
pred_grouped_weighted_pruned = predict(model_tree_grouped_weighted_pruned, test_grouped, type='class')

# Macierz pomyłek
confusionMatrix(pred_grouped_weighted_pruned, test_grouped$CreditRisk)
#               Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            65         47
# Wiarygodny               45        143
# 
# Accuracy : 0.6933         
# 95% CI : (0.6378, 0.745)
# No Information Rate : 0.6333         
# P-Value [Acc > NIR] : 0.01711        
# 
# Kappa : 0.3422         
# 
# Mcnemar's Test P-Value : 0.91697        
#                                          
#             Sensitivity : 0.5909         
#             Specificity : 0.7526         
#          Pos Pred Value : 0.5804         
#          Neg Pred Value : 0.7606         
#              Prevalence : 0.3667         
#          Detection Rate : 0.2167         
#    Detection Prevalence : 0.3733         
#       Balanced Accuracy : 0.6718         
#                                          
#      'Positive' Class : Niewiarygodny   


# Predykcja prawdopodobieństw dla klasy 'Wiarygodny'
pred_probs_grouped_weighted_pruned = predict(model_tree_grouped_weighted_pruned, test_grouped, type='prob')                                      

# Krzywa ROC + AUC
roc_obj_grouped_weighted_pruned = roc(response = test_grouped$CreditRisk ,
                                      predictor = pred_probs_grouped_weighted_pruned[, "Wiarygodny"],
                                      levels = c("Wiarygodny", "Niewiarygodny"),
                             direction =">")
plot(roc_obj_grouped_weighted_pruned, col='steelblue', legacy.axes=T) # Krzywa ROC - drzewo grouped pruned weighted
auc(roc_obj_grouped_weighted_pruned)  # Area under the curve: 0.7109


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Walidacja krzyżowa drzewa grouped z wagami

set.seed(123456)

# Ustalenie wag
class_weights_grouped_weighted = ifelse(credit_data_grouped$CreditRisk == "Niewiarygodny", 1/0.3, 1/0.7)

# Budowa drzewa z CV
model_tree_grouped_weighted_cv = train(CreditRisk ~ ., 
                                       data = credit_data_grouped,
                                       method = "rpart",
                                       metric = "ROC",
                                       trControl = cv_control_tree_grouped,
                                       tuneLength = 10,
                                       weights = class_weights_grouped_weighted)

print(model_tree_grouped_cv)
model_tree_grouped_cv$results
model_tree_grouped_cv$bestTune

# Macierz pomyłek
confusionMatrix(data = model_tree_grouped_weighted_cv$pred$pred,
                reference = model_tree_grouped_weighted_cv$pred$obs)
#               Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           195        220
# Wiarygodny              105        480
# 
# Accuracy : 0.675         
# 95% CI : (0.645, 0.704)
# No Information Rate : 0.7           
# P-Value [Acc > NIR] : 0.96          
# 
# Kappa : 0.3026        
# 
# Mcnemar's Test P-Value : 2.556e-10     
#                                         
#             Sensitivity : 0.6500        
#             Specificity : 0.6857        
#          Pos Pred Value : 0.4699        
#          Neg Pred Value : 0.8205        
#              Prevalence : 0.3000        
#          Detection Rate : 0.1950        
#    Detection Prevalence : 0.4150        
#       Balanced Accuracy : 0.6679        
#                                         
#        'Positive' Class : Niewiarygodny


roc_obj_tree_grouped_weighted_cv = roc(response  = model_tree_grouped_weighted_cv$pred$obs,
                                       predictor = model_tree_grouped_weighted_cv$pred$Wiarygodny,
                                       levels    = c("Wiarygodny", "Niewiarygodny"),
                                       direction = ">")

plot(roc_obj_tree_grouped_weighted_cv, col='steelblue', legacy.axes=T)
auc(roc_obj_tree_grouped_weighted_cv) # Area under the curve: 0.7166


#-------------------------------------------------------------------------------
#     4.5 Drzewo z z pogrupowanymi zmiennymi i interakcjami

set.seed(123456)

#         1. Zbiór treningowy i testowy
train_index_full = sample(1:nrow(credit_data), size=0.7*nrow(credit_data))
train_full = credit_data[train_index_full,]
test_full = credit_data[-train_index_full,]

#         2. Budowa drzewa z pogrupowanymi zmiennymi i interakcjami 
model_tree_full = rpart(CreditRisk ~ ., data=train_full, method='class', cp=0.01)

#         3. Wizualizacja drzewa
rpart.plot(model_tree_full, type=4, extra=104, fallen.leaves=T)

#         4. Predykcja na zbiorze testowym
pred_full = predict(model_tree_full, test_full, type='class')

#         5. Macierz pomyłek
confusionMatrix(pred_full, test_full$CreditRisk)
#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            43         28
# Wiarygodny               67        162
# 
# Accuracy : 0.6833          
# 95% CI : (0.6274, 0.7356)
# No Information Rate : 0.6333          
# P-Value [Acc > NIR] : 0.04012         
# 
# Kappa : 0.2632          
# 
# Mcnemar's Test P-Value : 9.67e-05        
#                                           
#             Sensitivity : 0.3909          
#             Specificity : 0.8526          
#          Pos Pred Value : 0.6056          
#          Neg Pred Value : 0.7074          
#              Prevalence : 0.3667          
#          Detection Rate : 0.1433          
#    Detection Prevalence : 0.2367          
#       Balanced Accuracy : 0.6218          
#                                           
#        'Positive' Class : Niewiarygodny    


#         6. Predykcja prawdopodobieństw dla klasy "wiarygodny"
pred_probs_full = predict(model_tree_full, test_full, type='prob')

#         7. Krzywa ROC + wartość AUC
roc_obj_full = roc(response = test_full$CreditRisk,
                   predictor = pred_probs_full[,'Wiarygodny'],
                   levels = c("Wiarygodny", "Niewiarygodny"),
                   direction = ">")
plot(roc_obj_full, col='steelblue', legacy.axes=T) # Krzywa ROC - Drzewo full
auc(roc_obj_full)         # Area under the curve: 0.685

#         8. Istotność zmiennych
model_tree_full$variable.importance

# Status_Savings    Purpose_Job     Status        Housing_Res       Savings             Savings2 
# 43.4433748        32.7036596      32.5247004    21.0683235        19.9369068          19.9369068 

# Emp_Install       CreditAmount    Duration      AmountPerAge      CreditHistory       Purpose 
# 18.4005066        17.5068380      15.3090944    11.3810386        10.2972336          9.2706376 

# MonthlyRate       Property        Job           ResidenceSince    InstallmentRate     Employment 
# 9.2078752         6.9818020       4.7044264     4.4981095         4.1648709           3.5575832 

# Property2         Age             Purpose2      Housing           PersonalStatusSex   ForeignWorker 
# 3.4115970         2.8230799       2.3764336     1.2814247         0.5719233           0.4857939 

barplot(model_tree_full$variable.importance, las=2, col='steelblue', cex.names=0.8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Przycinanie drzewa full

# Znalezienie optymalnego complexity parameter
model_tree_full$cptable

#           CP nsplit rel error    xerror       xstd
# 1 0.06578947      0 1.0000000 1.0000000 0.06192404
# 2 0.05263158      3 0.8000000 0.9894737 0.06171792
# 3 0.02105263      4 0.7473684 0.9578947 0.06107985
# 4 0.01842105      9 0.6421053 0.9842105 0.06161364
# 5 0.01578947     12 0.5842105 0.9947368 0.06182139
# 6 0.01052632     15 0.5368421 0.9947368 0.06182139
# 7 0.01000000     17 0.5157895 0.9947368 0.06182139

plotcp(model_tree_full)

best_cp_full = model_tree_full$cptable[which.min(model_tree_full$cptable[, 'xerror']), 'CP']
model_tree_full_pruned = prune(model_tree_full, cp=best_cp_full)

# Wizualizacja drzewa full_pruned
rpart.plot(model_tree_full_pruned, type=4, extra=104, fallen.leaves=T)

# Predykcja na zbiorze testowym
pred_full_pruned = predict(model_tree_full_pruned, test_full, type='class')

# Macierz pomyłek
confusionMatrix(pred_full_pruned, test_full$CreditRisk)

#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            43         23
# Wiarygodny               67        167
# 
# Accuracy : 0.7             
# 95% CI : (0.6447, 0.7513)
# No Information Rate : 0.6333          
# P-Value [Acc > NIR] : 0.009038        
# 
# Kappa : 0.2947          
# 
# Mcnemar's Test P-Value : 5.826e-06       
#                                           
#             Sensitivity : 0.3909          
#             Specificity : 0.8789          
#          Pos Pred Value : 0.6515          
#          Neg Pred Value : 0.7137          
#              Prevalence : 0.3667          
#          Detection Rate : 0.1433          
#    Detection Prevalence : 0.2200          
#       Balanced Accuracy : 0.6349          
#                                           
#        'Positive' Class : Niewiarygodny 

# Krzywa ROC + AUC
# Predykcja  prawdopodobieństw dla klasy "Wiarygodny"
pred_probs_full_pruned = predict(model_tree_full_pruned, test_full, type='prob')

roc_obj_full_pruned = roc(response = test_full$CreditRisk ,
                          predictor = pred_probs_full_pruned[,'Wiarygodny'],
                          levels = c("Wiarygodny", "Niewiarygodny"),
                          direction =">")

plot(roc_obj_full_pruned, col='steelblue', legacy.axes=T) # Krzywa ROC - drzewo full_pruned
auc(roc_obj_full_pruned)    # Area under the curve: 0.7097

# rpart.rules(model_tree_full_pruned, cover=T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Walidacja krzyżowa drzewa full

set.seed(123456)

# Konntroler CV
cv_control_tree_full = trainControl(method = "cv",
                                    number = 5,
                                    classProbs = T,
                                    summaryFunction = twoClassSummary,
                                    savePredictions = "final")

# Budowa drzewa z CV
model_tree_full_cv = train(CreditRisk ~ ., 
                              data = credit_data,
                              method = "rpart",
                              metric = "ROC",
                              trControl = cv_control_tree_full,
                              tuneLength = 10)
print(model_tree_full_cv)
model_tree_full_cv$results
model_tree_full_cv$bestTune

# Macierz pomyłek
confusionMatrix(data = model_tree_full_cv$pred$pred,
                reference = model_tree_full_cv$pred$obs)
#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           126         89
# Wiarygodny              174        611
# 
# Accuracy : 0.737           
# 95% CI : (0.7085, 0.7641)
# No Information Rate : 0.7             
# P-Value [Acc > NIR] : 0.005458        
# 
# Kappa : 0.3187          
# 
# Mcnemar's Test P-Value : 2.223e-07       
#                                           
#             Sensitivity : 0.4200          
#             Specificity : 0.8729          
#          Pos Pred Value : 0.5860          
#          Neg Pred Value : 0.7783          
#            Prevalence : 0.3000          
#          Detection Rate : 0.1260          
#    Detection Prevalence : 0.2150          
#       Balanced Accuracy : 0.6464          
#                                           
#        'Positive' Class : Niewiarygodny

roc_obj_tree_full_cv = roc(response  = model_tree_full_cv$pred$obs,
                           predictor = model_tree_full_cv$pred$Wiarygodny,
                           levels    = c("Wiarygodny", "Niewiarygodny"),
                           direction = ">")

plot(roc_obj_tree_full_cv, col='steelblue', legacy.axes=T)
auc(roc_obj_tree_full_cv) # Area under the curve: 0.73

#-------------------------------------------------------------------------------
#     4.6 Drzewo z pogrupowanymi zmiennymi i interakcjami oraz wagami


#         1. Ustalenie wag
class_weights_full = ifelse(train_full$CreditRisk == "Niewiarygodny", 1/0.3, 1/0.7)

#         2. Budowa drzewa z pogrupowanymi zmiennymi i interakcjami 
model_tree_full_weighted = rpart(CreditRisk ~ ., 
                                 data=train_full, 
                                 method='class', 
                                 weights = class_weights_full,
                                 cp=0.01)

#         3. Wizualizacja drzewa
rpart.plot(model_tree_full_weighted, type=4, extra=104, fallen.leaves=T)

#         4. Predykcja na zbiorze testowym
pred_full_weighted = predict(model_tree_full_weighted, test_full, type='class')

#         5. Macierz pomyłek
confusionMatrix(pred_full_weighted, test_full$CreditRisk)
#                 reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            57         49
# Wiarygodny               53        141
# 
# Accuracy : 0.66            
# 95% CI : (0.6033, 0.7135)
# No Information Rate : 0.6333          
# P-Value [Acc > NIR] : 0.1847          
# 
# Kappa : 0.2623          
# 
# Mcnemar's Test P-Value : 0.7664          
#                                           
#             Sensitivity : 0.5182          
#             Specificity : 0.7421          
#          Pos Pred Value : 0.5377          
#          Neg Pred Value : 0.7268          
#              Prevalence : 0.3667          
#          Detection Rate : 0.1900          
#    Detection Prevalence : 0.3533          
#       Balanced Accuracy : 0.6301          
#                                           
#        'Positive' Class : Niewiarygodny   



#         6. Predykcja prawdopodobieństw dla klasy "wiarygodny"
pred_probs_full_weighted = predict(model_tree_full_weighted, test_full, type='prob')

#         7. Krzywa ROC + wartość AUC
roc_obj_full_weighted = roc(response = test_full$CreditRisk,
                            predictor = pred_probs_full_weighted[,'Wiarygodny'],
                            levels = c("Wiarygodny", "Niewiarygodny"),
                            direction = ">")
plot(roc_obj_full_weighted, col='steelblue', legacy.axes=T) # Krzywa ROC - Drzewo full
auc(roc_obj_full_weighted)         # Area under the curve: 0.6464

#         8. Istotność zmiennych
model_tree_full_weighted$variable.importance

# Status_Savings    Status            Purpose_Job         Emp_Install       AmountPerAge        Savings 
# 140.4228478       113.8357303       63.9027825          62.6616986        55.7156247          46.6833320 

# Savings2          CreditAmount      CreditHistory       Purpose           MonthlyRate         Housing_Res 
# 43.4766847        36.1317828        35.9685674          32.0464692        30.2707864          29.1435378 

# Duration          InstallmentRate   OtherInstallment2   ResidenceSince    Age                 ExistingCredits 
# 27.5459749        19.6657105        12.6398569          11.7802332        10.0293158          9.2232305 

# Property          Purpose2          Job                 Housing           PersonalStatusSex   Property2 
# 7.4130103         6.4219235         5.6984526           3.7842586         2.9731057           2.9487730 

# ForeignWorker     Employment        OtherDebtors 
# 1.8558409         1.7674676         0.5204834

barplot(model_tree_full_weighted$variable.importance, las=2, col='steelblue', cex.names=0.8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Przycinanie drzewa full z wagmi

# Znalezienie optymalnego complexity parameter
model_tree_full_weighted$cptable

#            CP nsplit rel error    xerror       xstd
# 1  0.36466165      0 1.0000000 1.0000000 0.02906341
# 2  0.03784461      1 0.6353383 0.6699248 0.02698589
# 3  0.03533835      4 0.5218045 0.7105263 0.02740787
# 4  0.02857143      5 0.4864662 0.6691729 0.02697759
# 5  0.02406015      6 0.4578947 0.6676692 0.02696093
# 6  0.01766917      7 0.4338346 0.6593985 0.02686805
# 7  0.01578947      9 0.3984962 0.6578947 0.02685093
# 8  0.01353383     10 0.3827068 0.6631579 0.02691054
# 9  0.01328321     12 0.3556391 0.6729323 0.02701891
# 10 0.01000000     15 0.3157895 0.6759398 0.02705164

plotcp(model_tree_full_weighted)

best_cp_full_weighted = model_tree_full_weighted$cptable[which.min(model_tree_full_weighted$cptable[, 'xerror']), 'CP']
model_tree_full_weighted_pruned = prune(model_tree_full_weighted, cp=best_cp_full_weighted)

# Wizualizacja drzewa full_pruned
rpart.plot(model_tree_full_weighted_pruned, type=4, extra=104, fallen.leaves=T)

# Predykcja na zbiorze testowym
pred_full_weighted_pruned = predict(model_tree_full_weighted_pruned, test_full, type='class')

# Macierz pomyłek
confusionMatrix(pred_full_weighted_pruned, test_full$CreditRisk)

#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            54         42
# Wiarygodny               56        148
# 
# Accuracy : 0.6733          
# 95% CI : (0.6171, 0.7261)
# No Information Rate : 0.6333          
#  P-Value [Acc > NIR] : 0.08332         
# 
# Kappa : 0.2773          
# 
# Mcnemar's Test P-Value : 0.18912         
#                                           
#             Sensitivity : 0.4909          
#             Specificity : 0.7789          
#          Pos Pred Value : 0.5625          
#          Neg Pred Value : 0.7255          
#              Prevalence : 0.3667          
#          Detection Rate : 0.1800          
#    Detection Prevalence : 0.3200          
#       Balanced Accuracy : 0.6349          
#                                           
#        'Positive' Class : Niewiarygodny  

# Krzywa ROC + AUC
# Predykcja  prawdopodobieństw dla klasy "Wiarygodny"
pred_probs_full_weighted_pruned = predict(model_tree_full_weighted_pruned, test_full, type='prob')

roc_obj_full_weighted_pruned = roc(response = test_full$CreditRisk ,
                                   predictor = pred_probs_full_weighted_pruned[,'Wiarygodny'],
                                   levels = c("Wiarygodny", "Niewiarygodny"),
                                   direction =">")

plot(roc_obj_full_weighted_pruned, col='steelblue', legacy.axes=T) # Krzywa ROC - drzewo full_pruned z wagami
auc(roc_obj_full_weighted_pruned)    # Area under the curve: 0.6634

# rpart.rules(model_tree_full_pruned, cover=T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Walidacja krzyżowa drzewa full z wagami

set.seed(123456)

# Nadanie wag
class_weights_full_cv = ifelse(credit_data$CreditRisk == "Niewiarygodny", 1/0.3, 1/0.7)

# Budowa drzewa z CV
model_tree_full_weighted_cv = train(CreditRisk ~ ., 
                           data = credit_data,
                           method = "rpart",
                           metric = "ROC",
                           trControl = cv_control_tree_full,
                           tuneLength = 10,
                           weights = class_weights_full_cv)
print(model_tree_full_weighted_cv)
model_tree_full_cv$results
model_tree_full_cv$bestTune

# Macierz pomyłek
confusionMatrix(data = model_tree_full_weighted_cv$pred$pred,
                reference = model_tree_full_weighted_cv$pred$obs)
#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           211        244
# Wiarygodny               89        456
# 
# Accuracy : 0.667           
# 95% CI : (0.6368, 0.6962)
# No Information Rate : 0.7             
# P-Value [Acc > NIR] : 0.9891          
# 
# Kappa : 0.3091          
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.7033          
#             Specificity : 0.6514          
#          Pos Pred Value : 0.4637          
#          Neg Pred Value : 0.8367          
#              Prevalence : 0.3000          
#          Detection Rate : 0.2110          
#    Detection Prevalence : 0.4550          
#       Balanced Accuracy : 0.6774          
#                                           
#        'Positive' Class : Niewiarygodny

# Krzywa ROC + AUC
roc_obj_tree_full_weighted_cv = roc(response  = model_tree_full_weighted_cv$pred$obs,
                                    predictor = model_tree_full_weighted_cv$pred$Wiarygodny,
                                    levels    = c("Wiarygodny", "Niewiarygodny"),
                                    direction = ">")

plot(roc_obj_tree_full_weighted_cv, col='steelblue', legacy.axes=T)
auc(roc_obj_tree_full_weighted_cv) # Area under the curve: 0.7251


#-------------------------------------------------------------------------------
#     4.7 Drzewo na oczyszczonym zbiorze oraz z wagami


set.seed(123456)

#         1. Zbiór treningowy i testowy
train_index_clean = sample(1:nrow(credit_data_clean), size=0.7*nrow(credit_data_clean))
train_clean = credit_data_clean[train_index_clean,]
test_clean = credit_data_clean[-train_index_clean,]


#         2. Ustalenie wag
class_weights_clean = ifelse(train_clean$CreditRisk == "Niewiarygodny", 1/0.3, 1/0.7)

#         3. Budowa drzewa clean
model_tree_clean_weighted = rpart(CreditRisk ~ ., 
                                 data=train_clean, 
                                 method='class', 
                                 weights = class_weights_clean,
                                 cp=0.01)

#         3. Wizualizacja drzewa
rpart.plot(model_tree_clean_weighted, type=4, extra=104, fallen.leaves=T)

#         4. Predykcja na zbiorze testowym
pred_clean_weighted = predict(model_tree_clean_weighted, test_clean, type='class')

#         5. Macierz pomyłek
confusionMatrix(pred_clean_weighted, test_clean$CreditRisk)
#                             Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            47         43
# Wiarygodny               63        147
# 
# Accuracy : 0.6467          
# 95% CI : (0.5897, 0.7007)
# No Information Rate : 0.6333          
# P-Value [Acc > NIR] : 0.33913         
# 
# Kappa : 0.209           
# 
# Mcnemar's Test P-Value : 0.06497         
#                                           
#             Sensitivity : 0.4273          
#             Specificity : 0.7737          
#          Pos Pred Value : 0.5222          
#          Neg Pred Value : 0.7000          
#             Prevalence : 0.3667          
#          Detection Rate : 0.1567          
#    Detection Prevalence : 0.3000          
#       Balanced Accuracy : 0.6005          
#                                           
#        'Positive' Class : Niewiarygodny    

#         6. Predykcja prawdopodobieństw dla klasy "wiarygodny"
pred_probs_clean_weighted = predict(model_tree_clean_weighted, test_clean, type='prob')

#         7. Krzywa ROC + wartość AUC
roc_obj_clean_weighted = roc(response = test_clean$CreditRisk,
                            predictor = pred_probs_clean_weighted[,'Wiarygodny'],
                            levels = c("Wiarygodny", "Niewiarygodny"),
                            direction = ">")
plot(roc_obj_clean_weighted, col='steelblue', legacy.axes=T) # Krzywa ROC - Drzewo clean z wagami
auc(roc_obj_clean_weighted)         # Area under the curve: 0.6833

#         8. Istotność zmiennych
model_tree_clean_weighted$variable.importance

# Status_Savings  Status                Emp_Install     Purpose_Job       Savings             Savings2 
# 136.9164994     113.8357303           76.8514146      75.1369901        44.8274911          41.9394796 

# Purpose         CreditAmount          Duration        CreditHistory2    Housing_Res         Purpose2 
# 34.2777663      33.5064991            30.5719997      24.6275374        21.9438642          16.7131242 

# Age             InstallmentRate       Employment      CreditHistory     OtherInstallment    Property 
# 11.7757322      11.5653757            11.4020293      8.5469485         8.2841933           7.4130103 

# Property2       OtherInstallment2     Housing         Housing2          OtherDebtors 
# 6.9368250       6.8848363             3.7842586       2.2607259         0.5204834 

barplot(model_tree_clean_weighted$variable.importance, las=2, col='steelblue', cex.names=0.8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Przycinanie drzewa clean z wagmi

# Znalezienie optymalnego complexity parameter
model_tree_clean_weighted$cptable

#           CP nsplit rel error    xerror       xstd
# 1 0.36466165      0 1.0000000 1.0000000 0.02906341
# 2 0.03784461      1 0.6353383 0.6631579 0.02691054
# 3 0.03533835      4 0.5218045 0.7315789 0.02760688
# 4 0.02481203      5 0.4864662 0.7195489 0.02749479
# 5 0.01766917      6 0.4616541 0.6977444 0.02728051
# 6 0.01390977      8 0.4263158 0.7263158 0.02755837
# 7 0.01353383     11 0.3766917 0.7406015 0.02768814
# 8 0.01000000     13 0.3496241 0.7413534 0.02769480
plotcp(model_tree_clean_weighted)

best_cp_clean_weighted = model_tree_clean_weighted$cptable[which.min(model_tree_clean_weighted$cptable[, 'xerror']), 'CP']
model_tree_clean_weighted_pruned = prune(model_tree_clean_weighted, cp=best_cp_clean_weighted)

# Wizualizacja drzewa full_pruned
rpart.plot(model_tree_clean_weighted_pruned, type=4, extra=104, fallen.leaves=T)

# Predykcja na zbiorze testowym
pred_clean_weighted_pruned = predict(model_tree_clean_weighted_pruned, test_clean, type='class')

# Macierz pomyłek
confusionMatrix(pred_clean_weighted_pruned, test_clean$CreditRisk)

#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            84         83
# Wiarygodny               26        107
#
# Accuracy : 0.6367          
# 95% CI : (0.5794, 0.6912)
# No Information Rate : 0.6333          
# P-Value [Acc > NIR] : 0.4782          
# 
# Kappa : 0.2947          
#
# Mcnemar's Test P-Value : 8.148e-08       
#                                           
#             Sensitivity : 0.7636          
#             Specificity : 0.5632          
#          Pos Pred Value : 0.5030          
#          Neg Pred Value : 0.8045          
#              Prevalence : 0.3667          
#          Detection Rate : 0.2800          
#    Detection Prevalence : 0.5567          
#       Balanced Accuracy : 0.6634          
#                                           
#        'Positive' Class : Niewiarygodny  

# Krzywa ROC + AUC
# Predykcja  prawdopodobieństw dla klasy "Wiarygodny"
pred_probs_clean_weighted_pruned = predict(model_tree_clean_weighted_pruned, test_clean, type='prob')

roc_obj_clean_weighted_pruned = roc(response = test_clean$CreditRisk ,
                                   predictor = pred_probs_clean_weighted_pruned[,'Wiarygodny'],
                                   levels = c("Wiarygodny", "Niewiarygodny"),
                                   direction =">")

plot(roc_obj_clean_weighted_pruned, col='steelblue', legacy.axes=T) # Krzywa ROC - clean z wagami
auc(roc_obj_clean_weighted_pruned)    # Area under the curve: 0.6634

# rpart.rules(model_tree_full_pruned, cover=T)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Walidacja krzyżowa drzewa clean z wagami

set.seed(123456)

# Nadanie wag
class_weights_clean_cv = ifelse(credit_data_clean$CreditRisk == "Niewiarygodny", 1/0.3, 1/0.7)

# Budowa drzewa z CV
model_tree_clean_weighted_cv = train(CreditRisk ~ ., 
                                    data = credit_data_clean,
                                    method = "rpart",
                                    metric = "ROC",
                                    trControl = cv_control_tree_full,   # Może zostać użyty
                                    tuneLength = 10,
                                    weights = class_weights_clean_cv)
print(model_tree_clean_weighted_cv)
model_tree_clean_weighted_cv$results
model_tree_clean_weighted_cv$bestTune


# Wizualizacja drzewa model_tree_clean_weighted_cv
best_cp_clean_weighted_cv = model_tree_clean_weighted_cv$bestTune$cp


# Aktywować po model_tree_clean_weighted_cv
model_tree_clean_final = rpart(CreditRisk ~ ., 
                              data = credit_data_clean,
                              method = "class",
                              weights = class_weights_clean_cv,
                              cp = best_cp_clean_weighted_cv)

summary(model_tree_clean_final)
sum(model_tree_clean_final$frame$var == "<leaf>")  # 17 liści
nrow(model_tree_clean_final$frame)                 # 33 węzły
depths_tree_clean_final = rpart:::tree.depth(as.numeric(rownames(model_tree_clean_final$frame)))
max(depths_tree_clean_final)                                       # maksymalna głębokość - 8


#png("drzewo_clean_final.png", width = 2000, height = 1500, res = 300)
rpart.plot(model_tree_clean_final, type = 4, extra = 104, fallen.leaves = TRUE)
#dev.off()

rpart.plot(model_tree_clean_final, type=4, extra=104, fallen.leaves=T)
barplot(model_tree_base_final$variable.importance, las=2, col='steelblue', cex.names=0.8,
        ylab="Spadek indeksu Giniego")

varimp_df = data.frame(
  Zmienna = names(model_tree_clean_final$variable.importance),
  Waznosc = model_tree_clean_final$variable.importance
)

ggplot(varimp_df, aes(x = reorder(Zmienna, Waznosc), y = Waznosc)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +   
  labs(x='', y = "Ważność zmiennych") +
  theme_bw() +
  theme(axis.title = element_text(size=12),
        panel.grid=element_blank())
rm(varimp_df)

model_tree_clean_final$variable.importance
# Status_Savings  Status        Purpose_Job   Emp_Install     Savings           Savings2      Purpose 
# 185.643265      148.066060    120.266356    72.994695       69.845048         61.544907     45.120118 

# Employment      Housing_Res   Duration      CreditAmount    CreditHistory     Purpose2      OtherDebtors 
# 41.429424       32.053877     31.362587     30.336551       30.175675         27.513043     18.006925 

# Age             Property      Property2     ForeignWorker   InstallmentRate 
# 17.765198       6.099321      4.828629      2.361673        1.121795 


# Reguły
rpart.rules(model_tree_clean_final)


# Macierz pomyłek
confusionMatrix(data = model_tree_clean_weighted_cv$pred$pred,
                reference = model_tree_clean_weighted_cv$pred$obs)
#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           203        228
# Wiarygodny               97        472
#
# Accuracy : 0.675         
# 95% CI : (0.645, 0.704)
# No Information Rate : 0.7           
# P-Value [Acc > NIR] : 0.96          
# 
# Kappa : 0.312         
# 
# Mcnemar's Test P-Value : 5.55e-13      
#                                         
#             Sensitivity : 0.6767        
#             Specificity : 0.6743        
#          Pos Pred Value : 0.4710        
#          Neg Pred Value : 0.8295        
#              Prevalence : 0.3000        
#          Detection Rate : 0.2030        
#    Detection Prevalence : 0.4310        
#       Balanced Accuracy : 0.6755        
#                                         
#        'Positive' Class : Niewiarygodny 
                                        

# Krzywa ROC + AUC
roc_obj_tree_clean_weighted_cv = roc(response  = model_tree_clean_weighted_cv$pred$obs,
                                    predictor = model_tree_clean_weighted_cv$pred$Wiarygodny,
                                    levels    = c("Wiarygodny", "Niewiarygodny"),
                                    direction = ">")

plot(roc_obj_tree_clean_weighted_cv, col='steelblue', legacy.axes=T)
auc(roc_obj_tree_clean_weighted_cv) # Area under the curve: 0.7349


#-------------------------------------------------------------------------------
#     4.8 Las losowy (full)

set.seed(123456)

#         1. Generowanie lasu losowego
rf_model_full = randomForest(CreditRisk ~ .,
                             data = train_full,
                             ntree = 500,
                             mtry = 3,
                             importance = T)

#         2. Predykcja klasy na danych testowych
pred_rf_full = predict(rf_model_full, test_full, type='class')

#         3. Macierz pomyłek
confusionMatrix(pred_rf_full, test_full$CreditRisk)
#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            29          3
# Wiarygodny               81        187
# 
# Accuracy : 0.72            
# 95% CI : (0.6655, 0.7701)
# No Information Rate : 0.6333          
# P-Value [Acc > NIR] : 0.0009369       
# 
# Kappa : 0.2913          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.26364         
#             Specificity : 0.98421         
#          Pos Pred Value : 0.90625         
#          Neg Pred Value : 0.69776         
#              Prevalence : 0.36667         
#          Detection Rate : 0.09667         
#    Detection Prevalence : 0.10667         
#       Balanced Accuracy : 0.62392         
#                                           
#        'Positive' Class : Niewiarygodny  

#         4. Predykcja prawdopodobieństw dla klasy "Wiarygodny"
rf_pred_probs_full = predict(rf_model_full, test_full, type='prob')

#         5. Krzywa ROC + AUC
roc_obj_rf_full = roc(response = test_full$CreditRisk,
                      predictor = rf_pred_probs_full[, "Wiarygodny"],
                      levels = c("Wiarygodny", "Niewiarygodny"),
                      direction = ">")
plot(roc_obj_rf_full, col="darkgreen", legacy.axes=T)   # Krzywa ROC - las full
auc(roc_obj_rf_full)    # Area under the curve: 0.7839

#         6. Istotność zmiennych
varImpPlot(rf_model_full, col="steelblue")

importance_vals_rf_full = rf_model_full$importance[,"MeanDecreaseGini"]
importance_vals_rf_full = sort(importance_vals_rf_full, decreasing=T)
importance_vals_rf_full

# Status_Savings      Emp_Install       Purpose_Job         AmountPerAge  CreditAmount        MonthlyRate 
# 26.2137289          24.1826773        21.8222441          15.5128751    15.4968534          15.4632258 

# Status              Duration          Age                 Housing_Res   CreditHistory       Purpose 
# 14.6096070          13.2365683        13.0464115          12.8512475    9.1659889           9.0920569 

# Employment          Property          CreditHistory2      Savings       PersonalStatusSex   InstallmentRate 
# 7.8705298           6.7958140         6.6715059           6.2997539     5.0102663           4.5128075 

# ResidenceSince      Property2         OtherInstallment    Job           ExistingCredits     PersonalStatusSex2 
# 4.4262515           4.1067578         3.6420384           3.5580805     3.5575748           3.4163145 

# Savings2            OtherDebtors      Housing             Purpose2      Telephone           Housing2 
# 3.2024318           2.9526667         2.5763535           2.5709068     2.4152068           1.9631991 

# OtherInstallment2   NumPeopleLiable   ForeignWorker 
# 1.8957958           1.5250924         0.6462777 

barplot(importance_vals_rf_full,
        las=2,
        col="steelblue",
        cex.names=0.8,
        ylab="Istotność zmiennych")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Walidacja krzyżowa dla lasu losowego full

set.seed(123456)

#         1. Kontroler CV - obiekt definujący sposób przeprowadzenia walidacji krzyżowej
#                           (lub innej metody resamplingu) podczas trenowania modelu
#                           za pomocą funkcji train() z pakietu caret
cv_control_rf_full = trainControl(method = 'cv',                        # typ: k-kortna CV
                                  number = 5,                           # 5 korotna corss-validation
                                  classProbs = T,                       # do ROC/AUC
                                  summaryFunction = twoClassSummary,    # ocena wg AUC
                                  savePredictions = T)                  # Zapis predykcji dla wykresów

#         2. Trenowanie modelu z CV
rf_model_full_cv = train(CreditRisk ~ .,
                         data = credit_data,
                         method = 'rf',
                         metric = 'ROC',            # Optymalizacja pod AUC
                         trControl = cv_control_rf_full,
                         ntree = 500,
                         tuneLength = 3)            # Testowanie róznych wartości 'mtry'

#         3. Wyniki
print(rf_model_full_cv)
# Random Forest 
# 
# 1000 samples
# 33 predictor
# 2 classes: 'Niewiarygodny', 'Wiarygodny' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 800, 800, 800, 800, 800 
# Resampling results across tuning parameters:
#   
# mtry  ROC        Sens         Spec     
# 2     0.7585119  0.003333333  1.0000000
# 76    0.7911548  0.450000000  0.9000000
# 151   0.7850714  0.456666667  0.8842857
# 
# ROC was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 76.

plot(rf_model_full_cv)

#         4. Najlepsze metryki
rf_model_full_cv$results
#   mtry       ROC        Sens      Spec      ROCSD     SensSD     SpecSD
# 1    2 0.7585119 0.003333333 1.0000000 0.02884725 0.00745356 0.00000000
# 2   76 0.7911548 0.450000000 0.9000000 0.02531609 0.06009252 0.05075954
# 3  151 0.7850714 0.456666667 0.8842857 0.02052400 0.06302557 0.04021625
rf_model_full_cv$bestTune             # mtry = 76

#         5. Średnia AUC z CV
max(rf_model_full_cv$results$ROC)     # 0.7911548

mean(rf_model_full_cv$results$ROC)    # 0.778246 # Średnia uzyskana z każdego folda

mean(rf_model_full_cv$resample$Sens)  # 0.45
mean(rf_model_full_cv$resample$Spec)  # 0.9


sd(rf_model_full_cv$resample$ROC)     # 0.02531609

# Macierz pomyłek
confusionMatrix(data = rf_model_full_cv$pred$pred[rf_model_full_cv$pred$mtry == rf_model_full_cv$bestTune$mtry],
                reference = rf_model_full_cv$pred$obs[rf_model_full_cv$pred$mtry == rf_model_full_cv$bestTune$mtry],
                positive = "Niewiarygodny")

#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           135         70
# Wiarygodny              165        630
# 
# Accuracy : 0.765          
# 95% CI : (0.7375, 0.791)
# No Information Rate : 0.7            
# P-Value [Acc > NIR] : 2.677e-06      
# 
# Kappa : 0.3848         
# 
# Mcnemar's Test P-Value : 8.684e-10      
#                                          
#             Sensitivity : 0.4500         
#             Specificity : 0.9000         
#          Pos Pred Value : 0.6585         
#          Neg Pred Value : 0.7925         
#              Prevalence : 0.3000         
#          Detection Rate : 0.1350         
#    Detection Prevalence : 0.2050         
#       Balanced Accuracy : 0.6750         
#                                          
#        'Positive' Class : Niewiarygodny


varimp_rf_full_cv = varImp(rf_model_full_cv)$importance

varimp_df = data.frame(Zmienna = rownames(varimp_rf_full_cv),
                       Waznosc = varimp_rf_full_cv$Overall)

varimp_df_top20 = varimp_df[order(-varimp_df$Waznosc), ][1:20, ]

ggplot(varimp_df_top20, aes(x = reorder(Zmienna, Waznosc), y = Waznosc)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # poziomy wykres
  labs(x = '', y = "Ważność zmiennych") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        panel.grid = element_blank())
rm(varimp_df, varimp_df_top20)

roc_obj_rf_full_cv = roc(response  = rf_model_full_cv$pred$obs[rf_model_full_cv$pred$mtry == rf_model_full_cv$bestTune$mtry],
                         predictor = rf_model_full_cv$pred$Wiarygodny[rf_model_full_cv$pred$mtry == rf_model_full_cv$bestTune$mtry],
                         levels    = c("Wiarygodny", "Niewiarygodny"),
                         direction = ">")
plot(roc_obj_rf_full_cv, col="darkgreen", legacy.axes=T)  
auc(roc_obj_rf_full_cv)    # Area under the curve: 0.7875  
                                                            


#-------------------------------------------------------------------------------
#     4.9 Las losowy (full) z wagami

set.seed(123456)

# Nadanie wag
class_weights_rf_full = c("Niewiarygodny"=1/0.3, "Wiarygodny"=1/0.7)

# Buowa modelu z wagami
rf_model_full_weighted = randomForest(CreditRisk ~ .,
                                      data = train_full,
                                      ntree = 500,
                                      mtry = 3,
                                      importance = T,
                                      classwt = class_weights_rf_full)

# Predykcja klas na danych testowych
pred_rf_weighted_full = predict(rf_model_full_weighted, test_full, type = 'class')

# Macierz pomyłek
confusionMatrix(pred_rf_weighted_full, test_full$CreditRisk)
# Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            24          3
# Wiarygodny               86        187
# 
# Accuracy : 0.7033          
# 95% CI : (0.6481, 0.7545)
# No Information Rate : 0.6333          
# P-Value [Acc > NIR] : 0.006429        
# 
# Kappa : 0.2406          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.2182          
#             Specificity : 0.9842          
#          Pos Pred Value : 0.8889          
#          Neg Pred Value : 0.6850          
#              Prevalence : 0.3667          
#          Detection Rate : 0.0800          
#    Detection Prevalence : 0.0900          
#       Balanced Accuracy : 0.6012          
#                                           
#        'Positive' Class : Niewiarygodny

# Predykcja prawdopodobieństw
pred_probs_rf_full_weighted = predict(rf_model_full_weighted, test_full, type='prob')

# Krzywa ROC + AUC
roc_obj_rf_full_weighted = roc(response = test_full$CreditRisk,
                               predictor = pred_probs_rf_full_weighted[,'Wiarygodny'],
                               levels = c('Wiarygodny','Niewiarygodny'),
                               direction = '>')

plot(roc_obj_rf_full_weighted, col = 'darkred', legacy.axes=T)
auc(roc_obj_rf_full_weighted)     # Area under the curve: 0.7627

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Walidacja krzyżowa lasu losowego full z wagami

set.seed(123456)

# 1. Uwstawienie wag
class_weights_rf_weighted_full = class_weights_rf_full

# 2. Kontroler CV
cv_control_rf_weighted_full = trainControl(method  = "cv",
                                           number = 5,
                                           classProbs = T,
                                           summaryFunction = twoClassSummary,
                                           savePredictions = "final")

# 3. Trenowanie modelu z wagami i tuningiem mtry
rf_model_full_weighted_cv = train(CreditRisk ~ .,
                                  data = credit_data,
                                  method = 'rf',
                                  metric = 'ROC',
                                  trControl = cv_control_rf_weighted_full,
                                  ntree = 500,
                                  tuneLength = 5,
                                  weights = ifelse(credit_data$CreditRisk == "Niewiarygodny",
                                                   class_weights_rf_weighted_full["Niewiarygodny"],
                                                   class_weights_rf_weighted_full["Wiarygodny"]))

# 4. Wyniki
print(rf_model_full_weighted_cv)
# Random Forest 
# 
# 1000 samples
# 33 predictor
# 2 classes: 'Niewiarygodny', 'Wiarygodny' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 800, 800, 800, 800, 800 
# Resampling results across tuning parameters:
#   
# mtry  ROC        Sens       Spec     
# 2     0.7596786  0.0000000  1.0000000
# 39    0.7932857  0.4266667  0.9042857
# 76    0.7918690  0.4533333  0.9014286
# 113   0.7885476  0.4500000  0.8857143
# 151   0.7844048  0.4433333  0.8800000
# 
# ROC was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 39.


rf_model_full_weighted_cv$results
rf_model_full_weighted_cv$bestTune

# Macierz pomyłek
confusionMatrix(data = rf_model_full_weighted_cv$pred$pred[rf_model_full_weighted_cv$pred$mtry == rf_model_full_weighted_cv$bestTune$mtry],
                reference = rf_model_full_weighted_cv$pred$obs[rf_model_full_weighted_cv$pred$mtry == rf_model_full_weighted_cv$bestTune$mtry],
                positive = "Niewiarygodny")
# Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           128         69
# Wiarygodny              172        631
# 
# Accuracy : 0.759           
# 95% CI : (0.7313, 0.7852)
# No Information Rate : 0.7             
# P-Value [Acc > NIR] : 1.921e-05       
# 
# Kappa : 0.3638          
# 
# Mcnemar's Test P-Value : 5.018e-11       
#                                           
#             Sensitivity : 0.4267          
#             Specificity : 0.9014          
#          Pos Pred Value : 0.6497          
#          Neg Pred Value : 0.7858          
#              Prevalence : 0.3000          
#          Detection Rate : 0.1280          
#    Detection Prevalence : 0.1970          
#       Balanced Accuracy : 0.6640          
#                                          
#        'Positive' Class : Niewiarygodny 

# 5. Krzywa ROC + AUC
roc_obj_rf_full_weighted_cv = roc(response = rf_model_full_weighted_cv$pred$obs[rf_model_full_weighted_cv$pred$mtry == rf_model_full_weighted_cv$bestTune$mtry],
                                  predictor = rf_model_full_weighted_cv$pred$Wiarygodny[rf_model_full_weighted_cv$pred$mtry == rf_model_full_weighted_cv$bestTune$mtry],
                                  levels = c("Wiarygodny", "Niewiarygodny"),
                                  direction = ">")
plot(roc_obj_rf_full_weighted_cv, col='steelblue', legacy.axes=T)
auc(roc_obj_rf_full_weighted_cv)    # Area under the curve: 0.7906


#-------------------------------------------------------------------------------
#     4.10 Las losowy clean z wagami

set.seed(123456)

# Nadanie wag
class_weights_rf_clean = c("Niewiarygodny"=1/0.3, "Wiarygodny"=1/0.7)

# Buowa modelu z wagami
rf_model_clean_weighted = randomForest(CreditRisk ~ .,
                                      data = train_clean,
                                      ntree = 500,
                                      mtry = 3,
                                      importance = T,
                                      classwt = class_weights_rf_clean)

# Predykcja klas na danych testowych
pred_rf_weighted_clean = predict(rf_model_clean_weighted, test_clean, type = 'class')

# Macierz pomyłek
confusionMatrix(pred_rf_weighted_clean, test_clean$CreditRisk)
#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny            24          3
# Wiarygodny               86        187

# Accuracy : 0.7033          
# 95% CI : (0.6481, 0.7545)
# No Information Rate : 0.6333          
# P-Value [Acc > NIR] : 0.006429        
# 
# Kappa : 0.2406          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.2182          
#             Specificity : 0.9842          
#          Pos Pred Value : 0.8889          
#          Neg Pred Value : 0.6850          
#              Prevalence : 0.3667          
#          Detection Rate : 0.0800          
#    Detection Prevalence : 0.0900          
#       Balanced Accuracy : 0.6012          
#                                           
#        'Positive' Class : Niewiarygodny 

# Predykcja prawdopodobieństw
pred_probs_rf_clean_weighted = predict(rf_model_clean_weighted, test_clean, type='prob')


# Krzywa ROC + AUC
roc_obj_rf_clean_weighted = roc(response = test_clean$CreditRisk,
                                predictor = pred_probs_rf_clean_weighted[,'Wiarygodny'],
                                levels = c('Wiarygodny','Niewiarygodny'),
                                direction = '>')

plot(roc_obj_rf_clean_weighted, col = 'darkred', legacy.axes=T)
auc(roc_obj_rf_clean_weighted)     # Area under the curve: 0.7526

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Walidacja krzyżowa lasu losowego clean z wagami

set.seed(123456)

# 1. Uwstawienie wag
class_weights_rf_weighted_clean = class_weights_rf_clean

# 2. Kontroler CV
cv_control_rf_weighted_clean = trainControl(method  = "cv",
                                            number = 5,
                                            classProbs = T,
                                            summaryFunction = twoClassSummary,
                                            savePredictions = "final")

# 3. Trenowanie modelu z wagami i tuningiem mtry
rf_model_clean_weighted_cv = train(CreditRisk ~ .,
                                   data = credit_data_clean,
                                   method = 'rf',
                                   metric = 'ROC',
                                   trControl = cv_control_rf_weighted_clean,
                                   ntree = 500,
                                   tuneLength = 5,
                                   weights = ifelse(credit_data_clean$CreditRisk == "Niewiarygodny",
                                                   class_weights_rf_weighted_clean["Niewiarygodny"],
                                                   class_weights_rf_weighted_clean["Wiarygodny"]))

# 4. Wyniki
# print(rf_model_clean_weighted_cv)
# Random Forest 
# 
# 1000 samples
# 26 predictor
# 2 classes: 'Niewiarygodny', 'Wiarygodny' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 800, 800, 800, 800, 800 
# Resampling results across tuning parameters:
#   
# mtry  ROC        Sens       Spec     
# 2     0.7514048  0.0000000  1.0000000
# 37    0.7848929  0.4233333  0.8828571
# 72    0.7808571  0.4266667  0.8800000
# 107   0.7795476  0.4533333  0.8742857
# 142   0.7771429  0.4566667  0.8742857
# 
# ROC was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 37.


rf_model_clean_weighted_cv$results
rf_model_clean_weighted_cv$bestTune

# Macierz pomyłek
confusionMatrix(data = rf_model_clean_weighted_cv$pred$pred[rf_model_clean_weighted_cv$pred$mtry == rf_model_clean_weighted_cv$bestTune$mtry],
                reference = rf_model_clean_weighted_cv$pred$obs[rf_model_clean_weighted_cv$pred$mtry == rf_model_clean_weighted_cv$bestTune$mtry],
                positive = "Niewiarygodny")
#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           127         82
# Wiarygodny              173        618
# 
# Accuracy : 0.745           
# 95% CI : (0.7168, 0.7718)
# No Information Rate : 0.7             
# P-Value [Acc > NIR] : 0.0009244       
# 
# Kappa : 0.3352          
# 
# Mcnemar's Test P-Value : 1.74e-08        
#                                           
#             Sensitivity : 0.4233          
#             Specificity : 0.8829          
#          Pos Pred Value : 0.6077          
#          Neg Pred Value : 0.7813          
#              Prevalence : 0.3000          
#          Detection Rate : 0.1270          
#    Detection Prevalence : 0.2090          
#       Balanced Accuracy : 0.6531          
#                                           
#        'Positive' Class : Niewiarygodny 

# 5. Krzywa ROC + AUC
roc_obj_rf_clean_weighted_cv = roc(response = rf_model_clean_weighted_cv$pred$obs[rf_model_clean_weighted_cv$pred$mtry == rf_model_clean_weighted_cv$bestTune$mtry],
                                   predictor = rf_model_clean_weighted_cv$pred$Wiarygodny[rf_model_clean_weighted_cv$pred$mtry == rf_model_clean_weighted_cv$bestTune$mtry],
                                   levels = c("Wiarygodny", "Niewiarygodny"),
                                   direction = ">")

plot(roc_obj_rf_clean_weighted_cv, col='steelblue', legacy.axes=T)
auc(roc_obj_rf_clean_weighted_cv)    # Area under the curve: 0.7814


#-------------------------------------------------------------------------------
#     4.11 Model XGBoost full

set.seed(123456)

# 1. Kontroler cross-validation
xgb_control = trainControl(method = 'cv',
                           number = 5,
                           classProbs = T,
                           summaryFunction = twoClassSummary,
                           savePredictions = 'final')

# 2. Trenowanie modelu XGBoost
#xgb_model_full = train(CreditRisk ~ .,
#                        data = credit_data,
#                        method = 'xgbTree',
#                        trControl = xgb_control,
#                        metric = 'ROC',
#                        tuneLength = 10)

# 3. Wyniki modelu
print(xgb_model_full)
xgb_model_full$results
xgb_model_full$bestTune
#     nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 381      50         2 0.3     0              0.8                1 0.9444444
#     nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 261      50         2 0.3     0              0.6                1 0.8333333

unique(xgb_model_full$pred$nrounds)
unique(xgb_model_full$pred$max_depth)
unique(xgb_model_full$pred$eta)

# Macierz pomyłek
confusionMatrix(data = xgb_model_full$pred$pred,
                reference = xgb_model_full$pred$obs,
                positive = "Niewiarygodny")
#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           132         73
# Wiarygodny              168        627
# 
# Accuracy : 0.759           
# 95% CI : (0.7313, 0.7852)
# No Information Rate : 0.7             
# P-Value [Acc > NIR] : 1.921e-05       
# 
# Kappa : 0.3691          
# 
# Mcnemar's Test P-Value : 1.404e-09       
#                                           
#             Sensitivity : 0.4400          
#             Specificity : 0.8957          
#          Pos Pred Value : 0.6439          
#          Neg Pred Value : 0.7887          
#              Prevalence : 0.3000          
#          Detection Rate : 0.1320          
#    Detection Prevalence : 0.2050          
#       Balanced Accuracy : 0.6679          
#                                           
#       'Positive' Class : Niewiarygodny


# 4. Krzywa ROC + AUC
roc_obj_xgb_full = roc(response = xgb_model_full$pred$obs,
                       predictor = xgb_model_full$pred$Wiarygodny,
                       levels = c("Wiarygodny", "Niewiarygodny"),
                       direction = ">")

plot(roc_obj_xgb_full, col='steelblue', legacy.axes=T)
auc(roc_obj_xgb_full)     # Area under the curve: 0.8059



#-------------------------------------------------------------------------------
#     4.12 Model weighted XGBoost full

set.seed(123456)

# 1. Ustalenie wag
class_weights_xgb_weighted_full = ifelse(credit_data$CreditRisk == "Niewiarygodny",
                                         1 / 0.3,
                                         1 / 0.7)

# 2. Trenowanie modelu XGBoost
#xgb_model_full_weighted = train(CreditRisk ~ .,
#                                data = credit_data,
#                                method = 'xgbTree',
#                                trControl = xgb_control,
#                                metric = 'ROC',
#                                tuneLength = 10,
#                                weights = class_weights_xgb_weighted_full)

# 3. Wyniki modelu
print(xgb_model_full_weighted)
xgb_model_full_weighted$results
xgb_model_full_weighted$bestTune
#     nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 381      50         2 0.3     0              0.8                1 0.9444444
#     nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 142     100         1 0.3     0              0.8                1 0.7222222

# Macierz pomyłek
confusionMatrix(data=xgb_model_full_weighted$pred$pred,
               reference = xgb_model_full_weighted$pred$obs,
               positive = "Niewiarygodny")
#                 Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           220        184
# Wiarygodny               80        516
#
# Accuracy : 0.736           
# 95% CI : (0.7075, 0.7631)
# No Information Rate : 0.7             
# P-Value [Acc > NIR] : 0.006669        
# 
# Kappa : 0.4281          
#
# Mcnemar's Test P-Value : 2.309e-10       
#                                           
#             Sensitivity : 0.7333          
#             Specificity : 0.7371          
#          Pos Pred Value : 0.5446          
#          Neg Pred Value : 0.8658          
#              Prevalence : 0.3000          
#          Detection Rate : 0.2200          
#    Detection Prevalence : 0.4040          
#       Balanced Accuracy : 0.7352          
#                                           
#        'Positive' Class : Niewiarygodny 


# 4. Krzywa ROC + AUC
roc_obj_xgb_full_weighted = roc(response = xgb_model_full_weighted$pred$obs,
                                predictor = xgb_model_full_weighted$pred$Wiarygodny,
                                levels = c("Wiarygodny", "Niewiarygodny"),
                                direction = ">")

plot(roc_obj_xgb_full_weighted, col='steelblue', legacy.axes=T)
auc(roc_obj_xgb_full_weighted)     # Area under the curve: 0.8017


varimp_xgb = varImp(xgb_model_full_weighted)$importance

varimp_xgb$Zmienna = rownames(varimp_xgb)
varimp_xgb = varimp_xgb[order(-varimp_xgb$Overall), ]  # Sortowanie malejąco
varimp_xgb_top20 = head(varimp_xgb, 20)  # 20 najważniejszych

ggplot(varimp_xgb_top20, aes(x = reorder(Zmienna, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = '', y = 'Ważność zmiennych') +
  theme_bw() +
  theme(axis.title = element_text(size=12),
        panel.grid = element_blank())
rm(varimp_xgb, varimp_xgb_top20)




#-------------------------------------------------------------------------------
#     4.13 Model weighted XGBoost clean

set.seed(123456)

# 1. Ustalenie wag
class_weights_xgb_weighted_clean = ifelse(credit_data_clean$CreditRisk == "Niewiarygodny",
                                         1 / 0.3,
                                         1 / 0.7)

# 2. Trenowanie modelu XGBoost
xgb_model_clean_weighted = train(CreditRisk ~ .,
                                data = credit_data_clean,
                                method = 'xgbTree',
                                trControl = xgb_control,
                                metric = 'ROC',
                                tuneLength = 10,
                                weights = class_weights_xgb_weighted_clean)

# 3. Wyniki modelu
print(xgb_model_clean_weighted)
xgb_model_clean_weighted$results
xgb_model_clean_weighted$bestTune
# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 291      50         2 0.3     0              0.6                1         1

# Macierz pomyłek
confusionMatrix(data=xgb_model_clean_weighted$pred$pred,
                reference = xgb_model_clean_weighted$pred$obs,
                positive = "Niewiarygodny")
#               Reference
# Prediction      Niewiarygodny Wiarygodny
# Niewiarygodny           211        191
# Wiarygodny               89        509
# 
# Accuracy : 0.72            
# 95% CI : (0.6911, 0.7476)
# No Information Rate : 0.7             
# P-Value [Acc > NIR] : 0.08858         
# 
# Kappa : 0.3924          
# 
# Mcnemar's Test P-Value : 1.581e-09       
#                                           
#             Sensitivity : 0.7033          
#             Specificity : 0.7271          
#          Pos Pred Value : 0.5249          
#          Neg Pred Value : 0.8512          
#              Prevalence : 0.3000          
#          Detection Rate : 0.2110          
#   Detection Prevalence : 0.4020          
#       Balanced Accuracy : 0.7152          
#                                           
#        'Positive' Class : Niewiarygodny


# 4. Krzywa ROC + AUC
roc_obj_xgb_clean_weighted = roc(response = xgb_model_clean_weighted$pred$obs,
                                predictor = xgb_model_clean_weighted$pred$Wiarygodny,
                                levels = c("Wiarygodny", "Niewiarygodny"),
                                direction = ">")

plot(roc_obj_xgb_clean_weighted, col='steelblue', legacy.axes=T)
auc(roc_obj_xgb_clean_weighted)     # Area under the curve: 0.7973



#===============================================================================
# 5. Porówanie modeli

plot(roc_obj_tree_base_cv,
     col = "blue",
     legacy.axes = TRUE,
     main = "", xlab="1 - Specyficzność", ylab="Czułość",
     lwd = 2)

lines(roc_obj_tree_clean_weighted_cv, col = "orange", lwd = 2)
lines(roc_obj_rf_full_cv, col = "red", lwd = 2)
lines(roc_obj_xgb_full_weighted, col = "green", lwd = 2)

legend("bottomright",
       legend = c("Drzewo bazowe (CV)",
                  "Drzewo z selekcją + wagi (CV)",
                  "Las losowy (CV)",
                  "XGBoost + wagi"),
       col = c("blue", "orange", "red", "green"),
       lwd = 2,
       cex = 0.8)


