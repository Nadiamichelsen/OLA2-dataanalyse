# OLA 2 - Opgave 1

#Opgave 1.1 - Find tabellen og hent data

install.packages("remotes")
remotes::install_github("rOpenGov/dkstat")
library(dkstat)
by_meta <- dst_meta(
  table = "BY3",
  lang = "da"
)

by_meta$variables
by_meta$values
by_meta$values$BYER
by_meta$values$FOLKARTAET
by_meta$values$Tid

by_filter <- list(
  BYER = "*",
  FOLKARTAET = "Folketal",
  Tid = "2026"
)
bydata <- dst_get_data(
  table = "BY3",
  query = by_filter,
  lang = "da"
)
View(bydata)

#Opgave 1.2 - Kategori variabel 

names(bydata)

bydata$bycat <- ifelse(bydata$value < 1000, "landsby",
                       ifelse(bydata$value < 5000, "lille by",
                              ifelse(bydata$value < 20000, "almindelig by",
                                     ifelse(bydata$value < 100000, "større by",
                                            "storby"))))
View(bydata)

#Opgave 1.3 

boliger <- read.csv2("Boligsiden OLA 1.csv")
View(boliger)

names(boliger)
head(boliger)
names(boliger)
names(bydata)

head(bydata$BYER)

#Rense data 

bydata$by <- sub("^[0-9]+ [0-9-]+ ", "", bydata$BYER)
bydata$by <- sub(" \\(.*\\)", "", bydata$by)
bydata$by <- tolower(bydata$by)
head(bydata$by)

bydata$by <- gsub("ø", "oe", bydata$by)
bydata$by <- gsub("æ", "ae", bydata$by)
bydata$by <- gsub("å", "aa", bydata$by)
head(bydata$by)

#Merge  

boliger_ny <- merge(
  boliger,
  bydata,
  by = "by"
)
View(boliger_ny)
names(boliger_ny)
nrow(boliger)
nrow(boliger_ny)

#Lav en mere overskeulig visning af dataframe 

boliger_vis <- boliger_ny[, c("by", "pris", "kvmpris", "bycat")]

View(boliger_vis)

#Opgave 1.4 

table(boliger_ny$bycat) 
library(ggplot2)

ggplot(boliger_ny, aes(x = bycat)) +
  geom_bar()

#Tjek om bolig_ny er merge korrekt 

names(boliger)
names(bydata)
names(boliger_ny)



#Opgave 2.1 – Opdatering af DI’s forbrugertillidsindikator------------
#Opdatér DI’s forbrugertillidsindikator med data frem til og med 2023 fra artiklen ”Forbruget
#fortsætter fremgangen i 2016” (Baum, 2016). Lav vurdering af om forbrugertillidsindikatoren fra DI
#fortsat er bedre end forbrugertillidsindikatoren fra DST. (Hint: I bliver nødt til at nærlæse bilaget for DI-FTI for at finde starttidspunktet for estimationen, spørgsmålene, samt tabel, der sammenligner FTI og DI-FTI)

library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
#Loader data ind-----
#di----------
di<-read_xlsx("ftillid_di.xlsx")
dst<-read_xlsx("ftillid_dst.xlsx")
forbrug<-read_xlsx("pforbrug.xlsx")

maaneder_di <- as.character(
  unlist(di[2, 2:ncol(di)])
)

di_spm <- di[3:6, 2:ncol(di)]

di_spm[] <- lapply(di_spm, as.numeric)

di_maaned <- data.frame(
  maaned = maaneder_di,
  DI_FTI = colMeans(di_spm, na.rm = FALSE)
)
di_kvartal <- di_maaned %>%
  mutate(
    aar = as.numeric(substr(maaned, 1, 4)),
    maaned_nr = as.numeric(substr(maaned, 6, 7)),
    kvartal_nr = ceiling(maaned_nr / 3),
    kvartal = paste0(aar, "K", kvartal_nr)
  ) %>%
  group_by(kvartal) %>%
  summarise(
    DI_FTI = mean(DI_FTI, na.rm = FALSE),
    .groups = "drop"
  )


#dst----------
dst_maaned <- data.frame(
  maaned = as.character(
    unlist(dst[2, 2:ncol(dst)])
  ),
  DST_FTI = as.numeric(
    unlist(dst[3, 2:ncol(dst)])
  )
)
dst_kvartal <- dst_maaned %>%
  mutate(
    aar = as.numeric(substr(maaned, 1, 4)),
    maaned_nr = as.numeric(substr(maaned, 6, 7)),
    kvartal_nr = ceiling(maaned_nr / 3),
    kvartal = paste0(aar, "K", kvartal_nr)
  ) %>%
  group_by(kvartal) %>%
  summarise(
    DST_FTI = mean(DST_FTI, na.rm = FALSE),
    .groups = "drop"
  )

#pforbrug------

kvartaler_forbrug <- as.character(
  unlist(forbrug[2, 4:ncol(forbrug)])
)

forbrug_vaerdier <- as.numeric(
  unlist(forbrug[3, 4:ncol(forbrug)])
)

forbrug_kvartal <- data.frame(
  kvartal = kvartaler_forbrug,
  forbrug = forbrug_vaerdier
)

forbrug_kvartal <- forbrug_kvartal %>%
  mutate(
    aar = as.numeric(substr(kvartal, 1, 4)),
    kvartal_nr = as.numeric(substr(kvartal, 6, 6))
  ) %>%
  arrange(aar, kvartal_nr)
forbrug_kvartal <- forbrug_kvartal %>%
  mutate(
    realvaekst = (forbrug / lag(forbrug, 4) - 1) * 100
  )
analyse_2 <- forbrug_kvartal %>%
  select(kvartal, realvaekst) %>%
  left_join(di_kvartal, by = "kvartal") %>%
  left_join(dst_kvartal, by = "kvartal")

analyse_2 <- analyse_2 %>%
  filter(!is.na(realvaekst)) %>%
  mutate(
    realvaekst = round(realvaekst, 1),
    DI_FTI = round(DI_FTI, 1), #GØR DET PÆNT
    DST_FTI = round(DST_FTI, 1)
  )


#JEG VIL GERNE STARTE MED AT GENSKABE BAUMS FIGUR FOR AT SE VI HAR STYR PÅ METODEN.IKKE VIGTIGT FOR OLA----------

analyse_21 <- analyse_2 %>%
  mutate(
    aar = as.numeric(substr(kvartal, 1, 4)),
    k_nr = as.numeric(substr(kvartal, 6, 6))
  ) %>%
  filter(aar >= 2000, aar <= 2016)

# Baum-perioden: 2000K1 til 2016K2
baum_data <- analyse_2 %>%
  filter(
    kvartal >= "2000K1",
    kvartal <= "2016K2"
  )
png(
  "baum_replikation.png",
  width = 900,
  height = 450,
  res = 120
)
# Data
di_baum <- baum_data$DI_FTI
vaekst_baum <- baum_data$realvaekst

# Baum bruger cirka -25 til 25 på venstre akse
graense <- 25

# Højre akse går cirka fra -8 til 8 %
# 25 nettotal svarer derfor til 8 procent
skala <- 25 / 8

par(
  plt = c(0.10, 0.88, 0.30, 0.70)
)

# Søjlerne først
x_pos <- barplot(
  vaekst_baum * skala,
  ylim = c(-25, 25),
  col = "#2F9DDA",
  border = "white",
  space = 0.10,
  axes = FALSE
)

# Vandrette hjælpelinjer
abline(
  h = c(-25, -17, -8, 0, 8, 17, 25),
  col = "grey80"
)

# DI-linjen ovenpå
par(new = TRUE)

plot(
  x_pos,
  di_baum,
  type = "l",
  col = "grey35",
  lwd = 5,
  ylim = c(-25, 25),
  xlab = "",
  ylab = "",
  xaxt = "n",
  yaxt = "n"
)

# Venstre akse
axis(
  2,
  at = c(-25, -17, -8, 0, 8, 17, 25),
  las = 1
)

mtext(
  "Nettotal",
  side = 2,
  line = 2.5
)

# Højre akse
axis(
  4,
  at = c(-25, -17, -8, 0, 8, 17, 25),
  labels = c("-8", "-5", "-3", "0", "3", "5", "8"),
  las = 1
)

mtext(
  "Pct.",
  side = 4,
  line = 2.5
)

# År på x-aksen
aar_pos <- which(substr(baum_data$kvartal, 6, 6) == "1")

axis(
  1,
  at = x_pos[aar_pos],
  labels = substr(baum_data$kvartal[aar_pos], 3, 4)
)

legend(
  "bottomleft",
  inset = c(0, -0.32),
  xpd = TRUE,
  legend = c(
    "DI's forbrugertillidsindikator",
    "Årlig realvækst pr. kvartal i privatforbruget"
  ),
  col = c("grey35", "#2F9DDA"),
  lwd = c(2.5, NA),
  pch = c(NA, 15),
  bty = "n"
)

box()

title(
  main = "DI's forbrugertillidsindikator følger i højere grad privatforbruget",
  cex.main = 1.1
)

# ... hele plot-koden her ...

dev.off()

#LIGNER MEGET GODT NU
#LM MODEL FOR AT SE OM VI RAMMER FORKLARINGS OG R2 godt

baum_lm<- lm(
  realvaekst ~ DI_FTI,
  data = baum_data
)

summary(baum_lm)
#0.45 R2 Baum 0.54
#0.67 korrelation Baum 0.73
#Den eneste rationelle grund jeg kan finde frem til er at vi bruger 2020 kædede værdier, hvor Baum måske brugte 2015.
cor(
  baum_data$DI_FTI,
  baum_data$realvaekst,
  use = "complete.obs"
)

#IKKE MERE TID PÅ DET


#IGANG MED OPG 2.1 FOR REAL---------
#TJekker for NA
analyse_21_ny <- analyse_2 %>%
  filter(
    !is.na(realvaekst),
    !is.na(DI_FTI),
    !is.na(DST_FTI)
  )

#2 simple lineære regressioner

model_DI_21 <- lm(
  realvaekst ~ DI_FTI,
  data = analyse_21_ny
)

model_DST_21 <- lm(
  realvaekst ~ DST_FTI,
  data = analyse_21_ny
)
summary(model_DI_21)
summary(model_DST_21)


kor_DI <- cor(
  analyse_21_ny$DI_FTI,
  analyse_21_ny$realvaekst
)

kor_DST <- cor(
  analyse_21_ny$DST_FTI,
  analyse_21_ny$realvaekst
)
#Hiver R2 ud
R2_DI <- summary(model_DI_21)$r.squared
R2_DST <- summary(model_DST_21)$r.squared


#samler det i tabel så vi kan sammenligne
sammenligning_21 <- data.frame(
  indikator = c("DI-FTI", "DST-FTI"),
  korrelation = round(c(kor_DI, kor_DST), 3),
  R2 = round(c(R2_DI, R2_DST), 3)
)

sammenligning_21

p_DI <- summary(model_DI_21)$coefficients["DI_FTI", "Pr(>|t|)"]
p_DST <- summary(model_DST_21)$coefficients["DST_FTI", "Pr(>|t|)"]

sammenligning_21$p_vaerdi <- c(p_DI, p_DST)

sammenligning_21
#Den opdaterede analyse viser, at DI’s forbrugertillidsindikator fortsat har en stærkere lineær sammenhæng med den årlige realvækst i privatforbruget end Danmarks Statistiks forbrugertillidsindikator. 
#DI-FTI har en korrelation på 0,617 mod 0,516 for DST-FTI, og forklaringsgraden er 38,1 % mod 26,6 %. 
#Begge modeller er statistisk signifikante, men DI-FTI forklarer en større del af variationen i privatforbruget. 
#Forklaringsgraden er dog lavere end i Baum (2016), hvilket tyder på, at sammenhængen mellem forbrugertillid og privatforbrug er blevet svagere i den udvidede periode.




#PLOTTER DEM SÅ VI KAN SE FORSKELLEN



# DATA TIL PLOTTET

plotdata <- analyse_21_ny

# Hvis du kun vil have en bestemt periode, fx hele datasættet:
plotdata <- plotdata[
  !is.na(plotdata$realvaekst) &
    !is.na(plotdata$DI_FTI) &
    !is.na(plotdata$DST_FTI),
]

# Hvis du kun vil have fx 2000K1 til 2026K2, så behold ovenstående.
# Hvis du kun vil have til 2023K4, kan du filtrere sådan her:
# plotdata <- subset(plotdata, kvartal <= "2023K4")

# Variabler
x_labels <- plotdata$kvartal
di <- plotdata$DI_FTI
dst <- plotdata$DST_FTI
realv <- plotdata$realvaekst


# FASTE AKSE-GRÆNSER SOM BAUM

net_min <- -40
net_max <- 30
pct_min <- -15
pct_max <- 15

# Skaler privatforbrugets pct.-søjler over til venstre akse
scale_factor <- net_max / pct_max
realv_scaled <- realv * scale_factor


# ÅRSMÆRKER PÅ X-AKSEN

# tag kun K1 som mærker på x-aksen
year_pos_index <- grep("K1$", x_labels)

# Lav labels som 00, 01, 02 ... ligesom Baum
year_labels <- substring(x_labels[year_pos_index], 3, 4)


# PLOT

par(
  mar = c(5, 4.5, 4, 4.5),  # margen: bund, venstre, top, højre
  bg = "white"
)

# barplot først så vi får x-positionerne
bar_mid <- barplot(
  realv_scaled,
  col = "#4A90D9",
  border = NA,
  space = 0.15,
  axes = FALSE,
  ylim = c(net_min, net_max),
  xlab = "",
  ylab = "",
  main = "DI's og DST's forbrugertillidsindikator følger privatforbruget"
)

# vandrette hjælpelinjer
abline(h = seq(net_min, net_max, by = 8), col = "grey85", lwd = 0.8)
abline(h = 0, col = "grey55", lwd = 1)

# linjer ovenpå
lines(bar_mid, di, col = "grey25", lwd = 2.5)
lines(bar_mid, dst, col = "red", lwd = 2)

# venstre y-akse = nettotal
axis(
  side = 2,
  at = c(-40, -20, -10, 0, 10, 20, 30),
  labels = c("-40", "-20", "-10", "0", "10", "20", "30"),
  las = 1
)
mtext("Nettotal", side = 2, line = 2.8, cex = 1.1)

# højre y-akse = pct.
axis(
  side = 4,
  at = c(-40, -20, -10, 0, 10, 20, 30),
  labels = c("-15%", "-8%", "-3%", "0%", "3%", "8%", "15%"),
  las = 1
)
mtext("Pct.", side = 4, line = 2.8, cex = 1.1)

# x-akse = år
axis(
  side = 1,
  at = bar_mid[year_pos_index],
  labels = year_labels,
  tick = TRUE,
  line = 0
)

# legend
legend(
  "topleft",
  legend = c(
    "DI's forbrugertillidsindikator",
    "DST's forbrugertillidsindikator",
    "Årlig realvækst i privatforbruget"
  ),
  col = c("grey25", "red", "#4A90D9"),
  lwd = c(3.5, 3.5, NA),
  pch = c(NA, NA, 15),
  pt.cex = 1.6,
  bty = "n",
  cex = 0.95
)

box()



#OPG 2.2----------
#Opgave 2.2 – Forudsigelser af forbruget
#Beregn/forudsig den årlige realvækst i husholdningernes forbrugsudgift for 3. kvartal 2023 med
#henholdsvis DI’s forbrugertillidsindikator og forbrugertillidsindikatoren fra DST.

# Træningsdata: kun kvartaler hvor vi faktisk kender realvæksten
analyse_22_train <- analyse_2 %>%
  filter(
    kvartal <= "2026K2",
    !is.na(realvaekst),
    !is.na(DI_FTI),
    !is.na(DST_FTI)
  )

model_DI_22 <- lm(
  realvaekst ~ DI_FTI,
  data = analyse_22_train
)

model_DST_22 <- lm(
  realvaekst ~ DST_FTI,
  data = analyse_22_train
)

#Henter FTI dataene fra 2026 K3 - Grunden til vi har de data, må være fordi ftillidundersøgelsen er lavet tidligere denne måned
ny_DI_2026K3 <- di_kvartal %>%
  filter(kvartal == "2026K3")

ny_DST_2026K3 <- dst_kvartal %>%
  filter(kvartal == "2026K3")

#laver predictions
pred_DI_2026K3 <- predict(
  model_DI_22,
  newdata = ny_DI_2026K3
)

pred_DST_2026K3 <- predict(
  model_DST_22,
  newdata = ny_DST_2026K3
)

pred_DI_2026K3
pred_DST_2026K3

forudsigelser_22 <- data.frame(
  model = c("DI-FTI", "DST-FTI"),
  forudsagt_realvaekst = round(
    c(pred_DI_2026K3, pred_DST_2026K3),
    2
  )
)

forudsigelser_22
forbrug_2026K3_DI =
  285949 * (1 - 0.21/100)

forbrug_2026K3_DST =
  285949 * (1 - 0.70/100)
#model forudsagt_realvaekst
#1  DI-FTI                -0.21 Dansk Industri
#2 DST-FTI                -0.70 Danmarks Statistik

#Begge modeller forudsiger et mindre fald i husholdningernes realforbrug i 2026K3. 
#DI-FTI-modellen estimerer et fald på ca. 0,21 %, mens DST-FTI-modellen estimerer et større fald på ca. 0,70 %.

#forbruget i 2026K3 ifølge vores ftillid indikatorer er altså:
#DI forudsiger 285.348kr. og regnet årlig realvækst fra 2025k3
#DST forudsiger 283.947kr. og regnet årlig realvækst fra 2025k3
#Forbruget i 2026K2 var 287.852kr. 






#OPG 2.3-------------
#For at svare ordentligt sætter vi vores argumentation op. 


#Først - Det er meget små negative% så hvor præcis er modellerne overhovedet?

sigma(model_DI_22) #2,085 pp fra den forudsagte linje
sigma(model_DST_22) #2,26 pp

#Det fortæller os en del. Modellen har tidligere afviget ca. 2pp men har forudsagt et fald på 0,21% så den normale afvigelse er langt større end hvad vi har forudsagt 2026k3
#Altså -0,21% er et rigtig småt signal ift. den normale usikkerhed for modellen

#prediction intervaller - for at vise hvor den højeste pred og laveste pred er 
predict(
  model_DI_22,
  newdata = ny_DI_2026K3,
  interval = "prediction",
  level = 0.95
)
#fit       lwr      upr
#1 -0.2128781 -4.383845 3.958089
predict(
  model_DST_22,
  newdata = ny_DST_2026K3,
  interval = "prediction",
  level = 0.95
)
#fit       lwr      upr
#1 -0.6990068 -5.268333 3.870319

#denne viser os altså at der i forvejen er rimelig store udsving i nedre og øvre grænser for vores forudsigelse.
#Nedre grænse siger at der er 2,5% chance for at 2026k3 går under -4-5% og øvre grænse at der er 2,5% chance for at den går over 3-4% 
#altså høje tal ift. -0,21 og -0,69


#Derudover vores R2 som viser os at 
#DI:  ca. 62 % ikke forklaret ikke at tage noget fra modellen men vi kan altså ikke regne 100% med predictionsne
#DST: ca. 73 % ikke forklaret

#Derfor er konklusionen at vi ikke er bekymrede for virksomhedens salg til hr og fru danmark. Sker faldet, er det skidt, men et meget lille fald. 
#Det bekymringen kan være er at der er 95% chance for at det kan være en udvikling på alt mellem -5.6% og +3.9% 
#Får vi fald på 5.6% er jeg meget bekymret for virksomhedens salg
#Får vi stigning på 3.9% er jeg meget positiv om virksomhedens salg




#OPG 2.4------------
#Opgave 2.4 – Prognoser fra DI og Nationalbanken
#Hvor stor realvækst i privatforbruget forventer DI og Nationalbanken i deres seneste prognoser?
#Sammenhold deres tal med jeres svar i opgave 2.3.


# VI har fundet NB og DI prognoser. NB har til 2028 og DI til 2027.
#Nu laver vi prognoser på FTillid tal først til 2028. Derefter laver vi vores egne prognoser på privatforbruget. Og til sidst plotter vi det hele så vi kan se forskellen.

#laver dataframes hvor vi har ftillid tal i måneder
di_forecast_data <- di_maaned %>%
  mutate(
    tid = 1:n()
  )

dst_forecast_data <- dst_maaned %>%
  mutate(
    tid = 1:n()
  )

model_DI_FTI <- lm(
  DI_FTI ~ tid,
  data = di_forecast_data #VI bruger tid som x variabel for at kortlægge den fremtidige udviklingsretning. I det her tilfælde siger modellen hvor meget ftillid udvikler sig pr. måned fordi "TID" er hver eneste måned fra 2000MO1 til 2026MO9
)
summary(model_DI_FTI)
model_DST_FTI <- lm(
  DST_FTI ~ tid,
  data = dst_forecast_data
)

antal_frem <- 27   # okt. 2026 til dec. 2028 

fremtid_tid <- data.frame(
  tid = (nrow(di_forecast_data) + 1):
    (nrow(di_forecast_data) + antal_frem) #de fremtidige "TID" variabler altså 27 frem
)

fremtid_tid$DI_FTI <- predict(
  model_DI_FTI,
  newdata = fremtid_tid
)

fremtid_tid$DST_FTI <- predict(
  model_DST_FTI,
  newdata = fremtid_tid
)
#modelbaseret fremskrivning normalt er ftillid meget kojunkturfølsomt

#Lave måned til kvartal
fremtid_tid$maaned <- sprintf(
  "%dM%02d",
  rep(c(2026, 2027, 2028), c(3, 12, 12)),
  c(10:12, 1:12, 1:12)
)

fti_fremtid_kvartal <- fremtid_tid %>%
  mutate(
    aar = as.numeric(substr(maaned, 1, 4)),
    maaned_nr = as.numeric(substr(maaned, 6, 7)), #RENT KOLONNEARBEJDE FOR AT GØRE KVARTAL KOLONNE FLOT
    kvartal_nr = ceiling(maaned_nr / 3),
    kvartal = paste0(aar, "K", kvartal_nr)
  ) %>%
  group_by(kvartal) %>%
  summarise(
    DI_FTI = mean(DI_FTI), #FÅ ÅRS GNMSNIT
    DST_FTI = mean(DST_FTI),
    .groups = "drop"
  )


fti_fremtid_kvartal$vaekst_DI <- predict(
  model_DI_22,
  newdata = fti_fremtid_kvartal
)

fti_fremtid_kvartal$vaekst_DST <- predict(
  model_DST_22,
  newdata = fti_fremtid_kvartal
)

#Samler vækstprognoserne

forecast_kvartaler <- bind_rows(
  
  data.frame(
    kvartal = "2026K3",
    vaekst_DI = as.numeric(pred_DI_2026K3),
    vaekst_DST = as.numeric(pred_DST_2026K3)
  ),
  
  fti_fremtid_kvartal %>%
    select(
      kvartal,
      vaekst_DI,
      vaekst_DST
    )
)

lav_forbrugsprognose <- function(vaekst_variabel) {
  
  # Faktisk privatforbrug frem til seneste kendte kvartal
  niveau <- forbrug_kvartal %>%
    select(kvartal, forbrug) %>%
    filter(kvartal <= "2026K2")
  
  
  # Beregn hvert fremtidigt kvartal
  for (k in forecast_kvartaler$kvartal) {
    
    aar <- as.numeric(substr(k, 1, 4))
    kvartal_nr <- substr(k, 6, 6)
    
    # Samme kvartal året før
    sidste_aar <- paste0(
      aar - 1,
      "K",
      kvartal_nr
    )
    
    # Forbrugsniveau året før
    basis <- niveau$forbrug[
      niveau$kvartal == sidste_aar
    ]
    
    # Forudsagt år-til-år-vækst
    vaekst <- forecast_kvartaler[
      forecast_kvartaler$kvartal == k,
      vaekst_variabel
    ][[1]] #Fordi så siger vi ignorer alt andet bare giv mig først tal i række 1
    
    # Beregn nyt forbrugsniveau
    nyt_forbrug <- basis *
      (1 + vaekst / 100)
    
    niveau <- bind_rows(
      niveau,
      data.frame(
        kvartal = k,
        forbrug = nyt_forbrug
      )
    )
  }
  
  niveau
}

forbrug_DI <- lav_forbrugsprognose(
  "vaekst_DI"
)

forbrug_DST <- lav_forbrugsprognose(
  "vaekst_DST"
)


#Summerer vores kvartaler til år
aar_DI <- forbrug_DI %>%
  mutate(
    aar = as.numeric(
      substr(kvartal, 1, 4)
    )
  ) %>%
  group_by(aar) %>%
  summarise(
    aarsforbrug = sum(forbrug),
    .groups = "drop"
  ) %>%
  arrange(aar) %>%
  mutate(
    aarsvaekst =
      (aarsforbrug / lag(aarsforbrug) - 1) * 100
  )

aar_DST <- forbrug_DST %>%
  mutate(
    aar = as.numeric(
      substr(kvartal, 1, 4)
    )
  ) %>%
  group_by(aar) %>%
  summarise(
    aarsforbrug = sum(forbrug),
    .groups = "drop"
  ) %>%
  arrange(aar) %>%
  mutate(
    aarsvaekst =
      (aarsforbrug / lag(aarsforbrug) - 1) * 100
  )

aar_DI %>%
  filter(aar >= 2026)

aar_DST %>%
  filter(aar >= 2026)

sammenligning_24 <- data.frame( #SÆTTER DI DST OG NB OG DIPROGNOSE SAMMEN I EN DATAFRAME SÅ VI KAN LAVE PLOT
  aar = rep(c(2026, 2027, 2028), each = 4),
  prognose = rep(
    c(
      "Vores DI-model",
      "Vores DST-model",
      "DI",
      "Nationalbanken"
    ),
    times = 3
  ),
  vaekst = c(
    0.88, 0.77, 2.9, 1.8,
    -0.01, 0.05, 2.5, 2.1,
    -0.11, -0.04, NA, 2.0
  )
)

ggplot(
  sammenligning_24,
  aes(
    x = factor(aar),
    y = vaekst,
    fill = prognose
  )
) +
  
  # Søjler
  geom_col(
    position = position_dodge2(
      width = 0.8,
      preserve = "single"
    ),
    width = 0.7,
    na.rm = TRUE
  ) +
  
  # 0-linje
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 0.5
  ) +
  
  # Labels ovenpå/under søjlerne
  geom_text(
    aes(
      label = ifelse(
        is.na(vaekst),
        "",
        sprintf("%.1f%%", vaekst)
      ),
      vjust = ifelse(
        vaekst < 0,
        1.4,
        -0.4
      )
    ),
    position = position_dodge2(
      width = 0.8,
      preserve = "single"
    ),
    size = 4,
    fontface = "bold",
    na.rm = TRUE
  ) +
  
  # Farver
  scale_fill_manual(
    values = c(
      "DI" = "#E65100",
      "Nationalbanken" = "#1B5E20",
      "Vores DI-model" = "#0288D1",
      "Vores DST-model" = "#6A1B9A"
    )
  ) +
  
  # Y-akse
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(-0.5, 3.5),
    breaks = seq(-0.5, 3.5, by = 0.5)
  ) +
  
  # Titler
  labs(
    title = "Prognoser for realvæksten i privatforbruget",
    subtitle = "Sammenligning af egne modeller, DI og Nationalbanken",
    x = "År",
    y = "Årlig realvækst",
    fill = "Prognose",
    caption =
      "Anm.: Egne modeller bygger på fremskrevet DI-FTI og DST-FTI samt simple lineære regressionsmodeller.\nDI har ikke oplyst en prognose for 2028 i den anvendte kilde.\nKilde: Egne beregninger, DI prognose: Den private del af væksten aftager i de kommende år og Dansk økonomi
trodser global uro ."
  ) +
  
  # Design
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      color = "gray30",
      margin = margin(b = 12)
    ),
    
    plot.caption = element_text(
      hjust = 0,
      color = "gray40",
      size = 9,
      margin = margin(t = 15)
    ),
    
    plot.caption.position = "plot",
    
    legend.position = "bottom",
    legend.title = element_blank(),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
#Selve privatforbrugsmodellen ser korrekt ud. Den store forskel i forhold til DI og Nationalbanken skyldes primært, 
#at vores simple fremskrivning af forbrugertilliden holder DI-FTI og DST-FTI på et vedvarende lavt og negativt niveau gennem 2027 og 2028. 
#Når disse værdier indsættes i regressionsmodellerne, giver det naturligt vækst tæt på nul.


#DI forventer en realvækst i privatforbruget på 2,9 % i 2026 og 2,5 % i 2027. Nationalbanken forventer 1,8 % i 2026, 2,1 % i 2027 og 2,0 % i 2028. 
#Disse prognoser er væsentligt mere positive end vores egne modelbaserede fremskrivninger, som viser omkring 0,8 % vækst i 2026, omtrent nulvækst i 2027 og svagt negativ vækst i 2028. 
#Forskellen hænger sammen med, at vores modeller alene bygger på forbrugertillid, mens DI og Nationalbanken inddrager flere økonomiske forhold. 
#Samlet set understøtter de eksterne prognoser derfor vores vurdering fra opgave 2.3 om, at der ikke er grund til stor bekymring for virksomhedernes salg til husholdningerne resten af året, selv om vores egne modeller peger på svagere udvikling.






#Opgave 3 – Optimeringsalgoritme, forudsigelser og forklaringsgrad


#Opgave 3.1 – Modellens forudsigelser-----------
#Med udgangspunkt i jeres besvarelse i opgave 2, bedes I beregne jeres estimerede værdier for den
#kvartalsvise årlige vækstrate i husholdningernes forbrug. (hint: I skal gange jeres estimerede koefficienter med x-variablene fra den estimerede model).

#SÅ vi skal gange vores "Estimate" fra lm modellen med x-værdierne

#Altså hardcoded
analyse_3 <- analyse_22_train %>%
  mutate(
    estimeret_DI =
      2.18516 + 0.18019 * DI_FTI,
    
    estimeret_DST =
      1.39561 + 0.15142 * DST_FTI
  )


#Og ellers 

analyse_3 <- analyse_22_train %>%
  mutate(
    estimeret_DI =
      coef(model_DI_22)[1] +
      coef(model_DI_22)[2] * DI_FTI,
    
    estimeret_DST =
      coef(model_DST_22)[1] +
      coef(model_DST_22)[2] * DST_FTI
  )
analyse_3

#VI kan tjekke om vi får samme tal som R's egne

head(analyse_3$estimeret_DI)
head(fitted(model_DI_22))

head(analyse_3$estimeret_DST)
head(fitted(model_DST_22))
#gir præcis det samme

#Dette r fordi vores regressioner har lavet nogle ligninger fx. realvaekst = 2.18516 + 0.18019 * DI_FTI
#så fx 2000k1 siger ftillid er -3.7 så siger modellen realvaekst = 2.18516 + 0.18019 * -3.7 og så får vi vores estimerede værdier. 
#det modellen faktisk siger er, grundet at di_fti var -3.7 ville jeg ifølge min ligning estimere at din vækst er 1.5184%




#Opgave 3.2 – Residualer------------
#Med udgangspunkt i jeres besvarelse i opgave 3.1, bedes I beregne residualer for henholdsvis DI’s
#og DST’s forbrugertillidsindikator og plot disse i forhold til jeres forudsagte resultater fra opgave
#3.1 for de to modeller


analyse_3 <- analyse_3 %>%
  mutate(
    residual_DI = realvaekst - estimeret_DI,
    residual_DST = realvaekst - estimeret_DST
  )
#FAKTISK VÆRDI - ESTIMERET VÆRDI

analyse_3$residual_DI[1]
#FX. 2000K1 [1] -1.818455

#Nu plotter vi ift. vores forudsagte resultater

#Dansk industri
ggplot(
  analyse_3,
  aes(
    x = estimeret_DI,
    y = residual_DI
  )
) +
  geom_point(
    alpha = 0.7,
    size = 2
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  labs(
    title = "Residualplot for DI-modellen",
    subtitle = "Residualer i forhold til modellens estimerede værdier",
    x = "Estimeret årlig realvækst (%)",
    y = "Residual (procentpoint)"
  ) +
  theme_minimal(base_size = 13)

#OG for DST
ggplot(
  analyse_3,
  aes(
    x = estimeret_DST,
    y = residual_DST
  )
) +
  geom_point(
    alpha = 0.7,
    size = 2
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  labs(
    title = "Residualplot for DST-modellen",
    subtitle = "Residualer i forhold til modellens estimerede værdier",
    x = "Estimeret årlig realvækst (%)",
    y = "Residual (procentpoint)"
  ) +
  theme_minimal(base_size = 13)

#Viser bare at mine beregninger i analyse_3 giver det samme som R's beregninger i lm modellen
head(analyse_3$residual_DI)
head(residuals(model_DI_22))

head(analyse_3$residual_DST)
head(residuals(model_DST_22))
#Residualerne er beregnet som forskellen mellem den faktiske og den estimerede årlige realvækst. 

#For begge modeller ligger residualerne overordnet spredt omkring nul, hvilket tyder på, at modellerne ikke systematisk over- eller undervurderer væksten. 
#Der er dog enkelte store residualer, som viser, at modellerne i visse kvartaler rammer relativt langt fra den faktiske udvikling. 
#DI-modellen har en lidt lavere residual standard error end DST-modellen, henholdsvis ca. 2,09 og 2,27 procentpoint, hvilket indikerer en lidt mindre gennemsnitlig spredning omkring regressionslinjen.

sigma(model_DI_22)
sigma(model_DST_22)




#Opgave 3.3 – RSS og TSS -----------
#Opgaverne hænger sammen da jeg faktisk tror 3.3 og 3.4 bare er én opgave. 3.3 er en fejl at den er i dokumentet
#Opgave 3.4 – Forklaringsgraden 
#Opstil ligningen for forklaringsgraden og brug denne til at beregne forklaringsgraden for jeres
#model i opgave 2.

#Da teksten fra 3.2 ved en fejl er kopieret ind i 3.3 opgaven, vælger vi at gå ud fra overskriften og lege lidt med RSS og TSS
#med det samme data som i 3.1

RSS_DI <- sum(analyse_3$residual_DI^2)
RSS_DST <- sum(analyse_3$residual_DST^2)

RSS_DI #[1] 452.011
RSS_DST #[1] 535.8871
#modellens residualer kvadreret altså hvor meget uforklaret variation der er tilbage i modellen efter vi har kørt vores lm

TSS <- sum(
  (analyse_3$realvaekst - mean(analyse_3$realvaekst))^2
)

TSS #[1] 730.5343
#den samlede variation i den faktiske realvækst "Y" omkring gennemsnittet af realvæksten i privatforbug

#Faktisk er der noget sjovt, som faktisk giver mening når man tænker over det
#TSS er det samme for både DST og DI fordi TSS regnes på y-værdien som i dette tilfælde er privatforbruget og dette er det samme for begge


#R2 beregninger ud fra RSS og TSS fordi 1- RSS/TSS 
#altså hvor langt modellens estimeringer ligger fra de faktiske værdier, sammenlignet med den samlede variation i privatforbruget omkring gnmsnittet.
R2_DI <- 1 - RSS_DI / TSS
R2_DST <- 1 - RSS_DST / TSS

R2_DI #[1] 0.3812597 forklarer 38% af variationen i den årlige realvækst
R2_DST #[1] 0.266445 forklarer 26% af variationen i den årlige realvækst. 

#RSS viser os alt det modellen ikke har forklaret
#TSS viser os alt det modellen har forklaret + alt det modellen ikke har forklaret altså den samlede variation om den er forklaret eller ej

#Derfor giver 1- RSS/TSS R2 fordi R2 så må være alt vi har forklaret når det uforklarede er trukket fra
#Altså RSS/TSS i sig selv giver alt uforklarede variation i vores model. Når vi siger 1- RSS/TSS får vi alt det forklarede variation altså eks. 38% i DI.
#Desto lavere RSS desto bedre R2


#------------------Opgave 4 – Forbrug og forbrugertillidsindikatorer fra DST og DI, samt loops i lister-------------

#---------Opgave 4.1 – Illustration af forbrugertillid----------

#Hent data for forbrugertillidsundersøgelsen fra januar 1996 til i dag og omregn jeres data til
#kvartaler. Lav en grafisk illustration af jeres omregnede data for DST’s forbrugertillidsindikator og
#kommentér på, hvornår de danske forbrugere er mest og mindst optimistiske.

library(readxl)
ftillid<-read_excel("forbrugertillid.xlsx")

#extracter månederne
maaned <- as.character(
  unlist(ftillid[2, 2:ncol(ftillid)])
)
#spørgsmålene extractes
sporgsmaal <- ftillid[
  3:14,
  2:ncol(ftillid)
]

# Gør værdierne numeriske
sporgsmaal[] <- lapply(
  sporgsmaal,
  as.numeric
)

# Vender datasættet, så månederne bliver rækker
ftillid_maaned <- data.frame(
  maaned = maaned,
  t(sporgsmaal)
)

# Extracter året
ftillid_maaned$aar <- substr(
  ftillid_maaned$maaned,
  1,
  4
)

# Extracter månedsnummeret
ftillid_maaned$maanedsnr <- as.numeric(
  substr(
    ftillid_maaned$maaned,
    6,
    7
  )
)
# Omregner måned til kvartalsnummer
ftillid_maaned$kvartalsnr <- ceiling(
  ftillid_maaned$maanedsnr / 3
)

# Laver kvartalsnavn
ftillid_maaned$kvartal <- paste0(
  ftillid_maaned$aar,
  "K",
  ftillid_maaned$kvartalsnr
)

# Omregner de 12 månedlige indikatorer til kvartalsgennemsnit
ftillid_kvartal <- aggregate(
  ftillid_maaned[, 2:13],
  by = list(kvartal = ftillid_maaned$kvartal),
  FUN = mean,
  na.rm = TRUE
)
#1decimal
ftillid_kvartal[, -1] <- round(
  ftillid_kvartal[, -1],
  1
)


# 1. Laver et HELT NYT langt datasæt
library(tidyr)
ftillid_long2 <- ftillid_kvartal %>%
  pivot_longer(
    cols = X1:X12,
    names_to = "spm",
    values_to = "vaerdi"
  )

# Sørger for rigtig rækkefølge X1 -> X12
ftillid_long2$spm <- factor(
  ftillid_long2$spm,
  levels = paste0("X", 1:12)
)

# Nummererer kvartalerne 1, 2, 3...
ftillid_long2$kvartal_nr <- match(
  ftillid_long2$kvartal,
  ftillid_kvartal$kvartal
)


# 2. Titler til de 12 små grafer

spm_labels <- c(
  X1  = "Familiens økonomiske situation i dag,\nsammenlignet med for et år siden",
  
  X2  = "Familiens økonomiske situation om et år,\nsammenlignet med i dag",
  
  X3  = "Danmarks økonomiske situation i dag,\nsammenlignet med for et år siden",
  
  X4  = "Danmarks økonomiske situation om et år,\nsammenlignet med i dag",
  
  X5  = "Anskaffelse af større forbrugsgoder,\nfordelagtigt for øjeblikket",
  
  X6  = "Priser i dag,\nsammenlignet med for et år siden",
  
  X7  = "Priser om et år,\nsammenlignet med i dag",
  
  X8  = "Arbejdsløsheden om et år,\nsammenlignet med i dag",
  
  X9  = "Anskaffelse af større forbrugsgoder,\ninden for de næste 12 mdr.",
  
  X10 = "Anser det som fornuftigt at spare op\ni den nuværende økonomiske situation",
  
  X11 = "Regner med at kunne spare op\ni de kommende 12 måneder",
  
  X12 = "Familiens økonomiske situation lige nu:\nkan spare / penge slår til /\nbruger mere end man tjener"
)

# 3. Finder højeste og laveste værdi
#    for hvert spørgsmål

hoejeste2 <- ftillid_long2 %>%
  group_by(spm) %>%
  slice_max(
    vaerdi,
    n = 1,
    with_ties = FALSE
  )

laveste2 <- ftillid_long2 %>%
  group_by(spm) %>%
  slice_min(
    vaerdi,
    n = 1,
    with_ties = FALSE
  )


# --------------------------------------------------

# 4. Årstal på x-aksen

#    Kun hvert andet år så den kan læses

# --------------------------------------------------

aar_kvartaler <- ftillid_kvartal$kvartal[
  grepl("K1$", ftillid_kvartal$kvartal) &
    as.numeric(
      substr(ftillid_kvartal$kvartal, 1, 4)
    ) %% 2 == 0
]

aar_breaks <- match(
  aar_kvartaler,
  ftillid_kvartal$kvartal
)

aar_labels <- substr(
  aar_kvartaler,
  1,
  4
)

ftillid_kvartal$kvartal_nr <- 1:nrow(ftillid_kvartal)

# --------------------------------------------------

# 5. GRAF

# --------------------------------------------------
library(ggplot2)
ggplot(
  ftillid_long2,
  aes(
    x = kvartal_nr,
    y = vaerdi
  )
) +
  
  # Nul-linje
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.6,
    color = "grey50"
  ) +
  
  # Udviklingen
  geom_line(
    linewidth = 1,
    color = "#0072B2"
  ) +
  
  # Højeste værdi
  geom_point(
    data = hoejeste2,
    color = "#009E73",
    size = 2.7
  ) +
  
  # Laveste værdi
  geom_point(
    data = laveste2,
    color = "#D55E00",
    size = 2.7
  ) +
  
  # Kvartal ved højeste værdi
  geom_text(
    data = hoejeste2,
    aes(label = kvartal),
    color = "#007A5E",
    fontface = "bold",
    size = 2.7,
    vjust = -0.8
  ) +
  
  # Kvartal ved laveste værdi
  geom_text(
    data = laveste2,
    aes(label = kvartal),
    color = "#B34700",
    fontface = "bold",
    size = 2.7,
    vjust = 1.4
  ) +
  
  # 12 separate grafer
  facet_wrap(
    ~ spm,
    ncol = 3,
    scales = "free_y",
    labeller = as_labeller(spm_labels),
    axes = "all_x",
    axis.labels = "all_x"
  ) +
  
  # Årstal
  scale_x_continuous(
    breaks = aar_breaks,
    labels = aar_labels
  ) +
  
  labs(
    title = "Forbrugertillidsundersøgelsen i Danmark",
    subtitle = "Kvartalsdata fra 1996 til i dag",
    x = "År",
    y = "Nettotal",
    caption = "Grøn prik = højeste nettotal  •  Orange prik = laveste nettotal"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 7,
      face = "bold"
    ),
    
    axis.text.y = element_text(
      size = 9,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    
    strip.text = element_text(
      size = 9,
      face = "bold"
    ),
    
    plot.title = element_text(
      size = 20,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 11
    ),
    
    plot.caption = element_text(
      size = 9,
      face = "italic"
    ),
    
    panel.spacing = unit(
      1.2,
      "lines"
    )
  )
#Mangler data fra Anser det som fornuftigt at spare op i den nuværende økonomiske situation fra 2023-2026
#De kvartalsvise data viser betydelige udsving i danskernes forbrugertillid siden 1996. 
#Flere af indikatorerne peger på relativt høj optimisme i midten af 00'erne, hvor både vurderingen af familiens egen økonomi og Danmarks økonomiske situation lå på høje niveauer.
#Omkring 2008-2009 ses et markant fald i flere indikatorer samtidig, hvilket viser en tydelig forværring i forbrugernes økonomiske forventninger. Herefter sker der gradvist en forbedring gennem store dele af 2010'erne.
#Den mest markante negative periode ses omkring 2022-2023. Her falder vurderingen af familiens økonomi kraftigt, lysten til at anskaffe større forbrugsgoder svækkes, og indikatorerne for prisudviklingen stiger markant. Det tyder samlet på meget lav forbrugeroptimisme i denne periode.
#Efterfølgende ses en vis genopretning, men flere indikatorer ligger fortsat svagere end i de mest optimistiske perioder. Det er dog vigtigt, at de 12 spørgsmål ikke alle fortolkes i samme retning. Eksempelvis er en høj værdi for oplevede prisstigninger ikke i sig selv udtryk for højere optimisme.

#Det bør også nævnes at fx "priser om et år sammenlginet med i dag", "arbejdsløshed" "priser i dag sammenlignet med for et år siden"
#Alle ikke er ligeså lettilgåelige som de andre. En stigning i priser om et år sammenlignet med i dag er ikke nødvendigvis positivt, faktisk negativt. Da det vil blive dyrere at leve. 
#Derudover arbejdsløshed regner folk med arbejdsløshed stiger er det dårligt fordi færre danskere er i arbejde.


#----VI SAMLER DE 12 SPG FOR AT SE HVORNÅR DANSKERNE ER MEST OPTIMISTISKE OG PESSIMISTISKE----

#beregner gennemsnit for de 5 spørgsmål inkluderet i DST forbrugertillidsindikatoren for at få deres mål for hvornår folk er mest pessi/optimistiske

ftillid_kvartal$femspg <- rowMeans(
  ftillid_kvartal[, 2:6],
  na.rm = TRUE
)

# Afrunder til 1 decimal
ftillid_kvartal$femspg <- round(
  ftillid_kvartal$femspg,
  1
)

# Højeste samlede gennemsnit
hoejeste_samlet <- ftillid_kvartal[
  which.max(ftillid_kvartal$femspg),
]

# Laveste samlede gennemsnit
laveste_samlet <- ftillid_kvartal[
  which.min(ftillid_kvartal$femspg),
]

max_row <- ftillid_kvartal[
  which.max(ftillid_kvartal$femspg),
]

min_row <- ftillid_kvartal[
  which.min(ftillid_kvartal$femspg),
]


ggplot(
  ftillid_kvartal,
  aes(
    x = kvartal_nr,
    y = femspg
  )
) +
  
  # Nul-linje
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    color = "grey50"
  ) +
  
  # Udviklingen
  geom_line(
    linewidth = 1.2,
    color = "#0072B2"
  ) +
  
  # Højeste kvartal
  geom_point(
    data = max_row,
    color = "#009E73",
    size = 4
  ) +
  
  geom_text(
    data = max_row,
    aes(
      label = paste0(
        kvartal,
        "\n",
        femspg
      )
    ),
    color = "#007A5E",
    fontface = "bold",
    size = 4,
    vjust = -0.8
  ) +
  
  # Laveste kvartal
  geom_point(
    data = min_row,
    color = "#D55E00",
    size = 4
  ) +
  
  geom_text(
    data = min_row,
    aes(
      label = paste0(
        kvartal,
        "\n",
        femspg
      )
    ),
    color = "#B34700",
    fontface = "bold",
    size = 4,
    vjust = 1.5
  ) +
  
  # Årstal
  scale_x_continuous(
    breaks = aar_breaks,
    labels = aar_labels
  ) +
  
  labs(
    title = "Samlet udvikling i forbrugertillidsundersøgelsen",
    subtitle = "Gennemsnit af de 5 spørgsmål pr. kvartal, 1996 til i dag",
    x = "År",
    y = "Gennemsnitligt nettotal",
    caption = "Grøn = højeste gennemsnit  •  Orange = laveste gennemsnit"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 11,
      face = "bold"
    ),
    
    axis.text.y = element_text(
      size = 11,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 14,
      face = "bold"
    ),
    
    plot.title = element_text(
      size = 21,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 12
    ),
    
    plot.caption = element_text(
      size = 10,
      face = "italic"
    )
  )

#Følger faktisk nogenlunde samme udvikling som for alle 12 spørgsmål. Denne er værdierne dog langt lavere, vi er i lang tid under 0, hvor vi aldrig krydser 0 i den totale undersøgelse


# Beregner gennemsnittet af alle 12 spørgsmål
# for hvert kvartal

ftillid_kvartal$samlet_gennemsnit <- rowMeans(
  ftillid_kvartal[, 2:13],
  na.rm = TRUE
)

# Afrunder til 1 decimal
ftillid_kvartal$samlet_gennemsnit <- round(
  ftillid_kvartal$samlet_gennemsnit,
  1
)

# Højeste samlede gennemsnit
hoejeste_samlet <- ftillid_kvartal[
  which.max(ftillid_kvartal$samlet_gennemsnit),
]

# Laveste samlede gennemsnit
laveste_samlet <- ftillid_kvartal[
  which.min(ftillid_kvartal$samlet_gennemsnit),
]

hoejeste_samlet
laveste_samlet

library(ggplot2)

# Laver et nummer til kvartalerne
ftillid_kvartal$kvartal_nr <- 1:nrow(ftillid_kvartal)

# Årstal på x-aksen - hvert andet år
aar_kvartaler <- ftillid_kvartal$kvartal[
  grepl("K1$", ftillid_kvartal$kvartal) &
    as.numeric(
      substr(ftillid_kvartal$kvartal, 1, 4)
    ) %% 2 == 0
]

aar_breaks <- match(
  aar_kvartaler,
  ftillid_kvartal$kvartal
)

aar_labels <- substr(
  aar_kvartaler,
  1,
  4
)


# Finder højeste og laveste punkt til grafen
max_row <- ftillid_kvartal[
  which.max(ftillid_kvartal$samlet_gennemsnit),
]

min_row <- ftillid_kvartal[
  which.min(ftillid_kvartal$samlet_gennemsnit),
]


# Graf
ggplot(
  ftillid_kvartal,
  aes(
    x = kvartal_nr,
    y = samlet_gennemsnit
  )
) +
  
  # Nul-linje
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    color = "grey50"
  ) +
  
  # Udviklingen
  geom_line(
    linewidth = 1.2,
    color = "#0072B2"
  ) +
  
  # Højeste kvartal
  geom_point(
    data = max_row,
    color = "#009E73",
    size = 4
  ) +
  
  geom_text(
    data = max_row,
    aes(
      label = paste0(
        kvartal,
        "\n",
        samlet_gennemsnit
      )
    ),
    color = "#007A5E",
    fontface = "bold",
    size = 4,
    vjust = -0.8
  ) +
  
  # Laveste kvartal
  geom_point(
    data = min_row,
    color = "#D55E00",
    size = 4
  ) +
  
  geom_text(
    data = min_row,
    aes(
      label = paste0(
        kvartal,
        "\n",
        samlet_gennemsnit
      )
    ),
    color = "#B34700",
    fontface = "bold",
    size = 4,
    vjust = 1.5
  ) +
  
  # Årstal
  scale_x_continuous(
    breaks = aar_breaks,
    labels = aar_labels
  ) +
  
  labs(
    title = "Samlet udvikling i forbrugertillidsundersøgelsen",
    subtitle = "Gennemsnit af alle 12 spørgsmål pr. kvartal, 1996 til i dag",
    x = "År",
    y = "Gennemsnitligt nettotal",
    caption = "Grøn = højeste gennemsnit  •  Orange = laveste gennemsnit"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 11,
      face = "bold"
    ),
    
    axis.text.y = element_text(
      size = 11,
      face = "bold"
    ),
    
    axis.title = element_text(
      size = 14,
      face = "bold"
    ),
    
    plot.title = element_text(
      size = 21,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 12
    ),
    
    plot.caption = element_text(
      size = 10,
      face = "italic"
    )
  )

#Den samlede graf viser, at gennemsnittet på tværs af de 12 spørgsmål var højest omkring 2008K2 med 15,2 og lavest i 2022K4 med 1,6. Der ses også tydelige fald omkring finanskrisen i 2008-2009, coronaperioden og især i 2022-2023. Grafen peger derfor på, at forbrugernes samlede vurderinger var særligt svage i slutningen af 2022.
#Gennemsnittet skal dog fortolkes med forsigtighed, fordi alle 12 spørgsmål ikke har samme retning. Et højt nettotal for fx prisforventninger kan betyde, at forbrugerne forventer højere priser, hvilket ikke nødvendigvis er positivt. Tilsvarende kan et højt nettotal for forventet arbejdsløshed være udtryk for, at flere forventer 
#stigende arbejdsløshed, altså noget negativt. Grafen viser derfor bedst det samlede gennemsnitlige niveau i undersøgelsen, men ikke et rent mål for optimisme.



#----------Opgave 4.2 – Gennemsnit af underspørgsmål------------
#Beregn gennemsnittet for underspørgsmålet ”Set i lyset af den økonomiske situation, mener du, at
#det for øjeblikket er fordelagtigt at anskaffe større forbrugsgoder som fjernsyn, vaskemaskine eller
#lignende, eller er det bedre at vente?” for perioden 1. kvartal 2000 til og med 3. kvartal 2023.
#Vurdér jeres resultat set i forhold til spørgsmålet og svarmulighederne. (Hint: giver resultatet analytisk mening?)


# Udvælger perioden 2000K1 til og med 2023K3

x5_periode <- ftillid_kvartal[
  ftillid_kvartal$kvartal >= "2000K1" &
    ftillid_kvartal$kvartal <= "2023K3",
  c("kvartal", "X5")
]

# Kontrol
head(x5_periode)
tail(x5_periode)

# Der skal være 95 kvartaler
nrow(x5_periode)
# Gennemsnit for X5 i hele perioden

gennemsnit_x5 <- mean(
  x5_periode$X5,
  na.rm = TRUE
)

# Afrundes til 1 decimal
gennemsnit_x5 <- round(
  gennemsnit_x5,
  1
)

gennemsnit_x5 #[1] -10

# Gennemsnittet for perioden 2000K1-2023K3 er -10 nettotal.
# Resultatet giver analytisk mening som et gennemsnitligt niveau for indikatoren.
# Det viser, at vurderingerne over perioden samlet set har hældt i retning af,
# at det var bedre at vente med større forbrugskøb end at købe med det samme.
#
# -10 er dog ikke en konkret svarmulighed eller et gennemsnitligt personsvar,
# men et gennemsnit af indikatorens nettotal over tid.
# Desuden skjuler gennemsnittet store forskelle mellem de enkelte kvartaler.


#---------Opgave 4.3 – De 11 grupper af forbrug-------
#Hent data for de 11 grupper af forbrug blandt husholdningerne. Hvad brugte danskerne flest penge
#på i 2022? Hvilken gruppe af forbruget steg mest fra 2020 til 2023? (hint: I kan ikke lægge kvartalerne sammen, når I har kædede værdier)


#FØRST OG FREMMEST----JEG HENTER DATA FRA DST NAHC021 MED 15GRUPPERINGER DA 11GRUPPERING BLEV AFSLUTETT I 2023. DERUDOVER HENTER JEG ET DATASÆT I LØBENDE PRISER OG 1 I KÆDEDE PRISER
#DETTE GØR JEG FORDI LØBENDE PRISER SKAL BRUGES TIL AT SVARE PÅ "Hvad brugte danskerne flest penge på i 2022?" FOR AT VI KAN SE HVAD DANSKERNE FAKTISK BRUGTE FLEST KRONER PÅ I 2022 ER VI NØDT TIL AT HENTE I LØBENDE PRISER FOR AT FÅ DET RIGTIGE SVAR.
#DA VI DOG SKAL MÅLE HVAD DER STIGER MEST FRA 2020 TIL 2025 ER VI NØDT TIL AT BRUGE KÆDEDE VÆRDIER. DETTE SKYLDES AT VI IKKE KAN MÅLE EN STIGNING MED LØBENDE PRISER, FORDI DER VIL VÆRE INFLATION INDBLANDET. HER SKAL VI HOLDE PRISSTIGNINGER PÅ 0 SÅ DE IKKE INTERFERER MED SELVE PFORBRUG STIGNING
#VI BRUGER NAHC021 SÅ VI IKKE SKAL BEKYMRE OS OM KVARTALER KAN ADDERES, DA DETTE DATASÆT ER I ÅRSTAL.


#Først laver vi "Hvad brugte danskerne flest penge på i 2022?"--------------------

library(readxl)
loebende<-read_xlsx("lbnpris.xlsx")
head(loebende)

library(dplyr)

loebende_2022 <- loebende %>%
  # Fjerner de to øverste metadata-rækker
  slice(-c(1, 2)) %>%
  # Beholder kun formål og værdien for 2022
  select(2, 3)
# Giver kolonnerne ordentlige navne
names(loebende_2022) <- c("formaal", "forbrug_2022")

# Gør forbrug til numerisk
loebende_2022 <- loebende_2022 %>%
  mutate(forbrug_2022 = as.numeric(forbrug_2022))
#sortere fra størst til mindst #arrange Desc er descending order altså fra størst til mindst
loebende_2022 %>%
  arrange(desc(forbrug_2022))
#VI kan allerede se det med denne, men nu tager vi bare lige den største ud

loebende_2022 %>%
  slice_max(forbrug_2022, n = 1)
#1 Boligbenyttelse       283171 mio. kr.

#Danskerne brugte altså flest penge på boligbenyttelse i 2022. Dette indebærer Husleje, elektricitet, gas, fjernvarme og andet brændsel, Ejendomsskatter og reparation af boligen.
#Dernæst fødevarer, friti, drift a køretøjer osv..

#Vi laver søjlediagram med y akse der både viser procent af total privatforbrug samt mio kr. for de 15 grupper

library(ggplot2)
library(scales)

# Rens data: tag rækkerne med de 15 grupper
loebende_2022 <- loebende[3:nrow(loebende), c(2, 3)]

# Giv kolonnerne ordentlige navne
names(loebende_2022) <- c("gruppe", "mio_kr")

# Gør forbrugskolonnen numerisk
loebende_2022$mio_kr <- as.numeric(loebende_2022$mio_kr)

# Fjern eventuelle tomme rækker
loebende_2022 <- loebende_2022[
  !is.na(loebende_2022$gruppe) &
    !is.na(loebende_2022$mio_kr),
]

# Beregn totalforbrug som summen af de 15 grupper
total_forbrug <- sum(loebende_2022$mio_kr)

# Beregn andel i procent
loebende_2022$andel_pct <- loebende_2022$mio_kr / total_forbrug * 100

# Sortér fra størst til mindst
loebende_2022 <- loebende_2022[order(loebende_2022$andel_pct, decreasing = TRUE), ]

# Gør gruppe til faktor så rækkefølgen bevares i plottet
loebende_2022$gruppe <- factor(
  loebende_2022$gruppe,
  levels = loebende_2022$gruppe
)

# Plot
ggplot(loebende_2022, aes(x = gruppe, y = andel_pct)) +
  geom_col(fill = "#EE4B2B") +
  
  geom_text(
    aes(label = paste0(round(andel_pct, 1), "%")),
    vjust = -0.4,
    size = 3.5
  ) +
  
  scale_y_continuous(
    name = "Andel af samlet privatforbrug (%)",
    
    #venstre akse
    breaks=seq(0, 25, by=2.5),
    labels=function(x) paste0(x,"%"),
    
    #Højre akse
    sec.axis = sec_axis(
      ~ . * total_forbrug / 100/ 1000,
      name = "Forbrug i 2022 (mia. kr.)",
      breaks=seq(0, 300, by=20),
      labels = function(x) round(x, 0)
    )
  ) +
  
  labs(
    title = "Husholdningernes forbrug fordelt på 15 grupper i 2022",
    subtitle = "Søjlerne viser andel af samlet privatforbrug – venstre akse i %, højre akse i mia. kr.",
    x = "Forbrugsgruppe"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.left = element_text(face = "bold"),
    axis.title.y.right = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )


















#Nu laver vi "Hvilken gruppe af forbruget steg mest fra 2020 til 2023?" --------------
library(readxl)

keadede<-read_xlsx("keadpris.xlsx")
head(keadede)

#RENSER DATA
keadede_clean <- keadede %>%
  # Fjerner de to metadata-rækker
  slice(-c(1, 2)) %>%
  
  # Fjerner kolonnen med "2020-priser, kædede værdier"
  select(2:9)

# Giver kolonnerne rigtige navne
names(keadede_clean) <- c(
  "formaal",
  "2019",
  "2020",
  "2021",
  "2022",
  "2023",
  "2024",
  "2025"
)

#gør forbrug numerisk
keadede_clean <- keadede_clean %>%
  mutate(
    across(
      `2019`:`2025`,
      as.numeric
    )
  )

#Regner procentvis stigning
keadede_clean <- keadede_clean %>%
  mutate(
    vaekst_2020_2025 =
      (`2025` / `2020` - 1) * 100
  )


#Størst til mindst igen
keadede_clean %>%
  arrange(desc(vaekst_2020_2025)) %>%
  select(formaal, `2020`, `2025`, vaekst_2020_2025)

#Afrunder decimaler
keadede_clean <- keadede_clean %>%
  mutate(
    vaekst_2020_2025 = round(vaekst_2020_2025, 1)
  )

#og som svar på opgaven

keadede_clean %>%
  slice_max(vaekst_2020_2025, n = 1) %>%
  select(formaal, `2020`, `2025`, vaekst_2020_2025)
#formaal                      `2020` `2025` vaekst_2020_2025
# 1 Restauranter og hoteller  45211  68419             51.3
#51,3% stigning fra 2020 til 2025. Giver god mening, Danmark åbner op igen, så 2020 "restauranter og hoteller" må have været helt i bund så "stigningen" ser også vildere ud. Turisme + indelands gåning på restaurant og hotel boomer.
#denne post steg også næsten 2x af nr. 2 som er fritid og sport på 27% stigning.

#laver værdien for ændring i mia kr.

keadede_clean <- keadede_clean %>%
  mutate(
    vaekst_2020_2025 = (`2025` / `2020` - 1) * 100,
    aendring_mia = (`2025` - `2020`) / 1000
  )

#Fjerner NA værdier
keadede_clean <- keadede_clean %>%
  filter(
    !is.na(formaal),
    !is.na(`2020`),
    !is.na(`2025`)
  )


#Plotter udviklignen for hver post fra 2020 til 2025

ggplot(
  keadede_clean,
  aes(
    x = reorder(formaal, vaekst_2020_2025),
    y = vaekst_2020_2025,
    fill = formaal == "Restauranter og hoteller"
  )
) +
  geom_col() +
  
  # Tekst på positive søjler - til højre for søjlen
  geom_text(
    data = keadede_clean %>%
      filter(vaekst_2020_2025 >= 0),
    aes(
      y = vaekst_2020_2025 + 0.4,
      label = paste0(
        round(vaekst_2020_2025, 1), "%\n",
        "+", round(aendring_mia, 1), " mia. kr."
      )
    ),
    hjust = 0,
    size = 3.3
  ) +
  
  # Tekst på negative søjler - inde i søjlen
  geom_text(
    data = keadede_clean %>%
      filter(vaekst_2020_2025 < 0),
    aes(
      y = vaekst_2020_2025 + 0.5,
      label = paste0(
        round(vaekst_2020_2025, 1), "%\n",
        round(aendring_mia, 1), " mia. kr."
      )
    ),
    hjust = 0,
    size = 3.3
  ) +
  
  coord_flip() +
  
  scale_fill_manual(
    values = c(
      "FALSE" = "#FF7F82",
      "TRUE" = "#F04B2F"
    ),
    guide = "none"
  ) +
  
  # X-aksen efter coord_flip:
  # mærker for hver 5 procentpoint
  scale_y_continuous(
    breaks = seq(-15, 60, by = 5),
    labels = function(x) paste0(x, "%"),
    limits = c(-15, 60)
  ) +
  
  labs(
    title = "Udviklingen i husholdningernes forbrug fra 2020 til 2025",
    subtitle = "Procentvis real vækst i kædede 2020-priser",
    x = NULL,
    y = "Ændring fra 2020 til 2025",
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    
    # Lidt ekstra plads omkring figuren
    plot.margin = margin(
      t = 10,
      r = 30,
      b = 10,
      l = 10
    )
  )


#Nu regner vi hvornår restaurant og hotel udviklingen skete
#Nupper rækken med res og hotel
restaurant <- keadede_clean %>%
  filter(formaal == "Restauranter og hoteller")
#vi vil have den i langt format istedet for bredt

library(tidyr)

restaurant_aar <- restaurant %>%
  select(`2019`:`2025`) %>%
  pivot_longer(
    cols = everything(),
    names_to = "aar",
    values_to = "forbrug"
  ) %>%
  mutate(
    aar = as.numeric(aar),
    
    # Årlig procentvis vækst
    aarlig_vaekst = (forbrug / lag(forbrug) - 1) * 100,
    
    # Gør værdien til mia. kr.
    forbrug_mia = forbrug / 1000
  )
#VI får en NA i 2019 fordi 19 ikke er relevant år, bruges udelukkende til at regne årlig vækst i 2020.

ggplot(
  restaurant_aar %>% filter(!is.na(aarlig_vaekst)),
  aes(
    x = factor(aar),
    y = aarlig_vaekst,
    fill = aarlig_vaekst == max(aarlig_vaekst, na.rm = TRUE)
  )
) +
  geom_col(width = 0.7) +
  
  geom_hline(
    yintercept = 0,
    linewidth = 0.5
  ) +
  
  geom_text(
    aes(
      label = paste0(
        round(aarlig_vaekst, 1),
        "%\n",
        round(forbrug_mia, 1),
        " mia."
      )
    ),
    vjust = ifelse(
      restaurant_aar$aarlig_vaekst[-1] >= 0,
      -0.4,
      1.3
    ),
    size = 3.6
  ) +
  
  scale_fill_manual(
    values = c(
      "FALSE" = "#FF7F82",
      "TRUE" = "#F04B2F"
    ),
    guide = "none"
  ) +
  
  labs(
    title = "Hvornår steg forbruget på restauranter og hoteller mest?",
    subtitle = "Årlig real vækst i husholdningernes forbrug, 2020–2025",
    x = "År",
    y = "Årlig vækst (%)",
    caption = "Beløbet under procenten viser forbrugets niveau i mia. kr. i kædede 2020-priser"
  ) +
  scale_y_continuous(
    breaks = seq(-35, 35, by = 5),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.12)) #Gør bare lige y aksen lidt højere
  )+
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )
#Her bliver det tydeligt med vores tidligere kommentarer om at 2020 må have startet rigtig lavt fordi landet var lukket ned. Og rigtigt nok. Fra 2019 til 2020 faldt forbruget i res og hotel 32,8%.
#Det gør også at året efter hvor mange ting var åbnet op igen havde vi allerede stigning på 16,5% og året efter, ved total genåbning af landet også for turister kom stigningen på 28%. 
#Det allermest interessante er at efter 2022, faldt forbruget faktisk og 24/25 var stigningen under 3%. Det viser også at stigningen primært skyldes det store fald i 2020. Det er altså ikke nødvendigvis et "reelt" boom i res og hotel branchen.





#Opgave 4.4 Lav 22 simple lineære regressioner mellem hver af de 11 grupper i forbruget (y-variable) og-----------
#henholdsvis forbrugertillidsindikatoren fra DST og DI. I skal gemme summary i 22 lister. I skal
#lave jeres regressioner fra 1. kvartal 2000 til og med 2. kvartal 2023.




#Først renser vi data for DI datasæt--------------

keade_lm<-read_xlsx("kead_lm.xlsx")
ftillid_dst<-read_xlsx("ftillid_DST.xlsx")
ftillid_di<-read_xlsx("ftillid_DI.xlsx")

head(keade_lm)


#Vi renser DI dataene først så det bliver kvartaler


library(dplyr)
library(tidyr)

# Tager månederne fra række 2
maaneder <- as.character(
  unlist(ftillid_di[2, 2:ncol(ftillid_di)])
)

# Tager de fire DI-spørgsmål
di_spm <- ftillid_di[3:6, 2:ncol(ftillid_di)]

# Gør svarene numeriske
di_spm[] <- lapply(di_spm, as.numeric)

# Simpelt gennemsnit af de fire spørgsmål for hver måned
di_maaned <- data.frame(
  maaned = maaneder,
  DI_FTI = colMeans(di_spm, na.rm = TRUE)
)

#Nu har vi fået stillet det flot 

#Nu laver vi det til kvartaler


di_maaned <- di_maaned %>% # %>% betyder bare "og bagefter"
  mutate( #funktion i dplyr der skaber nye kolonner eller generelt bare ændrer i dit datasæt
    aar = as.numeric(substr(maaned, 1, 4)), #trækker årstallet ud og gør det numerisk
    maaned_nr = as.numeric(substr(maaned, 6, 7)), #samme bare med månedsnr
    kvartal_nr = ceiling(maaned_nr / 3), #tage månednr og divider med 3 og runder op til nærmeste tal. Så fx 2/3 0,66 = K1 4/3 0,33 = K2. Altså den runder ALTID OP. 
    kvartal = paste0(aar, "K", kvartal_nr) #klistrer årstallet vi hiver ud sammen med "K"+knr. som vi regner i ceiling
  )

di_kvartal <- di_maaned %>%
  group_by(kvartal) %>% #Grupperer vvores kvartaler så alle 2000K1 er sammen alle 2000K2 er sammen og der er jo 3 rækker i hver
  summarise( 
    DI_FTI = mean(DI_FTI, na.rm = TRUE) #summerer rækkerne så de bliver til én så vi får én række med ****K* ***** som et kvartal
  ) #mean gnms. na.rm ignorerer NA værdier 

di_kvartal <- di_kvartal %>%
  mutate(
    DI_FTI = round(DI_FTI, 1) #Fjerner alle de grimme decimaler
  )




#DONE med DI





#DSt oprydning--------------

#her behøves vi ikke engang aggregere noget vi har bare taget ftillidindikatoren samlet for de 5 spm. 
#vi skal bare samle månederne i kvartaler



dst_maaned <- data.frame(
  maaned = as.character(
    unlist(ftillid_dst[2, 2:ncol(ftillid_dst)]) #tager alt fra kolonne 2 hen til sidste kolonne for at springe første kolonne over. Unlist betyder bare tager værdierne ud af cellerne og stiller dem på en lang række
  ),
  DST_FTI = as.numeric(
    unlist(ftillid_dst[3, 2:ncol(ftillid_dst)]) #samme som før men fr akolonne 3 da ftillid starter her
  )
)

dst_maaned <- dst_maaned %>%
  mutate(
    aar = as.numeric(substr(maaned, 1, 4)),
    maaned_nr = as.numeric(substr(maaned, 6, 7)), #samme som DI
    kvartal_nr = ceiling(maaned_nr / 3),
    kvartal = paste0(aar, "K", kvartal_nr)
  )

dst_kvartal <- dst_maaned %>%
  group_by(kvartal) %>% #Samme som DI
  summarise(
    DST_FTI = mean(DST_FTI, na.rm = TRUE)
  )

dst_kvartal <- dst_kvartal %>%
  mutate(
    DST_FTI = round(DST_FTI, 1) #Fjerner alle de grimme decimaler
  )

#REnse pforbrug på de 15 grupper -----------

head(keade_lm)

# Kvartalsnavnene ligger i række 2 fra kolonne 4 og frem
kvartaler <- as.character(
  unlist(keade_lm[2, 4:ncol(keade_lm)])
)

# Behold forbrugsgruppe + alle kvartaler
forbrug_lm <- keade_lm[3:nrow(keade_lm), c(3, 4:ncol(keade_lm))]

# Giv første kolonne navn
names(forbrug_lm)[1] <- "formaal"

# Giv resten af kolonnerne deres rigtige kvartalsnavne
names(forbrug_lm)[-1] <- kvartaler

#Laver langt format istedet for bredt så vi kan lave ggplot og så vi bare kan group_by for at samle "formaal" og køre regression på hver 

forbrug_lm <- forbrug_lm %>%
  pivot_longer(
    cols = -formaal,
    names_to = "kvartal",
    values_to = "forbrug"
  ) %>%
  mutate(
    forbrug = as.numeric(forbrug)
  ) %>%
  filter(
    !is.na(formaal),
    !is.na(forbrug) 
  )


forbrug_lm <- forbrug_lm %>%
  mutate(
    aar = as.numeric(substr(kvartal, 1, 4)), #ligger år ind på ny kolonne
    kvartal_nr = as.numeric(substr(kvartal, 6, 6)) #ligger kvartals nr ind på ny kolonne
  ) %>%
  arrange(formaal, aar, kvartal_nr)



#så regner vi realvækst pr. kvartal

forbrug_lm <- forbrug_lm %>%
  group_by(formaal) %>%
  arrange(aar, kvartal_nr, .by_group = TRUE) %>%
  mutate(
    aarlig_vaekst = (forbrug / lag(forbrug, 4) - 1) * 100
  ) %>%
  ungroup()

forbrug_lm <- forbrug_lm %>%
  filter(
    kvartal >= "2000K1",
    kvartal <= "2025K4"
  )
#vil kun have med hvor tal faktisk bliver realvækst

forbrug_lm <- forbrug_lm %>%
  mutate(
    aarlig_vaekst = round(aarlig_vaekst, 1) #Fjerner alle de grimme decimaler
  )

head(di_kvartal)









#Kører regressioner-------

#sætter de 3 datasæt sammen
di_dst_forbrug<-forbrug_lm %>%
  left_join(dst_kvartal, by = "kvartal") %>%
  left_join(di_kvartal, by = "kvartal")

#for en sikkerhedsskyld fjerner NA'er
di_dst_forbrug <- di_dst_forbrug %>%
  filter(
    !is.na(aarlig_vaekst),
    !is.na(DST_FTI),
    !is.na(DI_FTI)
  )
femten_forbrug <- split(di_dst_forbrug, di_dst_forbrug$formaal)


#15 reg med DST data
lm_dst <- lapply(
  femten_forbrug,
  function(data) {
    lm(aarlig_vaekst ~ DST_FTI, data = data)
  }
)


#med DI data

lm_di <- lapply(
  femten_forbrug,
  function(data) {
    lm(aarlig_vaekst ~ DI_FTI, data = data)
  }
)

dst_lm<-lapply(lm_dst, summary) #15 summaries
di_lm<-lapply(lm_di, summary) #15 summaries



#en liste med dem alle sammen 30 simple lineære regressioner

alle_summaries <- c(
  setNames(
    dst_lm,
    paste0(names(dst_lm), " - DST")
  ),
  setNames(
    di_lm,
    paste0(names(di_lm), " - DI")
  )
)





#Laver listen pænere, så det bliver mere som en tabel----------
library(dplyr)
#Laver det til en tabel ud fra lm listerne
resultater <- bind_rows(
  lapply(names(lm_dst), function(navn) {
    
    s <- summary(lm_dst[[navn]])
    
    data.frame(
      formaal = navn,
      indikator = "DST",
      beta = s$coefficients["DST_FTI", "Estimate"], #beta er altså koefficienten
      p_vaerdi = s$coefficients["DST_FTI", "Pr(>|t|)"],
      R2 = s$r.squared,
      adj_R2 = s$adj.r.squared
    )
  }),
  
  lapply(names(lm_di), function(navn) {
    
    s <- summary(lm_di[[navn]])
    
    data.frame(
      formaal = navn,
      indikator = "DI",
      beta = s$coefficients["DI_FTI", "Estimate"],
      p_vaerdi = s$coefficients["DI_FTI", "Pr(>|t|)"],
      R2 = s$r.squared,
      adj_R2 = s$adj.r.squared
    )
  })
)

#Gør den pænere med færre decimaler
resultater <- resultater %>%
  mutate(
    beta = round(beta, 3),
    p_vaerdi = round(p_vaerdi, 4),
    R2 = round(R2, 3),
    adj_R2 = round(adj_R2, 3)
  )
#Nu gør vi så hver forbrugsgruppe bliver sat op head to head mellem dst og di
resultater_bred <- resultater %>%
  select(formaal, indikator, beta, p_vaerdi, R2) %>%
  pivot_wider(
    names_from = indikator,
    values_from = c(beta, p_vaerdi, R2)
  )

#Og så får vi lige stjerne på p værdien ligesom R selv gør

resultater <- resultater %>%
  mutate(
    signifikans = case_when(
      p_vaerdi < 0.001 ~ "***",
      p_vaerdi < 0.01  ~ "**",
      p_vaerdi < 0.05  ~ "*",
      p_vaerdi < 0.10  ~ ".",
      TRUE             ~ ""
    ),
    
    p_visning = paste0(
      format(round(p_vaerdi, 4), nsmall = 4),
      " ",
      signifikans
    )
  )
#Så får vi lige fjernet alle de ekstra kolonner til udregningen
resultater <- resultater %>%
  mutate(
    p_visning = case_when(
      p_vaerdi < 0.001 ~ paste0(format(round(p_vaerdi, 4), nsmall = 4), " ***"),
      p_vaerdi < 0.01  ~ paste0(format(round(p_vaerdi, 4), nsmall = 4), " **"),
      p_vaerdi < 0.05  ~ paste0(format(round(p_vaerdi, 4), nsmall = 4), " *"),
      p_vaerdi < 0.10  ~ paste0(format(round(p_vaerdi, 4), nsmall = 4), " ."),
      TRUE             ~ format(round(p_vaerdi, 4), nsmall = 4)
    )
  ) %>%
  select(-p_vaerdi, -signifikans)

#Den skal self ikke hedde p_visning men p_værdi

resultater <- resultater %>%
  rename(`P-Vaerdi` = p_visning)





## Opgave 5

## 5.1
# Beregn den kvartalsvise årlige realvækst for husholdningernes forbrugsudgift for Danmark,
# Belgien, Holland, Sverige, Østrig, Tyskland, Frankrig, Italien og Spanien i perioden 1. kvartal 2000
# til og med 2. kvartal 2023. I skal hente data vha. API'et fra Eurostat.

library(eurostat)
library(tidyr)

# 1. Hent data fra Eurostat API (ingen slutdato, så vi får de nyeste tal)
df_euro <- get_eurostat("namq_10_gdp", time_format = "date")

# 2. Filtrér lande, forbrug, enhed og periode
lande_koder <- c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES")

df_filtreret <- subset(df_euro,
                       geo %in% lande_koder &
                         na_item == "P31_S14" &               # Husholdningernes forbrugsudgifter
                         unit == "CLV_I20" &                  # Kædede værdier, indeks 2020 = 100
                         s_adj == "SCA" &                     # Sæson- og kalenderkorrigeret
                         TIME_PERIOD >= as.Date("1999-01-01")) # 1999 skal med for at beregne vækst i 2000

# Sortér kronologisk pr. land
df_filtreret <- df_filtreret[order(df_filtreret$geo, df_filtreret$TIME_PERIOD), ]

# 3. Lav datoer om til kvartaler (fx 2000-04-01 -> "2000-Q2")
maaned <- as.integer(format(df_filtreret$TIME_PERIOD, "%m"))
df_filtreret$Kvartal <- paste0(format(df_filtreret$TIME_PERIOD, "%Y"), "-Q", (maaned + 2) %/% 3)

# 4. Lav landekoder om til navne
landenavne <- c(DK = "Denmark", BE = "Belgium", NL = "Netherlands", SE = "Sweden",
                AT = "Austria", DE = "Germany", FR = "France", IT = "Italy", ES = "Spain")
df_filtreret$Land <- landenavne[as.character(df_filtreret$geo)]

# 5. Lande i rækker, kvartaler i kolonner
forbrug_wide <- pivot_wider(df_filtreret[, c("Land", "Kvartal", "values")],
                            names_from  = Kvartal,
                            values_from = values)
forbrug_wide <- as.data.frame(forbrug_wide)

# 6. Kvartalsvis årlig realvækst (samme kvartal året før = 4 kolonner tilbage)
forbrug <- as.matrix(forbrug_wide[, -1])
n <- ncol(forbrug)
vaekst <- (forbrug[, 5:n] / forbrug[, 1:(n - 4)] - 1) * 100
realvaekst <- data.frame(Land = forbrug_wide$Land, vaekst, check.names = FALSE)
print(realvaekst)


## 5.2
# Hvilket af de landene har gennemsnitligt haft den højeste kvartalsvise årlige realvækst i
# husholdningernes forbrugsudgift i perioden 1. kvartal 2000 til 2. kvartal 2023.

# Vælg perioden 2000-Q1 til 2026-Q2
start <- which(names(realvaekst) == "2000-Q1")
slut  <- which(names(realvaekst) == "2026-Q2")
periode <- realvaekst[, start:slut]

# Gennemsnit pr. land
gns_vaekst <- data.frame(Land = realvaekst$Land,
                         Gns_vaekst = round(rowMeans(periode), 2))

# Sortér fra højest til lavest
gns_vaekst <- gns_vaekst[order(gns_vaekst$Gns_vaekst, decreasing = TRUE), ]
gns_vaekst

library(ggplot2)

# Danske landenavne
dansk <- c(Sweden = "Sverige", Spain = "Spanien", Denmark = "Danmark",
           Belgium = "Belgien", Netherlands = "Holland", Austria = "Østrig",
           France = "Frankrig", Germany = "Tyskland", Italy = "Italien")
gns_vaekst$Land_dk <- dansk[gns_vaekst$Land]

# Markér landet med højest gennemsnit
gns_vaekst$Top <- gns_vaekst$Gns_vaekst == max(gns_vaekst$Gns_vaekst)
top_land   <- gns_vaekst$Land_dk[gns_vaekst$Top]
top_vaerdi <- format(max(gns_vaekst$Gns_vaekst), decimal.mark = ",", nsmall = 2)

p <- ggplot(gns_vaekst, aes(x = reorder(Land_dk, -Gns_vaekst), y = Gns_vaekst, fill = Top)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = format(Gns_vaekst, decimal.mark = ",", nsmall = 2)),
            vjust = -0.5, size = 3.8) +
  scale_fill_manual(values = c("TRUE" = "#1f5fa8", "FALSE" = "#1f5fa8"), guide = "none") +
  scale_y_continuous(labels = function(x) format(x, decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title    = paste0("Højest gennemsnitlige kvartalsvise årlige realvækst: ",
                         top_land, " ligger i top med ", top_vaerdi, " %"),
       subtitle = "I husholdningernes forbrugsudgift i perioden 1. kvartal 2000 til 2. kvartal 2026",
       x = NULL,
       y = "Gennemsnitlig årlig realvækst (%)",
       caption = paste0(
         "Kilde: Eurostat, namq_10_gdp (kædede værdier, indeks 2020 = 100)\n\n",
         "Figur: Her er en oversigt over de 9 landes gennemsnitlige kvartalsvise årlige realvækst fra 1. kvartal 2000 til\n",
         "2. kvartal 2026. Bemærk, at data kun er sæson- og kalenderkorrigeret og ikke korrigeret for økonomiske kriser.")) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey30", margin = margin(b = 10)),
        plot.caption  = element_text(hjust = 0, colour = "grey30", size = 9, margin = margin(t = 12)),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank())

p #Figuren

# Gem figuren i samme mappe som datafilen
ggsave(file.path(dirname(sti), "figur_5_2.png"), p, width = 10, height = 6.5, dpi = 300, bg = "white")

## 5.3 – Coronakrisen som outlier
# Fjerne Coronakrisen fra jeres data og find igen den gennemsnitligt kvartalsvise realvækst i
# husholdningernes forbrugsudgift i perioden 1. kvartal 2000 til 2. kvartal 2023. I hvilket af landene
# har Coronakrisen haft en største effekt på den gennemsnitligt kvartalsvise realvækst.
# Kvartaler der fjernes (Coronakrisen)
corona <- c("2020-Q1", "2020-Q2", "2020-Q3", "2020-Q4",
            "2021-Q1", "2021-Q2", "2021-Q3", "2021-Q4")

# Fjern coronakvartalerne fra forbrugsindekset, før væksten beregnes.
# Så bliver 2022-Q1 sammenlignet med 2019-Q1 (4 kolonner tilbage), fordi 2020 og 2021 er væk.
forbrug_uden <- forbrug[, !(colnames(forbrug) %in% corona)]

# Kvartalsvis årlig realvækst uden corona (samme formel som i 5.1)
n_uden <- ncol(forbrug_uden)
vaekst_uden <- (forbrug_uden[, 5:n_uden] / forbrug_uden[, 1:(n_uden - 4)] - 1) * 100

# Vælg perioden 2000-Q1 til 2026-Q2
uden_corona <- vaekst_uden[, which(colnames(vaekst_uden) == "2000-Q1"):which(colnames(vaekst_uden) == "2026-Q2")]

# Sammenlign gennemsnit med og uden corona
sammenligning <- data.frame(Land        = realvaekst$Land,
                            Med_corona  = round(rowMeans(periode), 2),
                            Uden_corona = round(rowMeans(uden_corona), 2))

# Effekten af corona = forskellen i procentpoint
sammenligning$Forskel <- sammenligning$Uden_corona - sammenligning$Med_corona

# Sortér: størst effekt øverst
sammenligning <- sammenligning[order(sammenligning$Forskel, decreasing = TRUE), ]
sammenligning

## Figur til 5.3
library(ggplot2)
library(tidyr)

# Brug kun Med_corona og Uden_corona (Forskel skal ikke med i figuren)
fig53 <- sammenligning[, c("Land", "Med_corona", "Uden_corona")]
fig53$Land_dk <- dansk[fig53$Land]

# Behold rækkefølgen fra sammenligning (størst corona-effekt først)
fig53$Land_dk <- factor(fig53$Land_dk, levels = fig53$Land_dk)

# Fra bred til lang: én række pr. land og søjle
fig53_long <- pivot_longer(fig53,
                           cols      = c(Med_corona, Uden_corona),
                           names_to  = "Serie",
                           values_to = "Gns_vaekst")
fig53_long$Serie <- factor(fig53_long$Serie,
                           levels = c("Med_corona", "Uden_corona"),
                           labels = c("Med Coronakrisen", "Uden Coronakrisen (2020-Q1 til 2021-Q4 fjernet)"))

# Markér lande hvor Coronakrisen ØGER gennemsnittet (Med_corona > Uden_corona)
oeger <- fig53$Land_dk[fig53$Med_corona > fig53$Uden_corona]
fig53_long$Farve <- as.character(fig53_long$Serie)
fig53_long$Farve[fig53_long$Land_dk %in% oeger & fig53_long$Serie == "Med Coronakrisen"] <- "Med Coronakrisen (krisen øger gennemsnittet)"
fig53_long$Farve <- factor(fig53_long$Farve,
                           levels = c("Med Coronakrisen",
                                      "Uden Coronakrisen (2020-Q1 til 2021-Q4 fjernet)",
                                      "Med Coronakrisen (krisen øger gennemsnittet)"))

# Landet med størst effekt og forskellen til overskriften
top_land    <- dansk[sammenligning$Land[1]]
top_forskel <- format(sammenligning$Forskel[1], decimal.mark = ",", nsmall = 2)

p53 <- ggplot(fig53_long, aes(x = Land_dk, y = Gns_vaekst, fill = Farve, group = Serie)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = format(Gns_vaekst, decimal.mark = ",", nsmall = 2)),
            position = position_dodge(width = 0.8), vjust = -0.5, size = 3.2) +
  scale_fill_manual(values = c("grey70","#1f5fa8","#d1603d"), drop = TRUE) +
  scale_y_continuous(labels = function(x) format(x, decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title    = paste0("Coronakrisen har haft størst effekt i ", top_land,
                         " (+", top_forskel, " procentpoint uden krisen)"),
       subtitle = "Gennemsnitlig kvartalsvis årlig realvækst i husholdningernes forbrugsudgift med og uden Coronakrisen, 1. kvartal 2000 til 2. kvartal 2026",
       x = NULL,
       y = "Gennemsnitlig årlig realvækst (%)",
       fill = NULL,
       caption = paste0(
         "Kilde: Eurostat, namq_10_gdp (kædede værdier, indeks 2020 = 100)\n\n",
         "Figur: Oversigt over de 9 landes gennemsnitlige kvartalsvise årlige realvækst fra 1. kvartal 2000 til 2. kvartal 2026, med og uden\n",
         "Coronakrisen. Uden krisen er kvartalerne fra 1. kvartal 2020 til 4. kvartal 2021 fjernet fra forbrugsniveauet, før væksten er beregnet,\n",
         "så 2022 sammenlignes med 2019. Landene er sorteret efter, hvor meget gennemsnittet ændrer sig. Data er sæson- og kalenderkorrigeret,\n",
         "men ikke korrigeret for andre økonomiske kriser.")) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey30", size = 10.5, margin = margin(b = 10)),
        plot.caption  = element_text(hjust = 0, colour = "grey30", size = 9, lineheight = 1.2, margin = margin(t = 12)),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position       = "top",
        legend.justification  = "left",
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank())

p53

# Gem figuren i samme mappe som datafilen
ggsave(file.path(dirname(sti), "figur_5_3.png"), p53, width = 11, height = 7, dpi = 300, bg = "white")
## 5.4
#I hvilket europæiske land faldt den gennemsnitligt kvartalsvise realvækst i husholdningernes
#forbrugsudgift, i perioden 1. kvartal 2020 til 2. kvartal 2023, mest?

# Vælg perioden 2020-Q1 til 2026-Q2
start <- which(names(realvaekst) == "2020-Q1")
slut  <- which(names(realvaekst) == "2026-Q2")
corona_periode <- realvaekst[, start:slut]

# Gennemsnit og laveste kvartal pr. land
fald <- data.frame(Land            = realvaekst$Land,
                   Gns_vaekst      = round(rowMeans(corona_periode), 2),
                   Laveste_kvartal = round(apply(corona_periode, 1, min), 2),
                   Kvartal         = names(corona_periode)[apply(corona_periode, 1, which.min)])

# Sortér: laveste gennemsnit øverst (= faldt mest)
fald <- fald[order(fald$Gns_vaekst), ]
fald

fig54 <- fald[, c("Land", "Gns_vaekst", "Laveste_kvartal")]
fig54$Land_dk <- dansk[fig54$Land]

# Behold rækkefølgen fra fald (laveste gennemsnit øverst)
fig54$Land_dk <- factor(fig54$Land_dk, levels = rev(fig54$Land_dk))

# Find landene der skal markeres
land_gns   <- fig54$Land_dk[which.min(fig54$Gns_vaekst)]       # laveste gennemsnit
land_fald  <- fig54$Land_dk[which.min(fig54$Laveste_kvartal)]  # dybeste enkeltfald
vaerdi_gns  <- format(min(fig54$Gns_vaekst), decimal.mark = ",", nsmall = 2)
vaerdi_fald <- format(min(fig54$Laveste_kvartal), decimal.mark = ",", nsmall = 2)
kvartal     <- paste(unique(fald$Kvartal), collapse = ", ")

# Fra bred til lang: ét panel pr. mål
fig54_long <- pivot_longer(fig54,
                           cols      = c(Gns_vaekst, Laveste_kvartal),
                           names_to  = "Maal",
                           values_to = "Vaerdi")
fig54_long$Maal <- factor(fig54_long$Maal,
                          levels = c("Gns_vaekst", "Laveste_kvartal"),
                          labels = c("Gennemsnitlig årlig realvækst",
                                     "Laveste kvartal (største enkeltfald)"))

# Farve: markér laveste gennemsnit og dybeste fald
fig54_long$Farve <- "Øvrige"
fig54_long$Farve[fig54_long$Maal == "Gennemsnitlig årlig realvækst" &
                   fig54_long$Land_dk == land_gns] <- "Laveste gennemsnit"
fig54_long$Farve[fig54_long$Maal == "Laveste kvartal (største enkeltfald)" &
                   fig54_long$Land_dk == land_fald] <- "Dybeste fald"

# Aksetekst under hvert panel (ggplot kan kun have én fælles x-akse-titel)
akse_tekst <- data.frame(Maal  = levels(fig54_long$Maal),
                         x     = c(mean(range(0, fig54$Gns_vaekst)),
                                   mean(range(0, fig54$Laveste_kvartal))),
                         tekst = c("Nettotal", "Procent (%)"))
akse_tekst$Maal <- factor(akse_tekst$Maal, levels = levels(fig54_long$Maal))

p54 <- ggplot(fig54_long, aes(x = Vaerdi, y = Land_dk, fill = Farve)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = "grey30") +
  geom_text(aes(label = format(Vaerdi, decimal.mark = ",", nsmall = 2),
                hjust = ifelse(Vaerdi > 0, -0.15, 1.15)),
            size = 3.4) +
  geom_text(data = akse_tekst, aes(x = x, y = -0.35, label = tekst),
            inherit.aes = FALSE, size = 4.2, vjust = 1) +
  coord_cartesian(ylim = c(1, nrow(fig54)), clip = "off") +
  facet_wrap(~ Maal, scales = "free_x") +
  scale_fill_manual(values = c("Øvrige" = "grey70",
                               "Laveste gennemsnit" = "#1f5fa8",
                               "Dybeste fald" = "#d1603d"),
                    guide = "none") +
  scale_x_continuous(labels = function(x) format(x, decimal.mark = ","),
                     expand = expansion(mult = 0.2)) +
  labs(title    = paste0(land_gns, " har den laveste gennemsnitlige vækst (", vaerdi_gns,
                         " %),\nmens ", land_fald, " havde det dybeste enkeltfald (", vaerdi_fald, " %)"),
       subtitle = "Kvartalsvis årlig realvækst i husholdningernes forbrugsudgift, 1. kvartal 2020 til 2. kvartal 2026",
       x = NULL,
       y = NULL,
       caption = paste0(
         "Kilde: Eurostat, namq_10_gdp (kædede værdier, indeks 2020 = 100)\n\n",
         "Figur: Venstre panel viser de 9 landes gennemsnitlige kvartalsvise årlige realvækst fra 1. kvartal 2020 til 2. kvartal 2026. Højre panel\n",
         "viser hvert lands laveste kvartal i samme periode. For alle lande pånær Danmark var det laveste kvartal 2020-Q2, hvor de første nedlukninger\n",
         "under Coronakrisen ramte. Dog var Danmarks laveste kvartal i 2022-Q4. Blå markerer det land med laveste gennemsnit,\n",
         "og orange markerer det dybeste enkeltfald. Bemærk, at de to paneler har forskellige skalaer. Data er sæson- og kalenderkorrigeret, men ikke korrigeret for økonomiske kriser.")) +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 14, lineheight = 1.1),
        plot.subtitle = element_text(colour = "grey30", margin = margin(b = 10)),
        plot.caption  = element_text(hjust = 0, colour = "grey30", size = 9, lineheight = 1.2, margin = margin(t = 12)),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        strip.text            = element_text(face = "bold", size = 11, hjust = 0),
        panel.spacing         = unit(2, "lines"),
        panel.grid.major.y    = element_blank(),
        panel.grid.minor      = element_blank(),
        axis.text.x           = element_text(margin = margin(t = 3, b = 22)))

p54

# Gem figuren i samme mappe som datafilen
ggsave(file.path(dirname(sti), "figur_5_4.png"), p54, width = 11, height = 7, dpi = 300, bg = "white")
