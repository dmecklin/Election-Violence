library(tidyverse)
library(foreign)
library(knitr)
library(readxl)
setwd("C:\\Users\\Dunbar\\Desktop\\Summer and Fall 2019\\pol_viol")
load("C:/Users/Dunbar/Desktop/Summer and Fall 2019/clea_lc_20190617_r/clea_lc_20190617_r/clea_lc_20190617.rdata")
#CLEA
CLEA <- clea_lc_20190617




#CLEA dat cleaning
#Zambia
# will not be includeing special presidential elections b/c no consiteuncy seats up
# 1991 & 1996 have no ACLED data 
Zambia <- filter(CLEA, ctr_n == "Zambia")

data<- Zambia %>% transmute(yr,sub,cvs1,cst_n,cst,seat)
ENoP_data_Z <- filter(data, seat == 1)
#

Zam_elect_11 <- filter(ENoP_data_Z, yr == 2011)

Zam_elect_16 <- filter(ENoP_data_Z, yr == 2016)

#
"2011_ENoP_Zam" <- Zam_elect_11 %>% group_by(cst) %>% summarise(ENoP = sum(1/(cvs1^2)))
"2016_ENoP_Zam" <- Zam_elect_16 %>% group_by(cst) %>% summarise(ENoP = sum((1/(cvs1^2))))
# six more districts were added before 2016 election
CST_n <- ENoP_data_Z %>% filter(yr == 2016) %>% transmute(sub, cst_n, cst)
ENoP_total_Z <- `2016_ENoP_Zam` %>%  left_join(`2011_ENoP_Zam`, by = "cst")
ENoP_total_Z %>% rename(ENoP_total,
                      "ENoP.x" == "ENoP_16", 
                      "ENoP.y" == "ENoP_11")
# why doesn't rename work

ENoP_total_Z
ENoP_cst_n_Z <- CST_n %>% inner_join(ENoP_total, by = "cst")
# now with constituency name, and province of constituency
ENoP_cst_n_Z
library(knitr)
kable(ENoP_cst_n_Z)
# More constituencies added befor 2016 election
#there's probably a better way to do this, naja so es ist.
view(Zambia)

#
# finding average ENoP
`2011_ENoP_Zam` %>% summarise(mean = mean(ENoP)) 
# 3.71
`2016_ENoP_Zam` %>% summarise(mean = mean(ENoP))
# 2.85
# Both = 3.28

inner <- `2016_ENoP_Zam` %>%  inner_join(`2011_ENoP`, by = "cst")
inner%>% summarise(mean = mean((ENoP.x + ENoP.y)/2))
# for both elections: 3.28 
#
# standard deviation

`2016_ENoP_Zam` %>% summarise(sd = sd(ENoP))

# 1.44
`2011_ENoP_Zam` %>% summarise(sd = sd(ENoP))


 ENoP_Zam_AVGf<- add_row(ENoP_cst_n_Z, sub = "LUSAKA", cst_n = c("LUSAKA AVG"), cst = "157", ENoP.x = 3.81, ENoP.y = 2.81)
 ENoP_Zam_AVGed<- add_row(ENoP_Zam_AVGf, sub = "COPPERBELT", cst_n = c("KITWE AVG"), cst = 158, ENoP.x = 2.65, ENoP.y = 2.86)
 #added averaged consitutencies: Kitwe and Lusaka
#
 #ACLED data cleaning

Zam_viol <- read_excel("C:/Users/Dunbar/Desktop/Summer and Fall 2019/pol_viol/Zam_ACLED_2011_2016.xlsx")
Zam_viol$viol <- 1
# making dichotomus var
viol_cst <- select(Zam_viol, admin1, cst_n, year, viol)
viol_cst
Zam_viol_2016 <- filter(viol_cst, year == 2016)
 Zambia_Viol2 <- Zam_viol_2016 %>% count(viol, by = cst_n, admin1)
Zam_viol_2011 <- filter(viol_cst, year == 2011)
Zambia_Viol5 <- Zam_viol_2011 %>% count(viol, by = cst_n, admin1)


#

#Zambia census
#census data being weird so have to paste in.
census_2010 <- tibble(
  plabs <- c("Zambia", "CENTRAL", "COPPERBELT", "EASTERN", "LUAPULA", "LUSAKA",  "MUCHINGA", "NORTHERN", "NORTH-WESTERN", "SOUTHERN", "WESTERN"),
  Bemba  = c(21,13.6,35.9,1.2,44.1,20.2,37.2,55.3,2.8,3.4,0.7),
  Lunda = c(0.9,0.3,1.7,0,7,0.6,0.1,0.1,0.1,0.1,0.1),
  Lala = c(3.1,20.3,4.8,0.1,0.3,1.5,0.8,0.2,0.2,0.1,0.0),
  Bisa = c(1.6, 0.6, 1.3, 1.3, 0.5, 0.6, 8.1, 6.7, 0.0, 0.1, 0.0),
  Ushi = c(1.9, 0.4, 2.7, 0.0, 17.1, 0.5, 0.2, 0.2, 0.1, 0.1, 0.0),
  Chishinga = c(0.5, 0.0, 0.3, 0.0, 6.0, 0.0, 0.0, 0.1, 0.0, 0.0, 0.0),
  Ngumbo = c(0.6, 0.1, 0.7, 0.0, 6.9, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0),
  Lamba = c(2.1, 2.2, 9.6, 0.0, 0.1, 1.1, 0.1, 0.1, 2.4, 0.1, 0.1),
  Tabwa = c(0.7, 0.0, 0.2, 0.0, 1.1, 0.1, 0.0, 7.1, 0.0, 0.0, 0.0),
  Tonga = c(13.6, 18.9, 3.5, 0.5, 0.3, 11.1, 0.5, 0.3, 0.7, 74.4, 0.9),
  Lenje = c(1.6, 9.5, 1.2, 0.1, 0.1, 2.2, 0.1, 0.1, 0.1, 0.3, 0.1),
  Soli = c(0.7, 0.8, 0.2, 0.1, 0.0, 3.2, 0.0, 0.0, 0.1, 0.2, 0.0),
  Ila = c(0.8, 2.3, 0.2, 0.0, 0.0, 0.8, 0.0, 0.0, 0.1, 3.1, 0.1),
  Luvale = c(2.2, 1.0, 2.4, 0.1, 0.0, 1.4, 0.1, 0.0, 16.5, 1.1, 6.4),
  Lunda_NW = c(2.6, 0.5, 2.7, 0.0, 0.1, 0.9, 0.1, 0.1, 34.5, 0.2, 0.7),
  Mbunda = c(1.2, 0.3, 0.2, 0.0, 0.0, 0.3, 0.0, 0.0, 1.6, 0.3, 14.7),
  Chokwe = c(0.5, 0.1, 1.1, 0.0, 0.0, 0.2, 0.0, 0.0, 3.5, 0.1, 1.2),
  Kaonde = c(2.9, 2.6, 4.8, 0.1, 0.1, 2.1, 0.1, 0.1, 26.9, 0.3, 0.5),
  Lozi = c(5.7, 3.5, 2.1, 0.3, 0.2, 4.8, 0.3, 0.2, 1.1, 6.1, 50.3),
  Nkoya = c(0.5, 0.4, 0.1, 0.0, 0.0, 0.3, 0.0, 0.0, 0.6, 0.3, 4.7),
  Chewa = c(7.4, 2.5, 2.4, 39.7, 0.1, 10.5, 0.4, 0.2, 0.2, 0.9, 0.2),
  Nsenga = c(5.3, 2.7, 3.5, 20.3, 0.1, 10.8, 0.3, 0.1, 0.2, 0.8, 0.1),
  Ngoni = c(4.0, 2.6, 3.2, 15.3, 0.2, 7.0, 0.5, 0.3, 0.3, 1.1, 0.1),
  Kunda = c(0.7, 0.2, 0.4, 3.3, 0.0, 1.0, 0.0, 0.0, 0.0, 0.1, 0.0),
  Tumbuka = c(4.4, 1.9, 4.6, 15.6, 0.2, 5.4, 9.4, 0.6, 0.3, 0.6, 0.1),
  Senga = c(0.9, 0.3, 0.5, 0.6, 0.0, 0.5, 11.3, 0.1, 0.1, 0.1, 0.0),
  Lungu = c(0.8, 0.1, 0.3, 0.0, 0.1, 0.2, 0.1, 8.3, 0.0, 0.0, 0.0),
  Mambwe = c(2.5, 1.4, 2.1, 0.1, 0.3, 2.9, 2.3, 16.3, 0.1, 0.3, 0.1),
  Namwanga = c(2.8, 2.0, 4.2, 0.2, 0.2, 2.4, 23.3, 2.4, 0.2, 0.3, 0.1))
plabs <- c("Zambia", "CENTRAL", "COPPERBELT", "EASTERN", "LUAPULA", "LUSAKA",  "MUCHINGA", "NORTHERN", "NORTH-WESTERN", "SOUTHERN", "WESTERN")

x <- select(census_2010, column_number = 2:28)
y <- x/100
census_sqrd <- y^2
EREG <- census_sqrd %>%
  replace(is.na(.), 0) %>%
  transmute(sum = 1/rowSums(.[1:27]))
EREG
Heterogenity <- EREG %>% mutate(sub = plabs) 
Heterogenity<- Heterogenity %>% rename(ethnic_heterogenity = sum)
# Effective Number of Electorally Relevant Ethnoregional Groups 
#
#
#
#

# Census data from 2010 CENSUS OF POPULATION AND HOUSING: https://drive.google.com/drive/folders/0BzixB3inWbiGZ2k1bmpsQ2pPR2M
# population density & unemployment %

demographic_data <- tibble(
  provence <- c("CENTRAL", "COPPERBELT", "EASTERN", "LUAPULA", "LUSAKA",  "MUCHINGA", "NORTH", "NORTH-WESTERN", "SOUTHERN", "WESTERN"),
  popdensity = c(13.8, 63.0, 30.9, 19.6, 100.1, 8.1, 14.2, 5.8, 18.6, 7.1),
unemployed = c( 12.7, 22.1, 8.8, 7.7, 20.0, 6.4, 6.3, 10.3, 12.1, 7.7))
provence <- c("CENTRAL", "COPPERBELT", "EASTERN", "LUAPULA", "LUSAKA",  "MUCHINGA", "NORTHERN", "NORTH-WESTERN", "SOUTHERN", "WESTERN")
demographic_data <- demographic_data %>% mutate(sub = provence) 


# V Regression
# controll for ethic heterogenity, pop density, unemployment


Zambia_Viol3 <- Zambia_Viol2 %>% rename(cst_n = by)
Zambia_Viol4 <- Zambia_Viol3 %>% rename(sub = admin1)
Zambia_Viol6 <- Zambia_Viol5 %>% rename(cst_n = by)
Zambia_Viol7 <- Zambia_Viol6 %>% rename(sub = admin1)
regression_zam <- ENoP_Zam_AVGed %>%  full_join(Zambia_Viol4, by = c("cst_n", "sub"))
regression_zam <- regression_zam %>%  full_join(Zambia_Viol7, by = c("cst_n", "sub"))
regression_zam[is.na(regression_zam)] <- 0
fast_alles_über_Sambia <- regression_zam %>% full_join(Heterogenity, by = "sub")
fast_alles_über_Sambia <- fast_alles_über_Sambia[-c(159),] 
DATEN_über_Sambia_1 <- fast_alles_über_Sambia %>% full_join(demographic_data, by = "sub")
DATEN_über_Sambia_1 <- select(DATEN_über_Sambia_1, "sub", "cst_n", "cst", "ENoP.x", "ENoP.y", "viol.x", "n.x", "viol.y", "n.y", "ethnic_heterogenity", "popdensity", "unemployed")
DATEN_über_Sambia <- DATEN_über_Sambia_1 %>% rename(ENoP_2016 = ENoP.x, ENoP_2011 = ENoP.y, viol_2016 = viol.x, viol_2011 = viol.y, n_2016 = n.x, n_2011 = n.y)
DATEN_über_Sambia_ohne_etwas <- DATEN_über_Sambia[-c(47, 64, 81, 87, 117, 131, 154),]
# removing consituencies included in average, for all data print DATEN_über_Sambia
print(DATEN_über_Sambia, n=200)
print(DATEN_über_Sambia_ohne_etwas, n = 200)


# messing around to see if only violent provences have relationship - they don't
Prov_of_interest <- filter(DATEN_über_Sambia, sub == c("CENTRAL") | sub == c("COPPERBELT") | sub == c("CENTRAL") | sub == c("SOUTHERN"))
print(Prov_of_interest, n = 200)

reg.zam.2016_prov <- lm(n_2016 ~ ENoP_2016 + ethnic_heterogenity + popdensity + unemployed, data = Prov_of_interest)
reg.zam.2011_prov <- lm(n_2011 ~ ENoP_2011 + ethnic_heterogenity + popdensity + unemployed, data = Prov_of_interest)
summary(reg.zam.2011_prov)


reg.zam.2016 <- lm(n_2016 ~ ENoP_2016 + viol_2011 + ethnic_heterogenity + popdensity + unemployed, data = DATEN_über_Sambia_ohne_etwas)
reg.zam.2011 <- lm(n_2011 ~ ENoP_2011 + ethnic_heterogenity + popdensity + unemployed, data = DATEN_über_Sambia_ohne_etwas)
probit <- glm(viol_2016 ~ ENoP_2016 + viol_2011 + ethnic_heterogenity + popdensity + unemployed, data = DATEN_über_Sambia_ohne_etwas)
probit_summary <- summary(probit)
reg_2016 <- summary(reg.zam.2016)

reg_2011 <- summary(reg.zam.2011)

reg_2016
