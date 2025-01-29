### Various analyses for the FLP 1ha plot
#Set working directory
setwd("C:/Users/rxf568/Dropbox/Peru - FLP 1ha Plot/Data Analysis")

#Load libraries
library(tidyverse)
library(vegan)
library(dplyr)
library(terra)
library(BIOMASS)
library(stringr)
library(goeveg)
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(readxl)
library(reshape2)
library(ggrepel)
library(sp)
library(raster)

######### Data analysis for the FLP 1-ha plot ##########
#Load data
flp_21 <- read_excel("FLP_01_21.xlsx", 
                     sheet = "Plot Dump")
flp_24 <- read_excel("FLP_01_24.xlsx", 
                     sheet = "Plot Dump")

#Change X coordinates to be based on entire 1 ha plot
flp_21$X <- ifelse(flp_21$Subplot==2 | flp_21$Subplot==12 | flp_21$Subplot==22 | flp_21$Subplot==32 | flp_21$Subplot==42 | flp_21$Subplot==52 | flp_21$Subplot==62 | flp_21$Subplot==72 | flp_21$Subplot==82 | flp_21$Subplot==92, flp_21$X+10, flp_21$X)
flp_21$X <- ifelse(flp_21$Subplot==3 | flp_21$Subplot==13 | flp_21$Subplot==23 | flp_21$Subplot==33 | flp_21$Subplot==43 | flp_21$Subplot==53 | flp_21$Subplot==63 | flp_21$Subplot==73 | flp_21$Subplot==83 | flp_21$Subplot==93, flp_21$X+20, flp_21$X)
flp_21$X <- ifelse(flp_21$Subplot==4 | flp_21$Subplot==14 | flp_21$Subplot==24 | flp_21$Subplot==34 | flp_21$Subplot==44 | flp_21$Subplot==54 | flp_21$Subplot==64 | flp_21$Subplot==74 | flp_21$Subplot==84 | flp_21$Subplot==94, flp_21$X+30, flp_21$X)
flp_21$X <- ifelse(flp_21$Subplot==5 | flp_21$Subplot==15 | flp_21$Subplot==25 | flp_21$Subplot==35 | flp_21$Subplot==45 | flp_21$Subplot==55 | flp_21$Subplot==65 | flp_21$Subplot==75 | flp_21$Subplot==85 | flp_21$Subplot==95, flp_21$X+40, flp_21$X)
flp_21$X <- ifelse(flp_21$Subplot==6 | flp_21$Subplot==16 | flp_21$Subplot==26 | flp_21$Subplot==36 | flp_21$Subplot==46 | flp_21$Subplot==56 | flp_21$Subplot==66 | flp_21$Subplot==76 | flp_21$Subplot==86 | flp_21$Subplot==96, flp_21$X+50, flp_21$X)
flp_21$X <- ifelse(flp_21$Subplot==7 | flp_21$Subplot==17 | flp_21$Subplot==27 | flp_21$Subplot==37 | flp_21$Subplot==47 | flp_21$Subplot==57 | flp_21$Subplot==67 | flp_21$Subplot==77 | flp_21$Subplot==87 | flp_21$Subplot==97, flp_21$X+60, flp_21$X)
flp_21$X <- ifelse(flp_21$Subplot==8 | flp_21$Subplot==18 | flp_21$Subplot==28 | flp_21$Subplot==38 | flp_21$Subplot==48 | flp_21$Subplot==58 | flp_21$Subplot==68 | flp_21$Subplot==78 | flp_21$Subplot==88 | flp_21$Subplot==98, flp_21$X+70, flp_21$X)
flp_21$X <- ifelse(flp_21$Subplot==9 | flp_21$Subplot==19 | flp_21$Subplot==29 | flp_21$Subplot==39 | flp_21$Subplot==49 | flp_21$Subplot==59 | flp_21$Subplot==69 | flp_21$Subplot==79 | flp_21$Subplot==89 | flp_21$Subplot==99, flp_21$X+80, flp_21$X)
flp_21$X <- ifelse(flp_21$Subplot==10 | flp_21$Subplot==20 | flp_21$Subplot==30 | flp_21$Subplot==40 | flp_21$Subplot==50 | flp_21$Subplot==60 | flp_21$Subplot==70 | flp_21$Subplot==80 | flp_21$Subplot==90 | flp_21$Subplot==100, flp_21$X+90, flp_21$X)

#Change Y coordinates to be based on entire 1 ha plot
flp_21$Y <- ifelse(flp_21$Subplot==11 | flp_21$Subplot==12 | flp_21$Subplot==13 | flp_21$Subplot==14 | flp_21$Subplot==15 | flp_21$Subplot==16 | flp_21$Subplot==17 | flp_21$Subplot==18 | flp_21$Subplot==19 | flp_21$Subplot==20, flp_21$Y+10, flp_21$Y)
flp_21$Y <- ifelse(flp_21$Subplot==21 | flp_21$Subplot==22 | flp_21$Subplot==23 | flp_21$Subplot==24 | flp_21$Subplot==25 | flp_21$Subplot==26 | flp_21$Subplot==27 | flp_21$Subplot==28 | flp_21$Subplot==29 | flp_21$Subplot==30, flp_21$Y+20, flp_21$Y)
flp_21$Y <- ifelse(flp_21$Subplot==31 | flp_21$Subplot==32 | flp_21$Subplot==33 | flp_21$Subplot==34 | flp_21$Subplot==35 | flp_21$Subplot==36 | flp_21$Subplot==37 | flp_21$Subplot==38 | flp_21$Subplot==39 | flp_21$Subplot==40, flp_21$Y+30, flp_21$Y)
flp_21$Y <- ifelse(flp_21$Subplot==41 | flp_21$Subplot==42 | flp_21$Subplot==43 | flp_21$Subplot==44 | flp_21$Subplot==45 | flp_21$Subplot==46 | flp_21$Subplot==47 | flp_21$Subplot==48 | flp_21$Subplot==49 | flp_21$Subplot==50, flp_21$Y+40, flp_21$Y)
flp_21$Y <- ifelse(flp_21$Subplot==51 | flp_21$Subplot==52 | flp_21$Subplot==53 | flp_21$Subplot==54 | flp_21$Subplot==55 | flp_21$Subplot==56 | flp_21$Subplot==57 | flp_21$Subplot==58 | flp_21$Subplot==59 | flp_21$Subplot==60, flp_21$Y+50, flp_21$Y)
flp_21$Y <- ifelse(flp_21$Subplot==61 | flp_21$Subplot==62 | flp_21$Subplot==63 | flp_21$Subplot==64 | flp_21$Subplot==65 | flp_21$Subplot==66 | flp_21$Subplot==67 | flp_21$Subplot==68 | flp_21$Subplot==69 | flp_21$Subplot==70, flp_21$Y+60, flp_21$Y)
flp_21$Y <- ifelse(flp_21$Subplot==71 | flp_21$Subplot==72 | flp_21$Subplot==73 | flp_21$Subplot==74 | flp_21$Subplot==75 | flp_21$Subplot==76 | flp_21$Subplot==77 | flp_21$Subplot==78 | flp_21$Subplot==79 | flp_21$Subplot==80, flp_21$Y+70, flp_21$Y)
flp_21$Y <- ifelse(flp_21$Subplot==81 | flp_21$Subplot==82 | flp_21$Subplot==83 | flp_21$Subplot==84 | flp_21$Subplot==85 | flp_21$Subplot==86 | flp_21$Subplot==87 | flp_21$Subplot==88 | flp_21$Subplot==89 | flp_21$Subplot==90, flp_21$Y+80, flp_21$Y)
flp_21$Y <- ifelse(flp_21$Subplot==91 | flp_21$Subplot==92 | flp_21$Subplot==93 | flp_21$Subplot==94 | flp_21$Subplot==95 | flp_21$Subplot==96 | flp_21$Subplot==97 | flp_21$Subplot==98 | flp_21$Subplot==99 | flp_21$Subplot==100, flp_21$Y+90, flp_21$Y)

#Change X coordinates to be based on entire 1 ha plot
flp_24$X <- ifelse(flp_24$Subplot==2 | flp_24$Subplot==12 | flp_24$Subplot==22 | flp_24$Subplot==32 | flp_24$Subplot==42 | flp_24$Subplot==52 | flp_24$Subplot==62 | flp_24$Subplot==72 | flp_24$Subplot==82 | flp_24$Subplot==92, flp_24$X+10, flp_24$X)
flp_24$X <- ifelse(flp_24$Subplot==3 | flp_24$Subplot==13 | flp_24$Subplot==23 | flp_24$Subplot==33 | flp_24$Subplot==43 | flp_24$Subplot==53 | flp_24$Subplot==63 | flp_24$Subplot==73 | flp_24$Subplot==83 | flp_24$Subplot==93, flp_24$X+20, flp_24$X)
flp_24$X <- ifelse(flp_24$Subplot==4 | flp_24$Subplot==14 | flp_24$Subplot==24 | flp_24$Subplot==34 | flp_24$Subplot==44 | flp_24$Subplot==54 | flp_24$Subplot==64 | flp_24$Subplot==74 | flp_24$Subplot==84 | flp_24$Subplot==94, flp_24$X+30, flp_24$X)
flp_24$X <- ifelse(flp_24$Subplot==5 | flp_24$Subplot==15 | flp_24$Subplot==25 | flp_24$Subplot==35 | flp_24$Subplot==45 | flp_24$Subplot==55 | flp_24$Subplot==65 | flp_24$Subplot==75 | flp_24$Subplot==85 | flp_24$Subplot==95, flp_24$X+40, flp_24$X)
flp_24$X <- ifelse(flp_24$Subplot==6 | flp_24$Subplot==16 | flp_24$Subplot==26 | flp_24$Subplot==36 | flp_24$Subplot==46 | flp_24$Subplot==56 | flp_24$Subplot==66 | flp_24$Subplot==76 | flp_24$Subplot==86 | flp_24$Subplot==96, flp_24$X+50, flp_24$X)
flp_24$X <- ifelse(flp_24$Subplot==7 | flp_24$Subplot==17 | flp_24$Subplot==27 | flp_24$Subplot==37 | flp_24$Subplot==47 | flp_24$Subplot==57 | flp_24$Subplot==67 | flp_24$Subplot==77 | flp_24$Subplot==87 | flp_24$Subplot==97, flp_24$X+60, flp_24$X)
flp_24$X <- ifelse(flp_24$Subplot==8 | flp_24$Subplot==18 | flp_24$Subplot==28 | flp_24$Subplot==38 | flp_24$Subplot==48 | flp_24$Subplot==58 | flp_24$Subplot==68 | flp_24$Subplot==78 | flp_24$Subplot==88 | flp_24$Subplot==98, flp_24$X+70, flp_24$X)
flp_24$X <- ifelse(flp_24$Subplot==9 | flp_24$Subplot==19 | flp_24$Subplot==29 | flp_24$Subplot==39 | flp_24$Subplot==49 | flp_24$Subplot==59 | flp_24$Subplot==69 | flp_24$Subplot==79 | flp_24$Subplot==89 | flp_24$Subplot==99, flp_24$X+80, flp_24$X)
flp_24$X <- ifelse(flp_24$Subplot==10 | flp_24$Subplot==20 | flp_24$Subplot==30 | flp_24$Subplot==40 | flp_24$Subplot==50 | flp_24$Subplot==60 | flp_24$Subplot==70 | flp_24$Subplot==80 | flp_24$Subplot==90 | flp_24$Subplot==100, flp_24$X+90, flp_24$X)

#Change Y coordinates to be based on entire 1 ha plot
flp_24$Y <- ifelse(flp_24$Subplot==11 | flp_24$Subplot==12 | flp_24$Subplot==13 | flp_24$Subplot==14 | flp_24$Subplot==15 | flp_24$Subplot==16 | flp_24$Subplot==17 | flp_24$Subplot==18 | flp_24$Subplot==19 | flp_24$Subplot==20, flp_24$Y+10, flp_24$Y)
flp_24$Y <- ifelse(flp_24$Subplot==21 | flp_24$Subplot==22 | flp_24$Subplot==23 | flp_24$Subplot==24 | flp_24$Subplot==25 | flp_24$Subplot==26 | flp_24$Subplot==27 | flp_24$Subplot==28 | flp_24$Subplot==29 | flp_24$Subplot==30, flp_24$Y+20, flp_24$Y)
flp_24$Y <- ifelse(flp_24$Subplot==31 | flp_24$Subplot==32 | flp_24$Subplot==33 | flp_24$Subplot==34 | flp_24$Subplot==35 | flp_24$Subplot==36 | flp_24$Subplot==37 | flp_24$Subplot==38 | flp_24$Subplot==39 | flp_24$Subplot==40, flp_24$Y+30, flp_24$Y)
flp_24$Y <- ifelse(flp_24$Subplot==41 | flp_24$Subplot==42 | flp_24$Subplot==43 | flp_24$Subplot==44 | flp_24$Subplot==45 | flp_24$Subplot==46 | flp_24$Subplot==47 | flp_24$Subplot==48 | flp_24$Subplot==49 | flp_24$Subplot==50, flp_24$Y+40, flp_24$Y)
flp_24$Y <- ifelse(flp_24$Subplot==51 | flp_24$Subplot==52 | flp_24$Subplot==53 | flp_24$Subplot==54 | flp_24$Subplot==55 | flp_24$Subplot==56 | flp_24$Subplot==57 | flp_24$Subplot==58 | flp_24$Subplot==59 | flp_24$Subplot==60, flp_24$Y+50, flp_24$Y)
flp_24$Y <- ifelse(flp_24$Subplot==61 | flp_24$Subplot==62 | flp_24$Subplot==63 | flp_24$Subplot==64 | flp_24$Subplot==65 | flp_24$Subplot==66 | flp_24$Subplot==67 | flp_24$Subplot==68 | flp_24$Subplot==69 | flp_24$Subplot==70, flp_24$Y+60, flp_24$Y)
flp_24$Y <- ifelse(flp_24$Subplot==71 | flp_24$Subplot==72 | flp_24$Subplot==73 | flp_24$Subplot==74 | flp_24$Subplot==75 | flp_24$Subplot==76 | flp_24$Subplot==77 | flp_24$Subplot==78 | flp_24$Subplot==79 | flp_24$Subplot==80, flp_24$Y+70, flp_24$Y)
flp_24$Y <- ifelse(flp_24$Subplot==81 | flp_24$Subplot==82 | flp_24$Subplot==83 | flp_24$Subplot==84 | flp_24$Subplot==85 | flp_24$Subplot==86 | flp_24$Subplot==87 | flp_24$Subplot==88 | flp_24$Subplot==89 | flp_24$Subplot==90, flp_24$Y+80, flp_24$Y)
flp_24$Y <- ifelse(flp_24$Subplot==91 | flp_24$Subplot==92 | flp_24$Subplot==93 | flp_24$Subplot==94 | flp_24$Subplot==95 | flp_24$Subplot==96 | flp_24$Subplot==97 | flp_24$Subplot==98 | flp_24$Subplot==99 | flp_24$Subplot==100, flp_24$Y+90, flp_24$Y)

flp_21$Height <- as.numeric(flp_21$Height)
flp_24$Height <- as.numeric(flp_24$Height)

#Make the map of the plot

map <- ggplot(flp_24, aes(y = Y, x = X))+
  labs(title = "FLP 1ha Plot", x = "Meters", y = "Meters", color = "") +
  geom_point(shape=16, aes(size = DBH)) +
  scale_x_continuous(expand = c(0,0), limits = c(0,100), labels=c(seq(0,100,10)), breaks=c(seq(0,100,10)))+ #assigns the x axis limits of 0 and 100, with 20 meter lines
  scale_y_continuous(expand = c(0,0), limits = c(0,100), labels=c(seq(0,100,10)), breaks=c(seq(0,100,10))) + #assigns the y axis limits of 0 and 100
  theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(color="black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = c(1.3,0.95),
        plot.title = element_text(size=22),
        text = element_text(size=20)) +
  #scale_color_manual(values = cols, labels = c("Living trees", "Living source trees with \n herbarium specimens")) +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  scale_size(guide = "none") +
  coord_fixed()
map

# Create histogram of DBH
flp_24$DBH <- as.numeric(flp_24$DBH)

ggplot(data = subset(flp_24, DBH > 0), aes(x=DBH/10)) +
  geom_histogram(color="black", fill="white", binwidth=5, boundary = min(flp_24$DBH)) +
  geom_vline(xintercept = 21.4, linetype="dashed", color="blue2", linewidth=1) +
  labs(x="Diameter at breast height (cm)", y="Count") +
  theme_bw()

## descriptive stats for the flp plot
#most common species
flp_24spp <- flp_24 %>%
  group_by(Family,Species) %>%
  summarise(
    "Number of individuals" = n(),
    "Max DBH" = max(DBH)
  ) 

#most speciose families
flp_24fam <- flp_24spp %>%
  group_by(Family) %>%
  summarise("Number of species" = n())

#calculate basal area of each tree
flp_24$ba <- (pi * (flp_24$DBH/2)^2)

#summarize basal area by species
sp_ba <- flp_24 %>%
  group_by(Species) %>%
  summarise(total_ba = sum(ba))
sum(sp_ba$total_ba)
sp_ba$perc_ba <- (sp_ba$total_ba)/33010706

###AGB for each census and change in biomass between censuses
#create column with just genus
flp_21$Genus <- word(flp_21$Species, 1)

#Get wood density
WD_flp21 <- getWoodDensity(flp_21$Genus, flp_21$Species, family = flp_21$Family, region="SouthAmericaTrop")

flp_21 <- cbind(flp_21, WD_flp21[,c(4:6)])

flp21_agb <- AGBmonteCarlo(flp_21$DBH/10, WD = flp_21$meanWD, errWD = flp_21$sdWD, 
                         H = flp_21$Height, errH = 0)

flp21_agb[[1]] #mean AGB = 285
flp21_agb[[2]] #medAGB = 283
flp21_agb[[3]] #sdAGB = 21.9

#create column with just genus
flp_24$Genus <- word(flp_24$Species, 1)

#Get wood density
WD_flp24 <- getWoodDensity(flp_24$Genus, flp_24$Species, family = flp_24$Family, region="SouthAmericaTrop")

flp_24 <- cbind(flp_24, WD_flp24[,c(4:6)])

flp24_agb <- AGBmonteCarlo(flp_24$DBH/10, WD = flp_24$meanWD, errWD = flp_24$sdWD, 
                           H = flp_24$Height, errH = 0)

flp24_agb[[1]] #mean AGB = 316
flp24_agb[[2]] #medAGB = 315
flp24_agb[[3]] #sdAGB = 22.5
(316-285)/3 #10.3 t per year

#load in each other plot, and make a list of each plot code.
ALM_01 <- read_excel("plot spp data/ALM_01.xlsx", 
                     sheet = "Field Sheet", skip = 1)
CUZ_01 <- read_excel("plot spp data/CUZ_01.xlsx", 
                     sheet = "Field Sheet", skip = 1)
CUZ_02 <- read_excel("plot spp data/CUZ_02.xlsx", 
                     sheet = "Field Sheet", skip = 1)
CUZ_03 <- read_excel("plot spp data/CUZ_03.xlsx", 
                     sheet = "Field Sheet", skip = 1)
CUZ_04 <- read_excel("plot spp data/CUZ_04.xlsx", 
                     sheet = "Field Sheet", skip = 1)
LAS_02 <- read_excel("plot spp data/LAS_02.xlsx", 
                     sheet = "Field Sheet", skip = 1)
MNU_05 <- read_excel("plot spp data/MNU_05.xlsx", 
                     sheet = "Field Sheet", skip = 1)
MNU_06 <- read_excel("plot spp data/MNU_06.xlsx", 
                     sheet = "Field Sheet", skip = 1)
POR_01 <- read_excel("plot spp data/POR_01.xlsx", 
                     sheet = "Field Sheet", skip = 1)
POR_02 <- read_excel("plot spp data/POR_02.xlsx", 
                     sheet = "Field Sheet", skip = 1)
RFH_01 <- read_excel("plot spp data/RFH_01.xlsx", 
                     sheet = "Field Sheet", skip = 1)
TAM_01 <- read_excel("plot spp data/TAM_01.xlsx", 
                     sheet = "Field Sheet", skip = 1)
TAM_02 <- read_excel("plot spp data/TAM_02.xlsx", 
                     sheet = "Field Sheet", skip = 1)
TAM_03 <- read_excel("plot spp data/TAM_03.xlsx", 
                     sheet = "Field Sheet", skip = 1)
TAM_04 <- read_excel("plot spp data/TAM_04.xlsx", 
                     sheet = "Field Sheet", skip = 1)
TAM_05 <- read_excel("plot spp data/TAM_05.xlsx", 
                     sheet = "Field Sheet", skip = 1)
TAM_06 <- read_excel("plot spp data/TAM_06.xlsx", 
                     sheet = "Field Sheet", skip = 1)
TAM_07 <- read_excel("plot spp data/TAM_07.xlsx", 
                     sheet = "Field Sheet", skip = 1)
TAM_08 <- read_excel("plot spp data/TAM_08.xlsx", 
                     sheet = "Field Sheet", skip = 1)
TAM_09 <- read_excel("plot spp data/TAM_09.xlsx", 
                     sheet = "Field Sheet", skip = 1)

#import and clean TRC data
TRC_01 <- read.csv("plot spp data/TRC_01.csv")

#remove spaces in all column names of TRC_01
colnames(TRC_01) <- gsub(" ", "", colnames(TRC_01))

#remove everything with the words "Liana" or "liana" in the SPECIES column
TRC_01 <- TRC_01[!grepl("Liana", TRC_01$SPECIES, ignore.case = TRUE),]

#

#capitalize the first letter of the SPECIES column 
TRC_01$SPECIES <- paste(toupper(substr(TRC_01$SPECIES, 1, 1)), substr(TRC_01$SPECIES, 2, nchar(TRC_01$SPECIES)), sep="")

#make a genus column
TRC_01$Genus <- word(TRC_01$SPECIES, 1)

#retrieve family name from getTaxonomy function
fams <- getTaxonomy(TRC_01$Genus)
TRC_01$Family <- fams$family

#replace missing families
TRC_01$Family <- ifelse(TRC_01$Genus == "Rubiac", "Rubiaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Laurac", "Lauraceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Mimosoid", "Fabaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Lauraceae", "Lauraceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Myrtac", "Myrtaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Annonac", "Annonaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Chrysobalanac", "Chrysobalanaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Neea/guapira", "Nyctaginaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Apocynac", "Apocynaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Bombac", "Bombacaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Caesalpinoid", "Fabaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Hippocrat", "Hippocrateaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Icacinac", "Icacinaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Clusiac/vismia?", "Clusiaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Solanac", "Solanaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Annonac?", "Annonaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Nyctaginacea", "Nyctaginaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Chrysobalanacea", "Chrysobalanaceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Lauracea", "Lauraceae", TRC_01$Family)
TRC_01$Family <- ifelse(TRC_01$Genus == "Fabaceae", "Fabaceae", TRC_01$Family)

#now remove some records that are lianas (in the family Hippocrateaceae)
TRC_01 <- TRC_01[!grepl("Hippocrateaceae", TRC_01$Family, ignore.case = TRUE),]

#make 2020 data from TRC_01 that have A in the Status 2020 column
TRC_01_2020 <- TRC_01[TRC_01$Status.2020 == "A",]
#remove rows with NA in Status2020 column
TRC_01_2020 <- TRC_01_2020[!is.na(TRC_01_2020$Status.2020),]

#make 2009 data from TRC_01 by keeping rows with numbers in the DBH2009 column
TRC_01_2009 <- TRC_01[grepl("[0-9]", TRC_01$DBH.2009),]

#keep only records in SP 1 through 16
TRC_01_2020 <- TRC_01_2020[TRC_01_2020$SP <= 16,]
TRC_01_2009 <- TRC_01_2009[TRC_01_2009$SP <= 16,]

#keep only columns tag, family, sp, and dbh in that order
TRC_01_2020 <- TRC_01_2020[,c(4,24,10,12)]
TRC_01_2009 <- TRC_01_2009[,c(4,24,10,20)]

#multiply dbh by 10
TRC_01_2020$DBH.2020 <- as.numeric(TRC_01_2020$DBH.2020)
TRC_01_2020$DBH.2020 <- TRC_01_2020$DBH.2020 * 10

TRC_01_2009$DBH.2009 <- as.numeric(TRC_01_2009$DBH.2009)
TRC_01_2009$DBH.2009 <- TRC_01_2009$DBH.2009 * 10

#change SPECIES to Species
TRC_01_2020 <- TRC_01_2020 %>% dplyr::rename(Species = SPECIES)
TRC_01_2009 <- TRC_01_2009 %>% dplyr::rename(Species = SPECIES)

#change DBH.2020 to DBH
TRC_01_2020 <- TRC_01_2020 %>% dplyr::rename(DBH = DBH.2020)
TRC_01_2009 <- TRC_01_2009 %>% dplyr::rename(DBH = DBH.2009)


#make list of plot codes
plots <- list(ALM_01 = ALM_01, CUZ_01 = CUZ_01, CUZ_02 = CUZ_02, CUZ_03 = CUZ_03, CUZ_04 = CUZ_04, LAS_02 = LAS_02, MNU_05 = MNU_05, MNU_06 = MNU_06, POR_01 = POR_01, POR_02 = POR_02, RFH_01 = RFH_01, TAM_01 = TAM_01, TAM_02 = TAM_02, TAM_03 = TAM_03, TAM_04 = TAM_04, TAM_05 = TAM_05, TAM_06 = TAM_06, TAM_07 = TAM_07, TAM_08 = TAM_08, TAM_09 = TAM_09, TRC_01_2020 = TRC_01_2020, TRC_01_2009 = TRC_01_2009)
plots_agb <- c("ALM_01", "CUZ_01", "CUZ_02", "CUZ_03", "CUZ_04", "LAS_02", "MNU_05", "MNU_06", "POR_01", "POR_02", "RFH_01", "TAM_01", "TAM_02", "TAM_03", "TAM_04", "TAM_05", "TAM_06", "TAM_07", "TAM_08", "TAM_09", "TRC_01_2020", "TRC_01_2009")
plots_agb <- as.data.frame(plots_agb)

#Make diameter-height model
Hmodel <- modelHD(D=NouraguesHD$D, H=NouraguesHD$H, method="log2", useWeight = TRUE)

#for loop to calculate AGB for each of the other plots
for(i in 1:length(plots)){
  colnames(plots[[i]])[4] <- "DBH"
  plots[[i]]$Genus <- word(plots[[i]]$Species, 1) #extract genus
  
  H <- retrieveH(D=(plots[[i]]$DBH)/10, model=Hmodel) #model tree height
  
  plots[[i]]$H <- H[[1]] #model tree height
  
  WD <- getWoodDensity(plots[[i]]$Genus, plots[[i]]$Species, family = plots[[i]]$Family, region="SouthAmericaTrop")
  
  plots[[i]] <- cbind(plots[[i]], WD[,c(4:6)])
  
  agb <- AGBmonteCarlo(plots[[i]]$DBH/10, WD = plots[[i]]$meanWD, errWD = plots[[i]]$sdWD, H = plots[[i]]$H, errH = 0)
  plots_agb$agb[[i]] <- agb[["meanAGB"]]
  plots_agb$sdagb[[i]] <- agb[["sdAGB"]]
}

colnames(plots_agb)[1] <- "Plot"
plots_agb$agb <- as.numeric(plots_agb$agb)
plots_agb$sdagb <- as.numeric(plots_agb$sdagb)
plots_agb$agb <- ifelse(plots_agb$Plot == "ALM_01", plots_agb$agb/2, plots_agb$agb) #divide agb by two because ALM_01 is 2 ha
plots_agb$agb <- ifelse(plots_agb$Plot == "MNU_05" | plots_agb$Plot == "MNU_06" , plots_agb$agb/2.25, plots_agb$agb) #divide agb by two because MNU_05 and MNU_06 are 2.25 ha
plots_agb$agb <- ifelse(plots_agb$Plot == "TAM_03", plots_agb$agb*(1/0.58), plots_agb$agb) #multiply because TAM_03 is 0.58 ha
plots_agb$agb <- ifelse(plots_agb$Plot == "TAM_04", plots_agb$agb*(1/0.42), plots_agb$agb) #multiply because TAM_04 is 0.42 ha

x <- data.frame(
  Plot = "FLP_01",
  agb = 317,
  sdagb = 22.5
)
plots_agb <- rbind(plots_agb, x)

#let's get rid of TAM_03 from the rest of the analyses since it is a very unique monodominant swamp plot.
plots_agb <- plots_agb %>% filter(Plot != "TAM_03")

#Plot AGB for each plot
plots_agb$FLP <- ifelse(plots_agb$Plot == "FLP_01", "Y", "N")

ggplot(data = plots_agb %>% filter(Plot != "TRC_01_2009"), aes(color = FLP, shape = FLP)) +
  geom_pointrange(mapping = aes(x= reorder(Plot, -agb), y = agb, ymin = agb-sdagb, ymax = agb+sdagb), size=1.25, fatten = 3, stroke = 2) +
  labs(x = "Site", y = "Above-ground biomass (Metric tons)", title = "AGB per hectare for permanent plots in the southwestern Amazon") +
  theme_bw()+
  scale_x_discrete(labels = c("TRC_01_2020" = "TRC_01")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "none") +
  scale_color_manual(values=c("Y"="blue2","N"="black"), guide="none") +
  scale_shape_manual(values=c(16,1))


###calculate stem turnover in the FLP plot
o <- 549 #number of stems in original census
s <- 549-22 #number original stems surviving to next census
r <- 55 #number of recruits
t <- 3 #time in years
  
mort <- (log(o) - log(s)) / t #1.36%
rec <- (log(s + r) - log(s)) / t #3.31%

mean(c(mort, rec)) #2.23%

#calculate stem turnover for the TRC plot
o <- 563
s <- TRC_01[grepl("[0-9]", TRC_01$DBH.2009),]
s <- s[s$SP <= 16,]
nrow(s %>% filter(Status.2020 == "A"))
s <- 423
r <- 112
t <- 11

mort <- (log(o) - log(s)) / t #2.37%
rec <- (log(s + r) - log(s)) / t #2.16%

mean(c(mort, rec)) #2.25%

#get rid of the recruits in flp21 and dead trees in flp24
flp_21 <- flp_21 %>% filter(DBH >= 100)
flp_24 <- flp_24 %>% filter(F1 != 0)

#make figure to show AGB change and turnover between FLP and the TAM plots
x <- read.csv("TableS1.csv")
x$flp <- ifelse(x$Plot =="FLP_01", TRUE, FALSE)
x <- x %>%
  pivot_longer(
  cols = c(20:23), 
  names_to = "variable",
  values_to = "value"
)
  
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

y <- ggplot(data = subset(x, variable != "agb_change"), aes(x = variable, y = value)) +
  stat_summary(fun.data = quantiles_95, geom = "boxplot", show.legend = FALSE, outlier.shape = NA) +
 # geom_boxplot() +
  scale_y_continuous(name = "Percent") +
  geom_point(aes(color = flp, shape = flp), alpha = 1, size = 3,  stroke = 2, position=position_jitter(width=0.1, height=0)) +
  scale_x_discrete(labels = c("turnover" = "Turnover",
                              "mortality" = "Mortality",
                              "recruitment" = "Recruitment")) +
  labs(x = "") +
  scale_color_manual(labels = c("Tambopata", "Finca las Piedras"), values = c("black", "blue")) +
  scale_shape_manual(labels = c("Tambopata", "Finca las Piedras"), values = c(16, 1)) +
  labs(color = "Plot", shape = "Plot") +
  theme_classic()+
  theme(legend.position = c(.8,.92),
        legend.background = element_blank(),
        legend.box.background  = element_rect(color = "black")) 
y

z <- ggplot(data = subset(x, variable == "agb_change"), aes(x = variable, y = value)) +
  stat_summary(fun.data = quantiles_95, geom = "boxplot", show.legend = FALSE, outlier.shape = NA) +
  scale_y_continuous(name = "Change in tons") +
  geom_point(aes(color = flp, shape = flp), alpha = 1, size = 3, stroke = 2, position=position_jitter(width=0.15, height=0.1)) +
  scale_x_discrete(labels = c("agb_change" = "AGB change")) +
  labs(x = "") +
  scale_color_manual(values = c("black", "blue")) +
  scale_shape_manual(values = c(16, 1)) +
  theme_classic() +
  theme(legend.position="none") 
z

plot_grid(z,y, ncol=2, rel_widths = c(1/3, 2/3))



###relate plot diversity to precipitation
#first combine all plots together and make a matrix
all_plots <- do.call(rbind, lapply(names(plots), function(name) {
  df <- plots[[name]]
  df_selected <- df %>% dplyr::select(2, 3) %>% mutate(Plot = name)
  return(df_selected)
}))

#remove TAM_03 and TRC_01_2009
all_plots <- all_plots %>% filter(Plot != "TRC_01_2009")
all_plots <- all_plots %>% filter(Plot != "TAM_03")

x <- flp_24%>%
  filter(F1 != "0")
x <- x[,c("Family", "Species")] 
x$Plot <- "FLP_01"

all_plots <- rbind(x, all_plots)
all_plots$Plot <- ifelse(all_plots$Plot == "TRC_01_2020", "TRC_01", all_plots$Plot)
plot_mat <- acast(all_plots, Plot ~ Species) 

plot_data <- read_excel("plot spp data/plot_metadata.xlsx")
plot_data <- data.frame(plot_data)

# do some diversity stats 
x <- as.data.frame(diversity(plot_mat, index = "shannon", MARGIN = 1)) #Shannon index for each plot
x <- rownames_to_column(x)
colnames(x)[1] <- "Plot"
colnames(x)[2] <- "H"
plot_data <- left_join(plot_data, x, by = "Plot")

#simpson
x <- as.data.frame(diversity(plot_mat, index = "simpson", MARGIN = 1)) #Simpson index for each plot
x <- rownames_to_column(x)
colnames(x)[1] <- "Plot"
colnames(x)[2] <- "Simpson"
plot_data <- left_join(plot_data, x, by = "Plot")

#inverse simpson
x <- as.data.frame(diversity(plot_mat, index = "inv", MARGIN = 1)) #Inv Simpson index for each plot
x <- rownames_to_column(x)
colnames(x)[1] <- "Plot"
colnames(x)[2] <- "Inv"
plot_data <- left_join(plot_data, x, by = "Plot")

#spp richness and stem density
TRC_01 <- TRC_01_2020
#replace NA values with 0 in DBH column of TRC_01
TRC_01$DBH <- ifelse(is.na(TRC_01$DBH), 10, TRC_01$DBH)

plots <- list(ALM_01 = ALM_01, CUZ_01 = CUZ_01, CUZ_02 = CUZ_02, CUZ_03 = CUZ_03, CUZ_04 = CUZ_04, LAS_02 = LAS_02, MNU_05 = MNU_05, MNU_06 = MNU_06, POR_01 = POR_01, POR_02 = POR_02, RFH_01 = RFH_01, TAM_01 = TAM_01, TAM_02 = TAM_02, TAM_03 = TAM_03, TAM_04 = TAM_04, TAM_05 = TAM_05, TAM_06 = TAM_06, TAM_07 = TAM_07, TAM_08 = TAM_08, TAM_09 = TAM_09, TRC_01 = TRC_01)
plots_names <- c("ALM_01", "CUZ_01", "CUZ_02", "CUZ_03", "CUZ_04", "LAS_02", "MNU_05", "MNU_06", "POR_01", "POR_02", "RFH_01", "TAM_01", "TAM_02", "TAM_03", "TAM_04", "TAM_05", "TAM_06", "TAM_07", "TAM_08", "TAM_09", "TRC_01")
plots_names <- as.data.frame(plots_names)

for(i in 1:length(plots)){
  x <- plots[[i]]
  x <- filter(x, x[,c(4)] > 0)
  spp <- length(unique(x$Species))
  plots_names$no_spp[[i]] <- spp
  
  stems <- length(x$Species)
  plots_names$stems[[i]] <- stems
}
colnames(plots_names)[1] <- "Plot"

plot_data2 <- left_join(plot_data, plots_names, by = "Plot")
plot_data2$no_spp <- ifelse(plot_data2$Plot == "FLP_01", 181, plot_data2$no_spp)
plot_data2$no_spp <- as.numeric(plot_data2$no_spp)
plot_data2$stems <- ifelse(plot_data2$Plot == "FLP_01", 583, plot_data2$stems)
plot_data2$stems <- as.numeric(plot_data2$stems)

# Calculate evenness for each plot 
plot_data2$evenness <-  (plot_data2$H)/(log(plot_data2$no_spp))


ggplot(data = subset(plot_data2, size == 1)) +
  geom_bar(stat = "identity", mapping = aes(x= reorder(Plot, -no_spp), y = no_spp)) +
  labs(x = "Site", y = "Species", title = "Species richness in 1-ha permanent plots in the southwestern Amazon") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
#scale_color_manual(values=c("Y"="blue2","N"="black"), guide="none") 

ggplot(data = plot_data2) +
  geom_bar(stat = "identity", mapping = aes(x= reorder(Plot, -H), y = H)) +
  labs(x = "Site", y = "Species", title = "Shannon diversity index of permanent plots in the southwestern Amazon") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 



#load in the precipitation tif from WorldClim
# Extract each climate variable value for each plot
# Rename latitude and longitude into new dataframe: coords
lats <- plot_data2$lat
lons <- plot_data2$lon
coords <- data.frame(x=lons, y=lats)

# Load rasters
raster.list = list.files(  
  pattern = "*.tif", full.names=T)
rasters <-lapply(raster.list, raster)

coordinates(coords) <- ~ x + y
proj4string(coords) <- CRS("+proj=longlat +datum=WGS84")

extract_from_raster <- function(raster_layer, coords) {
  extract(raster_layer, coords)
}

extracted_data <- lapply(rasters, extract_from_raster, coords = coords)

extracted_df <- do.call(cbind, extracted_data)
colnames(extracted_df) <- paste0("Raster_", seq_along(rasters))
extracted_df <- as.data.frame(extracted_df)
plot_data2 <- cbind(plot_data2, extracted_df)

#get mean annual precip by summing all the montly precipitation values
plot_data2$map <- (plot_data2$Raster_1 + plot_data2$Raster_2 + plot_data2$Raster_3 + plot_data2$Raster_4 + plot_data2$Raster_5 + plot_data2$Raster_6 + plot_data2$Raster_7 + plot_data2$Raster_8 + plot_data2$Raster_9 + plot_data2$Raster_10 + plot_data2$Raster_11 + plot_data2$Raster_12)
plot_data2 <- plot_data2[,-c(18:29)] 
plot_data2[6,18] <- 2600 #los amigos is wrong so we have to change that manually

#add a column for each plot cluster
plot_data2$group <- ifelse(grepl("TAM", plot_data2$Plot) | grepl("TRC", plot_data2$Plot), "Tambopata", "Manu")
plot_data2$group <- ifelse(grepl("CUZ", plot_data2$Plot), "Cuzco Amazonico", plot_data2$group)
plot_data2$group <- ifelse(grepl("POR", plot_data2$Plot)|grepl("RFH", plot_data2$Plot), "Acre", plot_data2$group)
plot_data2$group <- ifelse(grepl("FLP", plot_data2$Plot), "Finca las Piedras", plot_data2$group)
plot_data2$group <- ifelse(grepl("LAS", plot_data2$Plot), "Los Amigos", plot_data2$group)


#correlate diversity metrics with MAP and plot
summary(lm(H ~ map, data = plot_data2)) #no effect of precipitation on diversity
summary(lm(no_spp ~ map, data = subset(plot_data2, size == 1)))
summary(lm(evenness ~ map, data = plot_data2))
summary(lm(stems ~ map, data = subset(plot_data2, size == 1)))

#make spp richness NA for plots that aren't 1-ha
plot_data2$no_spp <- ifelse(plot_data2$size != 1, NA, plot_data2$no_spp)


#make data frame long to put into a facet_wrap
x <- plot_data2 %>% 
  pivot_longer(
    cols = c(12,15,17), 
    names_to = "div",
    values_to = "value"
  )


ggplot(transform(x,
                 div=factor(div,levels=c("H","evenness","no_spp"))), aes(x = map, y = value, color = group, shape = type2)) +
  geom_point(size = 2.5) +
  #scale_color_manual(values = c("#fd5f6d", "#7eb0d5", "#b2e061", "#bd7ebe", "#ffb55a", "#ffee65")) +
  scale_color_manual(values = c("#4053d3", "#ebbf23", "#b51d14", "#00beff", "#fb49b0", "#00b25d")) +
  facet_wrap(~div, scales="free_y", ncol = 1, strip.position = "left",
             labeller = as_labeller(c(H = "Shannon (H)", no_spp = "Species richness", evenness = "Evenness" ) ) ) +
  guides(color = guide_legend(title="Plot cluster"),
         shape = guide_legend(title = "Habitat")) +
  ylab(NULL) +
  labs(x = "Mean annual precipitation (mm)", title = "Diversity and precipitation of plots \nin the southwest Amazon") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.placement = "outside")


bn <- all_plots %>%
  filter(Species == "Bertholletia excelsa")

### make some rarefaction curves
#make a community matrix and convert to dataframe
plot_mat <- as.data.frame(plot_mat)

#rarefaction
rarefy(plot_mat, sample = 303)
rarecurve_data <- rarecurve(plot_mat, step = 1)

rare_df <- map_dfr(rarecurve_data, bind_rows) %>% 
  bind_cols(Plot = rownames(plot_mat),.) %>%
  pivot_longer(-Plot) %>%
  drop_na() %>%
  mutate(n_occs = as.numeric(str_replace(name, "N", ""))) %>%
  dplyr::select(-name)

rare_df<- merge(rare_df, plot_data2[,c(1,19)], by = "Plot")

order <- c("Manu", "Tambopata", "Cuzco Amazonico", "Acre", "Los Amigos", "Finca las Piedras")
rare_df$group <- factor(rare_df$group, levels = order)
order <- c("ALM_01", "MNU_05", "MNU_06", "TAM_01", "TAM_02", "TAM_04", "TAM_05", "TAM_06", "TAM_07", "TAM_08", "TAM_09", "TRC_01", "LAS_02", "RFH_01", "POR_01", "POR_02", "CUZ_01", "CUZ_02", "CUZ_03", "CUZ_04", "FLP_01")
rare_df$Plot <- factor(rare_df$Plot, levels = order)

options(scipen = 999)

group.colors <- c("Acre" = "#4053d3", "Cuzco Amazonico" = "#ebbf23", "Los Amigos" = "#00beff", "Manu" = "#fb49b0", "Tambopata" = "#00b25d", "Finca las Piedras" = "#b51d14")

ggplot(data = rare_df, aes(x = n_occs, y = value, group = Plot)) +
  theme_classic() +
  geom_line(aes(color = group), size = 1.2) +
  #scale_color_manual(name = "Plot cluster", values = c("#4053d3", "#ebbf23", "#b51d14", "#00beff", "#fb49b0", "#00b25d")) +
  scale_color_manual(name = "Plot cluster", values = group.colors) +
  xlab("Stems") +
  ylab("Rarefied species richness") +
  theme(legend.position = c(0.8, 0.25),
        legend.text = ) 

#cluster analysis
#make a community dissimilarity matrices and cluster plots
plot_dist <- vegdist(plot_mat, method = "bray", binary = FALSE)
pcluster <- hclust(plot_dist, "average")
plot(pcluster,
     main = "Cluster dendrogram", xlab = "", sub="")



###make table S1 - Plot details
#write.csv(plot_data2, file = "TableS1.csv")
#write.csv(plots_agb, file = "plots_agb.csv")

###make table with descriptors for each species in FLP-01
#summarize basal area by species
sp_ba <- flp_24 %>%
  group_by(Species) %>%
  summarise(total_ba = sum(ba))
sum(sp_ba$total_ba)
sp_ba$perc_ba <- (sp_ba$total_ba)/33010706

#make genus column
flp_24$Genus <- word(flp_24$Species, 1)
flp_24$Epithet <- word(flp_24$Species, 2)

#estimate biomass
flp_24$biomass <- computeAGB(flp_24$DBH, flp_24$meanWD, flp_24$Height) #for each tree

#Make table with Family, Species, n stems, n inds, WD, BA, %ba
#calculate plot basal area
table <- flp_24 %>%
  group_by(Family, Species) %>%
  summarise(
    "Number of individuals" = n(),
    "Max DBH" = max(DBH),
    "Wood density" = mean(meanWD),
    "Basal area" = sum(ba),
    "Percent of plot basal area" = (sum(ba)/33010706)*100
  ) 

#write.csv(table, file = "TableS2.csv")



###compositional analyses
#NMDS to compare species composition between plots

#Make a function to produce a scree plot for different dimensions of the NMDS
NMDS.scree <- function(x) { #where x is the name of the data frame variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS scree plot")
  abline(h=0.1, col = "gray", lty=2)
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
  }
}

#NMDS.scree(plot_mat) #looks like 2 dimensions is best

#Do the NMDS and plot
#set.seed(12)
NMDS <- metaMDS(plot_mat, distance = "bray", k=2) 
stressplot(NMDS) 

plot(NMDS)
orditorp(NMDS,display="species",col="red",air=0.01)

#Add NMDS scores to plot data
NMDSscores <- as.data.frame(scores(NMDS)$sites)
NMDSscores <- rownames_to_column(NMDSscores)
NMDSscores <- NMDSscores %>%
  dplyr::rename(Plot=rowname)

plot_data2 <- left_join(plot_data2, NMDSscores, by = "Plot")
plot_data2 <- plot_data2[-c(14),] #remove TAM_03

grp.a <- plot_data2[plot_data2$type2 == "floodplain", ][chull(plot_data2[plot_data2$type2 == 
                                                                   "floodplain", c("NMDS1", "NMDS2")]), ]  # hull values for floodplain
grp.b <- plot_data2[plot_data2$type2 == "terra firme", ][chull(plot_data2[plot_data2$type2 == 
                                                                   "terra firme", c("NMDS1", "NMDS2")]), ]  # hull values for terra firme
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b
hull.data

grp.c <- plot_data2[plot_data2$country == "Peru", ][chull(plot_data2[plot_data2$country == 
                                                                           "Peru", c("NMDS1", "NMDS2")]), ]  # hull values for Peru plots
grp.d <- plot_data2[plot_data2$country == "Brazil", ][chull(plot_data2[plot_data2$country == 
                                                                            "Brazil", c("NMDS1", "NMDS2")]), ]  # hull values for Acre plots
hull.data2 <- rbind(grp.c, grp.d)  #combine grp.c and grp.d
hull.data2

# precipitation arrow
en <- envfit(NMDS, plot_data2[, c(18)], permutations = 999)
head(en)
env.scores <- as.data.frame(scores(en, display = "vectors"))
env.scores <- cbind(env.scores, env.variables = rownames(env.scores))
env.scores[1,3] <- "MAP"

# radial shift function
rshift = function(r, theta, a=0.03, b=0.07) {
  r + a + b*abs(cos(theta))
}

# Calculate shift
env.scores = env.scores %>% 
  mutate(r = sqrt(NMDS1^2 + NMDS2^2),
         theta = atan2(NMDS2,NMDS1),
         rnew = rshift(r, theta),
         xnew = rnew*cos(theta),
         ynew = rnew*sin(theta))


ggplot(plot_data2, aes(x = NMDS1, y = NMDS2)) +
  geom_hline(yintercept=0, linetype="dashed", color = "gray") +
  geom_vline(xintercept=0, linetype="dashed", color = "gray") +
  geom_polygon(data = hull.data, aes(x=NMDS1, y=NMDS2, fill=type2, group=type2), alpha=0.20) + # add the convex hulls
  geom_polygon(data = hull.data2, aes(x=NMDS1, y=NMDS2, group = country, linetype = country), color = "black", size = 0.5, alpha = 0) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_point(size = 2.5, aes(color = type2)) +
  geom_text(data = env.scores, aes(x = xnew, y = ynew, label=env.variables), size = 4.5) +
  scale_colour_manual(values=c("floodplain" = "green4", "terra firme" = "orange2")) +
  scale_fill_manual(values = c("floodplain" = "green4", "terra firme" = "gold")) +
  geom_text_repel(data = subset(plot_data2, Plot != "FLP_01"), aes(label = Plot)) +
  geom_text_repel(data = subset(plot_data2, Plot == "FLP_01"), aes(fontface = "bold", label = Plot)) +
  guides(color = guide_legend(title="Habitat"),
         fill = guide_legend(title="Habitat"),
         linetype = guide_legend(title="Country", reverse = T)) +
  geom_segment(data = env.scores, aes(x = 0, xend = 1*NMDS1, y = 0, yend = 1*NMDS2), lineend = "round", arrow = arrow(length = unit(0.25, "cm")), color = "black", size = 1) +
  theme_classic() +
  theme(legend.position = c(.2,.1),
        legend.box = "horizontal")

###end of script




