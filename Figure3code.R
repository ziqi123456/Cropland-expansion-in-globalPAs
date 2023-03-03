library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(SuppDists)
library(ggbeeswarm)
library(viridis)
library(cowplot)

data <- read.csv("xxx/Figure3.csv")

figtheme <- theme(panel.background=element_rect(fill="white",colour="black",size=1.2),
                  axis.ticks.length.y = unit(.3, "cm"),
                  axis.ticks.length.x = unit(0.3, "cm"),
                  axis.title=element_text(size=20,color="black", face = "bold"),
                  axis.text = element_text(size=18,color="black"),
                  legend.position = "none"
)


commapos <- function(x, ...) {
  format(abs(x), big.mark = ",", trim = TRUE,
         scientific = FALSE, ...) }
commapos <- function(x, ...) {
  format(x/1000,  trim = TRUE,
         scientific = FALSE, ...) }

data$Realm[data$Realm == 5] <- "Afrotropic"
data$Realm[data$Realm == 2] <- "Australasia"
data$Realm[data$Realm == 7] <- "Indomalaya"
data$Realm[data$Realm == 4] <- "Nearctic"
data$Realm[data$Realm == 6] <- "Neotropic"
data$Realm[data$Realm == 3] <- "Palearctic"

data$IUCNCAT[data$IUCNCAT == 10] <- "No Category"
data$IUCNCAT[data$IUCNCAT == 4] <- "Ia"
data$IUCNCAT[data$IUCNCAT == 9] <- "Ib"
data$IUCNCAT[data$IUCNCAT == 2] <- "II"
data$IUCNCAT[data$IUCNCAT == 3] <- "III"
data$IUCNCAT[data$IUCNCAT == 6] <- "IV"
data$IUCNCAT[data$IUCNCAT == 1] <- "V"
data$IUCNCAT[data$IUCNCAT == 8] <- "VI"

data$SizeCAT[data$SizeCAT == 10] <- "1~10"
data$SizeCAT[data$SizeCAT == 20] <- "10~20"
data$SizeCAT[data$SizeCAT == 30] <- "20~30"
data$SizeCAT[data$SizeCAT == 40] <- "30~40"
data$SizeCAT[data$SizeCAT == 50] <- "40~50"
data$SizeCAT[data$SizeCAT == 100] <- "50~100"
data$SizeCAT[data$SizeCAT == 1000] <- "100~1000"
data$SizeCAT[data$SizeCAT == 10000] <- "1000~10000"
data$SizeCAT[data$SizeCAT == 100000] <- ">10000"

data <- na.omit(data)
data$Realm <- factor(as.character(data$Realm), levels = c("Palearctic","Neotropic","Nearctic", "Indomalaya","Australasia",
                                                          "Afrotropic"
))

RealmBar <- ggplot(data, aes(x = Realm, y = trend0319, fill = Class))+ 
  geom_bar(stat = "summary", fun ="mean", position = position_dodge(), width = 0.6) +
  stat_summary(fun.data = 'mean_sd', geom = "errorbar", colour = "black",
               width = 0.15,position = position_dodge( .5), alpha = 0.2)+
  scale_fill_manual(values = c(alpha("#5e3c99", 1),alpha("#b2abd2",1),alpha("#fdb863", 1),alpha("#e66101", 1)))+
  xlab("Realms")+
  ylab("")+
  theme_classic()+
  geom_hline(yintercept=0)+
  coord_flip()+
  figtheme


data$IUCNCAT <- factor(as.character(data$IUCNCAT), levels = c("No Category","VI", "V",
                                                              "IV","III","II","Ib","Ia" 
))

IUCNBar <- ggplot(data, aes(x = IUCNCAT, y = trend0319, fill = Class))+ 
  geom_bar(stat = "summary", fun ="mean", position = position_dodge(), width = 0.6) +
  stat_summary(fun.data = 'mean_sd', geom = "errorbar", colour = "black",
               width = 0.15,position = position_dodge( .5), alpha = 0.2)+
  scale_fill_manual(values = c(alpha("#5e3c99", 1),alpha("#b2abd2",1),alpha("#fdb863", 1),alpha("#e66101", 1)))+
  xlab("IUCN Category")+
  ylab("")+
 theme_classic()+
  geom_hline(yintercept=0)+
  coord_flip()+
  figtheme


data$SizeCAT <- factor(as.character(data$SizeCAT), levels = c(">10000","1000~10000","100~1000","50~100","40~50","30~40","20~30","10~20","1~10")) 

SizeBar <- ggplot(data, aes(x = SizeCAT, y = trend0319, fill = Class))+ 
  geom_bar(stat = "summary", fun ="mean", position = position_dodge(), width = 0.6) +
  stat_summary(fun.data = 'mean_sd', geom = "errorbar", colour = "black",
               width = 0.15,position = position_dodge( .5), alpha = 0.2)+
  scale_fill_manual(values = c(alpha("#5e3c99", 1),alpha("#b2abd2",1),alpha("#fdb863", 1),alpha("#e66101", 1)),labels = c("PA", "CA"))+
  xlab("Size")+
  ylab("")+
  theme_classic()+
  geom_hline(yintercept=0)+
  coord_flip()+
  figtheme_legend

labels_with_superscript <-  c("(km^2)")
data_draw <- ggdraw()+
  draw_plot(RealmBar, x = 0.007, y = 0, width = .33, height = .94)+
  draw_plot(IUCNBar, x = 0.34, y = 0, width = .33, height = .94)+
  draw_plot(SizeBar, x = 0.67, y = 0, width = .33, height = .94)+
  
  draw_plot_label(label = c("a", "b", "c"), size = 36,
                  x = c(0.01, 0.35, 0.685), y = c(0.97,0.97,0.97))+
  draw_plot_label(label = c("Cropland Change Rate (%)"), size = 20,
                  x = c(0.33), y = c(0.06))+
  draw_plot_label(labels_with_superscript, size = 20,
                  x = c(0.72), y = c(0.15), parse = TRUE)

ggsave("xxx.png", plot = data_draw, height = 7, width = 15, dpi = 600) 
