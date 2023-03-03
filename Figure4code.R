library(janitor)
library(CGPfunctions)

BIOexp <- read.csv("Figure4.csv")

## replace Acrop.aMER: 1AC-1M to 1...
BIOexp$Acrop.aMER[which(BIOexp$Acrop.aMER =='1AC~1M')] <- 1
BIOexp$Acrop.aMER[which(BIOexp$Acrop.aMER =='2AC~1M')] <- 2
BIOexp$Acrop.aMER[which(BIOexp$Acrop.aMER =='3AC~1M')] <- 3
BIOexp$Acrop.aMER[which(BIOexp$Acrop.aMER =='1AC~2M')] <- 4
BIOexp$Acrop.aMER[which(BIOexp$Acrop.aMER =='2AC~2M')] <- 5
BIOexp$Acrop.aMER[which(BIOexp$Acrop.aMER =='3AC~2M')] <- 6
BIOexp$Acrop.aMER[which(BIOexp$Acrop.aMER =='1AC~3M')] <- 7
BIOexp$Acrop.aMER[which(BIOexp$Acrop.aMER =='2AC~3M')] <- 8
BIOexp$Acrop.aMER[which(BIOexp$Acrop.aMER =='3AC~3M')] <- 9

## replace Acrop.aPTS: 1AC-1P to 1...
BIOexp$Acrop.aPTS[which(BIOexp$Acrop.aPTS =='1AC~1P')] <- 1
BIOexp$Acrop.aPTS[which(BIOexp$Acrop.aPTS =='2AC~1P')] <- 2
BIOexp$Acrop.aPTS[which(BIOexp$Acrop.aPTS =='3AC~1P')] <- 3
BIOexp$Acrop.aPTS[which(BIOexp$Acrop.aPTS =='1AC~2P')] <- 4
BIOexp$Acrop.aPTS[which(BIOexp$Acrop.aPTS =='2AC~2P')] <- 5
BIOexp$Acrop.aPTS[which(BIOexp$Acrop.aPTS =='3AC~2P')] <- 6
BIOexp$Acrop.aPTS[which(BIOexp$Acrop.aPTS =='1AC~3P')] <- 7
BIOexp$Acrop.aPTS[which(BIOexp$Acrop.aPTS =='2AC~3P')] <- 8
BIOexp$Acrop.aPTS[which(BIOexp$Acrop.aPTS =='3AC~3P')] <- 9

## replace Rcrop.aMER: 1RC-1M to 1...
BIOexp$Rcrop.aMER[which(BIOexp$Rcrop.aMER =='1RC~1M')] <- 1
BIOexp$Rcrop.aMER[which(BIOexp$Rcrop.aMER =='2RC~1M')] <- 2
BIOexp$Rcrop.aMER[which(BIOexp$Rcrop.aMER =='3RC~1M')] <- 3
BIOexp$Rcrop.aMER[which(BIOexp$Rcrop.aMER =='1RC~2M')] <- 4
BIOexp$Rcrop.aMER[which(BIOexp$Rcrop.aMER =='2RC~2M')] <- 5
BIOexp$Rcrop.aMER[which(BIOexp$Rcrop.aMER =='3RC~2M')] <- 6
BIOexp$Rcrop.aMER[which(BIOexp$Rcrop.aMER =='1RC~3M')] <- 7
BIOexp$Rcrop.aMER[which(BIOexp$Rcrop.aMER =='2RC~3M')] <- 8
BIOexp$Rcrop.aMER[which(BIOexp$Rcrop.aMER =='3RC~3M')] <- 9

## replace Rcrop.aPTS: 1RC-1P to 1...
BIOexp$Rcrop.aPTS[which(BIOexp$Rcrop.aPTS =='1RC~1P')] <- 1
BIOexp$Rcrop.aPTS[which(BIOexp$Rcrop.aPTS =='2RC~1P')] <- 2
BIOexp$Rcrop.aPTS[which(BIOexp$Rcrop.aPTS =='3RC~1P')] <- 3
BIOexp$Rcrop.aPTS[which(BIOexp$Rcrop.aPTS =='1RC~2P')] <- 4
BIOexp$Rcrop.aPTS[which(BIOexp$Rcrop.aPTS =='2RC~2P')] <- 5
BIOexp$Rcrop.aPTS[which(BIOexp$Rcrop.aPTS =='3RC~2P')] <- 6
BIOexp$Rcrop.aPTS[which(BIOexp$Rcrop.aPTS =='1RC~3P')] <- 7
BIOexp$Rcrop.aPTS[which(BIOexp$Rcrop.aPTS =='2RC~3P')] <- 8
BIOexp$Rcrop.aPTS[which(BIOexp$Rcrop.aPTS =='3RC~3P')] <- 9

figtheme1 <- theme(panel.background=element_rect(fill="white",colour="black",size=1.2),
                   axis.ticks.length.y = unit(.3, "cm"),
                   axis.ticks.x = element_blank(),
                   axis.title.y=element_text(size=18,color="black", face = "bold"),
                   axis.title.x = element_blank(),
                   axis.text.y = element_text(size=16,color="black"),
                   axis.text.x = element_blank(),
                   legend.position = "none"
)
figtheme2 <- theme(panel.background=element_rect(fill="white",colour="black",size=1.2),
                   axis.ticks.y = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.title = element_blank(),
                   axis.text = element_blank(),
                   legend.position = "none"
)
figtheme3 <- theme(panel.background=element_rect(fill="white",colour="black",size=1.2),
                   axis.ticks.y = element_blank(),
                   axis.ticks.length.x = unit(0.3, "cm"),
                   axis.title=element_text(size=18,color="black", face = "bold"),
                   axis.text.x = element_text(angle = 45, hjust = 1,size=16,color="black"),
                   axis.text.y = element_text(hjust = 1,size=16,color="black"),
                   legend.position = "none"
)
figtheme4 <- theme(panel.background=element_rect(fill="white",colour="black",size=1.2),
                   axis.ticks.y = element_blank(),
                   axis.ticks.length.x = unit(0.3, "cm"),
                   axis.title.x=element_text(size=18,color="black", face = "bold"),
                   axis.title.y = element_blank(),
                   axis.text.y = element_blank(),
                   axis.text.x = element_text(angle = 45, hjust = 1,size=16,color="black"),
                   legend.position = "none"
)

BIOexp$Acrop.aMER <- factor(as.character(BIOexp$Acrop.aMER), levels = c(9,8,7,6,5,4,3,2,1))
BIOexp$Acrop.aPTS <- factor(as.character(BIOexp$Acrop.aPTS), levels = c(9,8,7,6,5,4,3,2,1))
BIOexp$Rcrop.aMER <- factor(as.character(BIOexp$Rcrop.aMER), levels = c(9,8,7,6,5,4,3,2,1))
BIOexp$Rcrop.aPTS <- factor(as.character(BIOexp$Rcrop.aPTS), levels = c(9,8,7,6,5,4,3,2,1))

############### Realm ###############
Realm.AcropAMER <- ggplot(BIOexp[BIOexp$Realm == c("Afrotropic", "Australasia",
                                                   "Indomalaya", "Nearctic",
                                                   "Neotropic",
                                                   "Palearctic"),], aes(fill=Acrop.aMER, x=Realm)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Oranges", direction = - 1)+
  labs(y = "Acrop & aMER", 
       x = "Realms",
  ) +
  theme_minimal()+
  figtheme1

Realm.AcropAPTS <- ggplot(BIOexp[BIOexp$Realm == c("Afrotropic", "Australasia",
                                                   "Indomalaya", "Nearctic",
                                                   "Neotropic",
                                                   "Palearctic"),], aes(fill=Acrop.aPTS, x=Realm)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Oranges", direction = - 1)+
  labs(y = "Acrop & aPTS", 
       x = "Realms",
  ) +
  theme_minimal()+
  figtheme1

Realm.RcropAMER <- ggplot(BIOexp[BIOexp$Realm == c("Afrotropic", "Australasia",
                                                   "Indomalaya", "Nearctic",
                                                   "Neotropic",
                                                   "Palearctic"),], aes(fill=Rcrop.aMER, x=Realm)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Oranges", direction = - 1)+
  labs(y = "Rcrop & aMER", 
       x = "Realms",
  ) +
  theme_minimal()+
  figtheme1

Realm.RcropAPTS <- ggplot(BIOexp[BIOexp$Realm == c("Afrotropic", "Australasia",
                                                   "Indomalaya", "Nearctic",
                                                   "Neotropic",
                                                   "Palearctic"),], aes(fill=Rcrop.aPTS, x=Realm)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Oranges", direction = - 1)+
  scale_x_discrete(labels = c("Afrotr", "Austra", "Indoma", "Nearct", "Neotro", "Palear"))+
  labs(y = "Rcrop & aPTS", 
       x = "Realms",
  ) +
  theme_minimal()+
  figtheme3

####### IUCN  ########

BIOexp$IUCN9 <- factor(as.character(BIOexp$IUCN9), levels = c("Ia", "Ib", "II","III", "IV","V","VI","No Category"))

IUCN.AcropAMER <- ggplot(BIOexp, aes(fill=Acrop.aMER, x=IUCN9)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Blues", direction = - 1)+
  labs(y = "Acrop & aMER", 
       x = "IUCN category",
  ) +
  theme_minimal()+
  figtheme2

IUCN.AcropAPTS <- ggplot(BIOexp, aes(fill=Acrop.aPTS, x=IUCN9)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Blues", direction = - 1)+
  labs(y = "Acrop & aPTS", 
       x = "IUCN category",
  ) +
  theme_minimal()+
  figtheme2

IUCN.RcropAMER <- ggplot(BIOexp, aes(fill=Rcrop.aMER, x=IUCN9)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Blues", direction = - 1)+
  labs(y = "Rcrop & aMER", 
       x = "IUCN category",
  ) +
  theme_minimal()+
  figtheme2


IUCN.RcropAPTS <- ggplot(BIOexp, aes(fill=Rcrop.aPTS, x=IUCN9)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Blues", direction = - 1)+
  scale_x_discrete(labels = c("Ia", "Ib", "II", "III", "IV", "V", "VI",  "NC"))+
  labs(y = "Rcrop & aPTS", 
       x = "IUCN category",
  ) +
  theme_minimal()+
  figtheme4

############## size ################
BIOexp$GIS.Area_Category <- factor(as.character(BIOexp$GIS.Area_Category), levels = c("<1", "1~10", 
                                                                                      "10~20", "20~30",
                                                                                      "30~40","40~50", "50~100","100~1000","1000~10000", ">10000"))
size.AcropAMER <- ggplot(BIOexp, aes(fill=Acrop.aMER, x=GIS.Area_Category)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Greens", direction = - 1)+
  labs(y = "Acrop & aMER", 
       x = "Size",
  ) +
  theme_minimal()+
  figtheme2

size.AcropAPTS <- ggplot(BIOexp, aes(fill=Acrop.aPTS, x=GIS.Area_Category)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Greens", direction = - 1)+
  labs(y = "Acrop & aPTS", 
       x = "Size",
  ) +
  theme_minimal()+
  figtheme2

size.RcropAMER <- ggplot(BIOexp, aes(fill=Rcrop.aMER, x=GIS.Area_Category)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Greens", direction = - 1)+
  labs(y = "Rcrop & aMER", 
       x = "Size",
  ) +
  theme_minimal()+
  figtheme2

size.RcropAPTS <- ggplot(BIOexp, aes(fill=Rcrop.aPTS, x=GIS.Area_Category)) + 
  geom_bar(position="fill", color = "black", alpha=0.7, width = 0.4)+
  scale_y_continuous(breaks = seq(0, 1, .5),labels = scales::percent) +
  scale_fill_brewer(palette = "Greens", direction = - 1)+
  scale_x_discrete(labels = c("<1", "10", "20","30", "40", "50", "100", "1000", "10000", " >10000"))+
  labs(y = "Rcrop & aPTS", 
       x = "Size",
  ) +
  theme_minimal()+
  figtheme4

zong_draw <- ggdraw()+
  draw_plot(Realm.RcropAPTS, x = 0.003, y = 0.003, width = 0.35, height = 0.28)+
  draw_plot(IUCN.RcropAPTS, x = 0.363, y = 0.021, width = 0.27, height = 0.262)+
  draw_plot(size.RcropAPTS, x = 0.643, y = 0, width = 0.28, height = 0.283)+
  
  draw_plot(Realm.RcropAMER, x = 0, y = 0.29, width = 0.352, height = 0.22)+
  draw_plot(IUCN.RcropAMER, x = 0.363, y = 0.29, width = 0.27, height = 0.22)+
  draw_plot(size.RcropAMER, x = 0.643, y = 0.29, width = 0.28, height = 0.22)+
  
  draw_plot(Realm.AcropAPTS, x = 0, y = 0.52, width = 0.35, height = 0.22)+
  draw_plot(IUCN.AcropAPTS, x = 0.363, y = 0.52, width = 0.27, height = 0.22)+
  draw_plot(size.AcropAPTS, x = 0.643, y = 0.52, width = 0.28, height = 0.22)+
  
  draw_plot(Realm.AcropAMER, x = 0, y = 0.75, width = 0.35, height = 0.22)+
  draw_plot(IUCN.AcropAMER, x = 0.363, y = 0.75, width = 0.27, height = 0.22)+
  draw_plot(size.AcropAMER, x = 0.643, y = 0.75, width = 0.28, height = 0.22)+
  
  draw_plot_label(label = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"), size = 30,
                  x = c(0.078, 0.37, 0.65,0.078, 0.37, 0.65,0.078, 0.37, 0.65,0.078, 0.37, 0.65), 
                  y = c(1,1,1, 0.765,0.765,0.765, 0.535,0.535,0.535, 0.305,0.305,0.305 ))

ggsave("xxx", plot = zong_draw, height = 16, width = 12, dpi = 600)
