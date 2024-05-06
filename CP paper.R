#CP Select paper

getwd()
library(tidyverse)
library(gapminder)
library(ggplot2)
library(ggbeeswarm)
library(rstatix)
library(ggpubr)
library(dplyr)
library(survival)
library(corrplot)
library(NADA)
library(NADA2)
library(EnvStats)
library(stats)
library(base)
library(ggsignif)
library(readxl)
library(readxl)
library(patchwork)
library(cowplot)
library(lattice)
library(PASWR)


####CP round 1 *4 ####

library(readxl)
Round1 <- read_excel("A2.xlsx", col_types = c("text", 
                                          "text", "text", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric"))
head(Round1)

rrna <- ggplot(Round1, aes(x=Name, y=rna, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("ModifiedY", "ENFY")
  ), test = "t.test", color = "black",
  map_signif_level = function(p) {
    if(p < 0.001) {
      return("p < 0.001***")
    } else if(p < 0.01) {
      return("p < 0.01**")
    } else if(p < 0.05) {
      return("p < 0.05*")
    } else {
      return(sprintf("p = %.3f", p))
    }
  }, textsize = 3, y_position = c(9.1)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("16SrRNA " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.1, 0.98),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("DefaultY", "DefaultN", "ModifiedY", "ModifiedN", "ENFY", "ENFN")) +
  scale_fill_manual(values=c("#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3"),
                    labels=c("No", "Yes"))

rrna
save_plot("Bx1rna.jpeg", rrna)

#sfmD
head(Round1)
sfmd <- ggplot(Round1, aes(x=Name, y=sfmd, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("ModifiedY", "ENFY")
  ), test = "t.test", color = "black",
  map_signif_level = function(p) {
    if(p < 0.001) {
      return("p < 0.001***")
    } else if(p < 0.01) {
      return("p < 0.01**")
    } else if(p < 0.05) {
      return("p < 0.05*")
    } else {
      return(sprintf("p = %.3f", p))
    }
  }, textsize = 3, y_position = c(4.2)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("sfmD " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("DefaultY", "DefaultN", "ModifiedY", "ModifiedN", "ENFY", "ENFN")) +
  scale_fill_manual(values=c("#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3"),
                    labels=c("No", "Yes"))

sfmd
save_plot("Bx2sfmd.jpeg", sfmd)

#intl1
head(Round1)
intl <- ggplot(Round1, aes(x=ID_site, y=intl, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("Defaultu", "Defaultd"), 
                                 c("Modifiedu", "Modifiedd"),
                                 c("ENFu", "ENFd")
  ), test = "t.test", color = "black",
  map_signif_level = function(p) {
    if(p < 0.001) {
      return("p < 0.001***")
    } else if(p < 0.01) {
      return("p < 0.01**")
    } else if(p < 0.05) {
      return("p < 0.05*")
    } else {
      return(sprintf("p = %.3f", p))
    }
  }, textsize = 3, y_position = c(11.5, 11.5, 11.5)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("intl1 " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("Defaultu", "Defaultd", 
                            "Modifiedu", "Modifiedd", 
                            "ENFu", "ENFd")) +
  scale_fill_manual(values=c("#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3"),
                    labels=c("No", "Yes"))

intl
save_plot("Bxintl.jpeg", intl)


#sul1

head(Round1)

sul <- ggplot(Round1, aes(x=ID_site, y=sul, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("Defaultu", "Defaultd"), 
                                 c("Modifiedu", "Modifiedd"),
                                 c("ENFu", "ENFd")
  ), test = "t.test", color = "black",
  map_signif_level = function(p) {
    if(p < 0.001) {
      return("p < 0.001***")
    } else if(p < 0.01) {
      return("p < 0.01**")
    } else if(p < 0.05) {
      return("p < 0.05*")
    } else {
      return(sprintf("p = %.3f", p))
    }
  }, textsize = 3, y_position = c(8.1, 8.1, 8.1)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("Sul1 " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("Defaultu", "Defaultd", 
                            "Modifiedu", "Modifiedd", 
                            "ENFu", "ENFd")) +
  scale_fill_manual(values=c("#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3"),
                    labels=c("No", "Yes"))

sul
save_plot("Bxsul.jpeg", sul)

#PMMoV
head(Round1)

PMMoV <- ggplot(Round1, aes(x=Name, y=PMMoV, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("ModifiedY", "ENFY")
  ), test = "t.test", color = "black",
  map_signif_level = function(p) {
    if(p < 0.001) {
      return("p < 0.001***")
    } else if(p < 0.01) {
      return("p < 0.01**")
    } else if(p < 0.05) {
      return("p < 0.05*")
    } else {
      return(sprintf("p = %.3f", p))
    }
  }, textsize = 3, y_position = c(6.1)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("PMMoV " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("DefaultY", "DefaultN", "ModifiedY", "ModifiedN", "ENFY", "ENFN")) +
  scale_fill_manual(values=c("#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3"),
                    labels=c("No", "Yes"))

PMMoV
save_plot("BxPMMoV.jpeg", PMMoV)

#CrAss
head(Round1)

crAss <- ggplot(Round1, aes(x=Name, y=crAss, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("ModifiedY", "ENFY")
  ), test = "t.test", color = "black",
  map_signif_level = function(p) {
    if(p < 0.001) {
      return("p < 0.001***")
    } else if(p < 0.01) {
      return("p < 0.01**")
    } else if(p < 0.05) {
      return("p < 0.05*")
    } else {
      return(sprintf("p = %.3f", p))
    }
  }, textsize = 3, y_position = c(8.1)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("crAssphage " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("DefaultY", "DefaultN", "ModifiedY", "ModifiedN", "ENFY", "ENFN")) +
  scale_fill_manual(values=c("#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3", "#FFF6E9", "#40A2E3"),
                    labels=c("No", "Yes"))

crAss
save_plot("BxcrAss.jpeg", crAss)


fig1ADXcorrww <- plot_grid(rrna, sfmd, intl, sul, PMMoV, crAss, ncol = 2,
                         labels = c("a", "b", "c", "d", "e", "f"), label_size = 11)

ggsave(file="fig1ADXcorrww.jpeg", fig1ADXcorrww, width= 190, height = 240, units = "mm", dpi=600)


####CP round 2 *4 ####
library(readxl)
Round2 <- read_excel("AX2.xlsx", col_types = c("text", 
                                            "text", "text", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", "numeric"))
head(Round2)

Mean <- round(mean(Round2$rna, na.rm = TRUE),2)

rrna2 <- ggplot(Round2, aes(x=MethodID, y=rna, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "Site") + ylab(expression("16SrRNA " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.97, 0.97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.margin = margin(t = -5, r = -5, b = -5, l = -5),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = Mean, linetype = "dashed", color = "grey") +
  scale_x_discrete(limits=c("Modif05u", "Modif05d", "Defa45u", "Defa45d", "Modif45u", "Modif45d")) +
  scale_fill_manual(values=c("#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276"),
                    labels=c("Downstream", "Upstream"))

rrna2
save_plot("Bx1rna2.jpeg", rrna2)

#sfmD
head(Round2)

sfmd2 <- ggplot(Round2, aes(x=MethodID, y=sfmd, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "Site") + ylab(expression("sfmD " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = 0.6, linetype = "dashed", color = "grey") +
  annotate("text", x = 5, y = 0.6, label = "LOQ", vjust = -0.5, hjust = 1, color = "black") +
  scale_x_discrete(limits=c("Modif05u", "Modif05d", "Defa45u", "Defa45d", "Modif45u", "Modif45d")) +
  scale_fill_manual(values=c("#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276"),
                    labels=c("Downstream", "Upstream"))

sfmd2
save_plot("Bx2sfmd2.jpeg", sfmd2)

#intl1
head(Round2)
Mean <- round(mean(Round2$intl, na.rm = TRUE),2)
intl2 <- ggplot(Round2, aes(x=MethodID, y=intl, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "Site") + ylab(expression("intl1 " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = Mean, linetype = "dashed", color = "grey") +
  scale_x_discrete(limits=c("Modif05u", "Modif05d", "Defa45u", "Defa45d", "Modif45u", "Modif45d")) +
  scale_fill_manual(values=c("#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276"),
                    labels=c("Downstream", "Upstream"))

intl2
save_plot("Bxintl2.jpeg", intl2)


#sul1
head(Round2)
Mean <- round(mean(Round2$sul, na.rm = TRUE),2)
sul2 <- ggplot(Round2, aes(x=MethodID, y=sul, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "Site") + ylab(expression("sul1 " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = Mean, linetype = "dashed", color = "grey") +
  scale_x_discrete(limits=c("Modif05u", "Modif05d", "Defa45u", "Defa45d", "Modif45u", "Modif45d")) +
  scale_fill_manual(values=c("#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276"),
                    labels=c("Downstream", "Upstream"))

sul2
save_plot("Bxintl2.jpeg", sul2)

#PMMoV

head(Round2)
Mean <- round(mean(Round2$PMMoV, na.rm = TRUE),2)
PMMoV2 <- ggplot(Round2, aes(x=MethodID, y=PMMoV, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("Modif05d", "Defa45d"),
                                 c("Modif05d", "Modif45d")
  ), test = "t.test", color = "black",
  map_signif_level = function(p) {
    if(p < 0.001) {
      return("p < 0.001***")
    } else if(p < 0.01) {
      return("p < 0.01**")
    } else if(p < 0.05) {
      return("p < 0.05*")
    } else {
      return(sprintf("p = %.3f", p))
    }
  }, textsize = 3, y_position = c(5.8, 6.1)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "Site") + ylab(expression("PMMoV " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = Mean, linetype = "dashed", color = "grey") +
  scale_x_discrete(limits=c("Modif05u", "Modif05d", "Defa45u", "Defa45d", "Modif45u", "Modif45d")) +
  scale_fill_manual(values=c("#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276"),
                    labels=c("Downstream", "Upstream"))

PMMoV2
save_plot("BxPMMoV2.jpeg", PMMoV2)

#CrAss
head(Round2)
Mean <- round(mean(Round2$crAss, na.rm = TRUE),2)
crAss2 <- ggplot(Round2, aes(x=MethodID, y=crAss, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "Site") + ylab(expression("CrAssphage " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = Mean, linetype = "dashed", color = "grey") +
  scale_x_discrete(limits=c("Modif05u", "Modif05d", "Defa45u", "Defa45d", "Modif45u", "Modif45d")) +
  scale_fill_manual(values=c("#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276", "#BBE2EC", "#0D9276"),
                    labels=c("Downstream", "Upstream"))

crAss2
save_plot("BxcrAss2.jpeg", crAss2)


fig1ADXcorrww2 <- plot_grid(rrna2, sfmd2, intl2, sul2, PMMoV2, crAss2, ncol = 2,
                           labels = c("a", "b", "c", "d", "e", "f"), label_size = 11)

ggsave(file="fig1ADXcorrww2.jpeg", fig1ADXcorrww2, width= 190, height = 240, units = "mm", dpi=600)

#####color individuals by group####
library(readr)
iris <- read_csv("PCAmeth.csv", col_types = cols(...1 = col_character(), 
                                                 sfmD = col_number(), `16s` = col_number(), 
                                                 intl = col_number(), sul = col_number(), PMMoV = col_number(), 
                                                 crAss = col_number()))
head(iris)
# convert 1st to row name
df<-data.frame(iris)
df
rownames(df) <- df[,1]
df[,1] <- NULL
df
iris<-data.frame(df)
iris
iris.pca <- PCA(iris[,-7], graph = FALSE)

#finish look
p <- fviz_pca_biplot(iris.pca, title = "Principal Component Analysis",
                     # Individuals
                     geom.ind = "point",
                     fill.ind = iris$Methods, col.ind = "black",
                     pointshape = 21, pointsize = 2,
                     palette = "startrek",
                     addEllipses = TRUE, ellipse.type = "confidence",
                     # Variables
                     col.var = "contrib",
                     repel = TRUE,
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     legend.title = list(fill = "Methods", color = "Contrib")
)
p

px <- p + 
  xlab("PC1 (49.5%)") + labs(fill = "Methods") + ylab("PC2 (19.8%)") + labs(title = NULL) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
px
save_plot("PCAplotmeth.jpeg", px)

####phase 2

library(readr)
PCAsitep2 <- read_csv("PCAsitep2.csv", col_types = cols(`16s` = col_number(), 
                                                        sfmD = col_number(), intl = col_number(), 
                                                        sul = col_number(), PMMoV = col_number(), 
                                                        crAss = col_number()))
head(PCAsitep2)
# convert 1st to row name
df2 <- data.frame(PCAsitep2)
df2
rownames(df2) <- df2[,1]
df2[,1] <- NULL
df2

iris2 <- data.frame(df2)
iris2
iris.pca2 <- PCA(iris2[,-7], graph = FALSE)
iris.pca2

p2 <- fviz_pca_biplot(iris.pca2, title = "Principal Component Analysis",
                     geom.ind = "point",
                     fill.ind = iris2$MethodID, col.ind = "black",
                     pointshape = 21, pointsize = 2,
                     palette = "npg",
                     addEllipses = TRUE, ellipse.type = "confidence",
                     col.var = "contrib",
                     repel = TRUE,
                     gradient.cols = c("#9AD0C2", "#2D9596", "#265073"),
                     legend.title = list(fill = "Methods", color = "Contrib"))
p2

px2 <- p2 + 
  xlab("PC1 (72.2%)") + labs(fill = "Methods") + ylab("PC2 (11.5%)") + labs(title = NULL) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        axis.text.x = element_text(color = "black", size = 7),
        axis.text.y = element_text(color = "black", size = 7),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
px2
save_plot("PCAplotmeth2.jpeg", px2)

pcax <- plot_grid(px, px2, ncol = 1,
                  labels = c("a", "b"), label_size = 10)

ggsave(file="pcax.jpeg", pcax, width= 160, height = 220, units = "mm", dpi=600)
