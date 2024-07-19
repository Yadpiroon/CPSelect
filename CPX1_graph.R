# CP select graph
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

####mean sum site####
library(readr)
mean <- read_csv("mean.csv", col_types = cols(TC1 = col_number(), 
                                              EC1 = col_number(), TC2 = col_number(), 
                                              EC2 = col_number(), PTC = col_number(), 
                                              PEC = col_number(), pH = col_number(), 
                                              Turbidity = col_number()))
View(mean)

#only TC EC n=24
up <- mean[1:12, ]
dw <- mean[13:24, ]

summary(up$TC1)
sd(up$TC1)
shapiro.test(up$TC1)

summary(dw$TC1)
sd(dw$TC1)
shapiro.test(dw$TC1)

summary(up$EC1)
sd(up$EC1)
shapiro.test(up$EC1)

summary(dw$EC1)
sd(dw$EC1)
shapiro.test(dw$EC1)

summary(up$pH)
sd(up$pH)
shapiro.test(up$pH)

summary(dw$pH)
sd(dw$pH)
shapiro.test(dw$pH)

summary(up$Turbidity)
sd(up$Turbidity)
shapiro.test(up$Turbidity)

summary(dw$Turbidity)
sd(dw$Turbidity)
shapiro.test(dw$Turbidity)

summary(up$TC2)
sd(up$TC2)
shapiro.test(up$TC2)

summary(dw$TC2)
sd(dw$TC2)
shapiro.test(dw$TC2)

summary(up$EC2)
sd(up$EC2)
shapiro.test(dw$TC2)

summary(dw$EC2)
sd(dw$EC2)
shapiro.test(dw$TC2)

summary(up$TC2)
sd(up$TC2)
shapiro.test(up$TC2)

summary(dw$TC2)
sd(dw$TC2)
shapiro.test(dw$TC2)

summary(up$EC2)
sd(up$EC2)
shapiro.test(up$EC2)

summary(dw$EC2)
sd(dw$EC2)
shapiro.test(dw$EC2)

#gene n=36

library(readr)
mean_gene13Apr <- read_csv("mean_gene13Apr.csv", 
                           col_types = cols(sfmD = col_number(), 
                                            `16s` = col_number(), intl = col_number(), 
                                            sul = col_number(), BacHum = col_number(), 
                                            PMMoV = col_number(), crAss = col_number()))
head(mean_gene13Apr)

up <- mean_gene13Apr[1:18, ]
dw <- mean_gene13Apr[19:36, ]

summary(up$`16s`)
sd(up$`16s`)
shapiro.test(up$`16s`)

summary(dw$`16s`)
sd(dw$`16s`)
shapiro.test(dw$`16s`)

summary(up$sfmD)
sd(up$sfmD)
shapiro.test(up$sfmD)

summary(dw$sfmD)
sd(dw$sfmD)
shapiro.test(dw$sfmD)

summary(up$intl)
sd(up$intl)
shapiro.test(up$intl)

summary(dw$intl)
sd(dw$intl)
shapiro.test(dw$intl)

summary(up$sul)
sd(up$sul)
shapiro.test(up$sul)

summary(dw$sul)
sd(dw$sul)
shapiro.test(dw$sul)

summary(up$PMMoV)
sd(up$PMMoV)
shapiro.test(up$PMMoV)

summary(dw$PMMoV)
sd(dw$PMMoV)
shapiro.test(dw$PMMoV)

summary(up$crAss)
sd(up$crAss)
shapiro.test(up$crAss)

summary(dw$crAss)
sd(dw$crAss)
shapiro.test(dw$crAss)


####CP select analysis

####mean sum methods####
library(readr)
mean_genemethods_13Apr <- read_csv("mean_genemethods_13Apr.csv", 
                                   col_types = cols(sfmD = col_number(), 
                                                    `16s` = col_number(), intl = col_number(), 
                                                    sul = col_number(), BacHum = col_number(), 
                                                    PMMoV = col_number(), crAss = col_number()))
View(mean_genemethods_13Apr)

#only gene n=24
def <- mean_genemethods_13Apr[1:12, ]
enf <- mean_genemethods_13Apr[13:24, ]
mod <- mean_genemethods_13Apr[25:36, ]


summary(def$`16s`)
sd(def$`16s`)
shapiro.test(def$`16s`)

summary(enf$`16s`)
sd(enf$`16s`)
shapiro.test(enf$`16s`)

summary(mod$`16s`)
sd(mod$`16s`)
shapiro.test(mod$`16s`)

summary(def$sfmD)
sd(def$sfmD)
shapiro.test(def$sfmD)

summary(enf$sfmD)
sd(enf$sfmD)
shapiro.test(enf$sfmD)

summary(mod$sfmD)
sd(mod$sfmD)
shapiro.test(mod$sfmD)

summary(def$intl)
sd(def$intl)
shapiro.test(def$intl)

summary(enf$intl)
sd(enf$intl)
shapiro.test(enf$intl)

summary(mod$intl)
sd(mod$intl)
shapiro.test(mod$intl)

summary(def$sul)
sd(def$sul)
shapiro.test(def$sul)

summary(enf$sul)
sd(enf$sul)
shapiro.test(enf$sul)

summary(mod$sul)
sd(mod$sul)
shapiro.test(mod$sul)

summary(def$PMMoV)
sd(def$PMMoV)
shapiro.test(def$PMMoV)

summary(enf$PMMoV)
sd(enf$PMMoV)
shapiro.test(enf$PMMoV)

summary(mod$PMMoV)
sd(mod$PMMoV)
shapiro.test(mod$PMMoV)

summary(def$crAss)
sd(def$crAss)
shapiro.test(def$crAss)

summary(enf$crAss)
sd(enf$crAss)
shapiro.test(enf$crAss)

summary(mod$crAss)
sd(mod$crAss)
shapiro.test(mod$crAss)

########Data p1-4 def and dw#####
library(readxl)
G1 <- read_excel("G1.xlsx", 
                           col_types = c("text", "text", "numeric"))
View(G1)

library(readxl)
G2 <- read_excel("G2.xlsx", 
                 col_types = c("text", "text", "numeric"))
View(G2)

p1 <- ggplot(G1, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("CHROM", "CP_Select")), test = "t.test", color = "black",
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
              }, textsize = 3, y_position = c(170000)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "Site") + ylab(expression("Total coliform concentration (CFU/mL)")) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", s ize = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.97, 0.78),
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
  scale_x_discrete(limits=c("CHROM", "CP_Select")) +
  scale_fill_manual(values=c("#5bc0de", "#f0ad4e"),
                    labels=c("Upstream", "Downstream"))

p1
save_plot("p1.jpeg", p1)

p2 <- ggplot(G2, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("CHROM", "CP_Select")), test = "t.test", color = "black",
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
              }, textsize = 3, y_position = c(5500)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "Site") + ylab(expression(italic("E. coli"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.97, 0.78),
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
  scale_x_discrete(limits=c("CHROM", "CP_Select")) +
  scale_fill_manual(values=c("#5bc0de", "#f0ad4e"),
                    labels=c("Upstream", "Downstream"))

p2
save_plot("p2.jpeg", p2)

###########heat map only tc and rec and pH and turbidity####
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(vegan)
library(grid)
library(gridExtra)
library(readr)
library(readr)
corr_turbipH <- read_csv("corr_turbipH.csv", 
                         col_types = cols(`%recovery_total_coliform` = col_number(), 
                                          `%recovery_ecoli` = col_number(), 
                                          pH = col_number(), Turbidity = col_number()))
View(corr_turbipH)

attach(corr_turbipH)

p.mat <- cor_pmat(corr_turbipH) #correlation matrix with p-values
head(p.mat)
p.mat
m <- cor(corr_turbipH) 
m

ggcorrplot(m, hc.order = TRUE, type = "lower", 
           p.mat = p.mat, sig.level = .05, tl.cex = 7, pch = 4, insig = c("pch"), pch.cex = 3,
           lab = TRUE, outline.color = "white", lab_col = "black", lab_size = 3,
           ggtheme = ggplot2::theme_gray, colors = c("#F4EEEE", "#FFDBAA", "#96C291")) +
  theme(legend.text = element_text(color = "black", family = "Arial", size = 4), #detail site label
        legend.title = element_text(color = "black", family = "Arial", size = 4, face = "bold"), #site detail label
        axis.text = element_text(color = "black", family = "Arial", size = 4),
        panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.2))

#sig
# new corr plot
library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(reshape2)

corr <- round(cor(corr_turbipH), 1)

p.df <- as.data.frame(ggcorrplot::cor_pmat(corr_turbipH))

labs.function = function(x){
  case_when(x >= 0.05 ~ "",
            x < 0.05 & x >= 0.01 ~ "*",
            x < 0.01 & x >= 0.001 ~ "**",
            x < 0.001 ~ "***")
}

p.labs = p.df %>%
  mutate_all(labs.function)

p.labs$Var1 = as.factor(rownames(p.labs))
p.labs = melt(p.labs, id.vars = "Var1", variable.name = "Var2", value.name = "lab")

cor_plot = ggcorrplot(corr, hc.order = F, type = "lower",
                      lab = T, ggtheme = ggplot2::theme_gray, colors = c("#F4EEEE", "#FFDBAA", "#96C291")) +
  theme(legend.text = element_text(color = "black", family = "Arial", size = 10), #detail site label
        legend.title = element_text(color = "black", family = "Arial", size = 10, face = "bold"), #site detail label
        axis.text = element_text(color = "black", family = "Arial", size = 2),
        panel.background = element_rect(fill = "grey95", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.2))

p.labs$in.df = ifelse(is.na(match(paste0(p.labs$Var1, p.labs$Var2),
                                  paste0(cor_plot[["data"]]$Var1, cor_plot[["data"]]$Var2))),
                      "No", "Yes")

p.labs = select(filter(p.labs, in.df == "Yes"), -in.df)

cor.plot.labsa = cor_plot +
  geom_text(aes(x = p.labs$Var1,
                y = p.labs$Var2),
            label = p.labs$lab,
            nudge_y = 0.25,
            size = 5)

cor.plot.labsa


ggsave(file="cor.plot.labsa.jpeg", cor.plot.labsa,
       width= 150, height = 150, units = "mm", dpi=600)

fig1ADXcorr <- plot_grid(cor.plot.labsa, ncol = 1,
                     labels = c("c"), label_size = 10)

ggsave(file="fig1ADXcorr.jpeg", fig1ADXcorr, width= 100, height = 100, units = "mm", dpi=600)


######Combine p1-p4 ######
fig1ADX <- plot_grid(p1, p2, ncol = 2,
                    labels = c("a", "b"), label_size = 10)

ggsave(file="fig1ADX.jpeg", fig1ADX, width= 200, height = 200, units = "mm", dpi=600)

#####ref####
A2 <- ggplot(A2, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("DY", "DN"), c("EN", "DN"), c("EY", "DN"),
                                 c("MN", "DN"), c("MY", "DN"), c("EN", "DY"),
                                 c("EY", "DY"), c("MN", "DY"), c("MY", "DY"),
                                 c("EY", "EN"), c("MN", "EN"), c("MY", "EN"),
                                 c("MN", "EY"), c("MY", "EY"), c("MY", "MN")
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
  }, textsize = 3, y_position = c(5.1, 5.2, 5.3,
                                  5.4, 5.5, 5.6,
                                  5.7, 5.8, 5.9,
                                  6.0, 6.1, 6.2,
                                  6.3, 6.4, 6.5)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "MgCl2 Addition") + ylab(expression("sfmD Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("DY", "DN", "MY", "MN", "EY", "EN")) +
  scale_fill_manual(values=c("#114232", "#87A922", "#114232", "#87A922", "#114232", "#87A922"),
                    labels=c("Upstream", "Downstream"))

A6

########A1#####
#Compair Methods by mgcl2 addition
library(readxl)
mgcla1 <- read_excel("mgcla1.xlsx", col_types = c("text", 
                                                  "text", "numeric"))
View(mgcla1)

#check pair
one.way.anova_event <- aov(mgcla1$TC ~ mgcla1$Name, data = mgcla1)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

mgcla1 <- ggplot(mgcla1, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("ENF", "Default"), 
                                 c("Modified", "Default"), 
                                 c("Modified", "ENF")
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
  }, textsize = 3, y_position = c(4.1, 4.2, 4.3)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("sfmD Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("ENF", "Default", "Modified")) +
  scale_fill_manual(values=c("#75b0cf", "#ffd600", "#75b0cf", "#ffd600", "#75b0cf", "#ffd600"),
                    labels=c("No", "Yes"))

mgcla1
save_plot("mgcla1.jpeg", mgcla1)

#Mgcl2 addtion by up and dw only CP Select
library(readxl)
A1 <- read_excel("A1.xlsx", 
                 col_types = c("text", "text", "numeric"))
head(A1)

#check pair
one.way.anova_event <- aov(A1$TC ~ A1$Name, data = A1)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

A1 <- ggplot(A1, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("Default_Yes", "Default_No"), 
                                 c("Modified_No", "Default_No"), 
                                 c("Modified_Yes", "Default_No"),
                                 c("Modified_No", "Default_Yes"), 
                                 c("Modified_Yes", "Default_Yes"), 
                                 c("Modified_Yes", "Modified_No")
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
  }, textsize = 3, y_position = c(5.0, 4.2, 4.3,
                                  4.4, 4.5, 4.1)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "Site") + ylab(expression("sfmD Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("Default_Yes", "Default_No", "Modified_Yes", "Modified_No")) +
  scale_fill_manual(values=c("#c5c934", "#e3d38a", "#c5c934", "#e3d38a"),
                    labels=c("Upstream", "Downstream"))
 

A1
save_plot("A1.jpeg", A1)

Fig2AAX <- plot_grid(mgcla1, A1, ncol = 2,
                    labels = c("a", "b"), label_size = 10)


ggsave(file="Fig2AAX.jpeg", Fig2AAX, width= 220, height = 100, units = "mm", dpi=600)


####A2####
library(readxl)
A2 <- read_excel("A2.xlsx", 
                 col_types = c("text", "text", "numeric"))
head(A2)

#Compair Methods by mgcl2 addition
library(readxl)
mgcla2 <- read_excel("mgcla2.xlsx", col_types = c("text", 
                                                  "text", "numeric"))
View(mgcla2)

#check pair
one.way.anova_event <- aov(mgcla2$TC ~ mgcla2$Name, data = mgcla2)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

mgcla2 <- ggplot(mgcla2, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("ENF", "Default"), 
                                 c("Modified", "Default"), 
                                 c("Modified", "ENF")
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
  }, textsize = 3, y_position = c(9.1, 9.2, 9.3)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("16SrRNA Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("ENF", "Default", "Modified")) +
  scale_fill_manual(values=c("#75b0cf", "#ffd600", "#75b0cf", "#ffd600", "#75b0cf", "#ffd600"),
                    labels=c("Yes", "No"))

mgcla2
save_plot("mgcla2.jpeg", mgcla2)

#Mgcl2 addtion by up and dw only CP Select
library(readxl)
A2 <- read_excel("A2.xlsx", 
                 col_types = c("text", "text", "numeric"))
View(A2)


A2 <- ggplot(A2, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) + 
  geom_signif(comparisons = list(c("ENF_Yes", "Default_No"),
                                 c("ENF_Yes", "Default_Yes"),
                                 c("Modified_Yes", "ENF_Yes")
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
  }, textsize = 3, y_position = c(9.1, 9.2, 9.3)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("16SrRNA Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("Default_Yes", "Default_No", "Modified_Yes", "Modified_No", "ENF_Yes", "ENF_No")) +
  scale_fill_manual(values=c("#75b0cf", "#ffd600", "#75b0cf", "#ffd600", "#75b0cf", "#ffd600"),
                    labels=c("No", "Yes"))


A2
save_plot("A2.jpeg", A2)

Fig2AAX2 <- plot_grid(mgcla2, A2, ncol = 2,
                     labels = c("c", "d"), label_size = 10)


ggsave(file="Fig2AAX2.jpeg", Fig2AAX2, width= 220, height = 100, units = "mm", dpi=600)

FigDA <- plot_grid(A2, mgcla1, ncol = 2,
                      labels = c("a", "b"), label_size = 10)


ggsave(file="FigDA.jpeg", FigDA, width= 220, height = 100, units = "mm", dpi=600)


######A3####

#Compair Methods by mgcl2 addition and site
library(readxl)
mgcla4 <- read_excel("mgcla4.xlsx", col_types = c("text", 
                                                  "text", "numeric"))
View(mgcla4)

#check pair
one.way.anova_event <- aov(mgcla4$TC ~ mgcla4$Name, data = mgcla4)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

mgcla3 <- ggplot(mgcla4, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("Default_up", "Default_dw"), 
                                 c("ENF_dw", "Default_dw"), 
                                 c("ENF_up", "Default_dw"),
                                 c("Modified_up", "Default_dw"), 
                                 c("ENF_dw", "Default_up"), 
                                 c("Modified_dw", "Default_up"),
                                 c("ENF_up", "ENF_dw"),
                                 c("Modified_dw", "ENF_dw"),
                                 c("Modified_up", "ENF_dw"), 
                                 c("Modified_dw", "ENF_up"), 
                                 c("Modified_up", "Modified_dw") 
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
  }, textsize = 3, y_position = c(12.1, 
                                  11.2, 
                                  11.7, 
                                  11.9, 
                                  11.5,
                                  11.4, 
                                  11.1, 
                                  11.6, 
                                  11.3, 
                                  11.8,
                                  12.0)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("intl1 Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("Default_up", "Default_dw", 
                            "Modified_up", "Modified_dw", 
                            "ENF_up", "ENF_dw")) +
  scale_fill_manual(values=c("#8083c9", "#e0c68b", "#8083c9", "#e0c68b", "#8083c9", "#e0c68b"),
                    labels=c("Yes", "No"))

mgcla3
save_plot("mgcla3.jpeg", mgcla3)

#Compair Methods by mgcl2 addition and site
library(readxl)
mgcla4 <- read_excel("mgcla5.xlsx", col_types = c("text", 
                                                  "text", "numeric"))
View(mgcla4)

#check pair
one.way.anova_event <- aov(mgcla4$TC ~ mgcla4$Name, data = mgcla4)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

mgcla4 <- ggplot(mgcla5, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("Default_up", "Default_dw"), 
                                 c("ENF_up", "Default_dw"),
                                 c("ENF_dw", "Default_up"), 
                                 c("Modified_dw", "Default_up"),
                                 c("ENF_up", "ENF_dw"),
                                 c("Modified_up", "ENF_dw"), 
                                 c("Modified_dw", "ENF_up"), 
                                 c("Modified_up", "Modified_dw")
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
  }, textsize = 3, y_position = c(8.1, 8.2, 8.3, 8.4, 8.5,
                                  8.6, 8.7, 8.8)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("sul1 Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("Default_up", "Default_dw", 
                            "Modified_up", "Modified_dw", 
                            "ENF_up", "ENF_dw")) +
  scale_fill_manual(values=c("#8083c9", "#e0c68b", "#8083c9", "#e0c68b", "#8083c9", "#e0c68b"),
                    labels=c("Yes", "No"))

mgcla4
save_plot("mgcla4.jpeg", mgcla4)



FigBB <- plot_grid(mgcla3, mgcla4, ncol = 2,
                   labels = c("c", "d"), label_size = 10)


ggsave(file="FigBB.jpeg", FigBB, width= 200, height = 160, units = "mm", dpi=600)


###############A4####
library(readxl)
A4 <- read_excel("A4.xlsx", 
                 col_types = c("text", "text", "numeric"))
head(A4)

#check pair
one.way.anova_event <- aov(A4$TC ~ A4$Name, data = A4)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

A4 <- ggplot(A4, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("ENF_up", "Default_dw"),
                                 c("Modified_dw", "Default_up"),
                                 c("ENF_up", "ENF_dw"),
                                 c("Modified_up", "ENF_dw"), 
                                 c("Modified_dw", "ENF_up") 
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
  }, textsize = 3, y_position = c(8.6, 8.5, 8.3, 8.4, 8.7)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("BacHum Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("Default_up", "Default_dw", 
                            "Modified_up", "Modified_dw", 
                            "ENF_up", "ENF_dw")) +
  scale_fill_manual(values=c("#8083c9", "#e0c68b", "#8083c9", "#e0c68b", "#8083c9", "#e0c68b"),
                    labels=c("Yes", "No"))

A4
save_plot("A4.jpeg", A4)

library(readxl)
A5 <- read_excel("A5.xlsx", 
                 col_types = c("text", "text", "numeric"))
head(A5)

#check pair
one.way.anova_event <- aov(A5$TC ~ A5$Name, data = A5)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

A5 <- ggplot(A5, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("ENF_No", "Default_No"),
                                 c("ENF_Yes", "Default_No"),
                                 c("ENF_No", "Default_Yes"),
                                 c("Modified_No", "ENF_No"), 
                                 c("Modified_Yes", "ENF_No"),
                                 c("Modified_No", "ENF_Yes")
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
  }, textsize = 3, y_position = c(6.1, 6.2, 6.3, 6.4, 6.5, 6.6)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("PMMoV Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("Default_Yes", "Default_No", 
                            "Modified_Yes", "Modified_No", 
                            "ENF_Yes", "ENF_No")) +
  scale_fill_manual(values=c("#75b0cf", "#ffd600", "#75b0cf", "#ffd600", "#75b0cf", "#ffd600"),
                    labels=c("No", "Yes"))

A5
save_plot("A5.jpeg", A5)

Fig3AA <- plot_grid(A4, A5, ncol = 2,
                    labels = c("e", "f"), label_size = 10)


ggsave(file="Fig3AA.jpeg", Fig3AA, width= 240, height = 140, units = "mm", dpi=600)

library(readxl)
crass <- read_excel("crass.xlsx", col_types = c("text", 
                                                "text", "numeric"))
head(crass)

#check pair
one.way.anova_event <- aov(crass$crAss ~ crass$Name, data = crass)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

crass <- ggplot(crass, aes(x=Name, y=crAss, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("ENF_No", "Default_No"),
                                 c("ENF_Yes", "Default_No"),
                                 c("ENF_No", "Default_Yes"),
                                 c("Modified_No", "ENF_No"), 
                                 c("Modified_Yes", "ENF_No"),
                                 c("Modified_No", "ENF_Yes")
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
  }, textsize = 3, y_position = c(9.1, 9.2, 9.3, 9.4, 9.5, 9.6)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("CrAssphage Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("Default_Yes", "Default_No", 
                            "Modified_Yes", "Modified_No", 
                            "ENF_Yes", "ENF_No")) +
  scale_fill_manual(values=c("#75b0cf", "#ffd600", "#75b0cf", "#ffd600", "#75b0cf", "#ffd600"),
                    labels=c("No", "Yes"))

crass




############
library(readxl)
A1 <- read_excel("A1.xlsx", 
                 col_types = c("text", "text", "numeric"))
head(A1)
A1 <- ggplot(A1, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "MgCl2 Addition") + ylab(expression("sfmD Concentration " * (Log[10] * " copies/L"))) +
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
  scale_x_discrete(limits=c("DY", "DN", "MY", "MN", "EY", "EN")) +
  scale_fill_manual(values=c("#114232", "#87A922", "#114232", "#87A922", "#114232", "#87A922"),
                    labels=c("Upstream", "Downstream"))

A1
save_plot("A1.jpeg", A1)

library(readxl)
A2 <- read_excel("A2.xlsx", 
                 col_types = c("text", "text", "numeric"))
View(A2)

A2 <- ggplot(A2, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("MY", "EY"), c("EY", "DY")
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
  }, textsize = 3, y_position = c(9.1, 9.2)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "MgCl2 Addition") + ylab(expression("16SrRNA Concentration " * (Log[10] * " copies/L"))) +
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
  scale_x_discrete(limits=c("DY", "DN", "MY", "MN", "EY", "EN")) +
  scale_fill_manual(values=c("#114232", "#87A922", "#114232", "#87A922", "#114232", "#87A922"),
                    labels=c("Upstream", "Downstream"))

A2
save_plot("A2.jpeg", A2)


library(readxl)
A3 <- read_excel("A3.xlsx", 
                 col_types = c("text", "text", "numeric"))
head(A3)

A3 <- ggplot(A3, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "MgCl2 Addition") + ylab(expression("intl1 Concentration " * (Log[10] * " copies/L"))) +
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
  scale_x_discrete(limits=c("DY", "DN", "MY", "MN", "EY", "EN")) +
  scale_fill_manual(values=c("#114232", "#87A922", "#114232", "#87A922", "#114232", "#87A922"),
                    labels=c("Upstream", "Downstream"))
A3
save_plot("A3.jpeg", A3)

library(readxl)
A4 <- read_excel("A4.xlsx", 
                 col_types = c("text", "text", "numeric"))
head(A4)

A4 <- ggplot(A4, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "MgCl2 Addition") + ylab(expression("sul1 Concentration " * (Log[10] * " copies/L"))) +
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
  scale_x_discrete(limits=c("DY", "DN", "MY", "MN", "EY", "EN")) +
  scale_fill_manual(values=c("#114232", "#87A922", "#114232", "#87A922", "#114232", "#87A922"),
                    labels=c("Upstream", "Downstream"))

A4
save_plot("A4.jpeg", A4)

library(readxl)
A5 <- read_excel("A5.xlsx", 
                 col_types = c("text", "text", "numeric"))
head(A5)
A5 <- ggplot(A5, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "MgCl2 Addition") + ylab(expression("BacHum Concentration " * (Log[10] * " copies/L"))) +
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
  scale_x_discrete(limits=c("DY", "DN", "MY", "MN", "EY", "EN")) +
  scale_fill_manual(values=c("#114232", "#87A922", "#114232", "#87A922", "#114232", "#87A922"),
                    labels=c("Upstream", "Downstream"))

A5
save_plot("A5.jpeg", A5)

library(readxl)
A6 <- read_excel("A6.xlsx", 
                 col_types = c("text", "text", "numeric"))
head(A6)

A6 <- ggplot(A6, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("MY", "EY"), c("EY", "DY"),c("MN", "EN"), c("EN", "DN")
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
  }, textsize = 3, y_position = c(6.1, 6.2, 6.3,6.4)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "MgCl2 Addition") + ylab(expression("PMMoV Concentration " * (Log[10] * " copies/L"))) +
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
  scale_x_discrete(limits=c("DY", "DN", "MY", "MN", "EY", "EN")) +
  scale_fill_manual(values=c("#114232", "#87A922", "#114232", "#87A922", "#114232", "#87A922"),
                    labels=c("Upstream", "Downstream"))

A6
save_plot("A6.jpeg", A6)

######Combine p1-p4 ######
Fig2AA <- plot_grid(A1, A2, A3, A4, A5, A6, ncol = 2,
                    labels = c("a", "b", "c", "d", "e", "f"), label_size = 10)


ggsave(file="Fig2AA.jpeg", Fig2AA, width= 250, height = 350, units = "mm", dpi=600)


######MgCl2 and total coliform####

library(readxl)
MG1 <- read_excel("MG1.xlsx", 
                 col_types = c("text", "text", "numeric"))
View(MG1)

MG1 <- ggplot(MG1, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("Yes", "No")), test = "t.test", color = "black",
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
              }, textsize = 3, y_position = c(120)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "Site") + ylab(expression("Percent recovery of total coliform concentration (%)")) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = c(0.97, 0.78),
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
  scale_x_discrete(limits=c("Yes", "No")) +
  scale_fill_manual(values=c("#5bc0de", "#f0ad4e"),
                    labels=c("Default", "Modified"))

MG1
save_plot("MG1.jpeg", MG1)


library(readxl)
A5 <- read_excel("A5.xlsx", 
                 col_types = c("text", "text", "numeric"))
head(A5)

A5 <- ggplot(A5, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "MgCl2 Addition") + ylab(expression("BacHum Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = "none",
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("DY", "DN", "MY", "MN", "EY", "EN")) +
  scale_fill_manual(values=c("#114232", "#87A922", "#114232", "#87A922", "#114232", "#87A922"),
                    labels=c("Upstream", "Downstream"))

A5
save_plot("A5.jpeg", A5)

library(readxl)
A6 <- read_excel("A6.xlsx", 
                 col_types = c("text", "text", "numeric"))
head(A6)

A6 <- ggplot(A6, aes(x=Name, y=TC, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) +
  geom_signif(comparisons = list(c("MY", "EY"), c("EY", "DY"),c("MN", "EN"), c("EN", "DN")
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
  }, textsize = 3, y_position = c(6.1, 6.2, 6.3,6.4)) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Method") + labs(fill = "MgCl2 Addition") + ylab(expression("PMMoV Concentration " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 7, face = "bold"), #ชื่อแกนy
        legend.text = element_text(color = "black", size = 7), #detail site label
        legend.title = element_text(color = "black", size = 7, face = "bold"), #site detail label
        legend.position = "none",
        legend.position = c(0.98, 0.98),
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
  scale_x_discrete(limits=c("DY", "DN", "MY", "MN", "EY", "EN")) +
  scale_fill_manual(values=c("#114232", "#87A922", "#114232", "#87A922", "#114232", "#87A922"),
                    labels=c("Upstream", "Downstream"))

A6
save_plot("A6.jpeg", A6)