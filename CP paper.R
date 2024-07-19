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
library(factoextra)


####CP round 1 *4 ####

library(readxl)
Round1 <- read_excel("A2.xlsx", col_types = c("text", 
                                          "text", "text", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric"))
head(Round1)

rrna <- ggplot(Round1, aes(x=Name, y=rna, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("MD(+)", "EMF(+)")
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
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("DF(+)", "DF(-)", "MD(+)", "MD(-)", "EMF(+)", "EMF(-)")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values=c("#B0B0B0", "#008080", "#B0B0B0", "#008080", "#B0B0B0", "#008080"),
                    labels=c("No", "Yes"))

rrna
save_plot("Bx1rna.jpeg", rrna)

#sfmD
head(Round1)
sfmd <- ggplot(Round1, aes(x=Name, y=sfmd, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("MOD(+)", "EMF(+)")
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
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("DF(+)", "DF(-)", "MD(+)", "MD(-)", "EMF(+)", "EMF(-)")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values=c("#B0B0B0", "#008080", "#B0B0B0", "#008080", "#B0B0B0", "#008080"),
                    labels=c("No", "Yes"))

sfmd
save_plot("Bx2sfmd.jpeg", sfmd)

#intl1
head(Round1)
intl <- ggplot(Round1, aes(x=ID_site, y=intl, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("DF(U)", "DF(D)"), 
                                 c("MD(U)", "MD(D)"),
                                 c("EMF(U)", "EMF(D)")
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
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("DF(U)", "DF(D)", 
                            "MD(U)", "MD(D)", 
                            "EMF(U)", "EMF(D)")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values=c("#B0B0B0", "#008080", "#B0B0B0", "#008080", "#B0B0B0", "#008080"),
                    labels=c("No", "Yes"))

intl
save_plot("Bxintl.jpeg", intl)


#sul1

head(Round1)

sul <- ggplot(Round1, aes(x=ID_site, y=sul, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.1) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("DF(U)", "DF(D)"), 
                                 c("MD(U)", "MD(D)"),
                                 c("EMF(U)", "EMF(D)")
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
  xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(expression("sul1 " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("DF(U)", "DF(D)", 
                            "MD(U)", "MD(D)", 
                            "EMF(U)", "EMF(D)")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values=c("#B0B0B0", "#008080", "#B0B0B0", "#008080", "#B0B0B0", "#008080"),
                    labels=c("No", "Yes"))

sul
save_plot("Bxsul.jpeg", sul)

#PMMoV
head(Round1)

PMMoV <- ggplot(Round1, aes(x=Name, y=PMMoV, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("MD(+)", "EMF(+)")
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
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("DF(+)", "DF(-)", "MD(+)", "MD(-)", "EMF(+)", "EMF(-)")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values=c("#B0B0B0", "#008080", "#B0B0B0", "#008080", "#B0B0B0", "#008080"),
                    labels=c("No", "Yes"))

PMMoV
save_plot("BxPMMoV.jpeg", PMMoV)

#CrAss
head(Round1)

crAss <- ggplot(Round1, aes(x=Name, y=crAss, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("MD(+)", "EMF(+)")
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
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  scale_x_discrete(limits=c("DF(+)", "DF(-)", "MD(+)", "MD(-)", "EMF(+)", "EMF(-)")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values=c("#B0B0B0", "#008080", "#B0B0B0", "#008080", "#B0B0B0", "#008080"),
                    labels=c("No", "Yes"))

crAss
save_plot("BxcrAss.jpeg", crAss)


fig1ADXcorrww <- plot_grid(rrna, sfmd, intl, sul, PMMoV, crAss, ncol = 2,
                         labels = c("a", "b", "c", "d", "e", "f"), label_size = 11)

ggsave(file="fig1ADXcorrww.jpeg", fig1ADXcorrww, width= 190, height = 240, units = "mm", dpi=600)
#######add the name####
library(readxl)
library(ggplot2)
library(ggsignif)
library(cowplot)

Round1 <- read_excel("A2.xlsx", col_types = c("text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
head(Round1)

# Function to create the plots
create_plot <- function(data, x_var, y_var, y_label, comparisons, y_position, plot_title, x_limits) {
  ggplot(data, aes_string(x=x_var, y=y_var, fill="Poresize")) +
    geom_boxplot(alpha=1, width=0.4, size=0.2) +
    geom_jitter(size=0.4, alpha=0.5) +
    geom_signif(comparisons = comparisons, test = "t.test", color = "black",
                map_signif_level = function(p) {
                  if(p < 0.001) {
                    return("***")
                  } else if(p < 0.01) {
                    return("***")
                  } else if(p < 0.05) {
                    return("***")
                  } else {
                    return(sprintf("p = %.3f", p))
                  }
                }, textsize = 3, y_position = y_position) +
    stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
    xlab("Methods") + labs(fill = "MgCl2 Addition") + ylab(y_label) +
    ggtitle(plot_title) +
    theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"),
          axis.title.y = element_text(color = "black", size = 9, face = "bold"),
          legend.position = "none",
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
          axis.text.x = element_text(color = "black", size = 9),
          axis.text.y = element_text(color = "black", size = 9),
          strip.text.y = element_text(color = "black", size = 5, face = "bold"),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.major = element_line(colour = "white", size = 0.1),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
    scale_x_discrete(limits=x_limits) +
    scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
    scale_fill_manual(values=c("#B0B0B0", "#008080", "#B0B0B0", "#008080", "#B0B0B0", "#008080"),
                      labels=c("No", "Yes"))
}

rrna <- create_plot(Round1, "Name", "rna", expression("16SrRNA " * (Log[10] * " copies/L")), 
                    list(c("MD(+)", "EMF(+)")), c(9.1), "16SrRNA", 
                    c("DF(+)", "DF(-)", "MD(+)", "MD(-)", "EMF(+)", "EMF(-)"))

sfmd <- create_plot(Round1, "Name", "sfmd", expression("sfmD " * (Log[10] * " copies/L")), 
                    list(c("MOD(+)", "EMF(+)")), c(4.2), "sfmD", 
                    c("DF(+)", "DF(-)", "MD(+)", "MD(-)", "EMF(+)", "EMF(-)"))

intl1 <- create_plot(Round1, "ID_site", "intl", expression("intl1 " * (Log[10] * " copies/L")), 
                     list(c("DF(U)", "DF(D)"), c("MD(U)", "MD(D)"), c("EMF(U)", "EMF(D)")), 
                     c(11.5, 11.5, 11.5), "intl1", 
                     c("DF(U)", "DF(D)", "MD(U)", "MD(D)", "EMF(U)", "EMF(D)"))

sul1 <- create_plot(Round1, "ID_site", "sul", expression("sul1 " * (Log[10] * " copies/L")), 
                    list(c("DF(U)", "DF(D)"), c("MD(U)", "MD(D)"), c("EMF(U)", "EMF(D)")), 
                    c(8.1, 8.1, 8.1), "sul1", 
                    c("DF(U)", "DF(D)", "MD(U)", "MD(D)", "EMF(U)", "EMF(D)"))

PMMoV <- create_plot(Round1, "Name", "PMMoV", expression("PMMoV " * (Log[10] * " copies/L")), 
                     list(c("MD(+)", "EMF(+)")), c(6.1), "PMMoV", 
                     c("DF(+)", "DF(-)", "MD(+)", "MD(-)", "EMF(+)", "EMF(-)"))

crAss <- create_plot(Round1, "Name", "crAss", expression("crAssphage " * (Log[10] * " copies/L")), 
                     list(c("MD(+)", "EMF(+)")), c(8.1), "crAssphage", 
                     c("DF(+)", "DF(-)", "MD(+)", "MD(-)", "EMF(+)", "EMF(-)"))

# Combine the plots into a single figure
fig1ADXcorrww <- plot_grid(rrna, sfmd, intl1, sul1, PMMoV, crAss, ncol = 2,
                           labels = c("a", "b", "c", "d", "e", "f"), label_size = 11)

# Save the combined figure
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
  xlab("Methods") + labs(fill = "Sampling site") + ylab(expression("16SrRNA " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = Mean, linetype = "dashed", color = "grey") +
  scale_x_discrete(limits=c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values=c("#FF8C00", "#007BFF", "#FF8C00", "#007BFF", "#FF8C00", "#007BFF"),
                    labels=c("Downstream", "Upstream"))

rrna2
save_plot("Bx1rna2.jpeg", rrna2)

#sfmD
head(Round2)

sfmd2 <- ggplot(Round2, aes(x=MethodID, y=sfmd, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "Site") + ylab(expression("sfmD " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = 0.6, linetype = "dashed", color = "grey") +
  annotate("text", x = 5, y = 0.6, label = "LOQ", vjust = -0.5, hjust = 1, color = "black") +
  scale_x_discrete(limits=c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values=c("#FF8C00", "#007BFF", "#FF8C00", "#007BFF", "#FF8C00", "#007BFF"),
                    labels=c("Downstream", "Upstream"))

sfmd2
save_plot("Bx2sfmd2.jpeg", sfmd2)

#intl1
head(Round2)
Mean <- round(mean(Round2$intl, na.rm = TRUE), 2)
intl2 <- ggplot(Round2, aes(x = MethodID, y = intl, fill = Poresize)) +
  geom_boxplot(alpha = 1, width = 0.4, size = 0.2) +
  geom_jitter(size = 0.4, alpha = 0.5) +
  stat_boxplot(geom = 'errorbar', width = 0.4, alpha = 1) +
  xlab("Methods") +
  labs(fill = "Site") +
  ylab(expression("intl1 " * (Log[10] * " copies/L"))) +
  theme(
    axis.title.x = element_text(color = "black", size = 9, face = "bold"), # x-axis title
    axis.title.y = element_text(color = "black", size = 9, face = "bold"), # y-axis title
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
    axis.text.x = element_text(color = "black", size = 9),
    axis.text.y = element_text(color = "black", size = 9),
    strip.text.y = element_text(color = "black", size = 5, face = "bold"),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "white", size = 0.1),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  ) +
  geom_hline(yintercept = Mean, linetype = "dashed", color = "grey") +
  scale_x_discrete(limits = c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values = c("#FF8C00", "#007BFF", "#FF8C00", "#007BFF", "#FF8C00", "#007BFF"),
                    labels = c("Downstream", "Upstream"))

intl2

save_plot("Bxintl2.jpeg", intl2)


#sul1
head(Round2)
Mean <- round(mean(Round2$sul, na.rm = TRUE),2)
sul2 <- ggplot(Round2, aes(x=MethodID, y=sul, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
  xlab("Methods") + labs(fill = "Site") + ylab(expression("sul1 " * (Log[10] * " copies/L"))) +
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = Mean, linetype = "dashed", color = "grey") +
  scale_x_discrete(limits=c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values=c("#FF8C00", "#007BFF", "#FF8C00", "#007BFF", "#FF8C00", "#007BFF"),
                    labels=c("Downstream", "Upstream"))

sul2
save_plot("Bxintl2.jpeg", sul2)

#PMMoV

head(Round2)
Mean <- round(mean(Round2$PMMoV, na.rm = TRUE),2)
PMMoV2 <- ggplot(Round2, aes(x=MethodID, y=PMMoV, fill =Poresize)) +
  geom_boxplot(alpha=1, width=0.4, size=0.2) + geom_jitter(size=0.4, alpha=0.5) +
  geom_signif(comparisons = list(c("MD05D", "DF45D"),
                                 c("MD05D", "MD45D")
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
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = Mean, linetype = "dashed", color = "grey") +
  scale_x_discrete(limits=c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values=c("#FF8C00", "#007BFF", "#FF8C00", "#007BFF", "#FF8C00", "#007BFF"),
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
  theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนx
        axis.title.y = element_text(color = "black", size = 9, face = "bold"), #ชื่อแกนy
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
        axis.text.x = element_text(color = "black", size = 9),
        axis.text.y = element_text(color = "black", size = 9),
        strip.text.y = element_text(color = "black", size = 5, face = "bold"),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(colour = "white", size = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  geom_hline(yintercept = Mean, linetype = "dashed", color = "grey") +
  scale_x_discrete(limits=c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
  scale_fill_manual(values=c("#FF8C00", "#007BFF", "#FF8C00", "#007BFF", "#FF8C00", "#007BFF"),
                    labels=c("Downstream", "Upstream"))

crAss2
save_plot("BxcrAss2.jpeg", crAss2)


fig1ADXcorrww2 <- plot_grid(rrna2, sfmd2, intl2, sul2, PMMoV2, crAss2, ncol = 2,
                           labels = c("a", "b", "c", "d", "e", "f"), label_size = 11)

ggsave(file="fig1ADXcorrww2.jpeg", fig1ADXcorrww2, width= 190, height = 240, units = "mm", dpi=600)

###add title###
library(readxl)
library(ggplot2)
library(ggsignif)
library(cowplot)

Round2 <- read_excel("AX2.xlsx", col_types = c("text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
head(Round2)

# Function to create the plots with gene names at the top
create_plot <- function(data, x_var, y_var, y_label, comparisons, y_position, plot_title, x_limits) {
  ggplot(data, aes_string(x=x_var, y=y_var, fill="Poresize")) +
    geom_boxplot(alpha=1, width=0.4, size=0.2) +
    geom_jitter(size=0.4, alpha=0.5) +
    geom_signif(comparisons = comparisons, test = "t.test", color = "black",
                map_signif_level = function(p) {
                  if(p < 0.001) {
                    return("***")
                  } else if(p < 0.01) {
                    return("***")
                  } else if(p < 0.05) {
                    return("***")
                  } else {
                    return(sprintf("p = %.3f", p))
                  }
                }, textsize = 3, y_position = y_position) +
    stat_boxplot(geom= 'errorbar' , width = 0.4, alpha=1) +
    xlab("Methods") + labs(fill = "Sampling site") + ylab(y_label) +
    ggtitle(plot_title) +
    theme(axis.title.x = element_text(color = "black", size = 9, face = "bold"),
          axis.title.y = element_text(color = "black", size = 9, face = "bold"),
          plot.title = element_text(color = "black", size = 10, face = "bold", hjust = 0.5),
          legend.position = "none",
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "mm"),
          axis.text.x = element_text(color = "black", size = 9),
          axis.text.y = element_text(color = "black", size = 9),
          strip.text.y = element_text(color = "black", size = 5, face = "bold"),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.major = element_line(colour = "white", size = 0.1),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
    scale_x_discrete(limits=x_limits) +
    scale_y_continuous(labels = function(x) format(round(x, 1), nsmall = 1)) +
    scale_fill_manual(values=c("#FF8C00", "#007BFF", "#FF8C00", "#007BFF", "#FF8C00", "#007BFF"),
                      labels=c("Downstream", "Upstream"))
}

# Creating individual plots with appropriate annotations
Mean_rna <- round(mean(Round2$rna, na.rm = TRUE), 2)
rrna2 <- create_plot(Round2, "MethodID", "rna", expression(Log[10] * " copies/L"), NULL, NULL, "16SrRNA",
                     c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  geom_hline(yintercept = Mean_rna, linetype = "dashed", color = "grey")

sfmd2 <- create_plot(Round2, "MethodID", "sfmd", expression(Log[10] * " copies/L"), NULL, NULL, "sfmD",
                     c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  geom_hline(yintercept = 0.6, linetype = "dashed", color = "grey") +
  annotate("text", x = 5, y = 0.6, label = "LOQ", vjust = -0.5, hjust = 1, color = "black")

Mean_intl <- round(mean(Round2$intl, na.rm = TRUE), 2)
intl2 <- create_plot(Round2, "MethodID", "intl", expression(Log[10] * " copies/L"), NULL, NULL, "intl1",
                     c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  geom_hline(yintercept = Mean_intl, linetype = "dashed", color = "grey")

Mean_sul <- round(mean(Round2$sul, na.rm = TRUE), 2)
sul2 <- create_plot(Round2, "MethodID", "sul", expression(Log[10] * " copies/L"), NULL, NULL, "sul1",
                    c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  geom_hline(yintercept = Mean_sul, linetype = "dashed", color = "grey")

Mean_PMMoV <- round(mean(Round2$PMMoV, na.rm = TRUE), 2)
PMMoV2 <- create_plot(Round2, "MethodID", "PMMoV", expression(Log[10] * " copies/L"),
                      list(c("MD05D", "DF45D"), c("MD05D", "MD45D")), c(5.8, 6.1), "PMMoV",
                      c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  geom_hline(yintercept = Mean_PMMoV, linetype = "dashed", color = "grey")

Mean_crAss <- round(mean(Round2$crAss, na.rm = TRUE), 2)
crAss2 <- create_plot(Round2, "MethodID", "crAss", expression(Log[10] * " copies/L"), NULL, NULL, "CrAssphage",
                      c("MD05U", "MD05D", "DF45U", "DF45D", "MD45U", "MD45D")) +
  geom_hline(yintercept = Mean_crAss, linetype = "dashed", color = "grey")

# Combine the plots into a single figure
fig1ADXcorrww2 <- plot_grid(rrna2, sfmd2, intl2, sul2, PMMoV2, crAss2, ncol = 2,
                            labels = c("a", "b", "c", "d", "e", "f"), label_size = 11)

# Save the combined figure
ggsave(file="fig1ADXcorrww2.jpeg", fig1ADXcorrww2, width= 190, height = 240, units = "mm", dpi=600)


#####color individuals by group####
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
library(FactoMineR)
library(Factoshiny)
library(missMDA)
library(FactoInvestigate)
library(car)
library(factoextra)

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
iris.pca

# Contributions of variables to PC1
fviz_contrib(iris.pca, choice = "var", axes = 1, top = 10) #sul1 + intl1 variables contributing to the principal components
# Contributions of variables to PC2
fviz_contrib(iris.pca, choice = "var", axes = 2, top = 10) #PMMoV + 16s variables contributing to the principal components

dd <- fviz_contrib(iris.pca, choice = "var", axes = 1:2, top = 10) #sul1 and intl1 contributing to the principal components
head(dd)

fviz_pca_var(iris.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
#sul1 and intl top contributing variables

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

# Contributions of variables to PC1
fviz_contrib(iris.pca2, choice = "var", axes = 1, top = 10) #16s variables contributing to the principal components
# Contributions of variables to PC2
fviz_contrib(iris.pca2, choice = "var", axes = 2, top = 10) #sfmd + 16s variables contributing to the principal components

dd2 <- fviz_contrib(iris.pca2, choice = "var", axes = 1:2, top = 10) #sfmD PMMoV contributing to the principal components
head(dd2)

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


# CP Select #22Jan Old vs new

# library
getwd()
library(ggplot2)
library(rstatix)
library(ggpubr)
library(dplyr)
library(survival)
library(NADA)
library(NADA2)
library(EnvStats)
library(stats)
library(base)

#####Phase 1
######### Descriptive statistics size # size sum #############
library(readr)
sumdiff_v1all <- read_csv("sumdiff_v1all.csv", 
                          col_types = cols(pmmov = col_number(), 
                                           BacHum = col_number(), CrAss = col_number(), 
                                           MS2 = col_number(), EC1 = col_number(), 
                                           TC1 = col_number(), EC2 = col_number(), 
                                           TC2 = col_number(), rec_TC = col_number(), 
                                           rec_EC = col_number()))
head(sumdiff_v1all)

########cen test#####################
#sfmd
sumdiff_v1all$sfmD <- as.numeric(sumdiff_v1all$sfmD)
sumdiff_v1all$sfmd_cen <- as.logical(sumdiff_v1all$sfmd_cen)
sumdiff_v1all$Poresize <- as.factor(sumdiff_v1all$Poresize)
obs <- sumdiff_v1all$sfmD
censored <- sumdiff_v1all$sfmd_cen
cenfit_model <- cenfit(obs, censored, sumdiff_v1all$Poresize, data = sumdiff_v1all)
print(cenfit_model)

#BacHum
sumdiff_v1all$BacHum <- as.numeric(sumdiff_v1all$BacHum)
sumdiff_v1all$BacHum_cen <- as.logical(sumdiff_v1all$BacHum_cen)
sumdiff_v1all$Poresize <- as.factor(sumdiff_v1all$Poresize)
obs <- sumdiff_v1all$BacHum
censored <- sumdiff_v1all$BacHum_cen
cenfit_model <- cenfit(obs, censored, sumdiff_v1all$Poresize, data = sumdiff_v1all)
print(cenfit_model)

#pmmov
sumdiff_v1all$pmmov <- as.numeric(sumdiff_v1all$pmmov)
sumdiff_v1all$pmmov_cen <- as.logical(sumdiff_v1all$pmmov_cen)
sumdiff_v1all$Poresize <- as.factor(sumdiff_v1all$Poresize)
obs <- sumdiff_v1all$pmmov
censored <- sumdiff_v1all$pmmov_cen
cenfit_model <- cenfit(obs, censored, sumdiff_v1all$Poresize, data = sumdiff_v1all)
print(cenfit_model)
table(sumdiff_v1all$pmmov_cen)

#sul1 sul_cen
sumdiff_v1all$sul1 <- as.numeric(sumdiff_v1all$sul1)
sumdiff_v1all$sul_cen <- as.logical(sumdiff_v1all$sul_cen)
sumdiff_v1all$Poresize <- as.factor(sumdiff_v1all$Poresize)
obs <- sumdiff_v1all$sul1
censored <- sumdiff_v1all$sul_cen
cenfit_model <- cenfit(obs, censored, sumdiff_v1all$Poresize, data = sumdiff_v1all)
print(cenfit_model)

#intl1 intl_cen
sumdiff_v1all$intl1 <- as.numeric(sumdiff_v1all$intl1)
sumdiff_v1all$intl_cen <- as.logical(sumdiff_v1all$intl_cen)
sumdiff_v1all$Poresize <- as.factor(sumdiff_v1all$Poresize)
obs <- sumdiff_v1all$intl1
censored <- sumdiff_v1all$intl_cen
cenfit_model <- cenfit(obs, censored, sumdiff_v1all$Poresize, data = sumdiff_v1all)
print(cenfit_model)

#RNW RNW_cen
sumdiff_v1all$RNW <- as.numeric(sumdiff_v1all$RNW)
sumdiff_v1all$RNW_cen <- as.logical(sumdiff_v1all$RNW_cen)
sumdiff_v1all$Poresize <- as.factor(sumdiff_v1all$Poresize)
obs <- sumdiff_v1all$RNW
censored <- sumdiff_v1all$RNW_cen
cenfit_model <- cenfit(obs, censored, sumdiff_v1all$Poresize, data = sumdiff_v1all)
print(cenfit_model)

#EC2 EC2_cen
sumdiff_v1all$EC2 <- as.numeric(sumdiff_v1all$EC2)
sumdiff_v1all$EC2_cen <- as.logical(sumdiff_v1all$EC1_cen)
sumdiff_v1all$Poresize <- as.factor(sumdiff_v1all$Poresize)
obs <- sumdiff_v1all$EC2
censored <- sumdiff_v1all$EC1_cen
cenfit_model <- cenfit(obs, censored, sumdiff_v1all$Poresize, data = sumdiff_v1all)
print(cenfit_model)

###########Normality test ###########################

A <- sumdiff_v1all[1:8, ]
B <- sumdiff_v1all[9:16, ]
C <- sumdiff_v1all[17:24, ]

summary(A$BacHum)
sd(A$BacHum)
shapiro.test(A$BacHum)
summary(B$BacHum)
sd(B$BacHum)
shapiro.test(B$BacHum)
summary(C$BacHum)
sd(C$BacHum)
shapiro.test(C$BacHum)

summary(A$CrAss)
sd(A$CrAss)
shapiro.test(A$CrAss)
summary(B$CrAss)
sd(B$CrAss)
shapiro.test(B$CrAss)
summary(C$CrAss)
sd(C$CrAss)
shapiro.test(C$CrAss)

summary(A$pmmov)
sd(A$pmmov)
shapiro.test(A$pmmov)
summary(B$pmmov)
sd(B$pmmov)
shapiro.test(B$pmmov)
summary(C$pmmov)
sd(C$pmmov)
shapiro.test(C$pmmov)

summary(A$MS2)
sd(A$MS2)
shapiro.test(A$MS2)
summary(B$MS2)
sd(B$MS2)
shapiro.test(B$MS2)
summary(C$MS2)
sd(C$MS2)
shapiro.test(C$MS2)

summary(A$EC1)
sd(A$EC1)
shapiro.test(A$EC1)
summary(B$EC1)
sd(B$EC1)
shapiro.test(B$EC1)
summary(C$EC1)
sd(C$EC1)
shapiro.test(C$EC1)

summary(A$TC1)
sd(A$TC1)
shapiro.test(A$TC1)
summary(B$TC1)
sd(B$TC1)
shapiro.test(B$TC1)
summary(C$TC1)
sd(C$TC1)
shapiro.test(C$TC1)

summary(A$EC2)
sd(A$EC2)
shapiro.test(A$EC2)
summary(B$EC2)
sd(B$EC2)
shapiro.test(B$EC2)
summary(C$EC2)
sd(C$EC2)
shapiro.test(C$EC2)

summary(A$TC2)
sd(A$TC2)
shapiro.test(A$TC2)
summary(B$TC2)
sd(B$TC2)
shapiro.test(B$TC2)
summary(C$TC2)
sd(C$TC2)
shapiro.test(C$TC2)


######### diff size # prefilter sum #############

library(readr)
sumdiff_v1allpf <- read_csv("sumdiff_v1allpf.csv", 
                            col_types = cols(pmmov = col_number(), 
                                             BacHum = col_number(), CrAss = col_number(), 
                                             MS2 = col_number(), EC1 = col_number(), 
                                             TC1 = col_number(), EC2 = col_number(), 
                                             TC2 = col_number(), rec_TC = col_number(), 
                                             rec_EC = col_number()))
head(sumdiff_v1allpf)


########cen test#####################
#sfmd
Sumdiff_prefilter$sfmD <- as.numeric(Sumdiff_prefilter$sfmD)
Sumdiff_prefilter$sfmd_cen <- as.logical(Sumdiff_prefilter$sfmd_cen)
Sumdiff_prefilter$Prefilter <- as.factor(Sumdiff_prefilter$Prefilter)
obs <- Sumdiff_prefilter$sfmD
censored <- Sumdiff_prefilter$sfmd_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_prefilter$Prefilter, data = Sumdiff_prefilter)
print(cenfit_model)

#BacHum
sumdiff_v1allpf$BacHum <- as.numeric(sumdiff_v1allpf$BacHum)
sumdiff_v1allpf$BacHum_cen <- as.logical(sumdiff_v1allpf$BacHum_cen)
sumdiff_v1allpf$Prefilter <- as.factor(sumdiff_v1allpf$Prefilter)
obs <- sumdiff_v1allpf$BacHum
censored <- sumdiff_v1allpf$BacHum_cen
cenfit_model <- cenfit(obs, censored, sumdiff_v1allpf$Prefilter, data = sumdiff_v1allpf)
print(cenfit_model)

#pmmov
sumdiff_v1allpf$pmmov <- as.numeric(sumdiff_v1allpf$pmmov)
sumdiff_v1allpf$pmmov_cen <- as.logical(sumdiff_v1allpf$pmmov_cen)
sumdiff_v1allpf$Prefilter <- as.factor(sumdiff_v1allpf$Prefilter)
obs <- sumdiff_v1allpf$pmmov
censored <- sumdiff_v1allpf$pmmov_cen
cenfit_model <- cenfit(obs, censored, sumdiff_v1allpf$Prefilter, data = sumdiff_v1allpf)
print(cenfit_model)
table(sumdiff_v1allpf$pmmov_cen)

#sul1 sul_cen
Sumdiff_prefilter$sul1 <- as.numeric(Sumdiff_prefilter$sul1)
Sumdiff_prefilter$sul_cen <- as.logical(Sumdiff_prefilter$sul_cen)
Sumdiff_prefilter$Prefilter <- as.factor(Sumdiff_prefilter$Prefilter)
obs <- Sumdiff_prefilter$sul1
censored <- Sumdiff_prefilter$sul_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_prefilter$Prefilter, data = Sumdiff_prefilter)
print(cenfit_model)

#intl1 intl_cen
Sumdiff_prefilter$intl1 <- as.numeric(Sumdiff_prefilter$intl1)
Sumdiff_prefilter$intl_cen <- as.logical(Sumdiff_prefilter$intl_cen)
Sumdiff_prefilter$Prefilter <- as.factor(Sumdiff_prefilter$Prefilter)
obs <- Sumdiff_prefilter$intl1
censored <- Sumdiff_prefilter$intl_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_prefilter$Prefilter, data = Sumdiff_prefilter)
print(cenfit_model)

#RNW RNW_cen
Sumdiff_prefilter$RNW <- as.numeric(Sumdiff_prefilter$RNW)
Sumdiff_prefilter$RNW_cen <- as.logical(Sumdiff_prefilter$RNW_cen)
Sumdiff_prefilter$Prefilter <- as.factor(Sumdiff_prefilter$Prefilter)
obs <- Sumdiff_prefilter$RNW
censored <- Sumdiff_prefilter$RNW_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_prefilter$Prefilter, data = Sumdiff_prefilter)
print(cenfit_model)
table(Sumdiff_prefilter$RNW_cen)

#EC2 EC2_cen
Sumdiff_prefilter$TC2 <- as.numeric(Sumdiff_prefilter$TC2)
Sumdiff_prefilter$TC2_cen <- as.logical(Sumdiff_prefilter$TC2_cen)
Sumdiff_prefilter$Prefilter <- as.factor(Sumdiff_prefilter$Prefilter)
obs <- Sumdiff_prefilter$TC2
censored <- Sumdiff_prefilter$TC2_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_prefilter$Prefilter, data = Sumdiff_prefilter)
print(cenfit_model)

###########Normality test ###########################

W <- sumdiff_v1allpf[1:12, ]
WO <- sumdiff_v1allpf[13:24, ]

summary(W$pmmov)
sd(W$pmmov)
shapiro.test(W$pmmov)
summary(WO$pmmov)
sd(WO$pmmov)
shapiro.test(WO$pmmov)

summary(W$BacHum)
sd(W$BacHum)
shapiro.test(W$BacHum)
summary(WO$BacHum)
sd(WO$BacHum)
shapiro.test(WO$BacHum)

summary(W$CrAss)
sd(W$CrAss)
shapiro.test(W$CrAss)
summary(WO$CrAss)
sd(WO$CrAss)
shapiro.test(WO$CrAss)

summary(W$MS2)
sd(W$MS2)
shapiro.test(W$MS2)
summary(WO$MS2)
sd(WO$MS2)
shapiro.test(WO$MS2)

summary(W$TC2)
sd(W$TC2)
shapiro.test(W$TC2)
summary(WO$TC2)
sd(WO$TC2)
shapiro.test(WO$TC2)

summary(W$EC2)
sd(W$EC2)
shapiro.test(W$EC2)
summary(WO$EC2)
sd(WO$EC2)
shapiro.test(WO$EC2)

one.way.anova_event <- aov(Sumdiff_all$BacHum ~ Sumdiff_all$Poresize, data = Sumdiff_all)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

wilcox.test(W$pmmov, WO$pmmov)   
wilcox.test(W$BacHum, WO$BacHum, alternative="greater")   
wilcox.test(W$BacHum, WO$BacHum, alternative="less") 

wilcox.test(W$BacHum, WO$BacHum)   
wilcox.test(W$BacHum, WO$BacHum, alternative="greater")   
wilcox.test(W$BacHum, WO$BacHum, alternative="less") 

one.way.anova_event <- aov(Sumdiff_all$sfmD ~ Sumdiff_all$Poresize, data = Sumdiff_all)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

t.test(W$sfmD, WO$sfmD)
t.test(W$sfmD, WO$sfmD, paired = F, alternative="greater")
t.test(W$sfmD, WO$sfmD, paired = F, alternative="less")

one.way.anova_event <- aov(Sumdiff_all$pmmov ~ Sumdiff_all$Poresize, data = Sumdiff_all)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

t.test(W$pmmov, WO$pmmov)
t.test(W$pmmov, WO$pmmov, paired = F, alternative="greater")
t.test(W$pmmov, WO$pmmov, paired = F, alternative="less")

kruskal.test(Sumdiff_all$sul1 ~ Sumdiff_all$Poresize, data = Sumdiff_all)
library(dunn.test)
dunn.test(Sumdiff_all$sul1, g=Sumdiff_all$Poresize, method="holm", kw=TRUE, label=TRUE, table=FALSE, alpha=0.05, list=TRUE
          ,altp=TRUE)

one.way.anova_event <- aov(Sumdiff_all$sul1 ~ Sumdiff_all$Poresize, data = Sumdiff_all)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

wilcox.test(W$sul1, WO$sul1)   
wilcox.test(W$sul1, WO$sul1, alternative="greater")   
wilcox.test(W$sul1, WO$sul1, alternative="less") 

kruskal.test(Sumdiff_all$intl1 ~ Sumdiff_all$Poresize, data = Sumdiff_all)
library(dunn.test)
dunn.test(Sumdiff_all$intl1, g=Sumdiff_all$Poresize, method="holm", kw=TRUE, label=TRUE, table=FALSE, alpha=0.05, list=TRUE
          ,altp=TRUE)

one.way.anova_event <- aov(Sumdiff_all$intl1 ~ Sumdiff_all$Poresize, data = Sumdiff_all)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

wilcox.test(W$intl1, WO$intl1)   
wilcox.test(W$intl1, WO$intl1, alternative="greater")   
wilcox.test(W$intl1, WO$intl1, alternative="less") 

wilcox.test(W$intl1, WO$intl1, correct = TRUE)   
wilcox.test(W$intl1, WO$intl1, alternative="greater")   
wilcox.test(W$intl1, WO$intl1, alternative="less")

one.way.anova_event <- aov(Sumdiff_all$RNA ~ Sumdiff_all$Poresize, data = Sumdiff_all)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

t.test(W$RNA, WO$RNA)
t.test(W$RNA, WO$RNA, paired = F, alternative="greater")
t.test(W$RNA, WO$RNA, paired = F, alternative="less")

one.way.anova_event <- aov(Sumdiff_all$TC2 ~ Sumdiff_all$Poresize, data = Sumdiff_all)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

t.test(W$TC2, WO$TC2)
t.test(W$TC2, WO$TC2, paired = F, alternative="greater")
t.test(W$TC2, WO$TC2, paired = F, alternative="less")

one.way.anova_event <- aov(Sumdiff_all$EC2 ~ Sumdiff_all$Poresize, data = Sumdiff_all)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

t.test(W$EC2, WO$EC2)
t.test(W$EC2, WO$EC2, paired = F, alternative="greater")
t.test(W$EC2, WO$EC2, paired = F, alternative="less")



######### diff size # prefilter sumA #############

library(readr)
sumdiffv_A <- read_csv("sumdiffv_A.csv", 
                       col_types = cols(pmmov = col_number(), 
                                        BacHum = col_number(), CrAss = col_number(), 
                                        MS2 = col_number(), EC1 = col_number(), 
                                        TC1 = col_number(), EC2 = col_number(), 
                                        TC2 = col_number(), rec_TC = col_number(), 
                                        rec_EC = col_number()))
head(sumdiffv_A)

########cen test#####################
#BacHum
sumdiffv_A$BacHum <- as.numeric(sumdiffv_A$BacHum)
sumdiffv_A$BacHum_cen <- as.logical(sumdiffv_A$BacHum_cen)
sumdiffv_A$Prefilter <- as.factor(sumdiffv_A$Prefilter)
obs <- sumdiffv_A$BacHum
censored <- sumdiffv_A$BacHum_cen
cenfit_model <- cenfit(obs, censored, sumdiffv_A$Prefilter, data = sumdiffv_A)
print(cenfit_model)

#sfmd
sumdiffv_A$sfmD <- as.numeric(sumdiffv_A$sfmD)
sumdiffv_A$sfmd_cen <- as.logical(sumdiffv_A$sfmd_cen)
sumdiffv_A$Prefilter <- as.factor(sumdiffv_A$Prefilter)
obs <- sumdiffv_A$sfmD
censored <- sumdiffv_A$sfmd_cen
cenfit_model <- cenfit(obs, censored, sumdiffv_A$Prefilter, data = sumdiffv_A)
print(cenfit_model)

#pmmov
sumdiffv_A$pmmov <- as.numeric(sumdiffv_A$pmmov)
sumdiffv_A$pmmov_cen <- as.logical(sumdiffv_A$pmmov_cen)
sumdiffv_A$Prefilter <- as.factor(sumdiffv_A$Prefilter)
obs <- sumdiffv_A$pmmov
censored <- sumdiffv_A$pmmov_cen
cenfit_model <- cenfit(obs, censored, sumdiffv_A$Prefilter, data = sumdiffv_A)
print(cenfit_model)
table(sumdiffv_A$pmmov_cen)

#sul1 sul_cen
sumdiffv_A$sul1 <- as.numeric(sumdiffv_A$sul1)
sumdiffv_A$sul_cen <- as.logical(sumdiffv_A$sul_cen)
sumdiffv_A$Prefilter <- as.factor(sumdiffv_A$Prefilter)
obs <- sumdiffv_A$sul1
censored <- sumdiffv_A$sul_cen
cenfit_model <- cenfit(obs, censored, sumdiffv_A$Prefilter, data = sumdiffv_A)
print(cenfit_model)

#intl1 intl_cen
sumdiffv_A$intl1 <- as.numeric(sumdiffv_A$intl1)
sumdiffv_A$intl_cen <- as.logical(sumdiffv_A$intl_cen)
sumdiffv_A$Prefilter <- as.factor(sumdiffv_A$Prefilter)
obs <- sumdiffv_A$intl1
censored <- sumdiffv_A$intl_cen
cenfit_model <- cenfit(obs, censored, sumdiffv_A$Prefilter, data = sumdiffv_A)
print(cenfit_model)

#RNW RNW_cen
sumdiffv_A$RNA <- as.numeric(sumdiffv_A$RNA)
sumdiffv_A$RNA_cen <- as.logical(sumdiffv_A$RNA_cen)
sumdiffv_A$Prefilter <- as.factor(sumdiffv_A$Prefilter)
obs <- sumdiffv_A$RNA
censored <- sumdiffv_A$RNA_cen
cenfit_model <- cenfit(obs, censored, sumdiffv_A$Prefilter, data = sumdiffv_A)
print(cenfit_model)
table(sumdiffv_A$RNA_cen)

#TC2 TC2_cen
sumdiffv_A$TC2 <- as.numeric(sumdiffv_A$TC2)
sumdiffv_A$TC2_cen <- as.logical(sumdiffv_A$TC2_cen)
sumdiffv_A$Prefilter <- as.factor(sumdiffv_A$Prefilter)
obs <- sumdiffv_A$TC2
censored <- sumdiffv_A$TC2_cen
cenfit_model <- cenfit(obs, censored, sumdiffv_A$Prefilter, data = sumdiffv_A)
print(cenfit_model)

#EC2 EC2_cen
sumdiffv_A$EC2 <- as.numeric(sumdiffv_A$EC2)
sumdiffv_A$EC2_cen <- as.logical(sumdiffv_A$EC2_cen)
sumdiffv_A$Prefilter <- as.factor(sumdiffv_A$Prefilter)
obs <- sumdiffv_A$EC2
censored <- sumdiffv_A$EC2_cen
cenfit_model <- cenfit(obs, censored, sumdiffv_A$Prefilter, data = sumdiffv_A)
print(cenfit_model)

###########Normality test ###########################

W <- sumdiffv_A[1:4, ]
WO <- sumdiffv_A[5:8, ]

summary(W$pmmov)
sd(W$pmmov)
shapiro.test(W$pmmov)
summary(WO$pmmov)
sd(WO$pmmov)
shapiro.test(WO$pmmov)

summary(W$BacHum)
sd(W$BacHum)
shapiro.test(W$BacHum)
summary(WO$BacHum)
sd(WO$BacHum)
shapiro.test(WO$BacHum)

summary(W$CrAss)
sd(W$CrAss)
shapiro.test(W$CrAss)
summary(WO$CrAss)
sd(WO$CrAss)
shapiro.test(WO$CrAss)

summary(W$MS2)
sd(W$MS2)
shapiro.test(W$MS2)
summary(WO$MS2)
sd(WO$MS2)
shapiro.test(WO$MS2)

summary(W$TC2)
sd(W$TC2)
shapiro.test(W$TC2)
summary(WO$TC2)
sd(WO$TC2)
shapiro.test(WO$TC2)

summary(W$EC2)
sd(W$EC2)
shapiro.test(W$EC2)
summary(WO$EC2)
sd(WO$EC2)
shapiro.test(WO$EC2)

wilcox.test(W$BacHum, WO$BacHum)   
wilcox.test(W$BacHum, WO$BacHum, alternative="greater")   
wilcox.test(W$BacHum, WO$BacHum, alternative="less") 

wilcox.test(W$CrAss, WO$CrAss)
wilcox.test(W$CrAss, WO$CrAss, alternative="greater")
wilcox.test(W$CrAss, WO$CrAss, alternative="less")

t.test(W$pmmov, WO$pmmov)
t.test(W$pmmov, WO$pmmov, paired = F, alternative="greater")
t.test(W$pmmov, WO$pmmov, paired = F, alternative="less")

wilcox.test(W$MS2, WO$MS2)   
wilcox.test(W$MS2, WO$MS2, alternative="greater")   
wilcox.test(W$MS2, WO$MS2, alternative="less") 

wilcox.test(W$TC2, WO$TC2)
t.test(W$TC2, WO$TC2, paired = F, alternative="greater")
t.test(W$TC2, WO$TC2, paired = F, alternative="less")

wilcox.test(W$EC2, WO$EC2)
t.test(W$EC2, WO$EC2, paired = F, alternative="greater")
t.test(W$EC2, WO$EC2, paired = F, alternative="less")


######### diff size # prefilter sumB #############
library(readr)
sumdiffv_B <- read_csv("sumdiffv_B.csv", 
                       col_types = cols(pmmov = col_number(), 
                                        BacHum = col_number(), CrAss = col_number(), 
                                        MS2 = col_number(), EC1 = col_number(), 
                                        TC1 = col_number(), EC2 = col_number(), 
                                        TC2 = col_number(), rec_TC = col_number(), 
                                        rec_EC = col_number()))
head(sumdiffv_B)

########cen test#####################
#BacHum
Sumdiff_B$BacHum <- as.numeric(Sumdiff_B$BacHum)
Sumdiff_B$BacHum_cen <- as.logical(Sumdiff_B$BacHum_cen)
Sumdiff_B$Prefilter <- as.factor(Sumdiff_B$Prefilter)
obs <- Sumdiff_B$BacHum
censored <- Sumdiff_B$BacHum_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_B$Prefilter, data = Sumdiff_B)
print(cenfit_model)

#sfmd
Sumdiff_B$sfmD <- as.numeric(Sumdiff_B$sfmD)
Sumdiff_B$sfmd_cen <- as.logical(Sumdiff_B$sfmd_cen)
Sumdiff_B$Prefilter <- as.factor(Sumdiff_B$Prefilter)
obs <- Sumdiff_B$sfmD
censored <- Sumdiff_B$sfmd_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_B$Prefilter, data = Sumdiff_B)
print(cenfit_model)

#pmmov
Sumdiff_B$pmmov <- as.numeric(Sumdiff_B$pmmov)
Sumdiff_B$pmmov_cen <- as.logical(Sumdiff_B$pmmov_cen)
Sumdiff_B$Prefilter <- as.factor(Sumdiff_B$Prefilter)
obs <- Sumdiff_B$pmmov
censored <- Sumdiff_B$pmmov_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_B$Prefilter, data = Sumdiff_B)
print(cenfit_model)
table(Sumdiff_B$pmmov_cen)

#sul1 sul_cen
Sumdiff_B$sul1 <- as.numeric(Sumdiff_B$sul1)
Sumdiff_B$sul_cen <- as.logical(Sumdiff_B$sul_cen)
Sumdiff_B$Prefilter <- as.factor(Sumdiff_B$Prefilter)
obs <- Sumdiff_B$sul1
censored <- Sumdiff_B$sul_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_B$Prefilter, data = Sumdiff_B)
print(cenfit_model)

#intl1 intl_cen
Sumdiff_B$intl1 <- as.numeric(Sumdiff_B$intl1)
Sumdiff_B$intl_cen <- as.logical(Sumdiff_B$intl_cen)
Sumdiff_B$Prefilter <- as.factor(Sumdiff_B$Prefilter)
obs <- Sumdiff_B$intl1
censored <- Sumdiff_B$intl_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_B$Prefilter, data = Sumdiff_B)
print(cenfit_model)

#RNW RNW_cen
Sumdiff_B$RNA <- as.numeric(Sumdiff_B$RNA)
Sumdiff_B$RNA_cen <- as.logical(Sumdiff_B$RNA_cen)
Sumdiff_B$Prefilter <- as.factor(Sumdiff_B$Prefilter)
obs <- Sumdiff_B$RNA
censored <- Sumdiff_B$RNA_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_B$Prefilter, data = Sumdiff_B)
print(cenfit_model)
table(Sumdiff_B$RNA_cen)

#TC2 TC2_cen
Sumdiff_B$TC2 <- as.numeric(Sumdiff_B$TC2)
Sumdiff_B$TC2_cen <- as.logical(Sumdiff_B$TC2_cen)
Sumdiff_B$Prefilter <- as.factor(Sumdiff_B$Prefilter)
obs <- Sumdiff_B$TC2
censored <- Sumdiff_B$TC2_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_B$Prefilter, data = Sumdiff_B)
print(cenfit_model)
table(Sumdiff_B$TC2_cen)

#EC2 EC2_cen
Sumdiff_B$EC2 <- as.numeric(Sumdiff_B$EC2)
Sumdiff_B$EC2_cen <- as.logical(Sumdiff_B$EC2_cen)
Sumdiff_B$Prefilter <- as.factor(Sumdiff_B$Prefilter)
obs <- Sumdiff_B$EC2
censored <- Sumdiff_B$EC2_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_B$Prefilter, data = Sumdiff_B)
print(cenfit_model)

###########Normality test ###########################

W <- sumdiffv_B[1:4, ]
WO <- sumdiffv_B[5:8, ]

summary(W$pmmov)
sd(W$pmmov)
shapiro.test(W$pmmov)
summary(WO$pmmov)
sd(WO$pmmov)
shapiro.test(WO$pmmov)

summary(W$BacHum)
sd(W$BacHum)
shapiro.test(W$BacHum)
summary(WO$BacHum)
sd(WO$BacHum)
shapiro.test(WO$BacHum)

summary(W$CrAss)
sd(W$CrAss)
shapiro.test(W$CrAss)
summary(WO$CrAss)
sd(WO$CrAss)
shapiro.test(WO$CrAss)

summary(W$MS2)
sd(W$MS2)
shapiro.test(W$MS2)
summary(WO$MS2)
sd(WO$MS2)
shapiro.test(WO$MS2)

summary(W$TC2)
sd(W$TC2)
shapiro.test(W$TC2)
summary(WO$TC2)
sd(WO$TC2)
shapiro.test(WO$TC2)

summary(W$EC2)
sd(W$EC2)
shapiro.test(W$EC2)
summary(WO$EC2)
sd(WO$EC2)
shapiro.test(WO$EC2)

wilcox.test(W$BacHum, WO$BacHum)   
wilcox.test(W$BacHum, WO$BacHum, alternative="greater")   
wilcox.test(W$BacHum, WO$BacHum, alternative="less") 

wilcox.test(W$CrAss, WO$CrAss)
wilcox.test(W$CrAss, WO$CrAss, alternative="greater")
wilcox.test(W$CrAss, WO$CrAss, alternative="less")

t.test(W$pmmov, WO$pmmov)
t.test(W$pmmov, WO$pmmov, paired = F, alternative="greater")
t.test(W$pmmov, WO$pmmov, paired = F, alternative="less")

wilcox.test(W$MS2, WO$MS2)   
wilcox.test(W$MS2, WO$MS2, alternative="greater")   
wilcox.test(W$MS2, WO$MS2, alternative="less") 

wilcox.test(W$TC2, WO$TC2)
t.test(W$TC2, WO$TC2, paired = F, alternative="greater")
t.test(W$TC2, WO$TC2, paired = F, alternative="less")

wilcox.test(W$EC2, WO$EC2)
t.test(W$EC2, WO$EC2, paired = F, alternative="greater")
t.test(W$EC2, WO$EC2, paired = F, alternative="less")


######### diff size # prefilter sumC #############

library(readr)
sumdiffv_C <- read_csv("sumdiffv_C.csv", 
                       col_types = cols(pmmov = col_number(), 
                                        BacHum = col_number(), CrAss = col_number(), 
                                        MS2 = col_number(), EC1 = col_number(), 
                                        TC1 = col_number(), EC2 = col_number(), 
                                        TC2 = col_number(), rec_TC = col_number(), 
                                        rec_EC = col_number()))
head(sumdiffv_C)

########cen test#####################
#BacHum
Sumdiff_C$BacHum <- as.numeric(Sumdiff_C$BacHum)
Sumdiff_C$BacHum_cen <- as.logical(Sumdiff_C$BacHum_cen)
Sumdiff_C$Prefilter <- as.factor(Sumdiff_C$Prefilter)
obs <- Sumdiff_C$BacHum
censored <- Sumdiff_C$BacHum_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_C$Prefilter, data = Sumdiff_C)
print(cenfit_model)

#sfmd
Sumdiff_C$sfmD <- as.numeric(Sumdiff_C$sfmD)
Sumdiff_C$sfmd_cen <- as.logical(Sumdiff_C$sfmd_cen)
Sumdiff_C$Prefilter <- as.factor(Sumdiff_C$Prefilter)
obs <- Sumdiff_C$sfmD
censored <- Sumdiff_C$sfmd_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_C$Prefilter, data = Sumdiff_C)
print(cenfit_model)

#pmmov
Sumdiff_C$pmmov <- as.numeric(Sumdiff_C$pmmov)
Sumdiff_C$pmmov_cen <- as.logical(Sumdiff_C$pmmov_cen)
Sumdiff_C$Prefilter <- as.factor(Sumdiff_C$Prefilter)
obs <- Sumdiff_C$pmmov
censored <- Sumdiff_C$pmmov_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_C$Prefilter, data = Sumdiff_C)
print(cenfit_model)
table(Sumdiff_C$pmmov_cen)

#sul1 sul_cen
Sumdiff_C$sul1 <- as.numeric(Sumdiff_C$sul1)
Sumdiff_C$sul_cen <- as.logical(Sumdiff_C$sul_cen)
Sumdiff_C$Prefilter <- as.factor(Sumdiff_C$Prefilter)
obs <- Sumdiff_C$sul1
censored <- Sumdiff_C$sul_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_C$Prefilter, data = Sumdiff_C)
print(cenfit_model)

#intl1 intl_cen
Sumdiff_C$intl1 <- as.numeric(Sumdiff_C$intl1)
Sumdiff_C$intl_cen <- as.logical(Sumdiff_C$intl_cen)
Sumdiff_C$Prefilter <- as.factor(Sumdiff_C$Prefilter)
obs <- Sumdiff_C$intl1
censored <- Sumdiff_C$intl_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_C$Prefilter, data = Sumdiff_C)
print(cenfit_model)

#RNW RNW_cen
Sumdiff_C$RNA <- as.numeric(Sumdiff_C$RNA)
Sumdiff_C$RNA_cen <- as.logical(Sumdiff_C$RNA_cen)
Sumdiff_C$Prefilter <- as.factor(Sumdiff_C$Prefilter)
obs <- Sumdiff_C$RNA
censored <- Sumdiff_C$RNA_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_C$Prefilter, data = Sumdiff_C)
print(cenfit_model)
table(Sumdiff_C$RNA_cen)

#TC2 TC2_cen
Sumdiff_C$TC2 <- as.numeric(Sumdiff_C$TC2)
Sumdiff_C$TC2_cen <- as.logical(Sumdiff_C$TC2_cen)
Sumdiff_C$Prefilter <- as.factor(Sumdiff_C$Prefilter)
obs <- Sumdiff_C$TC2
censored <- Sumdiff_C$TC2_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_C$Prefilter, data = Sumdiff_C)
print(cenfit_model)

#EC2 EC2_cen
Sumdiff_C$EC2 <- as.numeric(Sumdiff_C$EC2)
Sumdiff_C$EC2_cen <- as.logical(Sumdiff_C$EC2_cen)
Sumdiff_C$Prefilter <- as.factor(Sumdiff_C$Prefilter)
obs <- Sumdiff_C$EC2
censored <- Sumdiff_C$EC2_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_C$Prefilter, data = Sumdiff_C)
print(cenfit_model)

###########Normality test ###########################

W <- sumdiffv_C[1:4, ]
WO <- sumdiffv_C[5:8, ]

summary(W$pmmov)
sd(W$pmmov)
shapiro.test(W$pmmov)
summary(WO$pmmov)
sd(WO$pmmov)
shapiro.test(WO$pmmov)

summary(W$BacHum)
sd(W$BacHum)
shapiro.test(W$BacHum)
summary(WO$BacHum)
sd(WO$BacHum)
shapiro.test(WO$BacHum)

summary(W$CrAss)
sd(W$CrAss)
shapiro.test(W$CrAss)
summary(WO$CrAss)
sd(WO$CrAss)
shapiro.test(WO$CrAss)

summary(W$MS2)
sd(W$MS2)
shapiro.test(W$MS2)
summary(WO$MS2)
sd(WO$MS2)
shapiro.test(WO$MS2)

summary(W$TC2)
sd(W$TC2)
shapiro.test(W$TC2)
summary(WO$TC2)
sd(WO$TC2)
shapiro.test(WO$TC2)

summary(W$EC2)
sd(W$EC2)
shapiro.test(W$EC2)
summary(WO$EC2)
sd(WO$EC2)
shapiro.test(WO$EC2)

wilcox.test(W$BacHum, WO$BacHum)   
wilcox.test(W$BacHum, WO$BacHum, alternative="greater")   
wilcox.test(W$BacHum, WO$BacHum, alternative="less") 

wilcox.test(W$CrAss, WO$CrAss)
wilcox.test(W$CrAss, WO$CrAss, alternative="greater")
wilcox.test(W$CrAss, WO$CrAss, alternative="less")

t.test(W$pmmov, WO$pmmov)
t.test(W$pmmov, WO$pmmov, paired = F, alternative="greater")
t.test(W$pmmov, WO$pmmov, paired = F, alternative="less")

wilcox.test(W$MS2, WO$MS2)   
wilcox.test(W$MS2, WO$MS2, alternative="greater")   
wilcox.test(W$MS2, WO$MS2, alternative="less") 

wilcox.test(W$TC2, WO$TC2)
t.test(W$TC2, WO$TC2, paired = F, alternative="greater")
t.test(W$TC2, WO$TC2, paired = F, alternative="less")

wilcox.test(W$EC2, WO$EC2)
t.test(W$EC2, WO$EC2, paired = F, alternative="greater")
t.test(W$EC2, WO$EC2, paired = F, alternative="less")



###########Normality test ###########################

W <- Sumdiff_time[1:4, ]
WO <- Sumdiff_time[5:8, ]

summary(W$AP)
sd(W$AP)
shapiro.test(W$AP)
summary(WO$AP)
sd(WO$AP)
shapiro.test(WO$AP)

summary(W$BP)
sd(W$BP)
shapiro.test(W$BP)
summary(WO$BP)
sd(WO$BP)
shapiro.test(WO$BP)

summary(W$CP)
sd(W$CP)
shapiro.test(W$CP)
summary(WO$CP)
sd(WO$CP)
shapiro.test(WO$CP)



#####Phase 2

###Phase 2
######### Descriptive statistics size # size sum #############
library(readr)
Sumdiff_mg <- read_csv("Sumdiff_mg.csv", 
                       col_types = cols(sfmD = col_number(), 
                                        pmmov = col_number(), intl1 = col_number(), 
                                        sul1 = col_number(), BacHum = col_number(), 
                                        RNA = col_number(), EC1 = col_number(), 
                                        TC1 = col_number(), EC2 = col_number(), 
                                        TC2 = col_number(), rec_TC = col_number(), 
                                        rec_EC = col_number()))
View(Sumdiff_mg)

########cen test#####################
#sfmd
Sumdiff_mg$sfmD <- as.numeric(Sumdiff_mg$sfmD)
Sumdiff_mg$sfmd_cen <- as.logical(Sumdiff_mg$sfmd_cen)
Sumdiff_mg$Poresize <- as.factor(Sumdiff_mg$Poresize)
obs <- Sumdiff_mg$sfmD
censored <- Sumdiff_mg$sfmd_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_mg$Poresize, data = Sumdiff_mg)
print(cenfit_model)

#BacHum
Sumdiff_mg$BacHum <- as.numeric(Sumdiff_mg$BacHum)
Sumdiff_mg$BacHum_cen <- as.logical(Sumdiff_mg$BacHum_cen)
Sumdiff_mg$Poresize <- as.factor(Sumdiff_mg$Poresize)
obs <- Sumdiff_mg$BacHum
censored <- Sumdiff_mg$BacHum_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_mg$Poresize, data = Sumdiff_mg)
print(cenfit_model)

#pmmov
Sumdiff_mg$pmmov <- as.numeric(Sumdiff_mg$pmmov)
Sumdiff_mg$pmmov_cen <- as.logical(Sumdiff_mg$pmmov_cen)
Sumdiff_mg$Poresize <- as.factor(Sumdiff_mg$Poresize)
obs <- Sumdiff_mg$pmmov
censored <- Sumdiff_mg$pmmov_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_mg$Poresize, data = Sumdiff_mg)
print(cenfit_model)
table(Sumdiff_mg$pmmov_cen)

#sul1 sul_cen
Sumdiff_mg$sul1 <- as.numeric(Sumdiff_mg$sul1)
Sumdiff_mg$sul_cen <- as.logical(Sumdiff_mg$sul_cen)
Sumdiff_mg$Poresize <- as.factor(Sumdiff_mg$Poresize)
obs <- Sumdiff_mg$sul1
censored <- Sumdiff_mg$sul_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_mg$Poresize, data = Sumdiff_mg)
print(cenfit_model)

#intl1 intl_cen
Sumdiff_mg$intl1 <- as.numeric(Sumdiff_mg$intl1)
Sumdiff_mg$intl_cen <- as.logical(Sumdiff_mg$intl_cen)
Sumdiff_mg$Poresize <- as.factor(Sumdiff_mg$Poresize)
obs <- Sumdiff_mg$intl1
censored <- Sumdiff_mg$intl_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_mg$Poresize, data = Sumdiff_mg)
print(cenfit_model)

#RNW RNW_cen
Sumdiff_mg$RNA <- as.numeric(Sumdiff_mg$RNA)
Sumdiff_mg$RNA_cen <- as.logical(Sumdiff_mg$RNA_cen)
Sumdiff_mg$Poresize <- as.factor(Sumdiff_mg$Poresize)
obs <- Sumdiff_mg$RNA
censored <- Sumdiff_mg$RNA_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_mg$Poresize, data = Sumdiff_mg)
print(cenfit_model)

#EC2 EC2_cen
Sumdiff_mg$EC2 <- as.numeric(Sumdiff_mg$EC2)
Sumdiff_mg$EC2_cen <- as.logical(Sumdiff_mg$EC1_cen)
Sumdiff_mg$Poresize <- as.factor(Sumdiff_mg$Poresize)
obs <- Sumdiff_mg$EC2
censored <- Sumdiff_mg$EC1_cen
cenfit_model <- cenfit(obs, censored, Sumdiff_mg$Poresize, data = Sumdiff_mg)
print(cenfit_model)

###########Normality test ###########################

A <- Sumdiff_mg[1:8, ]
C <- Sumdiff_mg[9:16, ]

summary(A$BacHum)
sd(A$BacHum)
shapiro.test(A$BacHum)
summary(C$BacHum)
sd(C$BacHum)
shapiro.test(C$BacHum)

summary(A$sfmD)
sd(A$sfmD)
shapiro.test(A$sfmD)
summary(C$sfmD)
sd(C$sfmD)
shapiro.test(C$sfmD)

summary(A$pmmov)
sd(A$pmmov)
shapiro.test(A$pmmov)
summary(C$pmmov)
sd(C$pmmov)
shapiro.test(C$pmmov)

summary(A$sul1)
sd(A$sul1)
shapiro.test(A$sul1)
summary(C$sul1)
sd(C$sul1)
shapiro.test(C$sul1)

summary(A$intl1)
sd(A$intl1)
shapiro.test(A$intl1)
summary(C$intl1)
sd(C$intl1)
shapiro.test(C$intl1)

summary(A$RNA)
sd(A$RNA)
shapiro.test(A$RNA)
summary(C$RNA)
sd(C$RNA)
shapiro.test(C$RNA)

summary(A$EC1)
sd(A$EC1)
shapiro.test(A$EC1)
summary(C$EC1)
sd(C$EC1)
shapiro.test(C$EC1)

summary(A$TC1)
sd(A$TC1)
shapiro.test(A$TC1)
summary(C$TC1)
sd(C$TC1)
shapiro.test(C$TC1)

summary(A$EC2)
sd(A$EC2)
shapiro.test(A$EC2)
summary(C$EC2)
sd(C$EC2)
shapiro.test(C$EC2)

summary(A$TC2)
sd(A$TC2)
shapiro.test(A$TC2)
summary(C$TC2)
sd(C$TC2)
shapiro.test(C$TC2)


######### Descriptive statistics size # mgcl sum #############

library(readr)
sumdiff_v2allmg <- read_csv("sumdiff_v2allmg.csv", 
                            col_types = cols(pmmov = col_number(), 
                                             BacHum = col_number(), CrAss = col_number(), 
                                             MS2 = col_number(), EC1 = col_number(), 
                                             TC1 = col_number(), EC2 = col_number(), 
                                             TC2 = col_number(), rec_TC = col_number(), 
                                             rec_EC = col_number()))
head(sumdiff_v2allmg)

########cen test#####################
#sfmd
sumdiff_mgcl$sfmD <- as.numeric(sumdiff_mgcl$sfmD)
sumdiff_mgcl$sfmd_cen <- as.logical(sumdiff_mgcl$sfmd_cen)
sumdiff_mgcl$Prefilter <- as.factor(sumdiff_mgcl$Prefilter)
obs <- sumdiff_mgcl$sfmD
censored <- sumdiff_mgcl$sfmd_cen
cenfit_model <- cenfit(obs, censored, sumdiff_mgcl$Prefilter, data = sumdiff_mgcl)
print(cenfit_model)

#BacHum
sumdiff_mgcl$BacHum <- as.numeric(sumdiff_mgcl$BacHum)
sumdiff_mgcl$BacHum_cen <- as.logical(sumdiff_mgcl$BacHum_cen)
sumdiff_mgcl$Prefilter <- as.factor(sumdiff_mgcl$Prefilter)
obs <- sumdiff_mgcl$BacHum
censored <- sumdiff_mgcl$BacHum_cen
cenfit_model <- cenfit(obs, censored, sumdiff_mgcl$Prefilter, data = sumdiff_mgcl)
print(cenfit_model)

#pmmov
sumdiff_mgcl$pmmov <- as.numeric(sumdiff_mgcl$pmmov)
sumdiff_mgcl$pmmov_cen <- as.logical(sumdiff_mgcl$pmmov_cen)
sumdiff_mgcl$Prefilter <- as.factor(sumdiff_mgcl$Prefilter)
obs <- sumdiff_mgcl$pmmov
censored <- sumdiff_mgcl$pmmov_cen
cenfit_model <- cenfit(obs, censored, sumdiff_mgcl$Prefilter, data = sumdiff_mgcl)
print(cenfit_model)
table(sumdiff_mgcl$pmmov_cen)

#sul1 sul_cen
sumdiff_mgcl$sul1 <- as.numeric(sumdiff_mgcl$sul1)
sumdiff_mgcl$sul_cen <- as.logical(sumdiff_mgcl$sul_cen)
sumdiff_mgcl$Prefilter <- as.factor(sumdiff_mgcl$Prefilter)
obs <- sumdiff_mgcl$sul1
censored <- sumdiff_mgcl$sul_cen
cenfit_model <- cenfit(obs, censored, sumdiff_mgcl$Prefilter, data = sumdiff_mgcl)
print(cenfit_model)

#intl1 intl_cen
sumdiff_mgcl$intl1 <- as.numeric(sumdiff_mgcl$intl1)
sumdiff_mgcl$intl_cen <- as.logical(sumdiff_mgcl$intl_cen)
sumdiff_mgcl$Prefilter <- as.factor(sumdiff_mgcl$Prefilter)
obs <- sumdiff_mgcl$intl1
censored <- sumdiff_mgcl$intl_cen
cenfit_model <- cenfit(obs, censored, sumdiff_mgcl$Prefilter, data = sumdiff_mgcl)
print(cenfit_model)

#RNA RNA_cen
sumdiff_mgcl$RNA <- as.numeric(sumdiff_mgcl$RNA)
sumdiff_mgcl$RNA_cen <- as.logical(sumdiff_mgcl$RNA_cen)
sumdiff_mgcl$Prefilter <- as.factor(sumdiff_mgcl$Prefilter)
obs <- sumdiff_mgcl$RNA
censored <- sumdiff_mgcl$RNA_cen
cenfit_model <- cenfit(obs, censored, sumdiff_mgcl$Prefilter, data = sumdiff_mgcl)
print(cenfit_model)
table(sumdiff_mgcl$RNA_cen)

#EC2 EC2_cen
sumdiff_mgcl$TC2 <- as.numeric(sumdiff_mgcl$TC2)
sumdiff_mgcl$TC2_cen <- as.logical(sumdiff_mgcl$TC2_cen)
sumdiff_mgcl$Prefilter <- as.factor(sumdiff_mgcl$Prefilter)
obs <- sumdiff_mgcl$TC2
censored <- sumdiff_mgcl$TC2_cen
cenfit_model <- cenfit(obs, censored, sumdiff_mgcl$Prefilter, data = sumdiff_mgcl)
print(cenfit_model)

###########Normality test ###########################

W <- sumdiff_v2allmg[1:8, ]
WO <- sumdiff_v2allmg[9:16, ]

summary(W$pmmov)
sd(W$pmmov)
shapiro.test(W$pmmov)
summary(WO$pmmov)
sd(WO$pmmov)
shapiro.test(WO$pmmov)

summary(W$BacHum)
sd(W$BacHum)
shapiro.test(W$BacHum)
summary(WO$BacHum)
sd(WO$BacHum)
shapiro.test(WO$BacHum)

summary(W$CrAss)
sd(W$CrAss)
shapiro.test(W$CrAss)
summary(WO$CrAss)
sd(WO$CrAss)
shapiro.test(WO$CrAss)

summary(W$MS2)
sd(W$MS2)
shapiro.test(W$MS2)
summary(WO$MS2)
sd(WO$MS2)
shapiro.test(WO$MS2)

summary(W$TC2)
sd(W$TC2)
shapiro.test(W$TC2)
summary(WO$TC2)
sd(WO$TC2)
shapiro.test(WO$TC2)

summary(W$EC2)
sd(W$EC2)
shapiro.test(W$EC2)
summary(WO$EC2)
sd(WO$EC2)
shapiro.test(WO$EC2)

one.way.anova_event <- aov(sumdiff_mgcl$BacHum ~ sumdiff_mgcl$Poresize, data = sumdiff_mgcl)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

wilcox.test(W$BacHum, WO$BacHum)   
wilcox.test(W$BacHum, WO$BacHum, alternative="greater")   
wilcox.test(W$BacHum, WO$BacHum, alternative="less") 

one.way.anova_event <- aov(sumdiff_mgcl$sfmD ~ sumdiff_mgcl$Poresize, data = sumdiff_mgcl)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

t.test(W$sfmD, WO$sfmD)
t.test(W$sfmD, WO$sfmD, paired = F, alternative="greater")
t.test(W$sfmD, WO$sfmD, paired = F, alternative="less")

one.way.anova_event <- aov(sumdiff_mgcl$pmmov ~ sumdiff_mgcl$Poresize, data = sumdiff_mgcl)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

t.test(W$pmmov, WO$pmmov)
t.test(W$pmmov, WO$pmmov, paired = F, alternative="greater")
t.test(W$pmmov, WO$pmmov, paired = F, alternative="less")

kruskal.test(sumdiff_mgcl$sul1 ~ sumdiff_mgcl$Poresize, data = sumdiff_mgcl)
library(dunn.test)
dunn.test(sumdiff_mgcl$sul1, g=sumdiff_mgcl$Poresize, method="holm", kw=TRUE, label=TRUE, table=FALSE, alpha=0.05, list=TRUE
          ,altp=TRUE)

one.way.anova_event <- aov(sumdiff_mgcl$sul1 ~ sumdiff_mgcl$Poresize, data = sumdiff_mgcl)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

wilcox.test(W$sul1, WO$sul1)   
wilcox.test(W$sul1, WO$sul1, alternative="greater")   
wilcox.test(W$sul1, WO$sul1, alternative="less") 

kruskal.test(sumdiff_mgcl$intl1 ~ sumdiff_mgcl$Poresize, data = sumdiff_mgcl)
library(dunn.test)
dunn.test(sumdiff_mgcl$intl1, g=sumdiff_mgcl$Poresize, method="holm", kw=TRUE, label=TRUE, table=FALSE, alpha=0.05, list=TRUE
          ,altp=TRUE)

one.way.anova_event <- aov(sumdiff_mgcl$intl1 ~ sumdiff_mgcl$Poresize, data = sumdiff_mgcl)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

wilcox.test(W$intl1, WO$intl1)   
wilcox.test(W$intl1, WO$intl1, alternative="greater")   
wilcox.test(W$intl1, WO$intl1, alternative="less") 

wilcox.test(W$intl1, WO$intl1, correct = TRUE)   
wilcox.test(W$intl1, WO$intl1, alternative="greater")   
wilcox.test(W$intl1, WO$intl1, alternative="less")

one.way.anova_event <- aov(sumdiff_mgcl$RNA ~ sumdiff_mgcl$Poresize, data = sumdiff_mgcl)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

t.test(W$RNA, WO$RNA)
t.test(W$RNA, WO$RNA, paired = F, alternative="greater")
t.test(W$RNA, WO$RNA, paired = F, alternative="less")

one.way.anova_event <- aov(sumdiff_mgcl$TC2 ~ sumdiff_mgcl$Poresize, data = sumdiff_mgcl)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

t.test(W$TC2, WO$TC2)
t.test(W$TC2, WO$TC2, paired = F, alternative="greater")
t.test(W$TC2, WO$TC2, paired = F, alternative="less")

one.way.anova_event <- aov(sumdiff_mgcl$EC2 ~ sumdiff_mgcl$Poresize, data = sumdiff_mgcl)
summary(one.way.anova_event)
TukeyHSD(one.way.anova_event)

t.test(W$EC2, WO$EC2)
t.test(W$EC2, WO$EC2, paired = F, alternative="greater")
t.test(W$EC2, WO$EC2, paired = F, alternative="less")



######### Descriptive statistics size # prefilter sumA #############

library(readr)
sumdiffv_mgA <- read_csv("sumdiffv_mgA.csv", 
                         col_types = cols(pmmov = col_number(), 
                                          BacHum = col_number(), CrAss = col_number(), 
                                          MS2 = col_number(), EC1 = col_number(), 
                                          TC1 = col_number(), EC2 = col_number(), 
                                          TC2 = col_number(), rec_TC = col_number(), 
                                          rec_EC = col_number()))
head(sumdiffv_mgA)

########cen test#####################
#BacHum
Sumdiffmg_A$BacHum <- as.numeric(Sumdiffmg_A$BacHum)
Sumdiffmg_A$BacHum_cen <- as.logical(Sumdiffmg_A$BacHum_cen)
Sumdiffmg_A$Prefilter <- as.factor(Sumdiffmg_A$Prefilter)
obs <- Sumdiffmg_A$BacHum
censored <- Sumdiffmg_A$BacHum_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_A$Prefilter, data = Sumdiffmg_A)
print(cenfit_model)

#sfmd
Sumdiffmg_A$sfmD <- as.numeric(Sumdiffmg_A$sfmD)
Sumdiffmg_A$sfmd_cen <- as.logical(Sumdiffmg_A$sfmd_cen)
Sumdiffmg_A$Prefilter <- as.factor(Sumdiffmg_A$Prefilter)
obs <- Sumdiffmg_A$sfmD
censored <- Sumdiffmg_A$sfmd_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_A$Prefilter, data = Sumdiffmg_A)
print(cenfit_model)

#pmmov
Sumdiffmg_A$pmmov <- as.numeric(Sumdiffmg_A$pmmov)
Sumdiffmg_A$pmmov_cen <- as.logical(Sumdiffmg_A$pmmov_cen)
Sumdiffmg_A$Prefilter <- as.factor(Sumdiffmg_A$Prefilter)
obs <- Sumdiffmg_A$pmmov
censored <- Sumdiffmg_A$pmmov_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_A$Prefilter, data = Sumdiffmg_A)
print(cenfit_model)
table(Sumdiffmg_A$pmmov_cen)

#sul1 sul_cen
Sumdiffmg_A$sul1 <- as.numeric(Sumdiffmg_A$sul1)
Sumdiffmg_A$sul_cen <- as.logical(Sumdiffmg_A$sul_cen)
Sumdiffmg_A$Prefilter <- as.factor(Sumdiffmg_A$Prefilter)
obs <- Sumdiffmg_A$sul1
censored <- Sumdiffmg_A$sul_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_A$Prefilter, data = Sumdiffmg_A)
print(cenfit_model)

#intl1 intl_cen
Sumdiffmg_A$intl1 <- as.numeric(Sumdiffmg_A$intl1)
Sumdiffmg_A$intl_cen <- as.logical(Sumdiffmg_A$intl_cen)
Sumdiffmg_A$Prefilter <- as.factor(Sumdiffmg_A$Prefilter)
obs <- Sumdiffmg_A$intl1
censored <- Sumdiffmg_A$intl_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_A$Prefilter, data = Sumdiffmg_A)
print(cenfit_model)

#RNW RNW_cen
Sumdiffmg_A$RNA <- as.numeric(Sumdiffmg_A$RNA)
Sumdiffmg_A$RNA_cen <- as.logical(Sumdiffmg_A$RNA_cen)
Sumdiffmg_A$Prefilter <- as.factor(Sumdiffmg_A$Prefilter)
obs <- Sumdiffmg_A$RNA
censored <- Sumdiffmg_A$RNA_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_A$Prefilter, data = Sumdiffmg_A)
print(cenfit_model)
table(Sumdiffmg_A$RNA_cen)

#TC2 TC2_cen
Sumdiffmg_A$TC2 <- as.numeric(Sumdiffmg_A$TC2)
Sumdiffmg_A$TC2_cen <- as.logical(Sumdiffmg_A$TC2_cen)
Sumdiffmg_A$Prefilter <- as.factor(Sumdiffmg_A$Prefilter)
obs <- Sumdiffmg_A$TC2
censored <- Sumdiffmg_A$TC2_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_A$Prefilter, data = Sumdiffmg_A)
print(cenfit_model)

#EC2 EC2_cen
Sumdiffmg_A$EC2 <- as.numeric(Sumdiffmg_A$EC2)
Sumdiffmg_A$EC2_cen <- as.logical(Sumdiffmg_A$EC2_cen)
Sumdiffmg_A$Prefilter <- as.factor(Sumdiffmg_A$Prefilter)
obs <- Sumdiffmg_A$EC2
censored <- Sumdiffmg_A$EC2_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_A$Prefilter, data = Sumdiffmg_A)
print(cenfit_model)

###########Normality test ###########################

W <- sumdiffv_mgA[1:4, ]
WO <- sumdiffv_mgA[5:8, ]

summary(W$pmmov)
sd(W$pmmov)
shapiro.test(W$pmmov)
summary(WO$pmmov)
sd(WO$pmmov)
shapiro.test(WO$pmmov)

summary(W$BacHum)
sd(W$BacHum)
shapiro.test(W$BacHum)
summary(WO$BacHum)
sd(WO$BacHum)
shapiro.test(WO$BacHum)

summary(W$CrAss)
sd(W$CrAss)
shapiro.test(W$CrAss)
summary(WO$CrAss)
sd(WO$CrAss)
shapiro.test(WO$CrAss)

summary(W$MS2)
sd(W$MS2)
shapiro.test(W$MS2)
summary(WO$MS2)
sd(WO$MS2)
shapiro.test(WO$MS2)

summary(W$TC2)
sd(W$TC2)
shapiro.test(W$TC2)
summary(WO$TC2)
sd(WO$TC2)
shapiro.test(WO$TC2)

summary(W$EC2)
sd(W$EC2)
shapiro.test(W$EC2)
summary(WO$EC2)
sd(WO$EC2)
shapiro.test(WO$EC2)

wilcox.test(W$BacHum, WO$BacHum)   
wilcox.test(W$BacHum, WO$BacHum, alternative="greater")   
wilcox.test(W$BacHum, WO$BacHum, alternative="less") 

wilcox.test(W$CrAss, WO$CrAss)
wilcox.test(W$CrAss, WO$CrAss, alternative="greater")
wilcox.test(W$CrAss, WO$CrAss, alternative="less")

t.test(W$pmmov, WO$pmmov)
t.test(W$pmmov, WO$pmmov, paired = F, alternative="greater")
t.test(W$pmmov, WO$pmmov, paired = F, alternative="less")

wilcox.test(W$MS2, WO$MS2)   
wilcox.test(W$MS2, WO$MS2, alternative="greater")   
wilcox.test(W$MS2, WO$MS2, alternative="less") 

wilcox.test(W$TC2, WO$TC2)
t.test(W$TC2, WO$TC2, paired = F, alternative="greater")
t.test(W$TC2, WO$TC2, paired = F, alternative="less")

wilcox.test(W$EC2, WO$EC2)
t.test(W$EC2, WO$EC2, paired = F, alternative="greater")
t.test(W$EC2, WO$EC2, paired = F, alternative="less")

######### Descriptive statistics size # prefilter sumC #############

library(readr)
sumdiffv_mgC <- read_csv("sumdiffv_mgC.csv", 
                         col_types = cols(pmmov = col_number(), 
                                          BacHum = col_number(), CrAss = col_number(), 
                                          MS2 = col_number(), EC1 = col_number(), 
                                          TC1 = col_number(), EC2 = col_number(), 
                                          TC2 = col_number(), rec_TC = col_number(), 
                                          rec_EC = col_number()))
head(sumdiffv_mgC)

########cen test#####################
#BacHum
Sumdiffmg_C$BacHum <- as.numeric(Sumdiffmg_C$BacHum)
Sumdiffmg_C$BacHum_cen <- as.logical(Sumdiffmg_C$BacHum_cen)
Sumdiffmg_C$Prefilter <- as.factor(Sumdiffmg_C$Prefilter)
obs <- Sumdiffmg_C$BacHum
censored <- Sumdiffmg_C$BacHum_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_C$Prefilter, data = Sumdiffmg_C)
print(cenfit_model)

#sfmd
Sumdiffmg_C$sfmD <- as.numeric(Sumdiffmg_C$sfmD)
Sumdiffmg_C$sfmd_cen <- as.logical(Sumdiffmg_C$sfmd_cen)
Sumdiffmg_C$Prefilter <- as.factor(Sumdiffmg_C$Prefilter)
obs <- Sumdiffmg_C$sfmD
censored <- Sumdiffmg_C$sfmd_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_C$Prefilter, data = Sumdiffmg_C)
print(cenfit_model)

#pmmov
Sumdiffmg_C$pmmov <- as.numeric(Sumdiffmg_C$pmmov)
Sumdiffmg_C$pmmov_cen <- as.logical(Sumdiffmg_C$pmmov_cen)
Sumdiffmg_C$Prefilter <- as.factor(Sumdiffmg_C$Prefilter)
obs <- Sumdiffmg_C$pmmov
censored <- Sumdiffmg_C$pmmov_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_C$Prefilter, data = Sumdiffmg_C)
print(cenfit_model)
table(Sumdiffmg_C$pmmov_cen)

#sul1 sul_cen
Sumdiffmg_C$sul1 <- as.numeric(Sumdiffmg_C$sul1)
Sumdiffmg_C$sul_cen <- as.logical(Sumdiffmg_C$sul_cen)
Sumdiffmg_C$Prefilter <- as.factor(Sumdiffmg_C$Prefilter)
obs <- Sumdiffmg_C$sul1
censored <- Sumdiffmg_C$sul_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_C$Prefilter, data = Sumdiffmg_C)
print(cenfit_model)

#intl1 intl_cen
Sumdiffmg_C$intl1 <- as.numeric(Sumdiffmg_C$intl1)
Sumdiffmg_C$intl_cen <- as.logical(Sumdiffmg_C$intl_cen)
Sumdiffmg_C$Prefilter <- as.factor(Sumdiffmg_C$Prefilter)
obs <- Sumdiffmg_C$intl1
censored <- Sumdiffmg_C$intl_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_C$Prefilter, data = Sumdiffmg_C)
print(cenfit_model)

#RNW RNW_cen
Sumdiffmg_C$RNA <- as.numeric(Sumdiffmg_C$RNA)
Sumdiffmg_C$RNA_cen <- as.logical(Sumdiffmg_C$RNA_cen)
Sumdiffmg_C$Prefilter <- as.factor(Sumdiffmg_C$Prefilter)
obs <- Sumdiffmg_C$RNA
censored <- Sumdiffmg_C$RNA_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_C$Prefilter, data = Sumdiffmg_C)
print(cenfit_model)
table(Sumdiffmg_C$RNA_cen)

#TC2 TC2_cen
Sumdiffmg_C$TC2 <- as.numeric(Sumdiffmg_C$TC2)
Sumdiffmg_C$TC2_cen <- as.logical(Sumdiffmg_C$TC2_cen)
Sumdiffmg_C$Prefilter <- as.factor(Sumdiffmg_C$Prefilter)
obs <- Sumdiffmg_C$TC2
censored <- Sumdiffmg_C$TC2_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_C$Prefilter, data = Sumdiffmg_C)
print(cenfit_model)

#EC2 EC2_cen
Sumdiffmg_C$EC2 <- as.numeric(Sumdiffmg_C$EC2)
Sumdiffmg_C$EC2_cen <- as.logical(Sumdiffmg_C$EC2_cen)
Sumdiffmg_C$Prefilter <- as.factor(Sumdiffmg_C$Prefilter)
obs <- Sumdiffmg_C$EC2
censored <- Sumdiffmg_C$EC2_cen
cenfit_model <- cenfit(obs, censored, Sumdiffmg_C$Prefilter, data = Sumdiffmg_C)
print(cenfit_model)

###########Normality test ###########################

W <- sumdiffv_mgC[1:4, ]
WO <- sumdiffv_mgC[5:8, ]

summary(W$pmmov)
sd(W$pmmov)
shapiro.test(W$pmmov)
summary(WO$pmmov)
sd(WO$pmmov)
shapiro.test(WO$pmmov)

summary(W$BacHum)
sd(W$BacHum)
shapiro.test(W$BacHum)
summary(WO$BacHum)
sd(WO$BacHum)
shapiro.test(WO$BacHum)

summary(W$CrAss)
sd(W$CrAss)
shapiro.test(W$CrAss)
summary(WO$CrAss)
sd(WO$CrAss)
shapiro.test(WO$CrAss)

summary(W$MS2)
sd(W$MS2)
shapiro.test(W$MS2)
summary(WO$MS2)
sd(WO$MS2)
shapiro.test(WO$MS2)

summary(W$TC2)
sd(W$TC2)
shapiro.test(W$TC2)
summary(WO$TC2)
sd(WO$TC2)
shapiro.test(WO$TC2)

summary(W$EC2)
sd(W$EC2)
shapiro.test(W$EC2)
summary(WO$EC2)
sd(WO$EC2)
shapiro.test(WO$EC2)

wilcox.test(W$BacHum, WO$BacHum)   
wilcox.test(W$BacHum, WO$BacHum, alternative="greater")   
wilcox.test(W$BacHum, WO$BacHum, alternative="less") 

wilcox.test(W$CrAss, WO$CrAss)
wilcox.test(W$CrAss, WO$CrAss, alternative="greater")
wilcox.test(W$CrAss, WO$CrAss, alternative="less")

t.test(W$pmmov, WO$pmmov)
t.test(W$pmmov, WO$pmmov, paired = F, alternative="greater")
t.test(W$pmmov, WO$pmmov, paired = F, alternative="less")

wilcox.test(W$MS2, WO$MS2)   
wilcox.test(W$MS2, WO$MS2, alternative="greater")   
wilcox.test(W$MS2, WO$MS2, alternative="less") 

wilcox.test(W$TC2, WO$TC2)
t.test(W$TC2, WO$TC2, paired = F, alternative="greater")
t.test(W$TC2, WO$TC2, paired = F, alternative="less")

wilcox.test(W$EC2, WO$EC2)
t.test(W$EC2, WO$EC2, paired = F, alternative="greater")
t.test(W$EC2, WO$EC2, paired = F, alternative="less")

####heat map####
library(readr)
corr_cpx <- read_csv("corr_cpx.csv", col_types = cols(sfmD = col_number(), 
                                                      `16SrRNA` = col_number(), intl1 = col_number(), 
                                                      sul1 = col_number(), PMMoV = col_number(), 
                                                      CrAss = col_number(), TotalColi = col_number(), 
                                                      Ecoli = col_number(), `%rec.TC` = col_number(), 
                                                      `%rec.EC` = col_number(), pH = col_number(), 
                                                      Turbidity = col_number()))
head(corr_cpx)

attach(corr_cpx)

p.mat <- cor_pmat(corr_cpx) #correlation matrix with p-values
head(p.mat)
p.mat
m <- cor(corr_cpx) 
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

corr <- round(cor(corr_cpx), 2)

p.df <- as.data.frame(ggcorrplot::cor_pmat(corr_cpx))

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
                      lab = T, ggtheme = ggplot2::theme_gray, colors = c("#3EDBF0", "white", "#FF75A0")) +
  theme(legend.text = element_text(color = "black", family = "Arial", size = 10), #detail site label
        legend.title = element_text(color = "black", family = "Arial", size = 10, face = "bold"), #site detail label
        axis.text = element_text(color = "black", family = "Arial", size = 2),
        panel.background = element_rect(fill = "#F1F1F1", colour = NA),
        panel.grid.minor = element_line(colour = "white", size = 0.2),
        panel.grid.major = element_line(colour = "white", size = 0.2))

p.labs$in.df = ifelse(is.na(match(paste0(p.labs$Var1, p.labs$Var2),
                                  paste0(cor_plot[["data"]]$Var1, cor_plot[["data"]]$Var2))),
                      "No", "Yes")

p.labs = select(filter(p.labs, in.df == "Yes"), -in.df)

cor.plot.labs = cor_plot +
  geom_text(aes(x = p.labs$Var1,
                y = p.labs$Var2),
            label = p.labs$lab,
            nudge_y = 0.25,
            size = 5)

cor.plot.labs


ggsave(file="cor.plot.labsa.jpeg", cor.plot.labs,
       width= 200, height = 200, units = "mm", dpi=600)