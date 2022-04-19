rm(list=ls(all=TRUE))

library(tidyverse)
library(ggrepel)
library(ggforce)
library(scPCA)
library(factoextra)
library(readit)
library(fastICA)
library(ggpubr)

## loading data for cal_voter
cal <- readRDS("/Users/tpliu/Dropbox/Mac/Desktop/Data/cal_voters.RDS")
#cal <- readRDS("C:/Users/sfmcr/Dropbox/cPCA_cMCA_Project/Data/cal_voters.RDS")

## recode the variable "party" into a binary one
cal$party[cal$party %in% 1:3] <- 1
cal$party[cal$party %in% 4] <- NA
cal$party[cal$party %in% 5:7] <- 2
cal$party <- factor(cal$party, labels = c("Democrat", "Republican"))
#cal$party <- factor(cal$party, levels = c("Republican", "Democrat"))
cal <- na.omit(cal)
#cal.dem <- cal[cal$party == "Democrat",]
cal.rep <- cal[cal$party == "Republican",]

## new data without "party" "strength" and "vote_house"
cal2 <- cal[, !colnames(cal) %in% c("party", "strength", "vote_house", "race")]
#cal2.dem <- cal.dem[, !colnames(cal) %in% c("party", "strength", "vote_house")]#; cal2.dem <- na.omit(cal2.dem)
cal2.rep <- cal.rep[, !colnames(cal) %in% c("party", "strength", "vote_house", "race")]#; cal2.rep <- na.omit(cal2.rep)

## Apply PCA to data 
cal.pca <- prcomp(cal2, center=T, scale=T) ## applying PCA to the data without vote choice last time
cal.pca.rep <- prcomp(cal2.rep, center=T, scale=T)

## max overlaps
options(ggrepel.max.overlaps = Inf)

## extract "scores" or "principal components"
c_score.rep.1 <- cal.pca$x[cal$party=="Republican",]
c_score.rep.2 <- cal.pca.rep$x

## create the dataset for individual plot on the contrstive space
pca.df.cal.rep.1 <- c_score.rep.1[,1:2] %>% as_tibble() %>%
  mutate(wall = factor(cal$favor_borderwall[cal$party == "Republican"], labels = c("Approve", "Disapprove")))
colnames(pca.df.cal.rep.1) <- c("cPC1", "cPC2", "Wall")
#pca.df.cal.dem.1$"Dem_Approval" <- factor(pca.df.cal.dem.1$"Dem_Approval", levels=c("Disapprove","Approve"))

pca.df.cal.rep.2 <- c_score.rep.2[,1:2] %>% as_tibble() %>%
  mutate(wall = factor(cal$favor_borderwall[cal$party == "Republican"], labels = c("Approve", "Disapprove")))
colnames(pca.df.cal.rep.2) <- c("cPC1", "cPC2", "Wall")

## indivual plot
f5.1 <- ggplot(pca.df.cal.rep.1, aes(x=cPC1, y=cPC2, colour = Wall, fill = Wall)) +
        geom_point() +
        stat_ellipse(geom = "polygon", alpha = 0, linetype = 1) +
        labs(title = "(a) PCA Reps Subset (Dems Included in PCA)", color = "Border Wall\n Approval", fill = "Border Wall\n Approval") +
        scale_fill_manual(values = c("#FC4E07", "#00AFBB")) + scale_shape_manual(values=c(20,20)) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        theme(plot.title = element_text(size=15), text = element_text(size=15))

f5.2 <- ggplot(pca.df.cal.rep.2, aes(x=cPC1, y=cPC2, colour = Wall, fill = Wall)) +
        geom_point() +
        stat_ellipse(geom = "polygon", alpha = 0, linetype = 1) +
        labs(title = "(b) PCA Reps Only (Dems Excluded in PCA)", color = "Border Wall\n Approval", fill = "Border Wall\n Approval") +
        scale_fill_manual(values = c("#FC4E07", "#00AFBB")) + scale_shape_manual(values=c(20,20)) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        theme(plot.title = element_text(size=15), text = element_text(size=15))

figure5 <- ggarrange(f5.1, f5.2, 
                     #labels = c("(a)", "(b)"),
                     font.label=list(color="black",size=5),
                     common.legend = TRUE, legend="bottom", nrow = 1)

figure5


#ggsave(f5.1, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/f5.1.eps", device="eps")
#ggsave(f5.2, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/f5.2.eps", device="eps")
#ggsave(figure5, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/figure5.eps", device="eps")

ggsave(figure5, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/figure5.v2.eps", device="eps", units="px", width = 3600,height = 2000)


