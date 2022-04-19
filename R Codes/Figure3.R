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
cal.dem <- cal[cal$party == "Democrat",]
#cal.rep <- cal[cal$party == "Republican",]

## new data without "party" "strength" and "vote_house"
cal2 <- cal[, !colnames(cal) %in% c("party", "strength", "vote_house", "race")]
cal2.dem <- cal.dem[, !colnames(cal) %in% c("party", "strength", "vote_house", "race")]#; cal2.dem <- na.omit(cal2.dem)
#cal2.rep <- cal.rep[, !colnames(cal) %in% c("party", "strength", "vote_house")]#; cal2.rep <- na.omit(cal2.rep)

## Apply PCA to data 
cal.pca <- prcomp(cal2, center=T, scale=T) ## applying PCA to the data without vote choice last time
cal.pca.dem <- prcomp(cal2.dem, center=T, scale=T)
#cal.pca.rep <- prcomp(cal2.rep, center=T, scale=T)

## max overlaps
options(ggrepel.max.overlaps = Inf)

## extract "scores" or "principal components"
c_score.dem.1 <- cal.pca$x[cal$party=="Democrat",]
c_score.dem.2 <- cal.pca.dem$x

## create the dataset for individual plot on the contrstive space
pca.df.cal.dem.1 <- c_score.dem.1[,1:2] %>% as_tibble() %>%
                    mutate(dem_approval = factor(cal$view_democrats[cal$party == "Democrat"], labels = c("Approve", "Disapprove")))
colnames(pca.df.cal.dem.1) <- c("cPC1", "cPC2", "Dem_Approval")
pca.df.cal.dem.1$"Dem_Approval" <- factor(pca.df.cal.dem.1$"Dem_Approval", levels=c("Disapprove","Approve"))
  
pca.df.cal.dem.2 <- c_score.dem.2[,1:2] %>% as_tibble() %>%
                    mutate(dem_approval = factor(cal$view_democrats[cal$party == "Democrat"], labels = c("Approve", "Disapprove")))
colnames(pca.df.cal.dem.2) <- c("cPC1", "cPC2", "Dem_Approval")
pca.df.cal.dem.2$"Dem_Approval" <- factor(pca.df.cal.dem.2$"Dem_Approval", levels=c("Disapprove","Approve"))

## indivual plot
f3.1 <- ggplot(pca.df.cal.dem.1, aes(x=cPC1, y=cPC2, colour = Dem_Approval, fill = Dem_Approval)) +
        geom_point() +
        stat_ellipse(geom = "polygon", alpha = 0, linetype = 1) +
        labs(title = "(a) PCA Dems Subset (Reps Included)", color = "Democratic Party\n Approval", fill = "Democratic Party\n Approval") +
        scale_fill_manual(values = c("#FC4E07", "#00AFBB")) + scale_shape_manual(values=c(20,20)) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        theme(plot.title = element_text(size=15), text = element_text(size=15))

f3.2 <- ggplot(pca.df.cal.dem.2, aes(x=cPC1, y=cPC2, colour = Dem_Approval, fill = Dem_Approval)) +
        geom_point() +
        stat_ellipse(geom = "polygon", alpha = 0, linetype = 1) +
        labs(title = "(b) PCA Dems Only (Reps Excluded)", color = "Democratic Party\n Approval", fill = "Democratic Party\n Approval") +
        scale_fill_manual(values = c("#FC4E07", "#00AFBB")) + scale_shape_manual(values=c(20,20)) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        theme(plot.title = element_text(size=15), text = element_text(size=15))

figure3 <- ggarrange(f3.1, f3.2, 
                     #labels = c("(a)", "(b)"),
                     font.label=list(color="black",size=5),
                     common.legend = TRUE, legend="bottom", nrow = 1)

figure3


#ggsave(f3.1, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/f3.1.eps", device="eps")
#ggsave(f3.2, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/f3.2.eps", device="eps")
ggsave(figure3, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/figure3.v2.eps", device="eps", units="px", width = 3600,height = 2000)




