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
cal$education[cal$education %in% 6] <- NA
cal$favor_gunregs <- as.numeric(revalue(as.character(cal$favor_gunregs), c("2" = "3", "3" = "2")))
cal <- na.omit(cal)
cal.dem <- cal[cal$party == "Democrat",]
cal.rep <- cal[cal$party == "Republican",]
#cal <- cal %>% mutate(education = na_if(education, 6))

## new data without "party" "strength" "race" and "vote_house"
cal2 <- cal[, !colnames(cal) %in% c("party", "strength", "vote_house", "race")]
cal2.dem <- cal.dem[, !colnames(cal) %in% c("party", "strength", "vote_house", "race")]#; cal2.dem <- na.omit(cal2.dem)
cal2.rep <- cal.rep[, !colnames(cal) %in% c("party", "strength", "vote_house", "race")]#; cal2.rep <- na.omit(cal2.rep)

## Apply PCA to data 
cal.pca <- prcomp(cal2, center=T, scale=T) ## applying PCA to the data without vote choice last time
cal.pca.dem <- prcomp(cal2.dem, center=T, scale=T)
cal.pca.rep <- prcomp(cal2.rep, center=T, scale=T)

## max overlaps
options(ggrepel.max.overlaps = Inf)

## loading plot
plot.cal.loading <- fviz_pca_var(cal.pca,
                                 col.var = "contrib", # Color by contributions to the PC
                                 arrowsize = .75,
                                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #Choose gradient colors
                                 title = "(a) Loadings Plot - PCA", #Change plot title
                                 legend.title = "Contribution", #Change legend title
                                 repel = TRUE, 
                                 labelsize = 3
                                 ) + theme(plot.title = element_text(size=15), text = element_text(size=15))# + theme(legend.position = "none")# Repel labels away from each other
plot.cal.loading

plot.cal.loading.dem <- fviz_pca_var(cal.pca.dem,
                                     col.var = "contrib", # Color by contributions to the PC
                                     arrowsize = .75,
                                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #Choose gradient colors
                                     title = "(b) Loadings Plot - PCA Democrats", #Change plot title
                                     legend.title = "Contribution", #Change legend title
                                     repel = TRUE, 
                                     labelsize = 3) + theme(plot.title = element_text(size=15), text = element_text(size=15)) # + theme(legend.position = "none")# Repel labels away from each other
plot.cal.loading.dem <- plot.cal.loading.dem + guides(color = guide_legend(override.aes = list(color = c(NA, NA, NA, NA)))) +
                        theme(legend.text = element_blank(), legend.title = element_blank())
plot.cal.loading.dem

plot.cal.loading.rep <- fviz_pca_var(cal.pca.rep,
                                     col.var = "contrib", # Color by contributions to the PC
                                     arrowsize = .75,
                                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #Choose gradient colors
                                     title = "(c) Loadings Plot - PCA Republicans", #Change plot title
                                     legend.title = "Contribution", #Change legend title
                                     repel = TRUE, 
                                     labelsize = 3) + theme(plot.title = element_text(size=15), text = element_text(size=15)) #theme(legend.position = "none")# Repel labels away from each other
plot.cal.loading.rep <- plot.cal.loading.rep + guides(color = guide_legend(override.aes = list(color = c(NA, NA, NA)))) +
                        theme(legend.text = element_blank(), legend.title = element_blank())
plot.cal.loading.rep


## individual plot
plot.cal.ind <- fviz_pca_ind(cal.pca,
                             col.ind = cal$party,
                             palette = c("#00AFBB", "#FC4E07"), #Choose colors for Dems v. Reps
                             addEllipses = TRUE, #Add group ellipses
                             legend.title = "Partisanship",
                             title = "(d) Individual Plot - PCA",
                             geom = "point",
                             repel = F) + 
                             scale_shape_manual(values=c(20,20)) +
                             theme(plot.title = element_text(size=15), text = element_text(size=15)) 

plot.cal.ind

plot.cal.ind.dem <- fviz_pca_ind(cal.pca.dem,
                                 col.ind = cal.dem$party,
                                 palette = "#00AFBB", #Choose colors for Dems v. Reps
                                 addEllipses = F, #Add group ellipses
                                 #legend.title = "Congressional\nVote Choice",
                                 title = "(e) Individual Plot - PCA Democrats",
                                 geom = "point",
                                 repel = F) + theme(legend.position = "none") +
                                 theme(plot.title = element_text(size=15), text = element_text(size=15)) 

plot.cal.ind.dem

plot.cal.ind.rep <- fviz_pca_ind(cal.pca.rep,
                                 col.ind = cal.rep$party,
                                 palette = "#FC4E07", #Choose colors for Dems v. Reps
                                 addEllipses = F, #Add group ellipses
                                 #legend.title = "Congressional\nVote Choice",
                                 title = "(f) Individual Plot - PCA Republicans",
                                 geom = "point",
                                 repel = F) + theme(legend.position = "none") +
                                 theme(plot.title = element_text(size=15), text = element_text(size=15)) 

plot.cal.ind.rep

########

#figure1 <- ggarrange()


f1.1 <- ggarrange(plot.cal.loading, plot.cal.loading.dem, plot.cal.loading.rep, nrow = 1,
                  #labels = c("(a)", "(b)", "(c)"),
                  font.label=list(color="black",size=10),
                  common.legend = TRUE, legend="top")
f1.2 <- ggarrange(plot.cal.ind, plot.cal.ind.dem, plot.cal.ind.rep, nrow = 1,
                  #labels = c("(d)", "(e)", "(f)"),
                  font.label=list(color="black",size=10),
                  common.legend = TRUE, legend="bottom")
figure1 <- ggarrange(f1.1, f1.2, nrow = 2)

figure1

ggsave(figure1, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/figure1.v4.eps", device="eps",units="px",width = 3700,height = 3100)

ggsave(figure1, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/figure1.v3.eps", device="eps",units="px",width = 4000,height = 3600)

#ggsave(f1.1, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/f1.1.eps", device="eps")
#ggsave(f1.2, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/f1.2.eps", device="eps")
#ggsave(figure1, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/figure1.eps", device="eps")

#ggsave(f1.1, file="C:/Users/sfmcr/Dropbox/cpca_plot/f1.1.v2.eps", device="eps")
#ggsave(f1.2, file="C:/Users/sfmcr/Dropbox/cpca_plot/f1.2.v2.eps", device="eps")
#ggsave(figure1, file="C:/Users/sfmcr/Dropbox/cpca_plot/figure1.v2.eps", device="eps")

#934x776

#ggsave(f1.1, file="C:/Users/sfmcr/Dropbox/cpca_plot/f1.1.v2.eps", device="eps")
#ggsave(f1.2, file="C:/Users/sfmcr/Dropbox/cpca_plot/f1.2.v2.eps", device="eps")

#This is the one we're using
ggsave(figure1, file="C:/Users/sfmcr/Dropbox/cpca_plot/figure1.v4.eps", device="eps",units="px",width = 3700,height = 3100)

ggsave(figure1, file="C:/Users/sfmcr/Dropbox/cpca_plot/figure1.v3.eps", device="eps",units="px",width = 4000,height = 3600)

