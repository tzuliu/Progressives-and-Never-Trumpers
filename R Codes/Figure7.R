rm(list=ls(all=TRUE))

library(tidyverse)
library(ggrepel)
library(ggforce)
library(factoextra)
library(readit)
library(fastICA)
library(MCMCpack)
library(lsa)

## loading data for cal_voter
cal <- readRDS("/Users/tpliu/Dropbox/Mac/Desktop/Data/cal_voters.RDS")

## recode the variable "party" into a binary one
cal$party[cal$party %in% 1:3] <- 1
cal$party[cal$party %in% 4] <- NA
cal$party[cal$party %in% 5:7] <- 2
cal$party <- factor(cal$party, labels = c("Democrat", "Republican"))
cal$party <- factor(cal$party, levels = c("Republican", "Democrat"))

## omit missing variables
cal <- na.omit(cal)

## new data without "party" "strength" and "vote_house"
cal2 <- cal[, !colnames(cal) %in% c("party", "strength", "vote_house")]
cal2 <- as.matrix(cal2)



calmcmc <- MCMCordfactanal(cal2, factors=2, lambda.constraints=list(approve_trump=list(2,"+"),
                                                                    education=list(3,"+")),
                           burnin=25000, mcmc=25000, thin=25, L0=0.5, store.lambda=TRUE, store.scores=TRUE)



means.sds <- summary(calmcmc)[[1]][,1:2]
ideal.points <- means.sds[grepl("phi",rownames(means.sds)),]
irt1.means <- ideal.points[seq(1,nrow(ideal.points), by=2),1]
irt2.means <- ideal.points[seq(2,nrow(ideal.points), by=2),1]

irtdt <- cbind.data.frame(irt1.means,irt2.means,cal$party)
colnames(irtdt) <- c("coord1D", "coord2D", "party")

irtdt_n <- irtdt %>% filter(party == "Democrat" | party == "Republican")
#irtdt_n$party <- ordered(irtdt_n$party, levels = c("Republican", "Democrat"))

fa2.1 <- ggplot(data=irtdt_n, aes(x=-coord1D, y=coord2D, color=party, fill=party)) +
         geom_point() +
         scale_fill_manual(values=c("#FC4E07", "#00AFBB")) + 
         stat_ellipse(geom = "polygon", linetype = 1, alpha=0) +
         labs(title = "Individual Plot - OIRT All Californians", x="Dim1", y="Dim2", color="Party ID", fill="Party ID") +
         theme_minimal()
ggsave(fa2.1, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/fa2.1.eps", device="eps")

########################################
calmcmc.dd <- MCMCordfactanal(cal2, factors=2, lambda.constraints=list(view_democrats=list(2,"+"),
                                                                       education=list(3,"+")),
                              burnin=25000, mcmc=25000, thin=25, L0=0.5, store.lambda=TRUE, store.scores=TRUE)


means.sds.dd <- summary(calmcmc.dd)[[1]][,1:2]
ideal.points.dd <- means.sds.dd[grepl("phi",rownames(means.sds.dd)),]
irt1.means.dd <- ideal.points.dd[seq(1,nrow(ideal.points.dd), by=2),1]
irt2.means.dd <- ideal.points.dd[seq(2,nrow(ideal.points.dd), by=2),1]

irtdt.dd <- cbind.data.frame(irt1.means.dd,irt2.means.dd,cal$party,cal$view_democrats)
colnames(irtdt.dd) <- c("coord1D", "coord2D", "party","view")

irtdt_n.dd <- irtdt.dd %>% filter(party == "Democrat")
#irtdt_n$party <- ordered(irtdt_n$party, levels = c("Republican", "Democrat"))
irtdt_n.dd$view <- factor(irtdt_n.dd$view, labels = c("Agree", "Disagree"))
irtdt_n.dd$view <- factor(irtdt_n.dd$view, levels=c("Disagree","Agree"))

fa2.2 <- ggplot(data=irtdt_n.dd, aes(x=coord1D, y=coord2D, color=view, fill=view)) +
         geom_point() +
         scale_fill_manual(values=c("#FC4E07", "#00AFBB")) + 
         stat_ellipse(geom = "polygon", linetype = 1, alpha=0) +
         labs(title = "OIRT, Democrats Subset (Republicans Included in OIRT)", x="Dim1", y="Dim2", color = "Democratic Party\n Approval", fill = "Democratic Party\n Approval") +
         theme_minimal()
ggsave(fa2.2, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/fa2.2.eps", device="eps")

####################
cal_d <- cal %>% filter(party == "Democrat")

## new data without "party" "strength" and "vote_house"
cal2_d <- cal_d[, !colnames(cal_d) %in% c("party", "strength", "vote_house")]
cal2_d <- as.matrix(cal2_d)

calmcmc.d <- MCMCordfactanal(cal2_d, factors=2, lambda.constraints=list(view_democrats=list(2,"+"),
                                                                        education=list(3,"+")),
                            burnin=25000, mcmc=25000, thin=25, L0=0.5, store.lambda=TRUE, store.scores=TRUE)


means.sds.d <- summary(calmcmc.d)[[1]][,1:2]
ideal.points.d <- means.sds.d[grepl("phi",rownames(means.sds.d)),]
irt1.means.d <- ideal.points.d[seq(1,nrow(ideal.points.d), by=2),1]
irt2.means.d <- ideal.points.d[seq(2,nrow(ideal.points.d), by=2),1]

irtdt.d <- cbind.data.frame(irt1.means.d,irt2.means.d,cal_d$view_democrats)
colnames(irtdt.d) <- c("coord1D", "coord2D", "view")

irtdt_n.d <- irtdt.d 
#irtdt_n$party <- ordered(irtdt_n$party, levels = c("Republican", "Democrat"))
irtdt_n.d$view <- factor(irtdt_n.d$view, labels = c("Agree", "Disagree"))
irtdt_n.d$view <- factor(irtdt_n.d$view, levels=c("Disagree","Agree"))

fa2.3 <- ggplot(data=irtdt_n.d, aes(x=coord1D, y=coord2D, color=view, fill=view)) +
         geom_point() +
         scale_fill_manual(values=c("#FC4E07", "#00AFBB")) + 
         stat_ellipse(geom = "polygon", linetype = 1, alpha=0) +
         labs(title = "OIRT, Democrats Subset (Republicans Excluded in OIRT)", x="Dim1", y="Dim2", color = "Democratic Party\n Approval", fill = "Democratic Party\n Approval") +
         theme_minimal()
ggsave(fa2.3, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/fa2.3.eps", device="eps")

####
########
calmcmc.rr <- MCMCordfactanal(cal2, factors=2, lambda.constraints=list(favor_borderwall=list(2,"+"),
                                                                    education=list(3,"+")),
                              burnin=25000, mcmc=25000, thin=25, L0=0.5, store.lambda=TRUE, store.scores=TRUE)

means.sds.rr <- summary(calmcmc.rr)[[1]][,1:2]
ideal.points.rr <- means.sds.rr[grepl("phi",rownames(means.sds.rr)),]
irt1.means.rr <- ideal.points.rr[seq(1,nrow(ideal.points.rr), by=2),1]
irt2.means.rr <- ideal.points.rr[seq(2,nrow(ideal.points.rr), by=2),1]

irtdt.rr <- cbind.data.frame(irt1.means.rr,irt2.means.rr,cal$party,cal$favor_borderwall)
colnames(irtdt.rr) <- c("coord1D", "coord2D", "party","wall")

irtdt_n.rr <- irtdt.rr %>% filter(party == "Republican")
#irtdt_n$party <- ordered(irtdt_n$party, levels = c("Republican", "Democrat"))
irtdt_n.rr$wall <- factor(irtdt_n.rr$wall, labels = c("Approval", "Disapproval"))
irtdt_n.rr$wall <- factor(irtdt_n.rr$wall, levels=c("Approval","Disapproval"))

fa2.4 <- ggplot(data=irtdt_n.rr, aes(x=-coord1D, y=coord2D, color=wall, fill=wall)) +
         geom_point() +
         scale_fill_manual(values=c("#FC4E07", "#00AFBB")) + 
         stat_ellipse(geom = "polygon", linetype = 1, alpha=0) +
         labs(title = "OIRT, Republicans Subset (Democrats Included in OIRT)", x="Dim1", y="Dim2", color = "Border Wall\n Approval", fill = "Border Wall\n Approval") +
         theme_minimal()
ggsave(fa2.4, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/fa2.4.eps", device="eps")

####################
cal_r <- cal %>% filter(party == "Republican")

## new data without "party" "strength" and "vote_house"
cal2_r <- cal_r[, !colnames(cal_r) %in% c("party", "strength", "vote_house")]
cal2_r <- as.matrix(cal2_r)

calmcmc.r <- MCMCordfactanal(cal2_r, factors=2, lambda.constraints=list(favor_borderwall=list(2,"+"),
                                                                        education=list(3,"+")),
                             burnin=25000, mcmc=25000, thin=25, L0=0.5, store.lambda=TRUE, store.scores=TRUE)


means.sds.r <- summary(calmcmc.r)[[1]][,1:2]
ideal.points.r <- means.sds.r[grepl("phi",rownames(means.sds.r)),]
irt1.means.r <- ideal.points.r[seq(1,nrow(ideal.points.r), by=2),1]
irt2.means.r <- ideal.points.r[seq(2,nrow(ideal.points.r), by=2),1]

irtdt.r <- cbind.data.frame(irt1.means.r,irt2.means.r,cal_r$favor_borderwall)
colnames(irtdt.r) <- c("coord1D", "coord2D", "wall")

irtdt_n.r <- irtdt.r 
#irtdt_n$party <- ordered(irtdt_n$party, levels = c("Republican", "Democrat"))
irtdt_n.r$wall <- factor(irtdt_n.r$wall, labels = c("Approval", "Disapproval"))
irtdt_n.r$wall <- factor(irtdt_n.r$wall, levels=c("Approval", "Disapproval"))

fa2.5 <- ggplot(data=irtdt_n.r, aes(x=-coord1D, y=coord2D, color=wall, fill=wall)) +
         geom_point() +
         scale_fill_manual(values=c("#FC4E07", "#00AFBB")) + 
         stat_ellipse(geom = "polygon", linetype = 1, alpha=0) +
         labs(title = "OIRT, Republicans Subset (Democrats Excluded in OIRT)", x="Dim1", y="Dim2", color = "Border Wall\n Approval", fill = "Border Wall\n Approval") +
         theme_minimal()
ggsave(fa2.5, file="/Users/tpliu/Dropbox/Mac/Desktop/cpca_plot/fa2.5.eps", device="eps")

