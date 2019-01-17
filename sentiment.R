#### ._DEFAULT SCRIPT ####

rm(list = ls())

Sys.setlocale(category = "LC_ALL", locale = "english")
options(scipen=999)

#### ._LIBRARIES####

pacman::p_load(plyr, purrr, BiocManager, ggplot2, rattle, RGtk2, corrplot, 
               plyr, psych, DataExplorer, caret, arules, randomForest, C50,dplyr)


setwd("C:/Users/pilar/Google Drive/A_DATA/UBIQUM/TASK4.1/Sentiment Analysis")

#iphone#
# iphonematrix_p<-read.csv(file = "iPhoneLargeMatrix.csv", header = TRUE, sep =",")
# iphonematrix_d<-read.csv(file= "iPhoneLargeMatrixDAVID.csv", header = TRUE, sep =",")
# iphonematrix_a<-read.csv(file= "iPhoneLargeMatrix_Ana.csv", header = TRUE, sep =",")
# iphonematrix_j<-read.csv(file= "iPhoneLargeMatrix_joe.csv", header = TRUE, sep =",")
# iphonematrix_t<-read.csv(file= "iPhoneTeo.csv", header = TRUE, sep =",")


#galaxy#
# galaxymatrix_p<-read.csv(file = "GalaxyLargeMatrix.csv", header = TRUE, sep =",")
# galaxymatrix_d<-read.csv(file= "GalaxyLargeMatrixDAVID.csv", header = TRUE, sep =",")
# galaxymatrix_a<-read.csv(file= "GalaxyLargeMatrix_Ana.csv", header = TRUE, sep =",")
# galaxymatrix_j<-read.csv(file= "GalaxyLargeMatrix _joe.csv", header = TRUE, sep =",")
# galaxymatrix_t<-read.csv(file= "GalaxyTeo.csv", header = TRUE, sep =",")



#newfile

#iphonematrix_all<-rbind(iphonematrix_p, iphonematrix_a, iphonematrix_d, iphonematrix_j, galaxymatrix_t)

#save(iphonematrix_all, file = "iphonematrix_all.Rdata")
#load(file = "iphonematrix_all.Rdata")



#galaxymatrix_all<-rbind(galaxymatrix_a, galaxymatrix_d, galaxymatrix_j, galaxymatrix_p, galaxymatrix_t)
load(file = "galaxymatrix_all.Rdata")

#ip_mat<-as.matrix(iphonematrix_all)
#heatmap(ip_mat, scale = "column")

head(iphonematrix_all)
sum(iphonematrix_all$iphoneSentiment)
count(iphonematrix_all$iphonecampos)
str(iphonematrix_all)
summary(iphonematrix_all)

anyNA(iphonematrix_all)

#### Multiple histograms ####

# iphonematrix_all[,1:10] %>%
#   keep(is.numeric) %>% 
#   gather() %>% 
#   ggplot(aes(value)) +
#   facet_wrap(~ key, scales = "free") +
#   geom_histogram()


#par(mfrow=c(3, 3))
# colnames <- dimnames(iphonematrix_all)[[2]]
# for (i in 2:60) {
#   d <- density(iphonematrix_all[,i])
#   plot(d, type="n", main=colnames[i])
#   polygon(d, col="red", border="gray")
# }

#rattle("iphonematrix_all.Rdata")

#plot_histogram(iphonematrix_all) ##distribution of each continuous variable###

# par(mfrow=c(1, 1))
# hist(iphonematrix_all$iphoneSentiment,freq=FALSE)
# iphonematrix_all <- data.frame(lapply(iphonematrix_all, 
#                                       function(x) as.numeric(x)))
# galaxymatrix_all <- data.frame(lapply(galaxymatrix_all, 
#                                       function(x) as.numeric(x)))
# 
# hist(galaxymatrix_all$galaxySentiment,freq=FALSE)
# str(iphonematrix_all$iphoneSentiment)
# summary(iphonematrix_all$iphoneSentiment)
# 
# plot.new()
# dev.off()


####REMOVING DUPLICATES####

#s<-distinct(galaxymatrix_all[, !names(galaxymatrix_all) %in%"id"])

wodup<-galaxymatrix_all%>%select( -id)%>%distinct()


####CORRELATION MATRIX####

# corrplot(cor(iphonematrix_all), order ="hclust")
# descrCor <- cor(iphonematrix_all)
# summary(descrCor[upper.tri(descrCor)])
# highlyCorDescr <- findCorrelation(descrCor, cutoff = .80) 
# 
# summary(highlyCorDescr) 
# 
# newiphone <- iphonematrix_all[, -highlyCorDescr]
# corrplot(cor(newiphone), order ="hclust")
# 
# 
# plot.new()
# dev.off()

corrplot(cor(galaxymatrix_all), order ="hclust")
descrCor <- cor(galaxymatrix_all)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .80) 


summary(highlyCorDescr) 
newgalaxy<- galaxymatrix_all[, -highlyCorDescr]

corrplot(cor(newgalaxy), order ="hclust")

#withoutduplicates
corrplot(cor(wodup), order ="hclust")
descrCor <- cor(wodup)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .80) 


summary(highlyCorDescr) 
newwodup<- wodup[, -highlyCorDescr]

corrplot(cor(newwodup), order ="hclust")

#### OPTIMIZATION ####
#as we are working with samsung products will remove all variables for samsung#


samsung<-grep("sam", names(newgalaxy), value = TRUE)
nodupsamsung<-grep("samsung", names(newwodup), value = TRUE)

nosam<-setdiff(galaxynames, samsung)
nosapdup<-setdiff(nodupnames, nodupsamsung)

newgalaxy<-newgalaxy[nosam]
newwodup<-newwodup[nosapdup]

corrplot(cor(newgalaxy), order ="hclust")
corrplot(cor(newwodup), order ="hclust")

#### SELECTING FEATURES####

# will keep the variables more correlated#

galcor<-cor(x =  newgalaxy, y =newgalaxy$galaxySentiment)
dupcor<-cor(x =  newwodup, y =newwodup$galaxySentiment)

galcordf<- as.data.frame(galcor)
dupcordf<- as.data.frame(dupcor)

namescor<- rownames(galcordf)
namesdup<-rownames(dupcordf)

galcordf<-abs(galcordf)
dupcordf<-abs(dupcordf)

galcordf$namescor<-namescor
dupcordf$namesdup<-namesdup


badcorr<-galcordf%>%dplyr::filter(galcordf$V1<.5 )
baddupcorr<-dupcordf%>%dplyr::filter(dupcordf$V1<.5 )

goodcor<-setdiff(names(newgalaxy), badcorr$namescor)
gooddupcor<-setdiff(names(newwodup), baddupcorr$namesdup)


corrplot(cor(newgalaxy[goodcor]), order ="hclust")
corrplot(cor(newwodup[gooddupcor]), order ="hclust")



#### MODELING PARTITION####

set.seed(123)

partition<-createDataPartition(newgalaxy$galaxySentiment,times = 1, p = 0.7, list = FALSE)
partitiondup<-createDataPartition(newwodup$galaxySentiment,times = 1, p = 0.7, list = FALSE)

gala_train<-newgalaxy[partition,]
gala_test<-newgalaxy[-partition,]
dup_train<-newgalaxy[partitiondup,]
dup_test<-newgalaxy[-partitiondup,]

###SVM###


# svm_3_nd<-train(galaxySentiment~.,
#              data=gala_train[goodcor],
#              method= "svmLinear")


#saveRDS(object = svm_3_nd, file="svm_3_nd.rds")
svm_3<-readRDS("svm_3.rds") #this models is discretizing before running the model and in this case the kappa is 0#
svm_3_nd<-readRDS("svm_3_nd.rds") #without discretizig

svmpredict<-predict(svm_3_nd, gala_test)

postResample(svmpredict,newgalaxy$galaxySentiment)

range(newgalaxy$galaxySentiment)
range(gala_test$galaxySentiment)

####KNN#### FAILED

# knn_3_nd<-train(galaxySentiment~., ###error#
#                 data=gala_train[goodcor],
#                 method= "knn")
# saveRDS(object = knn_3_nd, file = "knn_3_nd")

####RANDOM FOREST####

# rf_3_nd<-train(galaxySentiment~.,
#                data=gala_train[goodcor],
#                method= "rf", ntree = 100)

#saveRDS(rf_3_nd, file= "rf_3_nd.rds")
rf_3_nd<-readRDS(file= "rf_3_nd.rds")

rfpredict<-predict(rf_3_nd, gala_test)

postResample(rfpredict,newgalaxy$galaxySentiment)

# bestmtry = tuneRF(x=gala_train[goodcor],
#                   y=gala_train$galaxySentiment, ntreeTry = 100, plot = F)
# 
# rf_3_nd2<- randomForest(y=gala_train$galaxySentiment, 
#                         x=gala_train[goodcor], ntree = 100, mtry=3)

#saveRDS(rf_3_nd2, file= "rf_3_nd2.rds")
rf_3_nd2<-readRDS(file= "rf_3_nd2.rds")

rfpredict2<-predict(rf_3_nd2, gala_test)

postResample(rfpredict2,newgalaxy$galaxySentiment)


####C.50#### NOT VALID FOR REGRESSION

# C50<-train(galaxySentiment~.,
#                 data=gala_train[goodcor],
#                 method= "C5.0")

#####LINEAR REGRESION####

lm<-train(galaxySentiment~.,
                data=gala_train[goodcor],
                method= "lm")

lmdup<-train(galaxySentiment~.,
          data=dup_train[gooddupcor],
          method= "lm")

lmpredict<-predict(lm, gala_test)
lmdupredict<-predict(lmdup, dup_test)

postResample(lmpredict,newgalaxy$galaxySentiment)

####NEW SENTIMENT LABELS ####

# disfixed7_iphone <- discretize(newiphone$iphoneSentiment, "fixed",
#                                breaks= c(-Inf, -50, -10, -1, 1, 10, 50, Inf),
#                                labels = c("very_negative", "negative", "somewhat_negative",
#                                           "neutral", "somewhat_positive", "positive", "very_positive"))
# 
# summary(disfixed7_iphone)



# disfixed7_galaxy <- discretize(newgalaxy$galaxySentiment, "fixed", 
#                                breaks =c(-Inf, -50, -10, -1, 1, 10, 50, Inf),
#                                labels = c("very_negative", "negative", "somewhat_negative", 
#                                           "neutral", "somewhat_positive", "positive", "very_positive"))
# summary(disfixed7_galaxy)
# 
# str(disfixed7_galaxy)

#newiphone$iphoneSentiment<-disfixed7_iphone
#newgalaxy$galaxySentiment<-disfixed7_galaxy


#discretizing validation sentiment variable###
disfixed7_gala_test <- discretize(gala_test$galaxySentiment, "fixed",
                               breaks =c(-Inf, -50, -10, -1, 1, 10, 50, Inf),
                               labels = c("very_negative", "negative", "somewhat_negative",
                                          "neutral", "somewhat_positive", "positive", "very_positive"))
summary(disfixed7_gala_test)

gala_test$galaxySentiment<-disfixed7_gala_test #replacing for discretize data#

disfixed7_lmpredict <- discretize(lmpredict, "fixed",
                                  breaks =c(-Inf, -50, -10, -1, 1, 10, 50, Inf),
                                  labels = c("very_negative", "negative", "somewhat_negative",
                                             "neutral", "somewhat_positive", "positive", "very_positive"))

summary(disfixed7_lmpredict)

#dup#

disfixed7_dup_test <- discretize(dup_test$galaxySentiment, "fixed",
                                  breaks =c(-Inf, -50, -10, -1, 1, 10, 50, Inf),
                                  labels = c("very_negative", "negative", "somewhat_negative",
                                             "neutral", "somewhat_positive", "positive", "very_positive"))
summary(disfixed7_dup_test)

dup_test$galaxySentiment<-disfixed7_dup_test #replacing for discretize data#

disfixed7_lmdupredict <- discretize(lmdupredict, "fixed",
                                  breaks =c(-Inf, -50, -10, -1, 1, 10, 50, Inf),
                                  labels = c("very_negative", "negative", "somewhat_negative",
                                             "neutral", "somewhat_positive", "positive", "very_positive"))

summary(disfixed7_lmdupredict)
summary(dup_test$galaxySentiment)

confusionMatrix(disfixed7_lmpredict, gala_test$galaxySentiment) #any improvement#
confusionMatrix(disfixed7_lmdupredict, dup_test$galaxySentiment) #any improvement#


####NEXT STEPS####

## 1) discretizar despues de modelar#
# 3) Imbalanced Classification Problems in R, dowmsampling, upsampling
#2)eliminar duplicados sin tener en cuenta el ID#



