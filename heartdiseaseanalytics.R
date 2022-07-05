HT <- read.csv("C:/Users/Jzea/Downloads/heart_2020_cleaned.csv")
hd <- HT %>% group_by(HeartDisease) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
a <- ggplot(hd, aes(x = "", y = perc, fill = HeartDisease))+
  geom_col()+
  geom_label(aes(label = labels), position = position_stack(vjust=0.5),show.legend=FALSE)+
  coord_polar(theta="y")
b <- ggplot(HT, aes(x=BMI))+geom_histogram(fill = "blue", alpha=0.2, binwidth = 3)
sm <-  HT %>% group_by(Smoking) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
c <- ggplot(sm, aes(x = "", y = perc, fill = Smoking))+
  geom_col()+
  geom_label(aes(label = labels), position = position_stack(vjust=0.5),show.legend=FALSE)+
  coord_polar(theta="y")
ad <- HT %>% group_by(AlcoholDrinking) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
d <- ggplot(ad, aes(x = "", y = perc, fill = AlcoholDrinking))+
  geom_col()+
  geom_label(aes(label = labels), position = position_stack(vjust=0.5),show.legend=FALSE)+
  coord_polar(theta="y")
st <- HT %>% group_by(Stroke) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
e <- ggplot(st, aes(x = "", y = perc, fill = Stroke))+
  geom_col()+
  geom_label(aes(label = labels), position = position_stack(vjust=0.5),show.legend=FALSE)+
  coord_polar(theta="y")
f <- ggplot(HT, aes(x=PhysicalHealth))+geom_histogram(fill = "blue", alpha=0.2, binwidth = 1)
g <- ggplot(HT, aes(x=MentalHealth))+geom_histogram(fill = "blue", alpha=0.2, binwidth = 1)
dw <- HT %>% group_by(DiffWalking) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
h <- ggplot(dw, aes(x = "", y = perc, fill = DiffWalking))+
  geom_col()+
  geom_label(aes(label = labels), position = position_stack(vjust=0.5),show.legend=FALSE)+
  coord_polar(theta="y")
sex <- HT %>% group_by(Sex) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
i <- ggplot(sex, aes(x=Sex, y=n, fill = Sex)) + geom_bar(stat="identity")
age <- HT %>% group_by(AgeCategory) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
j <- ggplot(age, aes(x=AgeCategory, y=n, fill = AgeCategory)) + geom_bar(stat="identity")
race <- HT %>% group_by(Race) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
k <- ggplot(race, aes(x=Race, y=n, fill = Race)) + geom_bar(stat="identity")
di <- HT %>% group_by(Diabetic) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
l <- ggplot(di, aes(x=Diabetic, y=n, fill = Diabetic)) + geom_bar(stat="identity")
pa <- HT %>% group_by(PhysicalActivity) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
m <- ggplot(pa, aes(x = "", y = perc, fill = PhysicalActivity))+
  geom_col()+
  geom_label(aes(label = labels), position = position_stack(vjust=0.5),show.legend=FALSE)+
  coord_polar(theta="y")
gh <- HT %>% group_by(GenHealth) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
n <- ggplot(gh, aes(x=GenHealth, y=n, fill = GenHealth)) + geom_bar(stat="identity") + scale_x_discrete(limits=c("Excellent", "Very good", "Good", "Fair", "Poor"))
o <- ggplot(HT, aes(x=SleepTime))+geom_histogram(fill = "blue", alpha=0.2, binwidth = 1)
as <- HT %>% group_by(Asthma) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
p <- ggplot(as, aes(x = "", y = perc, fill = Asthma))+
  geom_col()+
  geom_label(aes(label = labels), position = position_stack(vjust=0.5),show.legend=FALSE)+
  coord_polar(theta="y")
kd <- HT %>% group_by(KidneyDisease) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
q <- ggplot(kd, aes(x = "", y = perc, fill = KidneyDisease))+
  geom_col()+
  geom_label(aes(label = labels), position = position_stack(vjust=0.5),show.legend=FALSE)+
  coord_polar(theta="y")
sc <- HT %>% group_by(SkinCancer) %>% count() %>% ungroup %>% mutate(perc = `n` / sum(`n`)) %>% arrange(perc) %>% mutate(labels = scales::percent(perc))
r <- ggplot(sc, aes(x = "", y = perc, fill = SkinCancer))+
  geom_col()+
  geom_label(aes(label = labels), position = position_stack(vjust=0.5),show.legend=FALSE)+
  coord_polar(theta="y")
grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r, nrow=6, ncol=3)
boxplot(HT$BMI, HT$PhysicalHealth, HT$MentalHealth, HT$SleepTime, names=c("BMI", "PhysicalHealth", "MentalHealth", "SleepTime"), cex.axis = 1.5)
AD1 <- data.frame(HT$BMI, HT$PhysicalHealth, HT$MentalHealth, HT$SleepTime)
names(AD1)=c("BMI", "PhysicalHealth", "MentalHealth", "SleepTime")
pheatmap(cor(AD1), cluster_rows = F, cluster_cols = F, display_numbers = T)
AD2 <- HT[, !colnames(HT) %in% c("HeartDisease", "BMI", "PhysicalHealth", "MentalHealth", "SleepTime")]
ttt <- AD2 %>% mutate(
  Smoking = ifelse(Smoking %in% "Yes", 1 , 0),
  AlcoholDrinking = ifelse(AlcoholDrinking %in% "Yes", 1 , 0),
  Stroke = ifelse(Stroke %in% "Yes", 1 , 0),
  DiffWalking = ifelse(DiffWalking %in% "Yes", 1 , 0),
  Sex = ifelse(Sex %in% "Male", 1 , 0),
  Diabetic = ifelse(Diabetic %in% "Yes", 1 , 0),
  PhysicalActivity = ifelse(PhysicalActivity %in% "Yes", 1 ,0),
  Asthma = ifelse(Asthma %in% "Yes", 1 , 0),
  KidneyDisease = ifelse(KidneyDisease %in% "Yes", 1 , 0),
  SkinCancer = ifelse(SkinCancer %in% "Yes", 1 , 0),
  AgeCategory = ifelse(AgeCategory %in% "18-24", 1,
                       ifelse(AgeCategory %in% "25-29", 2,
                              ifelse(AgeCategory %in% "30-34", 3,
                                     ifelse(AgeCategory %in% "35-39", 4,
                                            ifelse(AgeCategory %in% "40-44", 5,
                                                   ifelse(AgeCategory %in% "45-49", 6,
                                                          ifelse(AgeCategory %in% "50-54", 7,
                                                                 ifelse(AgeCategory %in% "55-59", 8,
                                                                        ifelse(AgeCategory %in% "60-64", 9,
                                                                               ifelse(AgeCategory %in% "65-69", 10,
                                                                                      ifelse(AgeCategory %in% "70-74", 11,
                                                                                             ifelse(AgeCategory %in% "75-80", 12, 13)))))))))))),
  Race = ifelse(Race %in% "White", 1,
                ifelse(Race %in% "Other", 2,
                       ifelse(Race %in% "Hispanic", 3,
                              ifelse(Race %in% "Black", 4,
                                     ifelse(Race %in% "Asian", 5, 6))))),
  GenHealth = ifelse(GenHealth %in% "Excellent", 5,
                     ifelse(GenHealth %in% "Very Good", 4,
                            ifelse(GenHealth %in% "Good", 3,
                                   ifelse(GenHealth %in% "Fair", 2, 1))))
)
pheatmap(cor(ttt), cluster_rows = F, cluster_cols = F, display_numbers = T)

HT_bi <- HT %>% mutate(HeartDisease = ifelse(HeartDisease %in% "Yes", 1, 0))
glm_HT <- glm(HeartDisease~., data = HT_bi, family = binomial)
summary(glm_HT)$coefficients
glm_HT2 <- step(glm_HT, trace=F)
summary(glm_HT2)
round(exp(glm_HT2$coefficients), 2)
maindata <- HT %>% mutate(
  HeartDisease = ifelse(HeartDisease %in% "Yes", 1, 0),
  Smoking = ifelse(Smoking %in% "Yes", 1 , 0),
  AlcoholDrinking = ifelse(AlcoholDrinking %in% "Yes", 1 , 0),
  Stroke = ifelse(Stroke %in% "Yes", 1 , 0),
  DiffWalking = ifelse(DiffWalking %in% "Yes", 1 , 0),
  Sex = ifelse(Sex %in% "Male", 1 , 0),
  Diabetic = ifelse(Diabetic %in% "Yes", 1 , 0),
  PhysicalActivity = ifelse(PhysicalActivity %in% "Yes", 1 ,0),
  Asthma = ifelse(Asthma %in% "Yes", 1 , 0),
  KidneyDisease = ifelse(KidneyDisease %in% "Yes", 1 , 0),
  SkinCancer = ifelse(SkinCancer %in% "Yes", 1 , 0),
  AgeCategory = ifelse(AgeCategory %in% "18-24", 1,
                       ifelse(AgeCategory %in% "25-29", 2,
                              ifelse(AgeCategory %in% "30-34", 3,
                                     ifelse(AgeCategory %in% "35-39", 4,
                                            ifelse(AgeCategory %in% "40-44", 5,
                                                   ifelse(AgeCategory %in% "45-49", 6,
                                                          ifelse(AgeCategory %in% "50-54", 7,
                                                                 ifelse(AgeCategory %in% "55-59", 8,
                                                                        ifelse(AgeCategory %in% "60-64", 9,
                                                                               ifelse(AgeCategory %in% "65-69", 10,
                                                                                      ifelse(AgeCategory %in% "70-74", 11,
                                                                                             ifelse(AgeCategory %in% "75-80", 12, 13)))))))))))),
  Race = ifelse(Race %in% "White", 1,
                ifelse(Race %in% "Other", 2,
                       ifelse(Race %in% "Hispanic", 3,
                              ifelse(Race %in% "Black", 4,
                                     ifelse(Race %in% "Asian", 5, 6))))),
  GenHealth = ifelse(GenHealth %in% "Excellent", 5,
                     ifelse(GenHealth %in% "Very Good", 4,
                            ifelse(GenHealth %in% "Good", 3,
                                   ifelse(GenHealth %in% "Fair", 2, 1))))
)
train <- sample(nrow(maindata), 240000)
modelWeights <- ifelse(maindata$HeartDisease == 1, 10, 1)
tree <- rpart(HeartDisease~., data = maindata, subset = train, weights = modelWeights, method = "class")
rpart.plot(tree)
printcp(tree)
plotcp(tree)
ptree <- prune(tree, cp=0.013)
rpart.plot(ptree, main = "Pruned Regression Tree", nn=TRUE)
predict <- predict(ptree, maindata[-train,], type="prob")
table <- table(y=HT$HeartDisease[-train], predict[,2]>0.5)
PR <- prediction(predict[,2], maindata$HeartDisease[-train])
perf <- performance(PR, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC of tree model")
auc=as.numeric(performance(PR, "auc")@y.values)

gbm <- gbm(HeartDisease~., data = maindata[train,], 
           distribution = "bernoulli",
           n.trees = 300,
           shrinkage = 0.1,
           interaction.depth = 3,
           bag.fraction = 0.5, train.fraction = 0.5,
           n.minobsinnode = 10,
           cv.folds = 5, keep.data = TRUE,
           verbose = FALSE, n.cores = 1)
best.iter = gbm.perf(gbm, method="cv")
min_MSE <- which.min(gbm$cv.error) # min_MSE는 286트리
gbm1 <- gbm(HeartDisease~., data = maindata[train,], 
            distribution = "bernoulli",
            n.trees = 286,
            shrinkage = 0.1,
            interaction.depth = 3,
            bag.fraction = 0.5, train.fraction = 0.5,
            n.minobsinnode = 10,
            cv.folds = 5, keep.data = TRUE,
            verbose = FALSE, n.cores = 1)
summary(gbm1, las = 2)
gbm_pred <- predict(gbm1, newdata = maindata[-train,], type = "response")
gbm_table <- table(prediction=gbm_pred>0.5, actural=maindata$HeartDisease[-train])
p_gbm <- prediction(gbm_pred, maindata$HeartDisease[-train])
roc_gbm <- performance(p_gbm, measure = "tpr", x.measure = "fpr")
auc_gbm <- round(performance(p_gbm, measure = "auc")@y.values[[1]], 4)
plot(roc_gbm, main = paste0("ROC of gbm (AUC=", auc_gbm, ")"))