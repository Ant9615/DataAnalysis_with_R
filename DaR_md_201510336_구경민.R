# 1. 데이터 파악
# 라이브러리 로드
library(MASS)
library(dplyr)
library(caret)
library(ggplot2)

# 데이터 로드
crime <- as.data.frame(MASS::UScrime)

# View(crime)
str(crime)

# 2. EDA 
# 상관관계 분석
# install.packages("corrplot")
library(corrplot)
crime_cor <- cor(crime)
crime_cor
corrplot(crime_cor)

# 3. Classification
# 3.1.1 로지스틱회귀를 이용한 소득불평등, 평균교육, 비백인수에 대한 남부주 분류분석

# 데이터 복사
so_crime <- crime

# so를 factor형으로 변환
so_crime$So <-as.factor(so_crime$So)
head(so_crime)
str(so_crime)

# 관련데이터만 추출
logi_subset <- so_crime  %>% 
  select(So, Ed, NW, Ineq)

# partition to train and test
set.seed(71)
logi_train <- sample_frac(logi_subset, size = 0.7)
logi_test <- logi_subset[setdiff(x=1:nrow(logi_subset), y=logi_train),]
print(head(logi_train))
print(head(logi_test))

# Ineq
so_logistic_Ineq <- glm(So~Ineq, data=logi_train, family = binomial)
summary(so_logistic_Ineq)

# 오즈비
print(exp(so_logistic_Ineq$coefficients))
# 오즈비의 95% 신뢰구간
print(exp(confint(so_logistic_Ineq)))
# 분산분석(카이제곱)
anova(so_logistic_Ineq, test = "Chisq")

library(pROC)
Pro_Ineq = predict(so_logistic_Ineq, newdata = logi_test, type = 'response')
ROC = roc(logi_subset$So,Pro_Ineq)

plot.roc(ROC,   
         col="royalblue",  
         print.auc=TRUE, 
         max.auc.polygon=TRUE,   
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")


# Ed
so_logistic_Ed <- glm(So~Ed, data=logi_train, family = binomial)
summary(so_logistic_Ed)
# 오즈비
print(exp(so_logistic_Ed$coefficients))
# 오즈비의 95% 신뢰구간
print(exp(confint(so_logistic_Ed)))
# 분산분석(카이제곱)
anova(so_logistic_Ed, test = "Chisq")
Pro_Ed = predict(so_logistic_Ed, newdata = logi_test, type = 'response')
ROC = roc(logi_subset$So,Pro_Ed)

plot.roc(ROC,   
         col="royalblue",  
         print.auc=TRUE, 
         max.auc.polygon=TRUE,   
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")
#NW
so_logistic_NW <- glm(So~NW, data=logi_train, family = binomial)
summary(so_logistic_NW)
# 오즈비
print(exp(so_logistic_NW$coefficients))
# 오즈비의 95% 신뢰구간
print(exp(confint(so_logistic_NW)))
# 분산분석(카이제곱)
anova(so_logistic_NW, test = "Chisq")
Pro_NW = predict(so_logistic_NW, newdata = logi_test, type = 'response')
ROC = roc(logi_subset$So,Pro_NW)

plot.roc(ROC,   
         col="royalblue",  
         print.auc=TRUE, 
         max.auc.polygon=TRUE,   
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")

# multi
so_logistic_multi <- glm(So~Ineq+Ed+NW, data=logi_train, family=binomial)
summary(so_logistic_multi)
# 오즈비
print(exp(so_logistic_multi$coefficients))
# 오즈비의 95% 신뢰구간
print(exp(confint(so_logistic_multi)))
# 분산분석(카이제곱)
anova(so_logistic_multi, test = "Chisq")
Pro_multi = predict(so_logistic_multi, newdata = logi_test, type = 'response')
ROC = roc(logi_subset$So,Pro_multi)

plot.roc(ROC,   
         col="royalblue",  
         print.auc=TRUE, 
         max.auc.polygon=TRUE,   
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB")

# 3.1.2 단순베이즈분류를 이용한 남부주 분류
library(e1071)

str(so_crime)

set.seed(71)
ba_train <- sample_frac(so_crime, size = 0.7)
ba_test <- logi_subset[setdiff(x=1:nrow(so_crime), y=ba_train),]
print(head(ba_train))
print(head(ba_test))

so_ba <- naiveBayes(So~., data=ba_train)
summary(so_ba)
so_ba_pre <- predict(so_ba, newdata = ba_test) # 예측값
so_ba_pre_ta <- table(so_ba_pre, so_crime$So) # 정오분류표
so_ba_pre_ta
table(so_crime$So)

# 3.2 구속률 수준 분류
#3.2.1 의사결정나무를 이용한 구속률 수준 분류
# 데이터 복사
des_c <- crime
summary(des_c$Prob)

des_c <- des_c  %>% 
  mutate(Prob_levels = ifelse(Prob< 0.03270,'low', #1Q 미만까지 LOW
                              ifelse(Prob<0.04709, 'middle', 'high'))) # 평균 미만까지 중간 그 외 높음
des_c$Prob_levels <- as.factor(des_c$Prob_levels)

des_c <- subset(des_c, select = -c(Prob,So))

# partition to train and test
set.seed(71) # 랜덤으로 추출하기 때문에 seed값을 지정해준다.
des_train <- sample_frac(des_c, size = 0.7)
des_test <- des_c[setdiff(x=1:nrow(des_c), y=des_train),]
print(head(des_train))
print(head(des_test))
# install.packages("rpart")
library(rpart)

dt_fit <- rpart(Prob_levels~., data=des_train)
dt_fit

library(rpart.plot); par(mfrow=c(1,2))
rpart.plot(dt_fit) ; plot(dt_fit) ; text(dt_fit)

# 예측력 확인
dt_pre <- predict(dt_fit, newdata = des_test, type="class")
dt_test <- sum(dt_pre==des_test$Prob_levels)/nrow(des_test)*100
print("validation model prediction")
print(dt_test)

# 과적합 확인
dt_ov <- predict(dt_fit, newdata = des_train, type='class')
dt_ov_val <- sum(dt_pre==des_test$Prob_levels)/nrow(des_test)*100
print("validation model overfitting")
print(dt_ov_val)

# 복잡성 확인
print("cptable")
print(dt_fit$cptable)
plotcp(dt_fit) 

# 모델 정보
print(dt_fit$control)