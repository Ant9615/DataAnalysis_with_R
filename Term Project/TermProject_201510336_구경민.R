library(dplyr)
library(caret)
library(ggplot2)

library(readr)
flare69 <- read_table2("C:/Users/user/Desktop/비대면/데분알/MiddleTest/DataAnalysis_with_R/Term Project/flare.data1",
                       col_names = FALSE)

names(flare69) <- c('Class','LSP','SD','Activity',
                    'Evoluation','Activity24','HC',
                    'RHC','Area','ALS', 'C_class', 'M_class', 'X_class')
head(flare69)

print("flare69 summary")
summary(flare69)
print("flare69 str")
str(flare69)

# as factor
flare69$Class <- as.factor(flare69$Class)
flare69$LSP <- as.factor(flare69$LSP)
flare69$SD <- as.factor(flare69$SD)

str(flare69)
str(flare78)

# 결측치 빈도확인
table(is.na(flare69))
table(is.na(flare78))

# 이상치 확인을 위한 boxplot 
boxplot(flare69$Activity, flare69$Evoluation, flare69$Activity24, 
        flare69$HC, flare69$RHC, flare69$Area, flare69$ALS, flare69$C_class, 
        flare69$M_class, flare69$X_class,
        main='Flare69 boxplot')

# correlation_preprocessing
flare69_1 <- flare69
flare69_1 <- within(flare69_1, {
  Class = ifelse(flare69_1$Class == 'A', 0, 
                 ifelse(flare69_1$Class == 'B', 1,
                        ifelse(flare69_1$Class == 'C', 2,
                               ifelse(flare69_1$Class == 'D', 3,
                                      ifelse(flare69_1$Class == 'E', 4,
                                             ifelse(flare69_1$Class == 'F',5, 6
                                             ))))))
  LSP = ifelse(flare69_1$LSP == 'X', 0,
               ifelse(flare69_1$LSP == 'R',1,
                      ifelse(flare69_1$LSP == 'S', 2,
                             ifelse(flare69_1$LSP == 'A', 3,
                                    ifelse(flare69_1$LSP=='H',4, 5)))))
  SD = ifelse(flare69_1$LSP == 'X', 0,
              ifelse(flare69_1$LSP == 'O', 1,
                     ifelse(flare69_1$LSP == 'I', 2, 3)))
})

str(flare69_1)

# correlation
library(corrplot)
flare69_cor <- cor(flare69_1)

corrplot(flare69_cor)

# correlation test (LSD and SD, HC, ALS)
cor.test(flare69_1$LSP, flare69_1$SD)
cor.test(flare69_1$LSP, flare69_1$HC)
cor.test(flare69_1$LSP, flare69_1$ALS)

# separates data for spot size classification 
# sc = Spot size Classification
sc_data <- flare69  %>% 
  select(LSP, SD, HC, ALS)
set.seed(71)
# train data
sc_train <- sample_frac(sc_data, size=0.7) 
# test data
sc_test <- flare69[setdiff(x=1:nrow(sc_data), y=sc_train),]
head(sc_train)

library(rpart)

# fitting
sct_fit <- rpart(LSP~., data=sc_train)
# plotting
library(rpart.plot) ; par(mfrow = c(1,2))
rpart.plot(sct_fit) ; plot(sct_fit) ; text(sct_fit)

# testing
sct_pre <- predict(sct_fit, newdata =  sc_test, type='class')
sct_pre_testing <- sum(sct_pre==sc_test$LSP)/nrow(sc_test)*100
print("Validation model prediction")
sct_pre_testing

# 복잡성 확인
print("cptable")
print(sct_fit$cptable)

# 교차검증
print('Cross-validation')
printcp(sct_fit)
print(plotcp(sct_fit))

# 모델정보
print("Model information")
print(sct_fit$control)

library(e1071)

# 나이브베이지안 피팅
scb_fit <- naiveBayes(LSP~., data=sc_train)
# 피팅 결과
summary(scb_fit)

# prediction
scb_pre <- predict(scb_fit, newdata = sc_test)
scb_pre_testing <- sum(scb_pre==sc_test$LSP)/nrow(sc_test)*100
scb_pre_testing

# 정오분류표
scb_pre_table <- table(scb_pre, sc_data$LSP)
scb_pre_table
table(sc_data$LSP)

#정분류율
print("정분류율")
sum(diag(scb_pre_table))/sum(scb_pre_table)