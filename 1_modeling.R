#' ---
#' title: "Feature Selection and Modeling"
#' author: "Tzu"
#' date: "2/7/2021"
#' output:
#'   html_document:
#'     df_print: paged
#'     number_sections: yes
#'     toc: yes
#'     toc_depth: 4
#'     toc_float: yes
#'   word_document: default
#'   pdf_document:
#'     toc: yes
#'     toc_depth: '4'
#' ---
#' 
#' # Setup
#' Library
## ---- warning = FALSE, message = FALSE--------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(feather)
library(gridExtra)
library(GGally) 
library(DescTools) # complete decimal

#' 
#' Data
## ---------------------------------------------------------------------------------------------------------------------
raw <- read.csv('data_1216.csv', header = TRUE, encoding = 'UTF-8') 
# colnames(raw)
raw.kp <- raw[, c(1, 3:8, 10, 12, 20:28, 31, 32)]
names(raw.kp)[names(raw.kp) == "TravelDist3"] <- "TravelDist"
colnames(raw.kp)

#' 
#' # Data Exploration
#' ## TMed (Purchasing Cycle) vs Numerical variables
#' 
## ---- echo = FALSE, message=FALSE-------------------------------------------------------------------------------------
#pairs(tmedf2[c(4, 2:3, 5:8)]) # percentage
p1 = raw %>% 
  filter(., Frequency > 1) %>% 
  ggpairs(c(4, 3, 5:8)) +
  theme(axis.text.x = element_text(angle = 90))
p1

#' 
## ---- message = FALSE-------------------------------------------------------------------------------------------------
p2 = raw %>% 
  filter(., Frequency > 1) %>% 
  ggpairs(c(4,  9:12)) 
p2

#' 
#' (1) Since `TMed` is calcualted by `Frequency`, and `RTRatio` is calculated by `TMed` so we would drop these two variables.
#' 
#' (3) The distribution of `TMed` is apparently right skewed, which means we can do log transformation.
#' 
#' ## TMed
## ---------------------------------------------------------------------------------------------------------------------
p4.1 = raw %>% 
  filter(., Frequency > 1) %>% 
  ggplot(aes(TMed)) +
  geom_histogram(bins = 50)

p4.2 = raw %>% 
  filter(., Frequency > 1) %>% 
  ggplot(aes(x = factor(1), TMed)) +
  geom_boxplot()

grid.arrange(p4.1, p4.2)

#' 
#' # Model Building
#' ## Multiple Linear Regression (MLR)
#' - Goal：predict purchasing cycle of customers `TMed`
#' 
#' - Data：the customer who make more than one purchase (old customer)
#' 
## ---------------------------------------------------------------------------------------------------------------------
res.all = raw.kp
res.allf2 = filter(res.all, Frequency > 1)
colnames(res.allf2)

#' 
#' ### Model1
#' (1) R squared = 0.2863，Residual standard error = 112.8, model doesn't fit well.
## ---------------------------------------------------------------------------------------------------------------------
mlr.1 = lm(TMed ~ ., data = res.allf2[, c(2, 3, 6:19)])
summary(mlr.1)

#' 
#' (2) Checking Model Assumptions
#' According to the QQ-Plot, the model violates the assumption of normality.
#' 
#' (3) Outliers
#' The cood distance of data point is almost smaller than 0.5, so we're going keep all data.
#' 
## ---------------------------------------------------------------------------------------------------------------------
plot(mlr.1)

#' 
#' ### Model2
#' Recall that TMed's distriution is skewed to right so that it would be better to take log.
#' But before that, we would like to run a suitable Box-Cox transformation of the dependent variable:
#' 
#' (1) Box-Cox Transformation
## ---------------------------------------------------------------------------------------------------------------------
library(lindia)
fit1 = lm(TMed ~., data = res.allf2[, c(2, 3, 6:19)])
gg_boxcox(fit1, showlambda = TRUE, lambdaSF = 3, scale.factor = 0.5)

#' 
#' According to the result of box-cox transformation, the maximum likelihood is when lambda equals 0.22. Since, 0.22 is similar to 0, so we're going to take log of `TMed`.
#' 
#' (2) R-squared = 0.3721、Residual standard error = 1.432, the model interpretability is low.
## ---------------------------------------------------------------------------------------------------------------------
mlr.2 = lm(tmedlog ~ ., data = res.allf2[, c(2, 6:20)])
summary(mlr.2)

#' 
#' (3) Checking Model Assumption
#' The model doesn't violate the assumptions.
## ---------------------------------------------------------------------------------------------------------------------
plot(mlr.2)

#' 
#' QQ plot looks better!
#' 
#' ### Model3 
#' We're going to add interaction terms in model3.
#' 
## ---------------------------------------------------------------------------------------------------------------------
mlr.3 <- lm(tmedlog ~ . + Monetary:DailyNote
            + Monetary:TravelDist
            + DailyQpon:Device 
            + DailyQpon:BnbType 
            + DailyQpon:Season 
            + NightType:TravelDist
            + ChildType:Season 
            + GuestNum:OrderPre
            + Device:DoW
            + OrderPre:Season
            + OrderPre:TravelDist
            + Season:TravelDist
            + Monetary:AvgPmt 
            + Monetary:GuestNum 
            + Monetary:Area 
            + Monetary:Season 
            + AvgPmt:DailyQpon
            + AvgPmt:Season 
            + AvgPmt:Area 
            + DailyQpon:DailyNote 
            + DailyQpon:TravelDist 
            + DailyNote:GuestNum
            + DailyNote:Device 
            + DailyNote:Season 
            + DailyNote:Area
            + GuestNum:TravelDist
            + Device:OrderPre 
            + DoW:Season 
            + Area:TravelDist, data = res.allf2[, c(2, 6:20)])

summary(mlr.3)

#' 
#' (1) Model assumptions are met
## ---------------------------------------------------------------------------------------------------------------------
plot(mlr.3)

#' 
#' (2) Adjusted R-squared:  0.4133 、Residual standard error = 1.383
#' 
#' According to the results, we decide model3 as our final model for predicting customer purchasing cycle (`TMed`) 
#' 
#' ## GLM: Binomial Logistic Regression
#' - Goal：predict the probability of new customer being alive
#' 
#' - Data：the customer who make more than one purchase (old customer)
#' 
#' - Definition:
#' 
#'   - Online room booking platform is a case of non-contractual setting for customer analysis
#'   
#'   - `Status`: We are going to build binomial logistic regression model to predict `Status`, which means the customer is alive or not. If `Status` equals 1 then is alive, otherwise dead (drop out the platform). And we defined the `Status` variable according to the RT-Ratio.
#' 
#' - `RT-Ratio` = Recency / TMed 
#'   - We assumed that each customer has their purchasing cycle, so we used RT-Ratio as measurement. When the value closes to 1, the retention rate would be higher; otherwise, when the value is too large, it means the customer may leave the platform.
#'   
## ---------------------------------------------------------------------------------------------------------------------
hist(res.allf2$RTRatio, freq = TRUE, breaks = seq(0, 1200, 1), labels = TRUE,
     xlim = c(0, 20), ylim = c(0, 12000),
     ylab = "#Members", xlab = "Recency-Purchasing Period Ratio",
     main = 'Histogram of R-T Ratio')
abline(v=8,col="red")

#'   
#'   - We are going to calculated the proportion of neighboring counts of RT-Ratio. When the proportion > 90%, we assumed the customer will jump to the higher RT-Ratio (jump to the right of graph).
#'   
#'   - We defined when RT-Ratio equals 8, the probability of the customer jump to the right is 90% (221/247), which means there is little chance they will come back. Therefore, when RT-Ratio is larger than 8, the customer is dead, otherwise alive.
#' 
#' Data
## ---------------------------------------------------------------------------------------------------------------------
res.allf2$Status <- 1
# [RTRatio = 8] / [RTRatio = 7] > 90%
res.allf2[res.allf2$RTRatio > 8, ]$Status <- 0
colnames(res.allf2)

#' 
#' ### Model1
#' Y：Status
#' 
#' X：Monetary、DailyQpon、DailyNote、ChildType、GuestNum、Device、BnbType、DoW、OrderPre、
#' Season、Area、TravelDist
#' 
## ---------------------------------------------------------------------------------------------------------------------
logit0 = glm(Status ~ . + Season:TravelDist
                 + ChildType:Season
                 + OrderPre:TravelDist
                 + DoW:Season
                 + Monetary:Season
                 + DailyQpon:Season
                 + OrderPre:Season, data = res.allf2[, c(6:19, 21)],
            family = binomial(link="logit"))
summary(logit0)

#' 
## ---------------------------------------------------------------------------------------------------------------------
(1-pchisq(deviance(logit0), df.residual(logit0)))

#' 
#' 
#' (1) Deviance decrease from 16411 (null distance) to 12626，the range is 3785, which means the variables are meaningful.
#' (2) The difference between Deviance and DF is small.
#' (3) According to the Likelihood Ratio Test, the p-value is 0.834, so we will accept the null hypothesis, which means the model is good.
#' 
#' # Prediction Result
## ---------------------------------------------------------------------------------------------------------------------
rd.f1 <- raw[, c(1, 3, 5:7, 10, 12, 21:28, 31, 32)]
res.f1 <- subset(rd.f1, rd.f1$Frequency == 1)

colnames(res.f1)
head(res.f1)

#' 
#' ## Predict TMed
## ---------------------------------------------------------------------------------------------------------------------
summary(res.all)
res.allf1 = filter(res.all, Frequency == 1)
colnames(res.allf1)

res.allf1$tmedlog <- predict.lm(mlr.3, res.allf1[, c(-1, -3, -4, -5)])
res.allf1$RTRatio <- round(res.allf1$Recency / exp(res.allf1$tmedlog), 2)
res.allf1$TMed = exp(res.allf1$tmedlog)
head(res.allf1)

#' 
#' the distribution of purchasing cycle of new customer
## ---------------------------------------------------------------------------------------------------------------------
p.mlr = res.allf1 %>%
  filter(TMed <720) %>%
  ggplot(aes(TMed)) +
  geom_histogram(bins = 50,color="white")

p.mlr

#' 
#' ## Predict P(Alive)
## ---------------------------------------------------------------------------------------------------------------------
colnames(res.allf1)
res.allf1$p.Status = predict(logit0, res.allf1[, c(-1, -3, -4, -5, -20, -21)], type="response")

#' 
#' the probability distribution of being alive
## ---------------------------------------------------------------------------------------------------------------------
p = res.allf1 %>%
  ggplot(aes(p.Status)) +
  geom_histogram(bins = 50)

p

#' 
#' # Marketing Plan
#' Select the target customer:
#' 
#' - In order to optimize the marketing budget, we think it would be better to define our target customer first. According to the data of old customers, the maximum value of purchasing cycle `TMed` is 660; therefore, 660 days of purchasing cycle would be one rule to decide whether a customer is dead.
#' 
## ---------------------------------------------------------------------------------------------------------------------
hist(res.allf1$TMed[res.allf1$TMed < 800], 
     breaks = seq(0, 800, 20), 
     labels = TRUE, 
     xlab = 'Purchasing cycle (TMed)',
     main = 'Histogram of Purchasing cycle (TMed)')
abline(v=660,col="red")

#' 
#' - Also, considering customer's different purchasing cycle, we use RT-Ratio as another rule. Recall that if RT-Ratio is bigger than 8, then the customer is dead.
## ---------------------------------------------------------------------------------------------------------------------
hist(res.allf1$RTRatio[res.allf1$TMed <= 660 & res.allf1$RTRatio<1000],
     freq = TRUE, 
     ylab = "#Members", xlab = "Recency-Purchasing Period Ratio",
     main = 'Histogram of R-T Ratio')

#' 
#' - Finally, we will consider the probability of a customer being alive to get more granular segment.
