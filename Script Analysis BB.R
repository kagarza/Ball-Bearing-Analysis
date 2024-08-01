#### Package installation and data upload ####
install.packages("alr3")
library(alr3)
install.packages("readr")
library(readr)
library(car)
bb <- read_delim("C:/Users/kg398/Desktop/Linear Regression 5345/Project/Ball Bearing/ballbearing.txt","\t", escape_double = FALSE, trim_ws = TRUE)
View(bb)
dim(bb)
colnames(bb)

#### Variable Names #####
L10 = bb$L10
P = bb$'Load (P)'
Z = bb$'No. of balls (Z)'
D = bb$'Diameter (D)'

########################### 
# Article Analysis
###########################
# Initial model (or model 2)
lnL10 = log(L10)
lnP = log(P)
lnZ = log(Z)
lnD = log(D)
bb.lm = lm(lnL10 ~ lnZ + lnD + lnP)
summary(bb.lm)
anova(bb.lm)
rst.lm = rstudent(bb.lm)
fit.lm = fitted(bb.lm)
pureErrorAnova(bb.lm)

# Initial model weighted
N = bb$'No. of bearings'
bb.lm.w = lm(lnL10 ~ lnZ + lnD + lnP, weights = N)
summary(bb.lm.w)
anova(bb.lm.w)
cook.lm.w = cooks.distance(bb.lm.w)
plot(cook.lm.w, ylab = "Cook's distances", main = "Cook's Distances: Same parameters for each company: Weighted")
rst.lm.w = rstudent(bb.lm.w)
fit.lm.w = fitted(bb.lm.w)
par(mfrow=c(1,2))
plot(fit.lm, rst.lm, xlab = "Fitted: Unweighted", ylab = "Externally Studentized Residuals")
abline(0,0)
plot(fit.lm.w, rst.lm.w, xlab = "Fitted: Weighted", ylab = "Externally Studentized Residuals")
abline(0,0)
qqnorm(rst.lm.w, main = "Normal Probability Plot: Same parameters for each company: Wieghted")
qqline(rst.lm.w)
pureErrorAnova(bb.lm.w)

#Adding indicator and interaction variables (full model or model 1)
B = ifelse(bb$Company==2,1,0)
C = ifelse(bb$Company==3,1,0)
BlnZ = B*lnZ
BlnD = B*lnD
BlnP = B*lnP
ClnP = C*lnP
ClnZ = C*lnZ
ClnD = C*lnD
bb.lmfull = lm(lnL10 ~ lnZ + lnD + lnP + B + C + BlnZ + BlnD + BlnP + ClnZ + ClnD + ClnP, weights = N)
summary(bb.lmfull)
anova(bb.lmfull)
cook.lmfull = cooks.distance(bb.lmfull)
plot(cook.lmfull, ylab = "Cook's distances", main = "Cook's Distances: All parameters free")
rst.lmfull = rstudent(bb.lmfull)
qqnorm(rst.lmfull, main = "Normal Probability Plot: All parameters free")
qqline(rst.lmfull)

#Assuming a Common p (model 3)
bb.lmCP = lm(lnL10 ~ lnZ + lnD + lnP + B + C + BlnZ + BlnD + ClnZ + ClnD, weights = N)
summary(bb.lmCP)
anova(bb.lmCP)
cook.lmCP = cooks.distance(bb.lmCP)
plot(cook.lmCP, ylab = "Cook's distances", main = "Cook's Distances: Common p")
rst.lmCP = rstudent(bb.lmCP)
qqnorm(rst.lmCP, main = "Normal Probability Plot: Common P")
qqline(rst.lmCP)

#Analysis for Model 3 - Model 1
anova(bb.lm.w, bb.lmfull)

#Analysis for Model 2 - Model 1
anova(bb.lmCP, bb.lmfull)

#Normal probability plots to check normality assumption
par(mfrow = c(1,3))
rst.lm.w = rstudent(bb.lm.w)
qqnorm(rst.lm.w, main = "Same parameters for each company")
qqline(rst.lm.w)
rst.lmfull = rstudent(bb.lmfull)
qqnorm(rst.lmfull, main = "All parameters free")
qqline(rst.lmfull)
rst.lmCP = rstudent(bb.lmCP)
qqnorm(rst.lmCP, main = "Common P")
qqline(rst.lmCP)

#Linear Realtionship between L10 and its regressors
par(mfrow = c(1,3))
plot(lnZ, lnL10, main = "lnZ v. lnL10")
abline(lm(lnL10 ~ lnZ))
plot(lnD, lnL10, main = "lnD v. lnL10")
abline(lm(lnL10 ~ lnD))
plot(lnP, lnL10, main = "lnP v. lnL10")
abline(lm(lnL10 ~ lnP))

#Cook's Distance to check for outliers
par(mfrow = c(1,3))
cook.lm.w = cooks.distance(bb.lm.w)
plot(cook.lm.w, ylim = c(0,1), ylab = "Cook's distances", main = "Same parameters for each company")
cook.lmfull = cooks.distance(bb.lmfull)
plot(cook.lmfull, ylim = c(0,1), ylab = "Cook's distances", main = "All parameters free")
cook.lmCP = cooks.distance(bb.lmCP)
plot(cook.lmCP, ylim = c(0,1), ylab = "Cook's distances", main = "Common p")


################################## 
#        Model Building
##################################

#Check explore

#Initial Model L10 ~ Z + D + P
bb.lm = lm(L10 ~ Z + D + P)
summary(bb.lm)
anova(bb.lm)
rst.lm = rstudent(bb.lm)
par(mfrow = c(1,1))
hist(rst.lm)
pureErrorAnova(bb.lm)

####Check Normality###
#L10 and some transformations
I = 1/L10
par(mfrow = c(2,3))
hist(L10)
hist(lnL10)
hist(I, main = "Histogram of 1/L10")
qqnorm(L10, main = "Normal Q-Q: L10")
qqline(L10)
qqnorm(lnL10, main = "Normal Q-Q: lnL10")
qqline(lnL10)
qqnorm(I, main = "Normal Q-Q: 1/L10")
qqline(I)
#Z and some transformations
I = 1/Z
par(mfrow = c(2,3))
hist(Z)
hist(lnZ)
hist(I, main = "Histogram of 1/Z")
qqnorm(Z, main = "Normal Q-Q: Z")
qqline(Z)
qqnorm(lnZ, main = "Normal Q-Q: lnZ")
qqline(lnZ)
qqnorm(I, main = "Normal Q-Q: 1/Z")
qqline(I)
#D and some transformations
I = 1/D
par(mfrow = c(2,3))
hist(D)
hist(lnD)
hist(I, main = "Histogram of 1/D")
qqnorm(D, main = "Normal Q-Q: D")
qqline(D)
qqnorm(lnD, main = "Normal Q-Q: lnD")
qqline(lnD)
qqnorm(I, main = "Normal Q-Q: 1/D")
qqline(I)
#P and some transformations
I = 1/P
par(mfrow = c(2,3))
hist(P)
hist(lnP)
hist(I, main = "Histogram of 1/P")
qqnorm(P, main = "Normal Q-Q: P")
qqline(P)
qqnorm(lnP, main = "Normal Q-Q: lnP")
qqline(lnP)
qqnorm(I, main = "Normal Q-Q: 1/P")
qqline(I)

####Lack of Fit Test####
pureErrorAnova(bb.lm)

#### Logarithmic Transformation ####
bb.lm = lm(lnL10 ~ lnZ + lnD + lnP)
#Check Constant Variance
par(mfrow = c(2,2))
plot(bb.lm)
# Add weights
bb.lm = lm(lnL10 ~ lnZ + lnD + lnP, weights = N)
#Check correlations
scatterplotMatrix(~lnL10+lnZ+lnD+lnP)
cor(lnL10, lnZ)
cor(lnL10, lnD)
cor(lnL10, lnP)
cor(lnZ, lnD)
cor(lnZ, lnP)
cor(lnD, lnP)
#Check Multicollinearity
vif(bb.lm)
#plot residuals against regressor variables
par(mfrow = c(1,3))
plot(lnZ,res.lm)
abline(0,0)
plot(lnD, res.lm)
abline(0,0)
plot(lnP, res.lm)
abline(0,0)

####Adding in Company indicators with interactions####
bb.lmfull = lm(lnL10 ~ lnZ + lnD + lnP + B + C + BlnZ + BlnD + BlnP + ClnZ + ClnD + ClnP, weights = N)
summary(bb.lmfull)
anova(bb.lmfull)

#Variable Selection algorithms
full = lm(lnL10 ~ lnZ + lnD + lnP + B + C + BlnZ + BlnD + BlnP + ClnZ + ClnD + ClnP, weights = N)
null = lm(lnL10 ~ 1)
forw = step(null, scope = list(lower = null, upper = full), direction = "forward")
back = step(full, direction = "backward")
stepw = step(null, scope = list(upper = full), direction = "both")
summary(forw)
summary(back)
summary(stepw)

######Final Model#####
summary(back)
anova(back)
rst.back = rstudent(back)
par(mfrow = c(1,3))
hist(rst.back, main = "Histogram of the Studentized Residuals", xlab = "Studentized Residuals")
pureErrorAnova(back)
vif(back)
cook.back = cooks.distance(back)
plot(cook.back, ylab = "Cook's distances", main = "Cook's Distances: Backward Variable Selection")
rst.back = rstudent(back)
qqnorm(rst.back, main = "Normal Probability Plot: Backward Variable Selection")
qqline(rst.back)

