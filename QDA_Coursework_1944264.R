#############################################
### Quantitative Data Analysis Coursework ###
#############################################

Aids2ann <- read.csv("Aids2ann.csv", header=TRUE)

### First have a look at the size of the data set, and the variables.

head(Aids2ann)
dim(Aids2ann)

###
### Missing data
###

library(VIM)
aggr(Aids2ann)

Aids2ann$state[is.na(Aids2ann$state)]
Aids2ann$sex[is.na(Aids2ann$sex)]
Aids2ann$diag[is.na(Aids2ann$diag)]
Aids2ann$death[is.na(Aids2ann$death)]
Aids2ann$status[is.na(Aids2ann$status)]
Aids2ann$T.categ[is.na(Aids2ann$T.categ)]
Aids2ann$age[is.na(Aids2ann$age)]
Aids2ann$year[is.na(Aids2ann$year)]
Aids2ann$outcome[is.na(Aids2ann$outcome)]

### No missing data

###
### Categorical Data ###
###

table(Aids2ann$state)
table(Aids2ann$sex)

### F    M 
### 202 5812 
### Females are way less than men -> the ratio between the two is less than 3.5% 
### First thing which is odd about the dataset

barplot(table(Aids2ann$sex))

table(Aids2ann$T.categ)

barplot(table(Aids2ann$state))
barplot(table(Aids2ann$T.categ))

unique(Aids2ann$sex)
unique(Aids2ann$T.categ)
unique(Aids2ann$status)
unique(Aids2ann$outcome)
unique(Aids2ann$year)

table(Aids2ann$sex,Aids2ann$T.categ)


###   blood haem  het   hs hsid   id mother other
### F    71    0   55    1    0   49      8    18
### M   116   89   47 5216  168   59      7   110

###
### Numerical Data
###

unique(Aids2ann$age)

### Diagnosis

quantile(Aids2ann$diag,p=c(0,0.025,0.25,0.5,0.75,0.975,1))

h.diag <- hist(Aids2ann$diag)
h.diag

mu.diag <- mean(Aids2ann$diag)
sig.diag <- sd(Aids2ann$diag)
plot(Aids2ann$diag,dnorm(Aids2ann$diag,mean=mu.diag,sd=sig.diag))

hist(Aids2ann$diag,freq=F)
points(Aids2ann$diag,dnorm(Aids2ann$diag,mean=mu.diag,sd=sig.diag))

qqnorm(Aids2ann$diag)
qqline(Aids2ann$diag)

### Hist diagnosis can be related to a normal curve
### Also the fited line and the QQ points show that it has a normal distribution

quantile(Aids2ann$death,p=c(0,0.025,0.25,0.5,0.75,0.975,1))

h.death <- hist(Aids2ann$death)
names(h.death)
h.death$breaks
h.death$counts

mu.death <- mean(Aids2ann$death)
sig.death <- sd(Aids2ann$death)
plot(Aids2ann$death,dnorm(Aids2ann$death,mean=mu.death,sd=sig.death))

hist(Aids2ann$death,freq=F)
points(Aids2ann$death,dnorm(Aids2ann$death,mean=mu.death,sd=sig.death))

qqnorm(Aids2ann$death)
qqline(Aids2ann$death)

### From every test it shows that it is too far from a normal dist
### The last number of deaths makes the whole data go into something
### more like an exponential thing

h.year <- hist(Aids2ann$year)
names(h.year)
h.year$breaks
h.year$counts

mu.year <- mean(Aids2ann$year)
sig.year <- sd(Aids2ann$year)
plot(Aids2ann$year,dnorm(Aids2ann$year,mean=mu.year,sd=sig.year))

hist(Aids2ann$year,freq=F)
points(Aids2ann$year,dnorm(Aids2ann$year,mean=mu.year,sd=sig.year))

qqnorm(Aids2ann$year)
qqline(Aids2ann$year)

### Good normal distribution cut at the end
### Not a lot of sense to look for a normal dist, observations will grow each year..

quantile(Aids2ann$age,p=c(0,0.025,0.25,0.5,0.75,0.975,1))

### 0%  2.5%   25%   50%   75% 97.5%  100% 
### 0    23    31    37    43    59    82 
### Data distribution is consistent

h.age <- hist(Aids2ann$age)
names(h.age)
h.age$breaks
h.age$counts

mu.age <- mean(Aids2ann$age)
sig.age <- sd(Aids2ann$age)
plot(Aids2ann$age,dnorm(Aids2ann$age,mean=mu.age,sd=sig.age))

hist(Aids2ann$age,freq=F)
points(Aids2ann$age,dnorm(Aids2ann$age,mean=mu.age,sd=sig.age))

qqnorm(Aids2ann$age)
qqline(Aids2ann$age)

### More a normal distribution than this one would have been hard



###
### Associations between variables ###
###

boxplot(Aids2ann$age~Aids2ann$T.categ)
t.test(Aids2ann$age~Aids2ann$T.categ=="het")

boxplot(Aids2ann$diag~Aids2ann$T.categ)

### Important graph, who has contracted the illness from the mother has a short lifespan
### To be seen in the regression as well

table(Aids2ann$age, Aids2ann$T.categ)

chisq.test(Aids2ann$age, Aids2ann$T.categ)

###Pearson's Chi-squared test

###data:  Aids2ann$age and Aids2ann$T.categ
###X-squared = 6984.1, df = 567, p-value < 2.2e-16

### The two variables should be highly correlated (dependent) with a p-value that small
### This goes well with the boxplot as well

boxplot(Aids2ann$year~Aids2ann$state)

chisq.test(Aids2ann$year, Aids2ann$state)

###	Pearson's Chi-squared test

###data:  Aids2ann$year and Aids2ann$state
###X-squared = 42.496, df = 27, p-value = 0.02939
###P-value = 2,9% reject the null hyp -> correlation

boxplot(Aids2ann$age~Aids2ann$state)

chisq.test(Aids2ann$age, Aids2ann$state)

###Pearson's Chi-squared test

###data:  Aids2ann$age and Aids2ann$state
###X-squared = 277.33, df = 243, p-value = 0.06436
###
###The p-val = 6,4% which means there is some evidence againts the null hyp

chisq.test(Aids2ann$status, Aids2ann$T.categ)

###	Pearson's Chi-squared test

###data:  Aids2ann$status and Aids2ann$T.categ
###X-squared = 52.862, df = 7, p-value = 3.947e-09
###The p-val << 0.1% means the null hyp is rejected hence the two var are dependent

chisq.test(Aids2ann$status, Aids2ann$state)

###Pearson's Chi-squared test

###data:  Aids2ann$status and Aids2ann$state
###X-squared = 18.918, df = 3, p-value = 0.0002843
###The p-val << 0.1% means the null hyp is rejected hence the two var are dependent

boxplot(Aids2ann$death~Aids2ann$status)
t.test(Aids2ann$death~Aids2ann$status)

###Welch Two Sample t-test

###data:  Aids2ann$death by Aids2ann$status
###t = 76.413, df = 3548.3, p-value < 2.2e-16
###alternative hypothesis: true difference in means is not equal to 0
###95 percent confidence interval:
###  714.7668 752.4123
###sample estimates:
###  mean in group A mean in group D 
###11503.01        10769.42
###
###There is a significant difference in the mean this is due to the fact that
###most end of obs are because the survey was done, the deaths are spread through all time


plot(Aids2ann$diag, Aids2ann$death)

###By this plot we understand that tere is a high chance of death right after the
###diagnosis, and then the possibility lowers. There is a straight line at 11500
###which shows the end of observations (it tricks the data a little bit)


###
### Logistic regression ###
###

contrasts(Aids2ann$T.categ)

mod <- glm(outcome~factor(T.categ)+age+year+factor(state), family = binomial, data = Aids2ann)
summary(mod)

mod1 <- glm(outcome~factor(T.categ)+age+year, family = binomial, data = Aids2ann)
summary(mod1)

mod2 <- glm(outcome~factor(T.categ)+age+year+factor(state)+factor(sex), family = binomial, data = Aids2ann)
summary(mod2)

mod3 <- glm(outcome~age+year, family = binomial, data = Aids2ann)
summary(mod3)

mod4 <- glm(outcome~age+year+factor(T.categ)+factor(sex), family = binomial, data = Aids2ann)
summary(mod4)

### Mod dovrebbe essere migliore di mod1
### Cerco di migliorarlo

library(lmtest)
lrtest(mod1)
lrtest(mod1, mod)
lrtest(mod1, mod2)
lrtest(mod1, mod3)
lrtest(mod1, mod4)

mod.resid <- resid(mod)
plot(Aids2ann$year, mod.resid) 

confint(mod1)
