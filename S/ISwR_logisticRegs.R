# logistic example - ch13

require(ISwR)
no.yes <- c("No", "Yes")
smoking <- gl(2,1,8, no.yes)
obesity <- gl(2,2,8, no.yes)
snoring <- gl(2,4,8, no.yes)
n.tot <- c(60, 17, 8, 2, 187, 85, 51, 23)
n.hyp <- c(5,2,1,0,35,13,15,8)
data.frame(smoking, obesity, snoring, n.tot, n.hyp)

hyp.tbl <- cbind(n.hyp, n.tot-n.hyp)

glm.hyp <- glm(hyp.tbl ~ smoking + obesity + snoring, family=binomial("logit"))
summary(glm.hyp, corr=T)

# alternately, you may enter y = proportion and use the weighting function
#prop.hyp <- n.hyp/n.tot
#glm(prop.hyp ~ smoking + obesity + snoring, family=binomial("logit"), weight=n.tot)

anova(glm.hyp, test="Chisq") #note order matters - can only drop last if insignificant
drop1(glm.hyp, test='Chisq') # this test shows a table of Chisq p values where var_x is in last post'n

#check it:
glm.hyp <- glm(hyp.tbl ~ obesity + snoring + smoking, family=binomial('logit'))
anova(glm.hyp, test='Chisq') # so, can drop smoking ...

glm.hyp <- glm(hyp.tbl ~ snoring + smoking + obesity, family=binomial('logit'))
anova(glm.hyp, test='Chisq') # cannot drop obesity  ...

glm.hyp <- glm(hyp.tbl ~ smoking + obesity + snoring, family=binomial('logit'))
anova(glm.hyp, test='Chisq') # cannot drop snoring ...

# drop smoking!

glm.hyp <- glm(hyp.tbl ~ snoring + obesity, family=binomial('logit'))

summary(glm.hyp)
anova(glm.hyp, test='Chisq')


confint(glm.hyp) # inverts the liklihood ratio test
confint.default(glm.hyp) # a Wald-test est := beta + 1.96*s.e 

require(MASS)
plot(profile(glm.hyp))
# when profile is non-linear the liklihood function is not well approximated 

### testing for trends -- an aside about shoe size and Caesarian section birth --

shoe.score <- 1:6
summary(glm(t(caesar.shoe) ~ shoe.score, binomial))

# we used the transpose of caesar.shoe, as we require column vactors for glm

anova(glm(t(caesar.shoe) ~ shoe.score, binomial), test='Chisq')

# shoe size is a predictor of C-section birth

# it is probably better to test this directly, however

caesar.shoe.yes <- caesar.shoe["Yes", ]
caesar.shoe.no <- caesar.shoe["No", ]
caesar.shoe.total <- caesar.shoe.yes + caesar.show.no
prop.trend.test(caesar.shoe.yes, caesar.shoe.total)
prop.test(caesar.shoe.yes, caesar.shoe.total)

# note the X-sq value from the prop.test is equivalent to the NULL glm model's residual deviance
# and ... X-sq from prop.trend.test is equiv to Deviance when shoe is added to the null model

glm.Cshoe <- glm(t(caesar.shoe) ~ shoe.score, binomial)
plot(profile(glm.Cshoe))

# tau(shoe.score) looks linear, so the liklihood function is probably well approximated

## logistic regression using raw data

juul$menarche <- factor(juul$menarche, labels=c('No', 'Yes')) # recodes 1 + 2 to factor('No'; 'Yes')
juul$tanner <- factor(juul$tanner) # sets tanner as a factor
juul.girl <- subset(juul, age>8 & age<20 & complete.cases(menarche)) # gest age range excl menarche = NA

glm.mnch <- glm(menarche ~ age, binomial, data=juul.girl)
summary(glm.mnch)

# note that at y = 0 the median girl reaches manarch -- thus
med.age <- -glm.mnch$coeff[1]/glm.mnch$coeff[2]
med.age

# adding complexity via Tanner stages

glm.mnch2 <- glm(menarche ~ age + tanner, binomial, data=juul.girl)
summary(glm.mnch2)

# for a joint test of the significance of the tanner variable, use drop1 + test='Chisq''
drop1(glm.mnch2, test='Chisq')

# both terms are significant at the 1% level...

# prediction

predict(glm.hyp, type='response')
        
plot(juul.girl$age, fitted(glm(menarche~age, binomial, data=juul.girl))) # fitted using actual ages

Age <- seq(8,20, 0.1) # a smooth Age series
newages <- data.frame(age=Age) # this titles the vector 'age', and uses data from the smooth Age series
predicted.probability  <- predict(glm.mnch, newages, type='resp') # model probs using smooth age
plot(predicted.probability ~ Age, type='l') #fitted data

# model checking

fitted(glm.hyp) # compare fitted incidence
prop.hyp <- n.hyp/n.tot # with actual incidence

#  sometimes frequency is better -- all together in a nice table
data.frame(fit=fitted(glm.hyp)*n.tot, n.hyp, n.tot)

age.group <- cut(juul.girl$age, c(8,10,12,13,14,15,16,18,20))
tb  <- table(age.group, juul.girl$menarche)
rel.freq  <-prop.table(tb,1)[,2] # a proportion table using sum of rows (,1), and extracting %Yes [,2]
# a sum of columns proportion may be obtained using (,2)

plot(predicted.probability ~ Age, type='l') #fitted data
points(rel.freq ~ c(9, 11, 12.5, 13.5, 14.5, 15.5, 17, 19), pch=5) # points are added to plot

## exercises -- ch 13

#1/ the malaria data set

glm.mal  <- glm(mal ~ age + log(ab), binomial, data=malaria)
anova(glm.mal, test='Chisq')
drop1(glm.mal, test='Chisq')

# age does not significantly improve the estimate

glm.mal  <- glm(mal ~ log(ab), binomial, data=malaria)
summary(glm.mal)

ab_fake  <- (seq(2:2066))
new_ab <- data.frame(ab=ab_fake)
mal.predict <- predict(glm.mal, log(new_ab), type='resp')
plot(mal.predict ~ log(ab_fake), type='l')

ab.group <- cut(malaria$ab, c(exp(1), exp(2), exp(3), exp(4), exp(5), exp(6), exp(7), exp(8)))
p  <- seq(1.5, 7.5, 1)
tb.ab  <- table(ab.group, malaria$mal)
rel.freq.mal  <- prop.table(tb.ab,1)[,2]
plot(rel.freq.mal ~ p, pch=5) # do it in this order to be sure the 'real' data sets the ranges
lines(mal.predict ~log(ab_fake), type='l') # adds the predicted line to the actual data

# the relationship appears to be more nonlinear -- ie small doses of ab have a large impact