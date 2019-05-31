library(MASS)
attach(VA)

set.seed(1)
samplesize = 0.80 * nrow(VA)
meanAvg = 0

for (i in 1:10) {
  
  #  RANDOMLY SPLIT INTO 80/20.
  index = sample( seq_len( nrow ( VA ) ), size = samplesize)
  datatrain = VA[ index, ]
  datatest = VA[ -index, ]
  
  #  FITTING & PREDICTION
  VAtrain = lm(stime ~ ., data = VA, subset = index)
  meanAvg = meanAvg + mean((VA$stime - predict(VAtrain, VA))[index]^2)
  
}

#  AVG OF 10 LIN. PREDICTIONS
print(meanAvg / 10)

VA$treat <- factor(VA$treat)
contrasts(VA$treat)
lm.fit = lm(stime ~ ., data = VA)
str(VA$treat)
summary(lm.fit)
anova(lm.fit)

lm.fit = lm(stime ~ treat, data = VA)
summary(lm.fit)


