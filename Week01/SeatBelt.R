library(readr)
britishSeatBeltStudy <- read_csv("britishSeatBeltStudy.csv", 
                                 col_types = cols(law = col_factor(levels = c("0", 
                                                                              "1")), Date = col_date(format = "%Y-%m-%d")))
View(britishSeatBeltStudy)

britishSeatBeltStudy$FractionOfDriversKilled = britishSeatBeltStudy$DriversKilled /britishSeatBeltStudy$drivers

summary(britishSeatBeltStudy$FractionOfDriversKilled)
df.plot.scatter(x = 'Date', y = 'FractionOfDriversKilled', c = 'law', colormap = 'viridis')
# Colormap scatterplot: 
df.plot.scatter(x = 'Date', y = 'DriversKilled', c = 'law', colormap = 'viridis')
t.test(britishSeatBeltStudy$DriversKilled [which(britishSeatBeltStudy$law %in% c(1))], 
       britishSeatBeltStudy$DriversKilled [which(britishSeatBeltStudy$law %in% c(0))], 
       var.equal = F, paired = F
)
t.test(britishSeatBeltStudy$FractionOfDriversKilled [which(britishSeatBeltStudy$law %in% c(1))], 
       britishSeatBeltStudy$FractionOfDriversKilled [which(britishSeatBeltStudy$law %in% c(0))], 
       var.equal = F, paired = F
)

#linear alanylsis: logistic regression[Classification]
fit <- glm (formula = law ~ DriversKilled + drivers + FractionOfDriversKilled + kms + PetrolPrice ,
            data = britishSeatBeltStudy, family = 'binomial')
summary(fit)

