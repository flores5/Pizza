library(factoextra)
library(devtools)
library(ggbiplot)
install_github("vqv/ggbiplot")
library(ggbiplot)
names(Pizza)
hist(Pizza$mois)
hist(Pizza$prot)
hist(Pizza$fat)
hist(Pizza$ash)
hist(Pizza$sodium)
hist(Pizza$carb)
hist(Pizza$cal)

shapiro.test(Pizza$mois)
````````````````````````
Shapiro-Wilk normality test

data:  Pizza$mois
W = 0.90018, p-value = 3.583e-13
````````````````````````
shapiro.test(Pizza$prot)
```````````````````````
Shapiro-Wilk normality test

data:  Pizza$prot
W = 0.81522, p-value < 2.2e-16
````````````````````````
shapiro.test(Pizza$fat)
``````````````````````
Shapiro-Wilk normality test

data:  Pizza$fat
W = 0.80468, p-value < 2.2e-16
````````````````````````
shapiro.test(Pizza$ash)
````````````````````````
Shapiro-Wilk normality test

data:  Pizza$ash
W = 0.86743, p-value = 2.213e-15
````````````````````````
shapiro.test(Pizza$sodium)
``````````````````````
Shapiro-Wilk normality test

data:  Pizza$sodium
W = 0.7099, p-value < 2.2e-16
````````````````````````
shapiro.test(Pizza$carb)
```````````````````````
Shapiro-Wilk normality test

data:  Pizza$carb
W = 0.84116, p-value < 2.2e-16
````````````````````````
shapiro.test(Pizza$cal)
```````````````````````
Shapiro-Wilk normality test

data:  Pizza$cal
W = 0.90354, p-value = 6.434e-13
``````````````````````

pizza.pca <- prcomp(Pizza[3:9], center=TRUE, scale=TRUE)
summary(pizza.pca)
````````````````````
Importance of components:
  PC1    PC2     PC3    PC4     PC5     PC6      PC7
Standard deviation     2.042 1.5134 0.64387 0.3085 0.16636 0.01837 0.003085
Proportion of Variance 0.596 0.3272 0.05922 0.0136 0.00395 0.00005 0.000000
Cumulative Proportion  0.596 0.9232 0.98240 0.9960 0.99995 1.00000 1.000000
``````````````````

pizza.pca
``````````
Standard deviations (1, .., p=7):
  [1] 2.042494038 1.513425713 0.643865158 0.308503205 0.166364113 0.018374149 0.003085252

Rotation (n x k) = (7 x 7):
  PC1        PC2         PC3        PC4          PC5           PC6           PC7
mois    0.06470937 -0.6282759  0.42166894  0.2207216  0.006470293 -0.4464499018 -0.4185690354
prot    0.37876090 -0.2697067 -0.74602744  0.0105932  0.387982788  0.0001715203 -0.2767646428
fat     0.44666592  0.2343791  0.19930871  0.5070422 -0.173367634  0.5254028685 -0.3776715255
ash     0.47188953 -0.1109904 -0.05627269 -0.5523985 -0.670885701 -0.0588609281 -0.0560214003
sodium  0.43570289  0.2016617  0.45516887 -0.4462769  0.602614079 -0.0031309852  0.0005243238
carb   -0.42491371  0.3203121 -0.05223651 -0.3343395 -0.007436899  0.0005088535 -0.7760679112
cal     0.24448730  0.5674576 -0.11331559  0.2792632 -0.078003175 -0.7219138527 -0.0120598098
``````````````````

pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal component", 
       ylab="Proportion of variance explained", 
       ylim=c(0,1), type='b')
  plot(cumsum(x.pvar),xlab="Principal component", 
       ylab="Cumulative Proportion of variance explained", 
       ylim=c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

pcaCharts(pizza.pca)

biplot(pizza.pca)
biplot(pizza.pca, choices= c(1,3))
biplot(pizza.pca, choices= c(2,3))

library(ggbiplot)

ggbiplot(pizza.pca, obs.scale = 1, var.scale = 1, 
         labels=rownames(Pizza),
         groups = Pizza[,1],
         ellipse = TRUE, 
         circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', 
        legend.position = 'top')

names(pizza.pca)

pizza.pca$x

#assign the pc scores to data frame to be used later in analysis
Pizza$pc1<-pizza.pca$x[,1]
Pizza$pc2<-pizza.pca$x[,2]
Pizza$pc3<-pizza.pca$x[,3]

names(Pizza)

plot(Pizza$mois, Pizza$pc1)
plot(Pizza$prot, Pizza$pc1)
plot(Pizza$fat, Pizza$pc1)
plot(Pizza$ash, Pizza$pc1)
plot(Pizza$sodium, Pizza$pc1)
plot(Pizza$carb, Pizza$pc1)
plot(Pizza$cal, Pizza$pc1)


plot(Pizza$mois, Pizza$pc2)
plot(Pizza$prot, Pizza$pc2)
plot(Pizza$fat, Pizza$pc2)
plot(Pizza$ash, Pizza$pc2)
plot(Pizza$sodium, Pizza$pc2)
plot(Pizza$carb, Pizza$pc2)
plot(Pizza$cal, Pizza$pc2)

plot(Pizza$mois, Pizza$pc3)
plot(Pizza$prot, Pizza$pc3)
plot(Pizza$fat, Pizza$pc3)
plot(Pizza$ash, Pizza$pc3)
plot(Pizza$sodium, Pizza$pc3)
plot(Pizza$carb, Pizza$pc3)
plot(Pizza$cal, Pizza$pc3)


pizza.pc1.lm <- lm(Pizza$pc1~Pizza$brand)
summary(pizza.pc1.lm)

````````````````````````
Call:
  lm(formula = Pizza$pc1 ~ Pizza$brand)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.84068 -0.15211  0.00607  0.16096  0.65299 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   4.61998    0.04701   98.27   <2e-16 ***
  Pizza$brandB -3.07267    0.06541  -46.98   <2e-16 ***
  Pizza$brandC -3.54469    0.06771  -52.35   <2e-16 ***
  Pizza$brandD -2.96737    0.06491  -45.72   <2e-16 ***
  Pizza$brandE -6.32911    0.06708  -94.35   <2e-16 ***
  Pizza$brandF -6.27298    0.06593  -95.14   <2e-16 ***
  Pizza$brandG -6.37024    0.06649  -95.81   <2e-16 ***
  Pizza$brandH -6.46243    0.06444 -100.28   <2e-16 ***
  Pizza$brandI -5.75217    0.06649  -86.52   <2e-16 ***
  Pizza$brandJ -5.30825    0.06491  -81.78   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2532 on 290 degrees of freedom
Multiple R-squared:  0.9851,	Adjusted R-squared:  0.9846 
F-statistic:  2130 on 9 and 290 DF,  p-value: < 2.2e-16
````````````````````````
Anova(pizza.pc1.lm)
````````````
Anova Table (Type II tests)

Response: Pizza$pc1
Sum Sq  Df F value    Pr(>F)    
Pizza$brand 1228.77   9    2130 < 2.2e-16 ***
  Residuals     18.59 290                      
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
````````````

plot(Pizza$pc1~Pizza$brand)


pizza.pc2.lm <- lm(Pizza$pc2~Pizza$brand)
summary(pizza.pc2.lm)
Anova(pizza.pc2.lm)

````````````````````
Call:
  lm(formula = Pizza$pc2 ~ Pizza$brand)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.11545 -0.15233  0.02548  0.17879  0.69285 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   2.36804    0.05543  42.724   <2e-16 ***
  Pizza$brandB -3.18021    0.07711 -41.243   <2e-16 ***
  Pizza$brandC -4.41432    0.07982 -55.302   <2e-16 ***
  Pizza$brandD -3.84734    0.07653 -50.276   <2e-16 ***
  Pizza$brandE -1.68530    0.07908 -21.311   <2e-16 ***
  Pizza$brandF -0.80630    0.07773 -10.373   <2e-16 ***
  Pizza$brandG -0.74349    0.07838  -9.485   <2e-16 ***
  Pizza$brandH -1.71787    0.07597 -22.612   <2e-16 ***
  Pizza$brandI -4.24760    0.07838 -54.189   <2e-16 ***
  Pizza$brandJ -3.02239    0.07653 -39.495   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.2985 on 290 degrees of freedom
Multiple R-squared:  0.9623,	Adjusted R-squared:  0.9611 
F-statistic: 821.9 on 9 and 290 DF,  p-value: < 2.2e-16

> Anova(pizza.pc2.lm)
Anova Table (Type II tests)

Response: Pizza$pc2
Sum Sq  Df F value    Pr(>F)    
Pizza$brand 659.01   9  821.91 < 2.2e-16 ***
  Residuals    25.84 290                      
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
````````````````````
plot(Pizza$pc2~Pizza$brand)


pizza.pc3.lm <- lm(Pizza$pc3~Pizza$brand)
summary(pizza.pc3.lm)
Anova(pizza.pc3.lm)
plot(Pizza$pc3~Pizza$brand)

``````````````````

Call:
  lm(formula = Pizza$pc3 ~ Pizza$brand)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.00138 -0.05467  0.00309  0.06257  0.44283 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.14661    0.02518   5.822 1.54e-08 ***
  Pizza$brandB  0.86627    0.03503  24.726  < 2e-16 ***
  Pizza$brandC -1.40135    0.03627 -38.639  < 2e-16 ***
  Pizza$brandD -0.75879    0.03477 -21.824  < 2e-16 ***
  Pizza$brandE -0.08349    0.03593  -2.324   0.0208 *  
  Pizza$brandF -0.43021    0.03532 -12.182  < 2e-16 ***
  Pizza$brandG -0.56396    0.03561 -15.835  < 2e-16 ***
  Pizza$brandH -0.16716    0.03452  -4.843 2.09e-06 ***
  Pizza$brandI  0.61579    0.03561  17.291  < 2e-16 ***
  Pizza$brandJ  0.32928    0.03477   9.470  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.1356 on 290 degrees of freedom
Multiple R-squared:  0.957,	Adjusted R-squared:  0.9556 
F-statistic: 716.6 on 9 and 290 DF,  p-value: < 2.2e-16

> Anova(pizza.pc3.lm)
Anova Table (Type II tests)

Response: Pizza$pc3
Sum Sq  Df F value    Pr(>F)    
Pizza$brand 118.621   9  716.65 < 2.2e-16 ***
  Residuals     5.333 290                      
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
``````````````````````

# Based on these results, all significant P-values, we can conclude that
## between all brands all components tested (mois, prot, etc) are significantly different. 




