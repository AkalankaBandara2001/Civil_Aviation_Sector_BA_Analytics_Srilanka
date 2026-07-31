library(tidyverse)
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)
library(ggcorrplot)
library(RColorBrewer)
library(patchwork)
library(ggpubr)
library(nortest)



set.seed(123)

###data loading
df <- read.csv("Data/Question-(a)/Air_Transport_Data.csv")


###getting insights
df %>% str()



# as mentioned in the data dictionary this is Air transport dataset with 200 observations with
# 7 variables with all of them are numerical variables and passenger_demand being target(dependent) variable
# and other variables are independent variables


df %>% summary()

# Based on the summary statistic of the Air transport dataset, airport traffic range from a 
# minimum of 69013 to a maximum of 336008, with a median of 199790 and a mean of 197962 
# The first and third quantiles are 164744and 225043 respectively. 
# This distribution around the mean suggests a diverse airport traffic


# The Average Income(USD per year) ranges from 26105 to 111233, 
# with a median of 65946 and a mean of 66030. The first and third quantiles are 57730 
# and 73247, respectively. 


# Fuel Price(USD per liter) varies from 0.5293 to 1.3618, with a median of 0.8884  
# and a mean of 0.8872. The first and third quartiles are 0.7809 and 0.9858 respectively. 



# Average Ticket Fare spans from 142.1 to 355.3, with a median of 250.9 and a mean of 250.4.
# The first and third quartiles are 221.7 and 277.4, respectively.


# Flight Frequency values range from 71.52 to 170.54, with a median of 122.61 and a mean of 122.57. 
# The first and third quartiles are 109.38 and 134.37 respectively.


# route_distance range from 341.5 to 2475.9 , with a median of 1579.1 and a mean of 1553.5. 
# The first and third quartiles are 1298.3 and 1849.4 respectively.

# Finally our target variable, Total Passenger Demand ranges from -60240(which is impossible in real life, because counts cannot be negative) 
# to 268128 with a median of 109586 and a mean of 107811. The first and third quartiles are 68592 
# and 140320 respectively.


###dataset peek

df %>% head(10)


### 1.univariant analysis

# we will analyze each variable graphically and get initial insights from graphs then
# we develop hypotheses about variable distributions using graphical insights and then we 
# will confirm them using Hypothesis testing statistics, specially normality test and 
# other distribution tests.

# basic hypothesis testing ---
# H0: variable follows a certain distribution
# H1: variable does not follow a certain distribution

#note that we will consider significance level(alpha) 5% in these tests

# if the test statistic's p-value is less than 5% we reject our null hypothesis(H0) 
# at 5% significance level, favoring our alternative hypothesis(H1) or in other words if 
# we can't be at least 95% sure that variable follows that certain distribution we will reject null hypothesis.
# if the test statistic's p-value is greater than or equal to 5% we reject our 
# alternative hypothesis(H1) at 5% significance level favoring our null hypothesis(H0)
# or in other words, if we can be 95% sure that variable follows that certain distribution we will accept our null hypothesis.



##1.airport_traffic Variable

g1_1<-ggplot(data = df, aes(x = airport_traffic, y = ..density..)) + 
  geom_histogram(bins =20, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1.0)

g1_1


#from looking at the histogram it seems like the airport traffic is normally 
#distributed.but we need to clarify the above claim using normality tests. 

#hypo
# H0:airport traffic variable follows a normal distribution
# H1:airport traffic variable does follows a normal distribution

#1.Shapiro-Wilk Test
shapiro.test(df$airport_traffic)

#since p-value = 0.829> 0.05(alpha) we fail to reject our null hypothesis. that is
#shapiro wilk test strongly suggests that our airport traffic variable is normally distributed.

#2.Anderson-Darling Test
ad.test(df$airport_traffic)

#since p-value = 0.6586 > 0.05(alpha) we fail to reject our null hypothesis. that is
#anderson test also suggests that our airport traffic variable is normally distributed.

#since both tests suggest that airport traffic is normally distributed we can verify
#it one more time using a QQ plot where we plot theoretical normal quantiles against our data sample quantiles

#3.Q-Q plot

g1_3<-ggqqplot(df, x = "airport_traffic", 
               color = "blue",              
               ggtheme = theme_minimal())   
g1_3

#from the Q-Q we can again conclude that our airport traffic variable is normally
#distributed as it almost perfectly align with the theoretical normal line shown in the Q-Q plot.

df %>% summary()



##2.avg_income Variable

g2_1<-ggplot(data = df, aes(x = avg_income, y = ..density..)) + 
  geom_histogram(bins =20, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1.0)

g2_1


#from looking at the histogram it seems like the avg_income Variable is normally 
#distributed as it shows a bell curved shape but we need to clarify this claim using several normality tests and Q-Q plot. 

#hypo
# H0: avg_income variable follows a normal distribution
# H1: avg_income variable does follows a normal distribution


#1.Shapiro-Wilk Test
shapiro.test(df$avg_income)

#since p-value = 0.2498 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#shapiro wilk test suggests that our avg_income variable is normally distributed.


#2.Anderson-Darling Test
ad.test(df$avg_income)

#since p-value = 0.5256 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#Anderson-Darling Test also suggests that our avg_income variable is normally distributed.

#since both tests suggest that average income is normally distributed we can verify this claim
#one more time using a Q-Q plot.


#3.Q-Q plot

g2_3<-ggqqplot(df, x = "avg_income", 
               color = "blue",              
               ggtheme = theme_minimal())   
g2_3

#from the Q-Q we can see that our avg_income variable is normally
#distributed as it perfectly aligns with the theoretical normal quantiles as
# shown in the Q-Q plot. so we can conclude that average income is also normally distributed.



##3.fuel_price Variable

g3_1<-ggplot(data = df, aes(x = fuel_price, y = ..density..)) + 
  geom_histogram(bins =20, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1.0)

g3_1


#from looking at the histogram it seems like the fuel_price Variable is also normally 
#distributed as the bell shape is present in the histogram but we need to clarify this claim using several normality tests and Q-Q plot. 

#hypo
# H0: fuel_price variable follows a normal distribution
# H1: fuel_price variable does follows a normal distribution


#1.Shapiro-Wilk Test
shapiro.test(df$fuel_price)

#since p-value = 0.9218 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#shapiro-wilk test statistic strongly suggests that fuel price variable is normally distributed.


#2.Anderson-Darling Test
ad.test(df$fuel_price)

#since p-value = 0.9789 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#Anderson-Darling test statistic also suggests that fuel price variable is normally distributed.

#since both tests suggest that fuel price is normally distributed we can verify this claim
#one more time using a Q-Q plot.


#3.Q-Q plot

g3_3<-ggqqplot(df, x = "fuel_price", 
               color = "blue",              
               ggtheme = theme_minimal())   
g3_3

#from the Q-Q we can see that our fuel_price variable is normally
#distributed as it perfectly align with the theoretical normal line, this suggests our fuel price variable is
#normally distributed.



##4.avg_ticket_fare Variable

g4_1<-ggplot(data = df, aes(x = avg_ticket_fare, y = ..density..)) + 
  geom_histogram(bins =20, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1.0)

g4_1

#from looking at the histogram it seems like the avg_ticket_fare Variable is also normally 
#distributed as the bell shape is present in the histogram but we need to clarify this claim using several normality tests and Q-Q plot. 

#hypo
# H0: avg_ticket_fare variable follows a normal distribution
# H1: avg_ticket_fare variable does follows a normal distribution


#1.Shapiro-Wilk Test
shapiro.test(df$avg_ticket_fare)

#since p-value =  0.7774 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#shapiro-wilk test strongly suggests that our avg_ticket_fare variable is normally distributed.


#2.Anderson-Darling Test
ad.test(df$avg_ticket_fare)

#since p-value = 0.8683 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#Anderson-Darling Test also suggests that our avg_ticket_fare variable is normally distributed.

#since both tests suggest that avg_ticket_fare is normally distributed we can verify this claim
#one more time using a Q-Q plot


#3.Q-Q plot

g4_3<-ggqqplot(df, x = "avg_ticket_fare", 
               color = "blue",              
               ggtheme = theme_minimal())   
g4_3

#from the Q-Q we can see that our avg_ticket_fare variable is normally
#distributed as it mostly align with the theoretical normal line and there almost no deviances
#from theoretical normal line as shown in the Q-Q plot. and also most of 
#data points are inside the confidence interval region, this suggest our avg_ticket_fare variable is
#normally distributed.




##5.flight_frequency Variable

g5_1<-ggplot(data = df, aes(x = flight_frequency, y = ..density..)) + 
  geom_histogram(bins =20, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1.0)

g5_1

#from looking at the histogram it seems like the flight_frequency Variable is normally 
#distributed as the bell shape is present in histogram 
#but we need to clarify this claim using several normality tests and Q-Q plot. 

#hypo
# H0: flight_frequency variable follows a normal distribution
# H1: flight_frequency variable does follows a normal distribution


#1.Shapiro-Wilk Test
shapiro.test(df$flight_frequency)

#since p-value = 0.6003 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#shapiro-wilk test suggests that our flight_frequency variable is normally distributed.


#2.Anderson-Darling Test
ad.test(df$flight_frequency)

#since p-value = 0.336 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#Anderson-Darling test also suggests that our flight_frequency variable is normally distributed.

#since both tests suggest that flight_frequency is normally distributed we can verify this claim
#one more time using a graphical method which is Q-Q plot.


#3.Q-Q plot

g5_3<-ggqqplot(df, x = "flight_frequency", 
               color = "blue",              
               ggtheme = theme_minimal())   
g5_3

#from the Q-Q we can see that our flight_frequency variable is normally
#distributed as it mostly align with the theoretical normal line and there almost no deviances
#from theoretical normal line as shown in the Q-Q plot. and also most of 
#data points are inside the confidence interval region, this suggest our flight_frequency variable is
#normally distributed.

##6.route_distance

g6_1<-ggplot(data = df, aes(x = route_distance, y = ..density..)) + 
  geom_histogram(bins =20, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1.0)

g6_1

#from looking at the histogram it seems like the route_distance Variable is normally 
#distributed as the bell shape is present in histogram 
#but we need to clarify this claim using several normality tests and Q-Q plot. 

#hypo
# H0: route_distance variable follows a normal distribution
# H1: route_distance variable does follows a normal distribution


#1.Shapiro-Wilk Test
shapiro.test(df$route_distance)

#since p-value = 0.4398 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#shapiro-wilk test suggests that our route_distance variable is normally distributed.


#2.Anderson-Darling Test
ad.test(df$route_distance)

#since p-value = 0.6498 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#Anderson-Darling test also suggests that our route_distance variable is normally distributed.

#since both tests suggest that route_distance is normally distributed we can verify this claim
#one more time using a graphical method which is Q-Q plot.


#3.Q-Q plot

g6_3<-ggqqplot(df, x = "route_distance", 
               color = "blue",              
               ggtheme = theme_minimal())   
g6_3

#from the Q-Q we can see that our route_distance variable is normally
#distributed as it mostly align with the theoretical normal line and there almost no deviances
#from theoretical normal line as shown in the Q-Q plot. and also most of 
#data points are inside the confidence interval region, this suggest our route_distance variable is
#normally distributed.




##7.passenger_demand(Target) Variable

g7_1<-ggplot(data = df, aes(x = passenger_demand,y = ..density..)) + 
  geom_histogram(bins =20, color = "black", fill = "blue") +
  geom_density(color = "red", size = 1.0)

g7_1

#from looking at the histogram it seems like the passenger_demand(Target) Variable is normally 
#distributed as follows the symmetric bell shape which can be seen in the histogram. 
#but we need to clarify this claim using several normality tests and Q-Q plot. 

#hypo
# H0: passenger_demand variable follows a normal distribution
# H1: passenger_demand variable does follows a normal distribution


#1.Shapiro-Wilk Test
shapiro.test(df$passenger_demand)

#since p-value = 0.4249 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#shapiro-wilk test suggests that our passenger_demand variable is normally distributed.


#2.Anderson-Darling Test
ad.test(df$passenger_demand)

#since p-value = 0.3862 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#Anderson-Darling test also suggests that our passenger_demand variable is normally distributed.

#since both tests suggest that passenger_demand is normally distributed we can verify this claim
#one more time using a Q-Q plot.


#3.Q-Q plot

g7_3<-ggqqplot(df, x = "passenger_demand", 
               color = "blue",              
               ggtheme = theme_minimal())   
g7_3

#from the Q-Q we can see that our passenger_demand variable is normally
#distributed as it mostly align with the theoretical normal line and there almost no deviances
#from theoretical normal line as shown in the Q-Q plot. and also most of 
#data points are inside the confidence interval region, this suggest our passenger_demand(Target) variable is
#normally distributed.






### 2.Bivariate Analysis(Correlation analysis)

#in this section we will analyse the relationship between our target variable(Total Passenger Demand) 
#and each independent variable using graphical methods and some correlation tests.

#here we will first inspect what kind of association is there between all independent variables and dependent variable using scatter plots.
#then we will use pearson correlation test as it shows linear association between two variables. but from the scatter plots
#if there seems to be a non linear relationship we will use kendal's tau test and spearman rho test statistics to further measeure the association.


#note that all test statistic scores are in between -1 and 1
#and,
# 1 suggests strong positive correlation
# 0 suggests no correlation at all(2 variables are independent)
#-1 suggests strong negative correlation
# and magnitude of score suggests how strong is the relationship and sign suggests the relationship
# negative or positive(ex: +0.9 means a strong positive association, -0.9 represents a strong negatvie association).

df %>% summary()

##1.airport_traffic Vs passenger_demand

g8<-ggplot(data=df,aes(x=airport_traffic,y=passenger_demand))+geom_point(color="blue")+
  geom_smooth(method = "lm", color = "black", se = FALSE)+labs(
    title = "airport_traffic Vs passenger_demand"
  )
g8

#from the scatter plot we can see that airport_traffic and passenger_demand have a linear a
#positive relationship we can clarify this using a above mentioned test

#hypo
#H0: airport_traffic and passenger_demand are independent
#H1: airport_traffic and passenger_demand are not independent

#1.Pearson test
cor.test(df$airport_traffic, df$passenger_demand, method = "pearson")
#r = 0.385401 suggests that airport_traffic and passenger_demand has a somewhat positive association
# and it is significant at 5% significant level

#2.Kendall's tau test
cor.test(df$airport_traffic, df$passenger_demand, method = "kendall")
#tau = 0.2518593  suggests that airport_traffic and passenger_demand has a somewhat positive association
# and it is significant at 5% significant level

#3.Spearman test
cor.test(df$airport_traffic, df$passenger_demand, method = "spearman")
#rho  = 0.3729573  suggests that airport_traffic and passenger_demand has a somewhat positive association
# and it is significant at 5% significant level

#since all tests suggest that airport_traffic and passenger_demand are not independent(reject H0 because all p values < 0.05) 
#and they have a weak positive relationship. we can say that the relationship is mostly linear by looking 
#at the scatter plot.



##2.avg_income Vs passenger_demand

g9<-ggplot(data=df,aes(x=avg_income,y=passenger_demand))+geom_point(color="blue")+
  geom_smooth(method = "lm", color = "black", se = FALSE)+labs(
    title = "avg_income Vs passenger_demand"
  )
g9

#from the scatter plot we can see that avg_income and passenger_demand do not have an
#association at all because the relationship line is almost a flat line

#hypo
#H0: avg_income and passenger_demand are independent
#H1: avg_income and passenger_demand are not independent

#1.Pearson test
cor.test(df$avg_income, df$passenger_demand, method = "pearson")
#r = 0.04954933 suggests that avg_income and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p = 0.4859>0.05)

#2.Kendall's tau test
cor.test(df$avg_income, df$passenger_demand, method = "kendall")
#tau = 0.03226131  suggests that avg_income and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p-value = 0.4975 > 0.05)

#3.Spearman test
cor.test(df$avg_income, df$passenger_demand, method = "spearman")
#rho  = 0.04349359 suggests that avg_income and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p-value = 0.5405 > 0.05)

#since all tests suggest that avg_income and passenger_demand are independent(fail to reject H0 because all p values > 0.05)
#we can conclude that there is no association between avg_income and passenger_demand.




##3.fuel_price Vs passenger_demand

g10<-ggplot(data=df,aes(x=fuel_price,y=passenger_demand))+geom_point(color="blue")+
  geom_smooth(method = "lm", color = "black", se = FALSE)+labs(
    title = "fuel_price Vs passenger_demand"
  )
g10

#from the scatter plot we can see that fuel_price and passenger_demand do not have an
#association at all because the relationship line is almost a flat line

#hypo
#H0: fuel_price and passenger_demand are independent
#H1: fuel_price and passenger_demand are not independent

#1.Pearson test
cor.test(df$fuel_price, df$passenger_demand, method = "pearson")
#r = 0.08567551 suggests that fuel_price and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p =  0.2277 > 0.05)

#2.Kendall's tau test
cor.test(df$fuel_price, df$passenger_demand, method = "kendall")
#tau = 0.03939698 suggests that fuel_price and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p-value = 0.4074 > 0.05)

#3.Spearman test
cor.test(df$fuel_price, df$passenger_demand, method = "spearman")
#rho  =0.059916 suggests that fuel_price and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p-value = 0.399 > 0.05)

#since all tests suggest that fuel_price and passenger_demand are independent(fail to reject H0 because all p values > 0.05) 
#we can conclude that there is no association between fuel_price and passenger_demand.



##4.avg_ticket_fare Vs passenger_demand

g11<-ggplot(data=df,aes(x=avg_ticket_fare,y=passenger_demand))+geom_point(color="blue")+
  geom_smooth(method = "lm", color = "black", se = FALSE)+labs(
    title = "avg_ticket_fare Vs passenger_demand"
  )
g11


#from the scatter plot we can see that avg_ticket_fare and passenger_demand do not have an
#association at all because the relationship line is almost a flat line

#hypo
#H0: avg_ticket_fare and passenger_demand are independent
#H1: avg_ticket_fare and passenger_demand are not independent

#1.Pearson test
cor.test(df$avg_ticket_fare, df$passenger_demand, method = "pearson")
#r = -0.03039567 suggests that avg_ticket_fare and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p = 0.6692 > 0.05)

#2.Kendall's tau test
cor.test(df$avg_ticket_fare, df$passenger_demand, method = "kendall")
#tau = 0.007135678 suggests that avg_ticket_fare and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p-value = 0.8807 > 0.05)

#3.Spearman test
cor.test(df$avg_ticket_fare, df$passenger_demand, method = "spearman")
#rho  =0.01307283 suggests that avg_ticket_fare and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p-value = 0.8541 > 0.05)

#since all tests suggest that avg_ticket_fare and passenger_demand are independent(fail to reject H0 because all p values > 0.05) 
#we can conclude that there is no association between avg_ticket_fare and passenger_demand.




##5.flight_frequency Vs passenger_demand

g12<-ggplot(data=df,aes(x=flight_frequency,y=passenger_demand))+geom_point(color="blue")+
  geom_smooth(method = "lm", color = "black", se = FALSE)+labs(
    title = "flight_frequency Vs passenger_demand"
  )
g12

#from the scatter plot we can see that flight_frequency and passenger_demand do not have an
#association at all because the relationship line is almost a flat line

#hypo
#H0: flight_frequency and passenger_demand are independent
#H1: flight_frequency and passenger_demand are not independent

#1.Pearson test
cor.test(df$flight_frequency, df$passenger_demand, method = "pearson")
#r = 0.05220582 suggests that flight_frequency and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p = 0.4628 > 0.05)

#2.Kendall's tau test
cor.test(df$flight_frequency, df$passenger_demand, method = "kendall")
#tau = 0.02824121 suggests that flight_frequency and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p-value = 0.5526 > 0.05)

#3.Spearman test
cor.test(df$flight_frequency, df$passenger_demand, method = "spearman")
#rho  =0.0438086  suggests that flight_frequency and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p-value = 0.5376 > 0.05)

#since all tests suggest that flight_frequency and passenger_demand are independent(fail to reject H0 because all p values > 0.05) 
#we can conclude that there is no association between flight_frequency and passenger_demand.



##6.route_distance Vs passenger_demand

g13<-ggplot(data=df,aes(x=route_distance,y=passenger_demand))+geom_point(color="blue")+
  geom_smooth(method = "lm", color = "black", se = FALSE)+labs(
    title = "route_distance Vs passenger_demand"
  )
g13

#from the scatter plot we can see that route_distance and passenger_demand do not have an
#association at all because the relationship line is almost a flat line

#hypo
#H0: route_distance and passenger_demand are independent
#H1: route_distance and passenger_demand are not independent

#1.Pearson test
cor.test(df$route_distance, df$passenger_demand, method = "pearson")
#r = -0.04116275 suggests that route_distance and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p = 0.5628 > 0.05)

#2.Kendall's tau test
cor.test(df$route_distance, df$passenger_demand, method = "kendall")
#tau = -0.01959799 suggests that route_distance and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p-value = 0.6802 > 0.05)

#3.Spearman test
cor.test(df$route_distance, df$passenger_demand, method = "spearman")
#rho  = -0.0280432 suggests that route_distance and passenger_demand are independent
# and it is not significant(i.e. no significant association) at 5% significant level(p-value = 0.6932 > 0.05)

#since all tests suggest that route_distance and passenger_demand are independent(fail to reject H0 because all p values > 0.05) 
#we can conclude that there is no association between route_distance and passenger_demand.




##now we will see how each variable is correlated with other using a correlation map


corr=df %>% relocate(passenger_demand, .after = last_col())

corr_matrix <- cor(corr, use = "complete.obs")

ggcorrplot(corr_matrix, 
           hc.order = TRUE, 
           type = "lower",
           outline.col = "black",
           ggtheme = ggplot2::theme_minimal(),
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE, lab_size = 3.6)


#as we can see from the correlation no independent variable is highly correlated with other independent
#variables, so there won't be any multicolinearity issues.


### 3.Regression analysis

#since each independent variables are in different units, to make it fair to a regression we 
#have to standardize

df_standard <- df %>% 
  mutate(
    airport_traffic  = as.vector(scale(airport_traffic)),
    avg_income       = as.vector(scale(avg_income)),
    fuel_price       = as.vector(scale(fuel_price)),
    avg_ticket_fare  = as.vector(scale(avg_ticket_fare)),
    flight_frequency = as.vector(scale(flight_frequency)),
    route_distance   = as.vector(scale(route_distance)),
    passenger_demand = as.vector(scale(passenger_demand))
  )

df_standard %>% summary()


#from the correlation analysis we identified that there is no significant association between target variable
#and independent variables except airport_traffic. so there is no point of carrying out a single variable linear regression
#analysis for other variables except airport_traffic.


#only airport_traffic as a independent variable
lm1 <- lm(passenger_demand ~ airport_traffic , data = df_standard)
summary(lm1)
AIC(lm1)

# since R-squared of the model is 0.1485 means airport traffic variable alone can explain
# 14.85% variability in the passenger demand data.
# adjusted R-squared is 0.1442 and AIC 540.4138


#even though adding other variables to the above model systematically doesn't make much difference
#(because they are mostly independent with the target variable)
#we can add and see which model makes the most predictive accuracy and find out overall best model using AIC and R squared value

# here we will use forward selection method

lm2 <- lm(passenger_demand ~ airport_traffic+avg_income , data = df_standard)
summary(lm2)
AIC(lm2)

#adjusted R squared = 0.1401 decreased a bit and AIC = 542.3745 increased. so we will drop the avg_income.


lm3 <- lm(passenger_demand ~ airport_traffic+fuel_price , data = df_standard)
summary(lm3)
AIC(lm3)

#adjusted R squared = 0.1593 increased a bit and AIC = 537.8466 dropped.so we will keep fuel_price.


lm4 <- lm(passenger_demand ~ airport_traffic+fuel_price+avg_ticket_fare, data = df_standard)
summary(lm4)
AIC(lm4)

#adjusted R squared = 0.1603 increased a bit and AIC = 538.5866 increased a bit.so we will keep avg_ticket_fare.


lm5 <- lm(passenger_demand ~ airport_traffic+fuel_price+avg_ticket_fare+flight_frequency, data = df_standard)
summary(lm5)
AIC(lm5)

#adjusted R squared =  0.1617 increased a bit and AIC = 539.2281 increased a bit.so we will keep flight_frequency.


lm6 <- lm(passenger_demand ~ airport_traffic+fuel_price+avg_ticket_fare+flight_frequency+route_distance, data = df_standard)
summary(lm6)
AIC(lm6)

#adjusted R squared =  0.1574 decreased and AIC = 539.2281 increased.so we will drop route_distance.



#so our final model is lm5 because it has the highest accuracy, but the AIC is higher than lm3 we will not consider that
#because the AIC increased in lm5 is considerably low.
#so the regression equation would be


# passenger_demand = 0.4133(airport_traffic) + 0.1483(fuel_price) - 0.08615(avg_ticket_fare) + 0.07601(flight_frequency) 


#Looking at the regression results, passenger demand in Sri Lanka is clearly driven more by infrastructure limits than by 
#ticket pricing. Overall airport traffic turned out to be the main growth factor, carrying the highest positive coefficient 
#in our model (0.4133, p < 0.001$). This proves that expanding physical capacity. specifically speeding up the BIA Phase II 
#expansion project should be the top priority for aviation authorities like AASL. The second coefficient for fuel price (0.1483) 
#likely reflects broader economic growth periods where high overall travel demand coincided with rising fuel costs, suggesting that 
#state backed fuel hedging programs could help protect local airlines from global oil shocks. Meanwhile, average ticket fares showed a 
#slight negative coefficient (-0.0862), which aligns with standard price elasticity. however, its impact is secondary, meaning airlines 
#can easily handle this using dynamic yield management to attract budget tourists without hurting business travel revenue. Lastly, the 
#positive coefficient for flight frequency (0.0760) highlights the value of schedule convenience, advising slot management teams to boost
#weekly flight frequencies to key source markets during peak tourism months.
