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



# Average Ticket Fare spans from 3,058 to 33,146, with a median of 19,666 and a mean of 19,783.
# The first and third quartiles are 16,380 and 23,392, respectively. 


# Flight Frequency values range from 71.52 to 170.54, with a median of 122.61 and a mean of 122.57. 
# The first and third quartiles are 109.38 and 134.37 respectively.


# route_distance range from 341.5 to 2475.9 , with a median of 1579.1 and a mean of 1553.5. 
# The first and third quartiles are 1298.3 and 1849.4 respectively.

# Finally our target variable, Total Passenger Demand ranges from -60240(which is impossible in real life, because counts cannot be negative) 
# to 268128 with a median of 109586 and a mean of 107811. The first and third quartiles are 68592 
# and 140320 respectively.


###dataset peek

df %>% head(10)


###univariant analysis

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

g4_1<-ggplot(data = df, aes(x = MarketingSpend , y = ..density..)) + 
  geom_histogram(binwidth =1500, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1.5)


g4_2<-ggplot(data = df, aes(x = MarketingSpend )) + 
  geom_histogram(binwidth = 1500, color = "black", fill = "lightblue") +
  geom_freqpoly(binwidth = 1500, color = "red", size = 1.5)

g4_1|g4_2

#from looking at the histogram it seems like the avg_ticket_fare Variable is also normally 
#distributed as the bell shape is present in the histogram but we need to clarify this claim using several normality tests and Q-Q plot. 

#hypo
# H0: avg_ticket_fare variable follows a normal distribution
# H1: avg_ticket_fare variable does follows a normal distribution


#1.Shapiro-Wilk Test
shapiro.test(df$avg_ticket_fare)

#since p-value = 0.5479 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#shapiro-wilk test suggests that our avg_ticket_fare variable is normally distributed.


#2.Anderson-Darling Test
ad.test(df$avg_ticket_fare)

#since p-value = 0.4406 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#Anderson-Darling Test suggests that our avg_ticket_fare variable is normally distributed.

#since both tests suggest that avg_ticket_fare is normally distributed we can verify this claim
#one more time using a Q-Q plot


#3.Q-Q plot

g4_3<-ggqqplot(df, x = "avg_ticket_fare", 
               color = "blue",              
               ggtheme = theme_minimal())   
g4_3

#from the Q-Q we can see that our avg_ticket_fare variable is normally
#distributed as it mostly align with the theoretical normal line and there almost no deviances
# from theoretical normal line as shown in the Q-Q plot. and also most of 
#data points are inside the confidence interval region, this suggest our MarketingSpend variable is
#normally distributed.




##5.StaffCount Variable

g5_1<-ggplot(data = df, aes(x = StaffCount , y = ..density..)) + 
  geom_histogram(binwidth =3.5, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1.5)

g5_1

g5_2<-ggplot(data = df, aes(x = StaffCount )) + 
  geom_histogram(binwidth = 3.5, color = "black", fill = "lightblue") +
  geom_freqpoly(binwidth = 3.5, color = "red", size = 1.5)

g5_1|g5_2

#from looking at the histogram it seems like the StaffCount Variable is not normally 
#distributed as the bell shape is not present in histogram(it most likely uniformly distributed). 
#but we need to clarify this claim using several normality tests and Q-Q plot. 

#hypo
# H0: StaffCount variable follows a normal distribution
# H1: StaffCount variable does follows a normal distribution


#1.Shapiro-Wilk Test
shapiro.test(df$StaffCount)

#since p-value = 1.106e-08 < 0.05(alpha) we reject our null hypothesis(H0). that is
#shapiro-wilk test suggests that our StaffCount variable is not normally distributed.


#2.Anderson-Darling Test
ad.test(df$StaffCount)

#since p-value = 2.814e-10 < 0.05(alpha) we reject our null hypothesis(H0). that is
#Anderson-Darling Test suggests that our StaffCount variable is not normally distributed.

#since both tests suggest that StaffCount is not normally distributed we can verify this claim
#one more time using a graphical method which is Q-Q plot.


#3.Q-Q plot

g5_3<-ggqqplot(df, x = "StaffCount", 
               color = "blue",              
               ggtheme = theme_minimal())   
g5_3

#from the Q-Q we can see that our StaffCount variable is not normally distributed
#as most data points fall outside confidence region of theoretical normal line. and there are huge
#deviance from theoretical normal line in the tails as shown in the Q-Q plot.this suggest our 
#StaffCount variable is not normally distributed.

##6.





##7.GuestSatisfactionScore Variable

g7_1<-ggplot(data = df, aes(x = GuestSatisfactionScore , y = ..density..)) + 
  geom_histogram(binwidth =0.5, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1.5)


g7_2<-ggplot(data = df, aes(x = GuestSatisfactionScore )) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "lightblue") +
  geom_freqpoly(binwidth = 0.5, color = "red", size = 1.5)

g7_1|g7_2

#from looking at the histogram it seems like the GuestSatisfactionScore Variable is also normally 
#distributed as the bell shape is present in the histogram but we need to clarify this claim using several normality tests and Q-Q plot. 

#hypo
# H0: GuestSatisfactionScore variable follows a normal distribution
# H1: GuestSatisfactionScore variable does follows a normal distribution


#1.Shapiro-Wilk Test
shapiro.test(df$GuestSatisfactionScore)

#since p-value = 0.1033 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#Shapiro-wilk test suggests that our GuestSatisfactionScore variable is normally distributed.


#since Shapiro-wilk test suggests that GuestSatisfactionScore is normally distributed we can verify this claim
#one more time using a graphical method which is Q-Q plot


#3.Q-Q plot

g7_3<-ggqqplot(df, x = "GuestSatisfactionScore", 
               color = "blue",              
               ggtheme = theme_minimal())   
g7_3

#from the Q-Q plot we can see that our GuestSatisfactionScore variable is normally
#distributed as it mostly align with the theoretical normal line and even though there are some deviances
# from theoretical normal line as shown in the Q-Q plot. and also most of 
#data points are inside the confidence interval region, this suggest our GuestSatisfactionScore variable is
#normally distributed.




##8.LoyaltyMembers Variable

g8_1<-ggplot(data = df, aes(x = LoyaltyMembers , y = ..density..)) + 
  geom_histogram(binwidth =400, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1.5)


g8_2<-ggplot(data = df, aes(x = LoyaltyMembers )) + 
  geom_histogram(binwidth = 400, color = "black", fill = "lightblue") +
  geom_freqpoly(binwidth = 400, color = "red", size = 1.5)

g8_1|g8_2

#from looking at the histogram it seems like the LoyaltyMembers Variable is also normally 
#distributed as the bell shape is present in the histogram but we need to clarify this claim using several normality tests and Q-Q plot. 

#hypo
# H0: LoyaltyMembers variable follows a normal distribution
# H1: LoyaltyMembers variable does follows a normal distribution


#1.Shapiro-Wilk Test
shapiro.test(df$LoyaltyMembers)

#since p-value = 8.761e-06 < 0.05(alpha) we reject our null hypothesis(H0). that is
#Shapiro-wilk test suggests that our LoyaltyMembers variable is not normally distributed.

#**this shows us heavily relying on graphical interpretations sometimes not as graphs shows
#likes LoyaltyMembers variable is normally distributed but in reality it is not

#2.Anderson-Darling Test
ad.test(df$LoyaltyMembers)

#since p-value = 2.814e-10 < 0.05(alpha) we reject our null hypothesis(H0). that is
#Anderson-Darling Test suggests that our LoyaltyMembers variable is not normally distributed.

#since both tests suggest that LoyaltyMembers is not normally distributed we can verify this claim
#one more time using a graphical method which is Q-Q plot.


#3.Q-Q plot

g8_3<-ggqqplot(df, x = "LoyaltyMembers", 
               color = "blue",              
               ggtheme = theme_minimal())   
g8_3

#from the Q-Q plot we can see that our LoyaltyMembers variable is not normally distributed
#as some data points from the starting tail fall outside confidence region of theoretical normal line. 
#this suggest our LoyaltyMembers variable is not normally distributed.




##9.Revenue(Target) Variable

g9_1<-ggplot(data = df, aes(x = Revenue , y = ..density..)) + 
  geom_histogram(binwidth =50000, color = "black", fill = "lightblue") +
  geom_density(color = "red", size = 1.5)

g9_1

g9_2<-ggplot(data = df, aes(x = Revenue )) + 
  geom_histogram(binwidth = 50000, color = "black", fill = "lightblue") +
  geom_freqpoly(binwidth = 50000, color = "red", size = 1.5)

g9_1|g9_2

#from looking at the histogram it seems like the Revenue Variable is not normally 
#distributed as it is positively skewed which can be seen in the histogram. 
#but we need to clarify this claim using several normality tests and Q-Q plot. 

#hypo
# H0: Revenue variable follows a normal distribution
# H1: Revenue variable does follows a normal distribution


#1.Shapiro-Wilk Test
shapiro.test(df$Revenue)

#since p-value = 3.714e-09 < 0.05(alpha) we reject our null hypothesis(H0). that is
#shapiro-wilk test suggests that our Revenue variable is not normally distributed.


#2.Anderson-Darling Test
ad.test(df$Revenue)

#since p-value = 1.195e-08 < 0.05(alpha) we reject our null hypothesis(H0). that is
#Anderson-Darling Test suggests that our Revenue variable is not normally distributed.

#since both tests suggest that Revenue is not normally distributed we can verify this claim
#one more time using a graphical method which is Q-Q plot.


#3.Q-Q plot

g9_3<-ggqqplot(df, x = "Revenue", 
               color = "blue",              
               ggtheme = theme_minimal())   
g9_3

# from the Q-Q we can see that our Revenue variable is not normally distributed
# as most data points fall outside confidence region of theoretical normal line. and there are huge
# deviance from theoretical normal line in the starting tail as shown in the Q-Q plot.this suggest our
# Revenue variable is not normally distributed.



