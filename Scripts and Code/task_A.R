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

#since p-value = 2.336 > 0.05(alpha) we fail to reject our null hypothesis(H0). that is
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

g7_1<-ggplot(data = df, aes(x = route_distance, y = ..density..)) + 
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

#since p-value = 0.4249 < 0.05(alpha) we fail to reject our null hypothesis(H0). that is
#shapiro-wilk test suggests that our passenger_demand variable is normally distributed.


#2.Anderson-Darling Test
ad.test(df$passenger_demand)

#since p-value = 1.195e-08 < 0.05(alpha) we fail to reject our null hypothesis(H0). that is
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
# -1 suggests strong negative correlation
# and magnitude of score suggests how strong is the relationship and sign suggests the relationship
# negative or positive(ex: +0.9 means a strong positive association, -0.9 represents a strong negatvie association).

df %>% summary()

##1.Revenue Vs RoomsAvailible

g10<-ggplot(data=df,aes(x=RoomsAvailable,y=Revenue))+geom_point(color="black")+
  geom_smooth(method = "lm", color = "blue", se = FALSE)+labs(
    title = "Revenue Vs RoomsAvailable"
  )
g10

#from the scatter plot we can see that Revenue and RoomsAvalibe have a somewhat strong
#positive relationship
#we can clarify this using a above mentioned test

#hypo
#H0: Revenue and RoomsAvailable are independent
#H1: Revenue and RoomsAvailable are not independent

#1.Kendall's tau test
cor.test(df$Revenue, df$RoomsAvailable, method = "kendall")

#tau = 0.7056502 this suggest that Revenue and RoomsAvailable has a strong association.

#2.Spearman test
cor.test(df$Revenue, df$RoomsAvailable, method = "spearman")

#rho = 0.8836842, this value suggest that Revenue and RoomsAvailable has a strong associated.

#since both tests suggest that Revenue and RoomsAvailable are not independent(fail to reject of H0) 
#and they have a strong positive relationship. we can say that the relationship is linear by looking 
#at the scatter plot.



##2.Revenue Vs OccupancyRate

g11<-ggplot(data=df,aes(x=OccupancyRate,y=Revenue))+geom_point(color="black")+
  geom_smooth(method = "lm", color = "blue", se = FALSE)+labs(
    title = "Revenue Vs OccupancyRate"
  )
g11

#from the scatter plot we can see that Revenue and OccupancyRate have a weak 
#positive relationship
#we can clarify this using a above mentioned test

#hypo
#H0: Revenue and OccupancyRate are independent
#H1: Revenue and OccupancyRate are not independent

#1.Kendall's tau test
cor.test(df$Revenue, df$OccupancyRate, method = "kendall")

#tau = 0.1899001 this suggest that Revenue and OccupancyRate have a weak positive association.

#2.Spearman test
cor.test(df$Revenue, df$OccupancyRate, method = "spearman")

#rho = 0.2746971, this value suggest that Revenue and OccupancyRate have a weak positive association.

#since both tests suggest that Revenue and OccupancyRate are not independent(fail to reject of H0) 
#and they have a weak positive relationship. we can say that the relationship is linear by looking 
#at the scatter plot.



##3.Revenue Vs ADR

g12<-ggplot(data=df,aes(x=ADR,y=Revenue))+geom_point(color="black")+
  geom_smooth(method = "lm", color = "blue", se = FALSE)+labs(
    title = "Revenue Vs ADR"
  )
g12

#from the scatter plot we can see that Revenue and ADR have a weak 
#positive relationship
#we can clarify this using a above mentioned test

#hypo
#H0: Revenue and ADR are independent
#H1: Revenue and ADR are not independent

#1.Kendall's tau test
cor.test(df$Revenue, df$ADR, method = "kendall")

#tau = 0.2255411 this suggest that Revenue and ADR have a weak positive association.

#2.Spearman test
cor.test(df$Revenue, df$ADR, method = "spearman")

#rho = 0.3290704, this value suggest that Revenue and ADR have a weak positive association.

#since both tests suggest that Revenue and ADR are not independent(fail to reject of H0) 
#and they have a weak positive relationship. we can say that the relationship is linear by looking 
#at the scatter plot.




##4.Revenue Vs MarketingSpend

g13<-ggplot(data=df,aes(x=MarketingSpend,y=Revenue))+geom_point(color="black")+
  geom_smooth(method = "lm", color = "blue", se = FALSE)+labs(
    title = "Revenue Vs MarketingSpend"
  )
g13

#from the scatter plot we can see that Revenue and MarketingSpend have a weak 
#positive relationship. almost looks like has no relations at all.
#we can clarify this using a above mentioned test

#hypo
#H0: Revenue and MarketingSpend are independent
#H1: Revenue and MarketingSpend are not independent

#1.Kendall's tau test
cor.test(df$Revenue, df$MarketingSpend, method = "kendall")

#tau = -0.001731463 this suggest that Revenue and MarketingSpend have a weak negative association.

#2.Spearman test
cor.test(df$Revenue, df$MarketingSpend, method = "spearman")

#rho = -0.002505322, this value suggest that Revenue and MarketingSpend have a weak negative association.

#since both tests suggest that Revenue and MarketingSpend are not independent(fail to reject of H0) 
#and they have a weak negative relationship. almost like there is no relationship at all.



##5.Revenue Vs StaffCount

g14<-ggplot(data=df,aes(x=StaffCount,y=Revenue))+geom_point(color="black")+
  geom_smooth(method = "lm", color = "blue", se = FALSE)+labs(
    title = "Revenue Vs StaffCount"
  )
g14

#from the scatter plot we can see that Revenue and StaffCount have a strong
#positive relationship
#we can clarify this using a above mentioned test

#hypo
#H0: Revenue and StaffCount are independent
#H1: Revenue and StaffCount are not independent

#1.Kendall's tau test
cor.test(df$Revenue, df$StaffCount, method = "kendall")

#tau = 0.6820105 this suggest that Revenue and StaffCount have a strong positive association.

#2.Spearman test
cor.test(df$Revenue, df$StaffCount, method = "spearman")

#rho = 0.8651238, this value suggest that Revenue and StaffCount have a strong positive association.

#since both tests suggest that Revenue and StaffCount are not independent(fail to reject of H0) 
#and they have a strong positive relationship. we can say that the relationship is linear by looking 
#at the scatter plot.



##7.Revenue Vs GuestSatisfactionScore

g16<-ggplot(data=df,aes(x=GuestSatisfactionScore,y=Revenue))+geom_point(color="black")+
  geom_smooth(method = "lm", color = "blue", se = FALSE)+
  labs(
    title = "Revenue Vs GuestSatisfactionScore"
  )
g16

#from the scatter plot we can see that Revenue and GuestSatisfactionScore have a weak
#positive relationship
#we can clarify this using a above mentioned test

#hypo
#H0: Revenue and GuestSatisfactionScoret are independent
#H1: Revenue and GuestSatisfactionScore are not independent

#1.Kendall's tau test
cor.test(df$Revenue, df$GuestSatisfactionScore, method = "kendall")

#tau = 0.1074324  this suggest that Revenue and GuestSatisfactionScore have a weak positive association.

#2.Spearman test
cor.test(df$Revenue, df$GuestSatisfactionScore, method = "spearman")

#rho = 0.156328, this value suggest that Revenue and GuestSatisfactionScore have a weak positive association.

#since both tests suggest that Revenue and GuestSatisfactionScore are not independent(fail to reject of H0) 
#and they have a weak positive relationship. we can say that the relationship is linear by looking 
#at the scatter plot.




##8.Revenue Vs LoyaltyMembers

g17<-ggplot(data=df,aes(x=LoyaltyMembers,y=Revenue))+geom_point(color="black")+
  geom_smooth(method = "lm", color = "blue", se = FALSE)+
  labs(
    title = "Revenue Vs LoyaltyMembers"
  )
g17

#from the scatter plot we can see that Revenue and LoyaltyMembers have a strong
#positive relationship
#we can clarify this using a above mentioned test

#hypo
#H0: Revenue and LoyaltyMemberst are independent
#H1: Revenue and LoyaltyMembers are not independent

#1.Kendall's tau test
cor.test(df$Revenue, df$LoyaltyMembers, method = "kendall")

#tau = 0.1074324  this suggest that Revenue and LoyaltyMembers have a strong positive association.

#2.Spearman test
cor.test(df$Revenue, df$LoyaltyMembers, method = "spearman")

#rho = 0.156328, this value suggest that Revenue and LoyaltyMembers have a strong positive association.

#since both tests suggest that Revenue and LoyaltyMembers are not independent(fail to reject of H0) 
#and they have a strong positive relationship. we can say that the relationship is linear by looking 
#at the scatter plot.


##now we will see how each variable is correlated with other using a correlation map

df %>% str()
corr=df %>% select(-c("HotelQualityRank")) %>% relocate(Revenue, .after = last_col())

corr_matrix <- cor(corr, use = "complete.obs")

ggcorrplot(corr_matrix, 
           hc.order = TRUE, 
           type = "lower",
           outline.col = "black",
           ggtheme = ggplot2::theme_minimal(),
           colors = c("#6D9EC1", "white", "#E46726"),
           lab = TRUE, lab_size = 3.6)


#as we can see from the correlation map RoomsAvailable is highly corrlelated with StaffCount & LoyaltyMembers
#and also LoyaltyMembers is also highly correlated with StaffCount.

#so when including these variables together in regression modelling might produce less
#accurate results as multicolinearity is present in the full model



