#TASK-1
transform(table(data2020$Region,dnn = "Region"))

transform(table(data2020$Subregion,dnn = "Sub-Region"))

fertilityBreaks <-seq(50.0,95.0,by=5);

transform(table(cut(data2020$Total.Fertility.Rate,fertilityBreaks),dnn = "Total Fertility Rate"))

breaks <-seq(1.0,8.0,by=1);

transform(table(cut(data2020$Life.Expectancy.at.Birth..Males,breaks),dnn = "Life Expectancy Of Male Genders"));

transform(table(cut(data2020$Life.Expectancy.at.Birth..Females,breaks),dnn = "Life Expectancy Of Female Genders"));

transform(table(cut(data2020$Life.Expectancy.at.Birth..Both.Sexes,breaks),dnn = "Life Expectancy Of Both Genders")) 

hist(data2020$Life.Expectancy.at.Birth..Males,breaks = breaks,main = "Life Expectancy Of Males",xlab = "Life Expectancy Of Males")

hist(data2020$Life.Expectancy.at.Birth..Females,breaks = breaks,main = "Life Expectancy Of Females",xlab = "Life Expectancy Of Females")

hist(data2020$Life.Expectancy.at.Birth..Both.Sexes,breaks = breaks,main = "Life Expectancy Of Both Genders",xlab = "Life Expectancy Of Both Genders")

fertilityBreaks <-seq(1,8,by=1);
hist(data2020$Total.Fertility.Rate,breaks = fertilityBreaks,main = "Total Fertility Rate",xlab = "Total Fertility Rate")

ggplot(data.frame(data2020$Region), aes(x=data2020$Region)) + geom_bar() +labs(y= "Frequency", x = "Regions")

plot(data2020$Life.Expectancy.at.Birth..Males,data2020$Total.Fertility.Rate,xlab="Life Expectancy Of Males",ylab="Total Fertility Rate")

plot(data2020$Life.Expectancy.at.Birth..Females,data2020$Total.Fertility.Rate,xlab="Life Expectancy Of Females",ylab="Total Fertility Rate")

#TASK-2

library("ggpubr")
ggscatter(data2020, x = "Life.Expectancy.at.Birth..Males", y = "Total.Fertility.Rate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Life Expectancy Of Males", ylab = "Total Fertility Rate") 

ggscatter(data2020, x = "Life.Expectancy.at.Birth..Females", y = "Total.Fertility.Rate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Life Expectancy Of Females", ylab = "Total Fertility Rate") 

ggscatter(data2020, x = "Life.Expectancy.at.Birth..Both.Sexes", y = "Total.Fertility.Rate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Life Expectancy Of Both Sexes", ylab = "Total Fertility Rate")



correlation_LFMale_And_Fertility <- cor.test(data2020$Life.Expectancy.at.Birth..Males, data2020$Total.Fertility.Rate, 
                                             method = "pearson")
correlation_LFMale_And_Fertility 

correlation_LFFemale_And_Fertility <- cor.test(data2020$Life.Expectancy.at.Birth..Females, data2020$Total.Fertility.Rate, 
                                               method = "pearson")
correlation_LFFemale_And_Fertility 

correlation_LFBothSexes_And_Fertility <- cor.test(data2020$Life.Expectancy.at.Birth..Both.Sexes, data2020$Total.Fertility.Rate, 
                                                  method = "pearson")
correlation_LFBothSexes_And_Fertility

#TASK-3

ggplot(data2020, aes(x=Total.Fertility.Rate, y=Subregion)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2)+
  labs(y= "Sub-Region", x = "Total Fertility Rate Per Woman")

ggplot(data2020, aes(x=Life.Expectancy.at.Birth..Both.Sexes, y=Subregion)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2)+
  labs(y= "Sub-Region", x = "Life.Expectancy at Birth Of Both Sexes")

ggplot(data2020, aes(x=Life.Expectancy.at.Birth..Males, y=Subregion)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2)+
  labs(y= "Sub-Region", x = "Life.Expectancy at Birth Of Males")

ggplot(data2020, aes(x=Life.Expectancy.at.Birth..Females, y=Subregion)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1,outlier.size=2)+
  labs(y= "Sub-Region", x = "Life.Expectancy at Birth Of Females")

#TASK-4

boxplot(my_data$Total.Fertility.Rate ~ my_data$Year,main = "Total Fertility Rate Comparision Between 2000 and 2020",xlab = "Year", ylab = "Total Fertility Rate")

boxplot(my_data$Life.Expectancy.at.Birth..Males ~ my_data$Year,main = "Life Expectancy Of Male Comparision Between 2000 and 2020",xlab = "Year", ylab = "Life Expectancy Of Male",ylim=c(40,90))

boxplot(my_data$Life.Expectancy.at.Birth..Females ~ my_data$Year,main = "Life Expectancy Of Female Comparision Between 2000 and 2020",xlab = "Year", ylab = "Life Expectancy Of Female",ylim=c(40,100))

boxplot(my_data$Life.Expectancy.at.Birth..Both.Sexes ~ my_data$Year,main = "Life Expectancy Of Both Genders Comparision Between 2000 and 2020",xlab = "Year", ylab = "Life Expectancy Of Both Genders",ylim=c(40,100))