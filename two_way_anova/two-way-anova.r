# Compare the effectiveness of different feed types on chick weight.
library(tidyverse)
library(readr)
df <- read_csv("chick.csv")
require("dplyr")
group_by(df, df$feed, df$sex) %>%
  summarise(
    count = n(),
    mean = mean(df$weight, na.rm = TRUE),
    sd = sd(df$weight, na.rm = TRUE)
  )
# Boxplot to visualize the data.
boxplot(df$weight ~ df$feed * df$sex, data=df, frame = FALSE, 
        col = c("#0047ab", "#c57300"), xlab="Feed.Sex", ylab="Weight")
legend("bottomright", inset=.02, title="Number of Cylinders",
       c("Feed", "Sex"), fill=c("#0047ab", "#c57300"), horiz=TRUE, cex=0.8)

# Two-way interaction plot to see interaction. 
interaction.plot(x.factor = df$feed, trace.factor = df$sex, 
                 response = df$weight, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Feed", ylab="Weight",
                 pch=c(1,19), col = c("#0047ab", "#c57300"))

# Post hoc test using a additive model and Tukey HSD test.
res.aov3 <- aov(df$weight ~ df$feed + df$sex, data = df)
TukeyHSD(res.aov3, which = "df$feed")
TukeyHSD(res.aov3, which = "df$sex")

# Checking normality and homoscedasticity. 
leveneTest(df$weight ~ df$feed * df$sex, data = df)
shapiro.test(x=residuals(object=res.aov3))

# Bar graph with error bars.
library(ggplot2)
library(plyr)
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
df2 <- data_summary(df, varname="weight", 
                    groupnames=c("feed", "sex"))
df2$dose=as.factor(df2$feed)
p<- ggplot(df2, aes(x=feed, y=weight, fill=sex)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=weight-sd, ymax=weight+sd), width=.2,
                position=position_dodge(.9)) 
print(p)

p+labs(title="", x="Feed", y = "Weight")+
  theme_classic() +
  scale_fill_manual(values=c('#0047ab','#c57300'))