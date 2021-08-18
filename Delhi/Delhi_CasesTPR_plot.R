library(ggplot2)
library(scales)
library(dplyr)
library(scales)
library(reshape2)

#Read file with daily tests and case counts

df=read.csv("CASES/DL_Cases_pos",sep="\t",header=T,colClasses=c("Date",NA,NA,NA))

#Aggregate tests and cases data to weeks

df$weeks <- cut(df[,"Date"], breaks="week")
agg <- df %>% group_by(weeks) %>% summarise(agg=sum(Daily.Cases))
agg2 <- df %>% group_by(weeks) %>% summarise(tests_agg=sum(Daily.Tests))
df2=data.frame(weeks=agg$weeks, agg_cases=agg$agg, agg_tests=agg2$tests_agg)

options(scipen=10000)

#Spline interpolation for smoothing raw data points

spline.d <- as.data.frame(spline(as.Date(df2$weeks), df2$agg_cases))
spline.d2 <- as.data.frame(spline(as.Date(df2$weeks), df2$agg_tests))
spline.d$tests=spline.d2$y

#Generate Plot

ggplot(spline.d, aes(as.Date(x,origin="1970-01-01"), y)) +   ylab("") + xlab("") + geom_line(size=0.8,color="#4286f5") + scale_x_date(expand = c(0, 0), limits = as.Date(c("2020-03-31","2021-06-30")),labels = date_format("%d-%b-%y"),    breaks = "2 weeks") + 
geom_line(linetype="dashed",size=0.8, aes(y=500000*y/tests,color="#cb392e")) +
scale_y_continuous(limits=c(0,200000),expand = c(0, 0),breaks = seq(0,300000,by=50000), sec.axis = sec_axis(~./5000, name = "Percentage (%)",breaks=seq(0,50,by=10))) +
theme_bw() + theme( axis.text.x = element_text(face= "bold", colour= "black" ,angle = 75,  hjust=1),
panel.grid.major.x = element_blank() ,panel.grid.minor.x =  element_blank(),panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line( size=.1, color="black" )) +
theme(legend.position = "none") +
theme(
    axis.text.y = element_text(size=14,colour="#4285f5",face= "bold"),
    axis.title.y = element_text(size=14,color = "#4285f5",face= "bold"),
    axis.text.y.right = element_text(size=14,color = "#bf4b48",face= "bold"),
    axis.title.y.right = element_text(size=14,color = "#bf4b48",face= "bold") ) +
theme( axis.text.x = element_blank()) +
theme(plot.margin = margin(10, 10, 10, 10))


#ggsave("PLOTS/cases_tpr.jpg",height=2,width=6.7,dpi=400)

