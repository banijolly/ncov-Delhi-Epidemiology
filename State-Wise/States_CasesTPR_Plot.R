##This code is used to generate line graph of Cases and Test Positivity Rate with Time for the states analysed. For reading the data of a particular state, uncomment and edit the data input and plot title lines appropriately.

##State data file paths:
##Punjab: 		Cases/PB/PB_lineage_Daily
##Haryana: 		Cases/HR/HR_lineage_Daily
##Uttar Pradesh: 	Cases/UP/UP_lineage_Daily
##Chandigarh: 		Cases/CH/CH_lineage_Daily
##Jammu & Kashmir: 	Cases/JK/JK_lineage_Daily
##Himachal Pradesh: 	Cases/HP/HP_lineage_Daily
##Madhya Pradesh:	Cases/MP/MP_lineage_Daily
##Uttarakhand: 		Cases/UK/UK_lineage_Daily
##Ladakh: 		Cases/LA/LA_lineage_Daily

library(ggplot2)
library(scales)
library(dplyr)
library(scales)
library(reshape2)

#df=read.csv("Cases/PB/PB_Cases",sep="\t",header=T,colClasses=c(NA,"Date",NA,NA))

df$weeks <- cut(df[,"Date"], breaks="week")
agg <- df %>% group_by(weeks) %>% summarise(agg=sum(Daily.Cases))
agg2 <- df %>% group_by(weeks) %>% summarise(tests_agg=sum(Daily.Tests))
df2=data.frame(weeks=agg$weeks, agg_cases=agg$agg, agg_tests=agg2$tests_agg)
df2[is.na(df2)] <- 0

options(scipen=10000)

spline.d <- as.data.frame(spline(as.Date(df2$weeks), df2$agg_cases))
spline.d2 <- as.data.frame(spline(as.Date(df2$weeks), df2$agg_tests))
spline.d$tests=spline.d2$y

ggplot(spline.d, aes(as.Date(x,origin="1970-01-01"), y)) +   ylab("Number of New Cases") + xlab("") + geom_line(size=0.8,color="#4286f5") + scale_x_date(expand = c(0, 0), limits = as.Date(c("2020-11-01","2021-06-30")),labels = date_format("%d-%b-%y"),    breaks = "2 weeks") + 
geom_line(linetype="dashed",size=0.8, aes(y=500000*y/tests,color="#cb392e")) +
scale_y_continuous(limits=c(0,200000),expand = c(0, 0),breaks = seq(0,300000,by=50000), sec.axis = sec_axis(~./5000, name = "Test Positivity Rate (%)",breaks=seq(0,50,by=10))) +
theme_bw() + theme( axis.text.x = element_text(face= "bold", colour= "black" ,angle = 75,  hjust=1),
panel.grid.major.x = element_blank() ,panel.grid.minor.x =  element_blank(),panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line( size=.1, color="black" )) +
theme(legend.position = "none") +
#ggtitle("Punjab") +
theme(plot.title = element_text(face= "bold",color= "black",hjust = 0.5)) +
theme(
    axis.text = element_text(size=14,face="bold"),
    axis.text.y = element_text(size=14,colour="#4285f5",face= "bold"),
    axis.title.y = element_text(size=12,color = "#4285f5",face= "bold"),
    axis.text.y.right = element_text(size=14,color = "#bf4b48",face= "bold"),
    axis.title.y.right = element_text(size=12,color = "#bf4b48",face= "bold") ) 
#theme( axis.text.x = element_blank()) 
#ggsave("PLOTS/PB_cases_tpr.jpg",height=3.5,width=6,dpi=400)

