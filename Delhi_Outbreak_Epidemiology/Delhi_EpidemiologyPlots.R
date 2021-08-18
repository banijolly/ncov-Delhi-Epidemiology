##Figure 1A - Tests vs Cases

library(ggplot2)
library(scales)
library(dplyr)
library(scales)
library(reshape2)
library(stringr)

df=read.csv("Data/DL_Cases_pos",sep="\t",header=T,colClasses=c("Date",NA,NA,NA))
df$weeks <- cut(df[,"Date"], breaks="week")
agg <- df %>% group_by(weeks) %>% summarise(agg=sum(Daily.Cases))
agg2 <- df %>% group_by(weeks) %>% summarise(tests_agg=sum(Daily.Tests))
df2=data.frame(weeks=agg$weeks, agg_cases=agg$agg, agg_tests=agg2$tests_agg)
df2$pos_rate=100*(df2$agg_cases/df2$agg_tests)
df_cases_tests=melt(df2,id.vars=c("weeks","pos_rate"))

options(scipen=10000)

ggplot(df_cases_tests, aes(as.Date(weeks), value,fill=variable)) +    xlab("Week") + ylab(" ") +
scale_fill_manual(values=c("#7a9be9","#c6c2c3")) +
geom_bar(alpha=0.6,stat="identity") + scale_x_date(expand = c(0, 0),limits = as.Date(c("2020-03-31","2021-06-27")),labels = date_format("%d-%b-%y"),    breaks = "2 weeks") +
geom_line(color="#b30643",size=0.6, aes(y=20000*pos_rate)) +
scale_y_continuous( expand=c(0,0), sec.axis = sec_axis(~./20000, name = "Positivity Rate (%)")) +
theme_bw() + theme( axis.text.x = element_text(face= "bold", colour= "black" ,angle = 75,  hjust=1),
panel.grid.major.x = element_blank() ,panel.grid.minor.x =  element_blank(),panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line( size=.1, color="black" )) +
theme(legend.position = "none",
    axis.text.y.right = element_text(size=14,color = "#b30643",face= "bold"),
    axis.title = element_text(size=14,color = "#b30643",face= "bold"),
    axis.text = element_text(size=14,color="black", face= "bold"),
    axis.title.x = element_text(size=14,color="black", face= "bold") )  +
coord_cartesian(ylim = c(0, 1000000)) 
#ggsave("PLOTS/1A_cases_tests.jpg", height=5,width=10.76,dpi=400)





##Figure 1B - Hosp ICU

df=read.csv("Data/DL_Hosp_ICU",sep="\t",header=T,colClasses=c("Date",NA,NA))
options(scipen=10000)

ggplot(df, aes(as.Date(Date),  Hospitalised)) +    ylab("Number of Cases") + xlab("") +
geom_bar(width=0.8,fill="#cea0b0", alpha=0.5,stat="identity") + scale_x_date(expand = c(0, 0),limits = as.Date(c("2020-04-01","2021-06-30")),labels = date_format("%b-%y"),    breaks = "1 month") +
geom_line(color="#8b2e4f",size=1, aes(y=ICU)) +
scale_y_continuous( expand=c(0,0)) +
theme_bw() + theme( axis.text.x = element_text(face= "bold", colour= "black",angle = 35, hjust=1),
panel.grid.major.x = element_blank() ,panel.grid.minor.x =  element_blank(),panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line( size=.1, color="black" )) +
theme(legend.position = "none",
    axis.text.y.right = element_text(size=14,color = "#000000",face= "bold"),
    axis.title.y.right = element_text(size=14,color = "#000000",face= "bold"),
    axis.text = element_text(size=14,color="#000000", face= "bold"),
    axis.title.y = element_text(size=14,color = "#000000",face= "bold"),
 ) +
coord_cartesian(ylim = c(0, 25000)) 
#ggsave("PLOTS/1B_hosp_icu.jpg", height=4.3,width=10,dpi=400)

##Figure 1C - Deaths vs Cases

df=read.csv("Data/DL_Cases_Deaths",sep="\t",header=T,colClasses=c("Date",NA,NA))
options(scipen=10000)

ggplot(df, aes(as.Date(Date), Daily.DEATHS)) +    ylab("Number of Deaths") + xlab("") +
geom_bar(width=0.8,fill="#ff6666", alpha=0.5,stat="identity") + scale_x_date(expand = c(0, 0),limits = as.Date(c("2021-01-01","2021-06-30")),labels = date_format("%b-%y"),    breaks = "1 month") +
geom_line(color="#3c60b8",size=1, aes(y=Daily.CASES/60)) +
scale_y_continuous( expand=c(0,0), sec.axis = sec_axis(~.*60, name = "Number of Cases",breaks=seq(0,30000,by=5000))) +
theme_bw() + theme( axis.text.x = element_text(face= "bold", colour= "black"),
panel.grid.major.x = element_blank() ,panel.grid.minor.x =  element_blank(),panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line( size=.1, color="black" )) +
theme(legend.position = "none",
    axis.text = element_text( size=14),
    axis.text.y.right = element_text(size=14,color = "#3c60b8",face= "bold"),
    axis.title.y.right = element_text(size=14,color = "#3c60b8",face= "bold"),
    axis.text.y = element_text(size=14,color="#c04646", face= "bold"),
    axis.title.y = element_text(size=14,color = "#c04646",face= "bold"),
 ) +
coord_cartesian(ylim = c(0, 500)) 

#ggsave("PLOTS/1C_cases_deaths.jpg", height=5,width=7.5,dpi=400)


##Figure 1D - Deaths vs Cases Scaled

df=read.csv("Data/DL_Cases_Deaths_scaled",sep="\t",header=T,colClasses=c("Date",NA,NA))
options(scipen=10000)

ggplot(df, aes(as.Date(Date), Cumulative.Deaths)) +    ylab("Number of Deaths") + xlab("") +
geom_bar(width=0.8,fill="#ff6666", alpha=0.5,stat="identity") + scale_x_date(expand = c(0, 0),limits = as.Date(c("2021-01-01","2021-06-30")),labels = date_format("%b-%y"),    breaks = "1 month") +
geom_line(color="#3c60b8",size=1, aes(y=Cumulative.Cases/60)) +
scale_y_continuous( expand=c(0,0), sec.axis = sec_axis(~.*60, name = "Number of Cases", breaks=seq(0,900000,by=100000))) +
theme_bw() + theme( axis.text.x = element_text(face= "bold", colour= "black"),
panel.grid.major.x = element_blank() ,panel.grid.minor.x =  element_blank(),panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line( size=.1, color="black" )) +
theme(legend.position = "none",
    axis.text = element_text( size=14),
    axis.text.y.right = element_text(size=14,color = "#3c60b8",face= "bold"),
    axis.title.y.right = element_text(size=14,color = "#3c60b8",face= "bold"),
    axis.text.y = element_text(size=14,color="#c04646", face= "bold"),
    axis.title.y = element_text(size=14,color = "#c04646",face= "bold"),
 ) +
coord_cartesian(ylim = c(0, 16000)) 

#ggsave("PLOTS/1D_cases_deaths_scaled.jpg", height=5,width=7.5,dpi=400)
