##This code is used to generate the bar graph of monthly lineage proportions of different states analysed. For reading the data of a particular state, uncomment and edit the data input and plot title lines appropriately.

##State data file paths:
##Punjab: 		Lineages/PB/PB_lineage_Daily
##Haryana: 		Lineages/HR/HR_lineage_Daily
##Uttar Pradesh: 	Lineages/UP/UP_lineage_Daily
##Chandigarh: 		Lineages/CH/CH_lineage_Daily
##Jammu & Kashmir: 	Lineages/JK/JK_lineage_Daily
##Himachal Pradesh: 	Lineages/HP/HP_lineage_Daily
##Madhya Pradesh:	Lineages/MP/MP_lineage_Daily
##Uttarakhand: 		Lineages/UK/UK_lineage_Daily
##Ladakh: 		Lineages/LA/LA_lineage_Daily


library(ggplot2)
library(scales)
library(dplyr)
library(scales)
library(reshape2)
library(stringr)

#Read daily lineage counts data for a state
#df_lin=read.csv("Lineages/PB/PB_lineage_Daily",sep="\t",header=T, colClasses=c("Date",NA,NA,NA,NA,NA,NA,NA))

#Aggregate lineage counts to monthly
df_lin$months <- cut(df_lin[,"Date"], breaks="month")
alpha_agg <- df_lin %>% group_by(months) %>% summarise(alpha=sum(B.1.1.7))
delta_agg <- df_lin %>% group_by(months) %>% summarise(delta=sum(B.1.617.2))
kappa_agg <- df_lin %>% group_by(months) %>% summarise(kappa=sum(B.1.617.1))
B1 <- df_lin %>% group_by(months) %>% summarise(B1=sum(B.1))
B136 <- df_lin %>% group_by(months) %>% summarise(B136=sum(B.1.36))
others <- df_lin %>% group_by(months) %>% summarise(others=sum(Others))
total_agg <- df_lin %>% group_by(months) %>% summarise(total=sum(Total))

df2=data.frame(Date=alpha_agg$months)
df2$B.1.1.7=100*alpha_agg$alpha/total_agg$total
df2$B.1.617.2=100*delta_agg$delta/total_agg$total
df2$B.1.617.1=100*kappa_agg$kappa/total_agg$total
df2$B.1=100*B1$B1/total_agg$total
df2$B.1.36=100*B136$B136/total_agg$total
df2$Others=100*others$others/total_agg$total
df2$Total=total_agg$total
df2$Date=format(as.Date(df2$Date),"%b-%Y")
df2$check=ifelse(df2$Total>3,0,1)


df2[is.na(df2)] <- 0
df2$Date2=str_c(as.character(df2$Date)," (N=",df2$Total,")")


#cols=c("#40c1e1","#de5e75","#f6bd92","#927bc9","#cfcbec","#d9d9d9")
cols=c("#d9d9d9","#de5e75","#40c1e1","#927bc9","#cfcbec","#f6bd92","#FFFFFF")
lins_df_2=melt(df2,id.vars=c("Date2","check"),measure.vars =c("Others","B.1.36","B.1","B.1.617.1","B.1.617.2","B.1.1.7"))

lins_df_2$variable=factor(lins_df_2$variable, levels=c("Others","B.1.36","B.1","B.1.617.1","B.1.617.2","B.1.1.7"))
levels_date=unlist(unique(lins_df_2[c("Date2")]),F,F)
lins_df_2$Date2=factor(lins_df_2$Date2,levels=levels_date)

ggplot(lins_df_2, aes(fill=ifelse(check==0,variable,"white"), y=value, x=Date2)) + 
#ggtitle("Punjab") +
geom_bar(stat="identity") +
scale_fill_manual(values=cols) +
scale_y_continuous(expand = c(0, 0))+
theme_bw() + theme( axis.text.x = element_text(face= "bold",color= "black",angle = 50,  hjust=1),
axis.text=element_text(face= "bold",color= "black"),
axis.title=element_text(face= "bold",color= "black"),
panel.grid.major.x = element_blank() ,panel.grid.minor.x =  element_blank(),panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())  + ylab("Proportion of Genomes (%)") + xlab("") + guides(fill=guide_legend(title="PANGO Lineage")) +
theme(plot.title = element_text(size=14, face= "bold",color= "black",hjust = 0.5)) + theme(legend.position = "none") +
theme(
    axis.text = element_text(size=14,colour="black",face= "bold"),
    axis.title = element_text(size=14,colour="black",face= "bold"),
   axis.text.x = element_text(size=12,colour="black",face= "bold")
) +
theme(plot.margin = margin(10, 10, 10, 10)) 

#ggsave("PLOTS/PB.jpg",height=5.2,width=3.5,dpi=400)

