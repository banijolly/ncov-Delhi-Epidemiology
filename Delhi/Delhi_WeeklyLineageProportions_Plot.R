library(ggplot2)
library(scales)
library(dplyr)
library(scales)
library(reshape2)

#Read Daily lineage count Data
df_lin=read.csv("Lineages/DL/DL_lineage_Daily",sep="\t" ,header=T,colClasses=c("Date",NA,NA,NA,NA,NA,NA,NA))

#Aggregate daily lineage counts to weeks
df_lin$weeks <- cut(df_lin[,"Date"], breaks="week")
alpha_agg <- df_lin %>% group_by(weeks) %>% summarise(alpha=sum(B.1.1.7))
delta_agg <- df_lin %>% group_by(weeks) %>% summarise(delta=sum(B.1.617.2))
kappa_agg <- df_lin %>% group_by(weeks) %>% summarise(kappa=sum(B.1.617.1))
B1 <- df_lin %>% group_by(weeks) %>% summarise(B1=sum(B.1))
B136 <- df_lin %>% group_by(weeks) %>% summarise(B136=sum(B.1.36))
others <- df_lin %>% group_by(weeks) %>% summarise(others=sum(Others))
total_agg <- df_lin %>% group_by(weeks) %>% summarise(total=sum(Total))

#Calculate weekly lineage proportions

df2=data.frame(Date=alpha_agg$weeks)
df2$B.1.1.7=100*alpha_agg$alpha/total_agg$total
df2$B.1.617.2=100*delta_agg$delta/total_agg$total
df2$B.1.617.1=100*kappa_agg$kappa/total_agg$total
df2$B.1=100*B1$B1/total_agg$total
df2$B.1.36=100*B136$B136/total_agg$total
df2$Others=100*others$others/total_agg$total
#df2$Total=total_agg$total

delhi_df=df2


del_df_2=melt(delhi_df,id.vars="Date")
#cols=c("#40c1e1","#de5e75","#f6bd92","#927bc9","#cfcbec","#d9d9d9")
cols=c("#d9d9d9","#de5e75","#40c1e1","#927bc9","#cfcbec","#f6bd92")
del_df_2[is.na(del_df_2)] <- 0


del_df_2$variable=factor(del_df_2$variable, levels=c("Others","B.1.36","B.1","B.1.617.1","B.1.617.2","B.1.1.7"))


ggplot(del_df_2, aes(x=as.Date(Date), y=value, fill=variable)) + geom_area() + scale_x_date( limits = as.Date(c("2020-03-30","2021-06-28")), expand = c(0, 0),labels = date_format("%d-%b-%y"),    breaks = "3 weeks") + scale_fill_manual(values=cols) +
scale_y_continuous(expand = c(0, 0))+
theme_bw() + theme( axis.text.x = element_text(face= "bold",color= "black",angle = 75,  hjust=1),
panel.grid.major.x = element_blank() ,panel.grid.minor.x =  element_blank(),panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())  + ylab("Proportion of Genomes (%)") + xlab("") + guides(fill=guide_legend(title="PANGO Lineage")) +
theme(legend.position = "none",
axis.text=element_text(size=14,face= "bold",color= "black"),
axis.text.x=element_text(size=12,face= "bold",color= "black"),
axis.title=element_text(size=14,face= "bold",color= "black"),
) +
theme(plot.margin = margin(10, 10, 10, 10))

#ggsave("PLOTS/DL_lineages.jpg",height=3,width=5.9,dpi=400)

