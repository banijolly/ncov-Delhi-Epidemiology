


df=read.csv("DL_cases_Delta",sep="\t",header=T)
df$Date=as.Date(df$Date)

library(dplyr)

## Check for normality

shapiro.test(df$New.Cases)
shapiro.test(df$DeltaPer)
shapiro.test(df$Daily.Tests)
shapiro.test(df$Test.Positivity.Rate)
shapiro.test(df$Active.Cases)
shapiro.test(df$Hospitalized)
shapiro.test(df$ICU)
shapiro.test(df$Daily.Deaths)


library("ggpubr")

## New Cases vs % of Delta

p = ggscatter(df, x = "New.Cases", y = "DeltaPer", color="black",
          add = "reg.line", add.params = list(color = "#3797d3", fill = "gray"),
          conf.int = TRUE,
          xlab = "No. of Cases", ylab = "% B.1.617.2 (Delta)")

p + theme(plot.margin = margin(10, 10, 10, 10)) + theme(axis.title=element_text(size=16,face='bold'),axis.text=element_text(size=16,face='bold')) + theme(plot.title = element_text(face= "bold",color= "black",hjust = 0.5)) + ggtitle("New Cases") + stat_cor(method = "spearman",label.x=15000,label.y=15)


#ggsave("PLOTS/DL_Cases_Delta.jpg",height=4,width=4,dpi=400)

### Hospitalizations vs % of Delta

p = ggscatter(df, x = "Hospitalized", y = "DeltaPer", color="black",
          add = "reg.line", add.params = list(color = "#f06060", fill = "gray"),
          conf.int = TRUE,
          xlab = "No. of Cases", ylab = "% B.1.617.2 (Delta)")

p + theme(plot.margin = margin(10, 10, 10, 10)) + theme(axis.title=element_text(size=16,face='bold'),axis.text=element_text(size=16,face='bold')) + theme(plot.title = element_text(face= "bold",color= "black",hjust = 0.5)) + ggtitle("Hospitalizations") + stat_cor(method = "spearman", label.x=10000, label.y=15)


#ggsave("PLOTS/DL_Hosp_Delta.jpg",height=4,width=4,dpi=400)

## ICU Admissions vs % of Delta

p = ggscatter(df, x = "ICU", y = "DeltaPer", color="black",
          add = "reg.line", add.params = list(color = "#f0608f", fill = "gray"),
          conf.int = TRUE,
          xlab = "No. of Cases", ylab = "% B.1.617.2 (Delta)")

p + theme(plot.margin = margin(10, 10, 10, 10)) + scale_x_continuous(breaks=seq(0,6000,by=2000)) + coord_cartesian(xlim=c(0,6000)) +theme(axis.title=element_text(size=16,face='bold'),axis.text=element_text(size=16,face='bold')) + theme(plot.title = element_text(face= "bold",color= "black",hjust = 0.5)) + ggtitle("ICU Admissions") + stat_cor(method = "spearman", label.x=3000,label.y=17)


#ggsave("PLOTS/DL_ICU_Delta.jpg",height=4,width=4,dpi=400)



## New Deaths vs % of Delta

p = ggscatter(df, x = "Daily.Deaths", y = "DeltaPer", color="black",
          add = "reg.line", add.params = list(color = "#591708", fill = "gray"),
          conf.int = TRUE,
          xlab = "No. of Cases", ylab = "% B.1.617.2 (Delta)")

p + theme(plot.margin = margin(10, 10, 10, 10)) + scale_y_continuous(breaks=seq(0,100,by=25)) + theme(axis.title=element_text(size=16,face='bold'),axis.text=element_text(size=16,face='bold')) + theme(plot.title = element_text(face= "bold",color= "black",hjust = 0.5)) + ggtitle("New Deaths") + stat_cor(method = "spearman",label.x=200,label.y=17)


#ggsave("PLOTS/DL_Deaths_Delta.jpg",height=4,width=4,dpi=400)
