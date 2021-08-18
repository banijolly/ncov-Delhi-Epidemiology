library(ggplot2)
library(scales)
library(dplyr)
library(scales)
library(reshape2)


#Read Month-wise Ct values data for ORF1a (Target 1)
ct_df=read.csv("CT/DL_CT",sep="\t",header=T)
ct_df$Month=factor(ct_df$Month, levels=c("Jul 20","Aug 20","Sep 20","Oct 20","Nov 20","Dec 20","Jan 21","Feb 21","Mar 21","Apr 21","May 21","Jun 21"))


ggplot(ct_df) + geom_bar(aes (Month,Mean.Ct), stat="identity" ,width = 0.7, fill="#4f9abd",alpha=0.95) + 
geom_errorbar( aes(x=Month, ymin=Mean.Ct-SEM.Ct, ymax=Mean.Ct+SEM.Ct), width=0.1, colour="#085fa8", alpha=0.9) + 
ylab("Ct Values") +
geom_line(color="#d20707",size=0.6, aes(as.numeric(Month),y=18+(70*Fraction.of.CT.20))) + 
geom_errorbar( color="#d20707",width=0.1, aes(x=Month, ymin=18+(70*(Fraction.of.CT.20-Standard.error.of.proportions)), ymax=18+(70*(Fraction.of.CT.20+Standard.error.of.proportions)))) +
scale_y_continuous(sec.axis = sec_axis(name="Fraction with Ct < 20",~(.-18)/70), expand=c(0,0),breaks = seq(0,34,by=2)) + 
coord_cartesian(ylim = c(20, 34)) +
theme_bw() + 
theme(panel.grid.major.x = element_blank() ,panel.grid.minor.x =  element_blank(),panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line( size=.1, color="black" )) +
theme(legend.position = "none") +
theme(
    axis.title.x = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 30,  hjust=1,size=14, face="bold"),
    axis.text.y = element_text(size=14,colour="#085fa8",face= "bold"),
    axis.title.y = element_text(size=14,color = "#085fa8",face= "bold"),
    axis.text.y.right = element_text(size=14,color = "#d20707",face= "bold"),
    axis.title.y.right = element_text(size=14,color = "#d20707",face= "bold") ) 
#ggsave("PLOTS/CTvalue.jpg",height=3.5,width=6,dpi=400)

## Read E Gene Data (Target 2)
ct_df=read.csv("CT/DL_CT_Egene",sep="\t",header=T)
ct_df$Month=factor(ct_df$Month, levels=c("Jul 20","Aug 20","Sep 20","Oct 20","Nov 20","Dec 20","Jan 21","Feb 21","Mar 21","Apr 21","May 21","Jun 21"))

ggplot(ct_df) + geom_bar(aes (Month,Mean.Ct), stat="identity" ,width = 0.7, fill="#4f9abd",alpha=0.95) + 
geom_errorbar( aes(x=Month, ymin=Mean.Ct-SEM.Ct, ymax=Mean.Ct+SEM.Ct), width=0.1, colour="#085fa8", alpha=0.9) + 
ylab("Ct Values") +
geom_line(color="#d20707",size=0.6, aes(as.numeric(Month),y=18.5+(60*Fraction.of.CT.20))) + 
geom_errorbar( color="#d20707",width=0.1, aes(x=Month, ymin=18.5+(60*(Fraction.of.CT.20-Standard.error.of.proportions)), ymax=18.5+(60*(Fraction.of.CT.20+Standard.error.of.proportions)))) +
scale_y_continuous(sec.axis = sec_axis(name="Fraction with Ct < 20",~(.-18.5)/60), expand=c(0,0),breaks = seq(0,34,by=2)) + 
coord_cartesian(ylim = c(20, 32)) +
theme_bw() + 
theme(panel.grid.major.x = element_blank() ,panel.grid.minor.x =  element_blank(),panel.grid.minor.y = element_blank(), panel.grid.major.y = element_line( size=.1, color="black" )) +
theme(legend.position = "none") +
theme(
    axis.title.x = element_text(size=14, face="bold"),
    axis.text.x = element_text(angle = 30,  hjust=1,size=14, face="bold"),
    axis.text.y = element_text(size=14,colour="#085fa8",face= "bold"),
    axis.title.y = element_text(size=14,color = "#085fa8",face= "bold"),
    axis.text.y.right = element_text(size=14,color = "#d20707",face= "bold"),
    axis.title.y.right = element_text(size=14,color = "#d20707",face= "bold") ) 
#ggsave("PLOTS/CTvalue_Egene.jpg",height=3.5,width=6,dpi=400)


