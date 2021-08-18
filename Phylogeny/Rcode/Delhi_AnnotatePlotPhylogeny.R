library(ape)
library(tidyverse)
library(ggtree)
data=read.csv('metadata_R_final.tsv',sep="\t", header = TRUE)
tree = read.tree("Delhi.nwk")
df = data.frame(loc3=data$PANGO.Lineage)
rownames(df) = tree$tip.label
c2=c("#79bfe0", "#abd5e5","#e9cb6d", "#ffbe8a", "#efabb1", "#ca7171", "#5a6e8e",  "#937bc9", "#c8c9e9", "#afafaf", "#ffffff")
c1=c("#dadada","#27dc5e","#f9b2df","#e8ec3a")
p <- ggtree(tree, layout = "circular", size=0.15)
p <- p %<+% data + geom_tippoint(aes(color=State),alpha=0.65) + scale_colour_manual(values=c1) + scale_size(range = c(0, 2))

p1 <- gheatmap(p,df,width=.1,colnames=FALSE,color=data$loc3 ) + scale_fill_manual(values=c2) + guides(alpha=FALSE) + guides(size=FALSE) +theme(legend.position="right") 
p1
ggsave("PLOTS/Delhi_tree.jpg",height=10,width=10,dpi=400)
