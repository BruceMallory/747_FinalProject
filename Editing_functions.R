n=0
j=1
for (i in 1:max(Article_wdfrq$Article)){
  n=n+Article_wdfrq[j,6]/Article_wdfrq[j,7]
  j=j+nrow(Article_wdfrq[Article_wdfrq$Article==i,])
}
n/113

n=0
j=1
for (i in 1:29){
  n=Article_wdfrq[j,6]/Article_wdfrq[j,7]
  j=j+nrow(Article_wdfrq[Article_wdfrq$Article==i,])
}
n

install.packages("devtools")
devtools::install_github("kassambara/ggcorrplot")
library(ggcorrplot)
corr <- round(cor(Article_sntmts[7:13]),3)
ggcorrplot(corr, hc.order = FALSE, type = "lower",
           outline.col = "white",
           lab = TRUE)
ggplot(Article_sntmts, aes(title_score, afinn_score)) + 
  geom_point() +
  geom_smooth(method="lm", color="red") +
  geom_jitter(shape=20, size=3, color="blue") +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x=element_text(angle=45, hjust=1, vjust=1, face="bold"),
        axis.text.y=element_text(face="bold"),
        plot.title = element_text(face="bold", size=20),
        plot.subtitle = element_text(size=14),
        legend.position = "bottom",
        legend.justification = "right",
        legend.title = element_text(size=16, color = "blue"),
        legend.text = element_text(size=12, color = "blue"),
        legend.background = element_rect(colour = "blue"),
        panel.grid.minor = element_blank()) 

    
  