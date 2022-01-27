setwd("C:/Users/Tracy/Desktop/ALPL/保守性与共演化性计算")
entropy<-read.table("entropy.txt",header = F)
consurf<-read.table("consurf-grades.txt",header = T)$SCORE
co_evo<-read.table("TNAP_MI-mean.txt",header = F)

#计算相关性
cor(entropy,consurf,method = "spearman")
#-0.9126477
cor(entropy,co_evo,method = "pearson")
#0.9291851

#比较不同致病性的单位点突变的保守性&共演化性
library(openxlsx)
mutsites<-read.xlsx("../mutations-stastics.xlsx",sheet=2)
mutsites_fmt2<-data.frame(
  type=mutsites$type,
  site=gsub("\\D", "", mutsites$mutation),
  former=substr(gsub("\\d", "", mutsites$mutation),1,3),
  later=substr(gsub("\\d", "",mutsites$mutation),4,6)
)
#将保守性&共演化性与table相匹配
mutsites_fmt2[,5]<-entropy[mutsites_fmt2$site,]
mutsites_fmt2[,6]<-co_evo[mutsites_fmt2$site,]
colnames(mutsites_fmt2)[5:6]<-c("entropy","co_evolution")
mutsites_fmt2$type <- factor(mutsites_fmt2$type, levels = c("mild","severe","hSNP/asymptomatic"))

mild_entropy<-subset(mutsites_fmt2)

#作图，比较不同组别之间的差异
library(ggplot2)
library(ggpubr)

my_comparisons<-list(
  c("mild","severe"),c("severe","hSNP/asymptomatic"),
  c("mild","hSNP/asymptomatic"))

ggviolin(mutsites_fmt2, x="type", y="entropy", fill = "type", 
         add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(comparisons = my_comparisons)+ 
  xlab("")+
  ylab("Entropy")+
  guides(fill=FALSE)+
  theme(axis.text.x =element_text(size = 20),
        axis.text.y =element_text(size = 15),
        axis.title.y = element_text(size = 20))


ggviolin(mutsites_fmt2, x="type", y="co_evolution", fill = "type", 
         add = "boxplot", add.params = list(fill="white"))+ 
  stat_compare_means(comparisons = my_comparisons)+#label这里表示选择显著性标记（星号） 
  xlab("")+
  ylab("Co-evolution")+
  guides(fill=FALSE)+
  theme(axis.text.x =element_text(size = 20),
        axis.text.y =element_text(size = 15),
        axis.title.y = element_text(size = 20))

#entropy 下四分位数：0.5777158
quantile(mutsites_fmt2$entropy)
#0%       25%       50%       75%      100% 
#0.0000000 0.5777158 1.4042284 2.0246949 2.5064723 

#co-evolution 下四分位数：0.09164122
quantile(mutsites_fmt2$co_evolution)
#        0%        25%        50%        75%       100% 
#0.00000000 0.09164122 0.21416985 0.32462309 0.44935115 

write.xlsx(mutsites_fmt2,"mutation_format2.xlsx")
