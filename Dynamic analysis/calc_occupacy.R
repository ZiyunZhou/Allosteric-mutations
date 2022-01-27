setwd("D:/ALPL/低磷血症表型相关ALPL错义突变的分子特征和变构调控机制-周子筠/~ 实验过程/12 最短路径/2 MD-TASK result/S-M355I")
library(igraph)
#读入每一帧构建的网络
edgelist<-c()
for (i in 0:1001) {
  assign(paste("frame",i,sep = ""),
         read_graph(paste(paste("355_",i,sep=""),"_graph.gml",sep=""),format="gml"))
  edgelist<-rbind(edgelist,as_edgelist(get(paste("frame",i,sep = ""))))
}

#根据所有的边构建网络，注：edgelist必须是matrix格式
g = graph_from_edgelist(edgelist, directed = "FALSE")
#创建邻接矩阵，除以总帧数，计算边出现的频率
#设置sparse为F,保存未经解析的matrix格式
#该矩阵沿对角线对称【无向图】
adjMat = as_adjacency_matrix(g,sparse = FALSE)
adjMat = (adjMat/1002) 
#筛选，剔除掉出现频率小于50%的边
# for(i in 1:nrow(adjMat)){
#   for(j in 1:ncol(adjMat)){
#     if(adjMat[i,j]<0.5)
#       adjMat[i,j]=0
#   }
# }

#根据邻接矩阵创建最终的网络图，权重为边出现的频率
g = graph_from_adjacency_matrix(adjMat, weighted = T, mode = "undirected")

write_graph(g,"../M355I_occupacy_network.ncol",format = "ncol")
#下一步用resid+resname对顶点进行重命名
