# Plot Heatmap by ComplexHeatmap package
ARGV<-commandArgs(TRUE)

if(length(ARGV)!=3){
  cat("\nUsage: Rscript drawComplexHeatmap.R input groupFile outpath\n")
  q()
}

library(ComplexHeatmap)
library(openxlsx)
library(stringr)
library(circlize)
library(RColorBrewer)
library(Cairo)

n <- length(ARGV)

if (n==3){
  input <- as.character(ARGV[1])
  groupFile <- as.character(ARGV[2])
  outpath <- as.character(ARGV[3])
  if (FALSE){
  input <- "E:/projects_2022/shouhou/1/ALL.xlsx"
  groupFile <- "E:/projects_2022/shouhou/1/group_ALL.txt"
  outpath <- "E:/projects_2022/shouhou/1"
}

}


if(stringr::str_ends(input,'xlsx')){
  dat<-read.xlsx(input,1)
}else{
  dat<-read.delim(input,check.names = FALSE)
}

if(stringr::str_ends(groupFile,'xlsx')){
  dg<-read.xlsx(groupFile,1)
}else{
  dg<-read.delim(groupFile,check.names = FALSE)
}


if (!dir.exists(outpath)) {
  dir.create(outpath)
}

# 绘图函数
drawComplexHeatmap <- function(mat,anCol,pn,type){
  rownames(mat) <- mat[[1]]
  dat <- mat[,-1]
  dat[is.na(dat)] <- 0        # 如果输入文件中含有缺失值，则会将其替换成0值              
  
  d1 <- dat[,which(anCol[[1]] %in% colnames(dat))]
  d2 <- as.data.frame(dat[,-which(anCol[[1]] %in% colnames(dat))])
  rownames(d2) <- rownames(dat)
  colnames(d2) <- colnames(dat)[-which(anCol[[1]] %in% colnames(dat))]
  
  rownames(anCol) <- anCol[[1]]
  colAnnot <- anCol[2]
  
  
  if (type=="scale") {
    dat1 <- t(apply(d1,1,scale))
    colnames(dat1) <- colnames(d1)
    
    colfun <- colorRamp2(c(-4,0,4),c("blue", "snow2", "red"))
  }else if(type=="-log2"){
    
    dat1 <- as.matrix(-log2(d1+0.000001))
    colfun <- colorRamp2(c(0,round(max(dat1),digits = 0)),c("#FFFF33", "#E41A1C"))
  }else if(type=="-log10"){
    
    dat1 <- as.matrix(-log10(d1+0.000001))
    colfun <- colorRamp2(c(0,round(max(dat1),digits = 0)),c("#FFFF33", "#E41A1C"))
  }else if(type=="log2"){
    
    dat1 <- as.matrix(log2(d1+0.000001))
    colfun <- colorRamp2(c(0,round(max(dat1),digits = 0)),c("#FFFF33", "#E41A1C"))
  }else if(type=="log10"){
    
    dat1 <- as.matrix(log10(d1+0.000001))
    colfun <- colorRamp2(c(0,round(max(dat1),digits = 0)),c("#FFFF33", "#E41A1C"))
  }
  
  
  
  
  dat2 <- as.matrix(d2)
  
  
  mycolor <- brewer.pal(9,"Set1")
  mycolor <- c(mycolor,mycolor)
  groupColor <- mycolor[1:length(unique(dg$group))]
  names(groupColor)<-unique(dg$group)
  
 
  colfun2 <- colorRamp2(c(0,1),c("white", "white"))
  
  # 列（sample）的分组注释
  ha <- HeatmapAnnotation(group=colAnnot[[1]],col=list(group=groupColor),
                          annotation_name_side = "left", 
                          simple_anno_size = unit(0.3, "cm"))
  
  # Heatmap 1 参数设置
  p1 <- Heatmap(dat1,col = colfun,
          name = " ",
          cluster_rows = T, row_dend_gp = gpar(col="black"),      # 是否行聚类 & 聚类树颜色
          cluster_columns = T,column_dend_gp = gpar(col="black"),  # 是否列聚类 & 聚类树颜色
          row_names_gp = gpar(fontsize=6),                       # 行名字体设置
          column_names_gp = gpar(fontsize=7),                    # 列名字体设置
          column_dend_height = unit(1.5, "cm"),                  # 列树状图的高度
          top_annotation = ha,                                   # 列分组注释的位置（top_annotation,bottom_annotation...)
          na_col = "black",
          rect_gp = gpar(col="gray",lwd=0.5),                      # 小方格边界线颜色 & 线宽度
          clustering_distance_rows = "euclidean",                # 聚类距离方法：pearson，spearman,kendall
          heatmap_legend_param = list(direction = "vertical")    # 图例显示方式：水平 & 垂直
          )
  
  # Heatmap 2 方格中文本显示参数设置
  cell_fun <- function(j, i, x, y, width, height, fill) {
    if (dat2[i, j] < 0.01) {
      grid.text("**",x, y,
                gp = gpar(fontsize = 12))
    }else if(0.05 > dat2[i, j]){
      grid.text("*",x, y,
                gp = gpar(fontsize = 12))
    }
  }
  
  p2 <- Heatmap(dat2,col = colfun2,
               show_heatmap_legend = F,
                cluster_rows = T,
                row_dend_gp = gpar(col="black"),
                cluster_columns = F,
                width = unit(4*ncol(dat2),"mm"),
                row_names_gp = gpar(fontsize=6),
                column_names_gp = gpar(fontsize=7),
                rect_gp = gpar(col="gray",lwd=0.5),
                cell_fun = cell_fun
                )
  Cairo::CairoPNG(paste0(pn,"/",type,"-ComplexHeatmap.png"),width = 7,height = 7,units = "in",dpi = 300)
  draw(p1+p2,heatmap_legend_side = "right", 
       annotation_legend_side = "right")
  dev.off()

  Cairo::CairoPDF(paste0(pn,"/",type,"-ComplexHeatmap.pdf"),width = 7,height = 7)
  draw(p1+p2,heatmap_legend_side = "right", 
       annotation_legend_side = "right")
  dev.off()
  
  
# Heatmap 1 不做列聚类

  p3 <- Heatmap(dat1,col = colfun,
              name = " ",
              cluster_rows = T, row_dend_gp = gpar(col="black"),      # 是否行聚类 & 聚类树颜色
              cluster_columns = F,                                   # 是否列聚类 
              row_names_gp = gpar(fontsize=6),                       # 行名字体设置
              column_names_gp = gpar(fontsize=7),                    # 列名字体设置
              column_dend_height = unit(1.5, "cm"),                  # 列树状图的高度              
              top_annotation = ha,                                   # 列分组注释的位置（top_annotation,bottom_annotation...)
              na_col = "black",
              rect_gp = gpar(col="gray",lwd=0.5),                      # 小方格边界线颜色 & 线宽度
              clustering_distance_rows = "euclidean",                # 聚类距离方法：pearson，spearman,kendall
              heatmap_legend_param = list(direction = "vertical")    # 图例显示方式：水平 & 垂直
              

  )
  Cairo::CairoPNG(paste0(pn,"/",type,"-ComplexHeatmap-noCol.png"),width = 7,height = 7,units = "in",dpi = 300)
  draw(p3+p2,heatmap_legend_side = "right", 
       annotation_legend_side = "right")
  dev.off()
  
  Cairo::CairoPDF(paste0(pn,"/",type,"-ComplexHeatmap-noCol.pdf"),width = 7,height = 7)
  draw(p3+p2,heatmap_legend_side = "right", 
       annotation_legend_side = "right")
  dev.off()
}

# drawComplexHeatmap(mat,anCol,pn,type)
# mat: input file
# anCol: samples group file
# pn: output path
# type: data transform methods(scale,log2,log10,-log2,-log10)

drawComplexHeatmap(dat,dg,outpath,"scale")
drawComplexHeatmap(dat,dg,outpath,"-log2")
drawComplexHeatmap(dat,dg,outpath,"-log10")





