#!/usr/bin/Rscript
library(openxlsx)
args<-commandArgs(TRUE)

input<-as.character(args[1])
groupfile<-as.character(args[2])
pn<-as.character(args[3])
input <- "Heatmap_wtUP_c1m8.txt"
groupfile <- "group_c1m8.txt"
pn <- "../output"

if(stringr::str_ends(input,'xlsx')){
  dd<-read.xlsx(input,1)
}else{
  dd<-read.delim(input,check.names = FALSE)
}
if(stringr::str_ends(groupfile,'xlsx')){
  gdd<-read.xlsx(groupfile,1)
}else{
  gdd<-read.delim(groupfile)
}



library(pheatmap)

if(!dir.exists(pn)){
  dir.create(pn)
}


drawHeatmap<-function(phdd, col_annotation, pn, prefix, type='comp', cuttree=NA){
  cellheight=12
  cellwidth=12
  fontsize=12
  border_color="white"
  showrowname=TRUE
  hw<-dim(phdd)
  if(hw[1]<2){
    cat("Records<2, Cannot draw heatmap\n")
    q()
  }
  
  if(hw[1]<50){
    cellheight=12
    cellwidth=12
    fontsize=12
  }else{
    cellheight=8
    fontsize=8
    cellwidth=8
    if(hw[1]/hw[2]>6){
      cellwidth<-cellheight*2
    }
    if(hw[1]/hw[2]>12){
      cellwidth<-cellheight*4
    }
    #cellwidth= hw[1]/50*6/hw[2]*cellheight
    #if(cellwidth<cellheight){
    #  cellwidth=cellheight
    #}
    #if(cellwidth>4*cellheight){
    #  cellwidth=4*cellheight
    #}
  }
  if(hw[1]>200){
    fontsize=6
  }
  if(hw[1] > 300){
    cellheight=2
    border_color=NA
    showrowname=FALSE
  }
  if(hw[1]>500){
    cellheight=1
  }
  cutreerows=cuttree
  
  pheatmap(phdd,scale="row",
           annotation_col=col_annotation, 
           cutree_rows=cutreerows,
           
           fontsize = fontsize,
           cellheight=cellheight,
           show_rownames = showrowname,
           cellwidth=cellwidth, border_color = border_color,
           clustering_distance_cols="euclidean",
           color = colorRampPalette(c("blue", "snow2", "red"))(50),
           #main='A Heat Map of Differential Expression Proteins',
           filename=paste0(pn,"/",prefix,".pheatmapcolcluster.png"))
  
  
  pheatmap(phdd,scale="row",
           annotation_col=col_annotation,
           cutree_rows=cutreerows,
           fontsize = fontsize,
           cellheight=cellheight,
           show_rownames = showrowname,
           cellwidth=cellwidth, border_color = border_color,
           clustering_distance_cols="euclidean",
           color = colorRampPalette(c("blue", "snow2", "red"))(50),
           #main='A Heat Map of Differential Expression Proteins',
           filename=paste0(pn,"/",prefix,".pheatmapcolcluster.pdf"))
  
  pheatmap(phdd,scale="row",
           annotation_col=col_annotation, 
           cutree_rows=cutreerows,
           cluster_cols = FALSE,
           fontsize = fontsize,
           cellheight=cellheight,
           show_rownames = showrowname,
           cellwidth=cellwidth, border_color = border_color,
           clustering_distance_cols="euclidean",
           color = colorRampPalette(c("blue", "snow2", "red"))(50),
           #main='A Heat Map of Differential Expression Proteins',
           filename=paste0(pn,"/",prefix,".pheatmap.png"))
  
  
  pheatmap(phdd,scale="row",
           annotation_col=col_annotation,
           cutree_rows=cutreerows,
           cluster_cols = FALSE,
           fontsize = fontsize,
           cellheight=cellheight,
           show_rownames = showrowname,
           cellwidth=cellwidth, border_color = border_color,
           clustering_distance_cols="euclidean",
           color = colorRampPalette(c("blue", "snow2", "red"))(50),
           #main='A Heat Map of Differential Expression Proteins',
           filename=paste0(pn,"/",prefix,".pheatmap.pdf"))
}
mat<-dd[,gdd[[1]]]
rownames(gdd)<-gdd[[1]]
annotationcol<-data.frame(Group=gdd[colnames(mat),][[2]])
rownames(annotationcol) <- colnames(mat)
rownames(mat)<-dd[[1]]
drawHeatmap(mat, annotationcol, pn, "Sig")
