#!/usr/bin/env Rscript
#version:1.01
#modified at 2021-06-29

ARGV<-commandArgs(TRUE)

if(length(ARGV)<3){
  cat("Usage: dVenn.R input1 input2[input3, input4, input5] output_prefix Categorynames")
  q()
}

library(VennDiagram)
library(RColorBrewer)
library(ggsci)
library(eulerr)
library(ggplot2)
mycols<-brewer.pal(8,"Dark2")
#mycols<-pal_jama("default")(7)

n<-length(ARGV)


if(n==4){
  input1<-as.character(ARGV[1])
  input2<-as.character(ARGV[2])
  output<-as.character(ARGV[3])
  cn<-as.character(ARGV[4])
  if(FALSE){
    input1<-'A.tab'
    input2<-'B.tab'
    output<-'Venn.2set'
  }
  categorynames<-stringr::str_split(cn,';')[[1]]
  A<-as.data.frame(read.delim(input1,quote="",stringsAsFactors = FALSE))
  B<-as.data.frame(read.delim(input2,quote="",stringsAsFactors = FALSE))
  CommonAcc<-intersect(A[[1]],B[[1]])
  CommonAcc<-as.data.frame(CommonAcc)
  names(CommonAcc)<-"CommonID"
  if(length(CommonAcc)>0){
    write.table(CommonAcc,file=paste0(output,".CommonID.xls"), row.names = FALSE,sep="\t",quote=FALSE)
  }
  
  
  venn.plot <- venn.diagram(
    x = list(
      S1 = A[[1]],
      S2 = B[[1]]
    ),
    filename = paste0(output,".tif"),
    lwd = 4,
    #fill = c("firebrick1", "steelblue"),
    category.names=categorynames,
    na="remove",
    fill = mycols[1:2],
    alpha = 0.75,
    label.col = "white",
    cex = 2.5,
    fontfamily = "serif",
    fontface = "bold",
    cat.col = mycols[1:2],
    #cat.col = c("firebrick1", "steelblue"),
    cat.cex = 2.5,
    cat.fontfamily = "serif",
    cat.fontface = "bold",
    cat.dist = c(0.05, 0.05),
    cat.pos = c(-20, 14),
    margin = 0.1
  )
  
  UnionID<-union(A[[1]],B[[1]])
  dd<-data.frame(ID=UnionID)
  dd$A<-FALSE
  dd$A[dd$ID %in% A[[1]]]<-TRUE
  dd$B<-FALSE
  dd$B[dd$ID %in% B[[1]]]<-TRUE
  write.table(dd, file=paste0(output,".merge.xls"), row.names=FALSE, sep="\t", quote=FALSE)
 
}else if(n==5){
  input1<-as.character(ARGV[1])
  input2<-as.character(ARGV[2])
  input3<-as.character(ARGV[3])
  output<-as.character(ARGV[4])
  cn<-as.character(ARGV[5])
  if(FALSE){
    input1<-'/home/bpadmin/project/BP2021582/BP2021582_fc1.2/CshCIRBP.vs.CU6/DESelection/Up.xls'
    input2<-'/home/bpadmin/project/BP2021582/BP2021582_fc1.2/DU6.vs.CU6/DESelection/Up.xls'
    input3<-'/home/bpadmin/project/BP2021582/BP2021582_fc1.2/DshCIRBP.vs.DU6/DESelection/Up.xls'
    output<-'/home/bpadmin/project/BP2021582/BP2021582_fc1.2/Venn/up'
    cn<-"CshCIRBP.vs.CU6;DU6.vs.CU6;DshCIRBP.vs.DU6"
  }
  categorynames<-stringr::str_split(cn,';')[[1]]
  A<-as.data.frame(read.delim(input1,quote="",stringsAsFactors = FALSE))
  B<-as.data.frame(read.delim(input2,quote="",stringsAsFactors = FALSE))
  C<-as.data.frame(read.delim(input3,quote="",stringsAsFactors = FALSE))
  CommonAcc<-intersect(A[[1]],B[[1]])
  CommonAcc<-intersect(CommonAcc,C[[1]])
  CommonAcc<-as.data.frame(CommonAcc)
  if(NROW(CommonAcc)>0){
    names(CommonAcc)<-"CommonID"
    write.table(CommonAcc,paste0(output,".CommonAcc.xls"),quote=FALSE,row.names = FALSE,sep="\t")
  }
  
  
  venn.plot <- venn.diagram(
    x = list(
      S1 = A[[1]],
      S2 = B[[1]],
      S3 = C[[1]]
    ),
    #scaled=TRUE,
    category.names=categorynames,
    filename = paste0(output,".tif"),
    euler.d=TRUE,
    scaled=TRUE,
    lwd = 5,
    col = "black",
    fill = mycols[1:3],
    alpha = 0.5,
    #label.col = c("darkred", "white", "darkblue", "white",
    #              "white", "white", "darkgreen"),
    label.col = "white",
    cex = 1.5,
    fontfamily = "serif",
    fontface = "bold",
    cat.col=mycols[1:3],
    #cat.col = c("darkred", "darkblue", "darkgreen"),
    cat.cex = 1.2,
    cat.fontfamily = "serif",
    cat.dist = c(0.08, 0.08, 0.08),
    margin = 0.1
  )

  UnionID<-union(A[[1]],B[[1]])
  UnionID<-union(UnionID,C[[1]])
  dd<-data.frame(ID=UnionID)
  dd[,categorynames[1]]<-FALSE
  dd[dd$ID %in% A[[1]], categorynames[1]]<-TRUE
  dd[,categorynames[2]]<-FALSE
  dd[dd$ID %in% B[[1]], categorynames[2]]<-TRUE
  dd[,categorynames[3]]<-FALSE
  dd[dd$ID %in% C[[1]], categorynames[3]]<-TRUE
  write.table(dd, file=paste0(output,".merge.xls"), row.names=FALSE, sep="\t", quote=FALSE)
  
  p<-plot(euler(dd[,2:4],shape="ellipse"),
       fills = list(#fill = c("#fbb4ae", "#b3cde3", "#ccebc5"), alpha = 0.6),
       fill = mycols[1:3], alpha = 0.5),
       labels = list(col = "black", font = 8), 
       edges = list(lex=5),
       quantities = TRUE
       )
  ggsave(paste0(output,".eulerEllipse.png"),plot=p,width=6,height=6,dpi=300,units="in")
  ggsave(paste0(output,".eulerEllipse.pdf"),plot=p,width=6,height=6,dpi=300,units="in")
  
  
  p2<-plot(euler(dd[,2:4]),
       fills = list(#fill = c("#fbb4ae", "#b3cde3", "#ccebc5"), alpha = 0.6),
         fill = mycols[1:3], alpha = 0.5),
       labels = list(col = "black", font = 6),
       edges = list(lex=5),
       quantities = TRUE
  )
  ggsave(paste0(output,".euler.png"),plot=p2,width=6,height=6,dpi=300,units="in")
  ggsave(paste0(output,".euler.pdf"),plot=p2,width=6,height=6,dpi=300,units="in")

}else if(n==6){
  input1<-as.character(ARGV[1])
  input2<-as.character(ARGV[2])
  input3<-as.character(ARGV[3])
  input4<-as.character(ARGV[4])
  output<-as.character(ARGV[5])
  cn<-as.character(ARGV[6])
  
  if(FALSE){
    input1<-'A.tab'
    input2<-'B.tab'
    input3<-'C.tab'
    input4<-'D.tab'
    output<-'Venn.4Set'
  }
  categorynames<-stringr::str_split(cn,';')[[1]]
  
  S1<-as.data.frame(read.table(input1,header=TRUE,sep="\t",quote="",stringsAsFactors=FALSE))
  S2<-as.data.frame(read.table(input2,header=TRUE,sep="\t",quote="",stringsAsFactors=FALSE))
  S3<-as.data.frame(read.table(input3,header=TRUE,sep="\t",quote="",stringsAsFactors=FALSE))
  S4<-as.data.frame(read.table(input4,header=TRUE,sep="\t",quote="",stringsAsFactors=FALSE))
  
  print(length(S4[[1]]))
  
  S12<-intersect(S1[[1]],S2[[1]])
  S123<-intersect(S12,S3[[1]])
  CommonAcc<-intersect(S123,S4[[1]])
  CommonAcc<-as.data.frame(CommonAcc)
  
  #print(CommonAcc)
  if(length(CommonAcc)>0){
    names(CommonAcc)<-"CommonID"
    write.table(CommonAcc,file=paste(output,".CommonAcc.xls",sep=""),row.names = FALSE,sep="\t",quote=FALSE)
  }
  venn.diagram(
    x = list(
      A = S1[[1]],
      B = S2[[1]],
      C = S3[[1]],
      D = S4[[1]]
    ),
    filename = paste0(output,".tiff"),
    lwd = 5,
    col = "black",
    category.names=categorynames,
    #fill = c("cornflowerblue", "green", "yellow", "darkorchid1"),
    fill = mycols[1:4],
    alpha = 0.50,
    #label.col = c("orange", "white", "darkorchid4", "white", "white", "white", "white", "white", "darkblue", "white", "white", "white", "white", "darkgreen", "white"),
    label.col = "white",
    cex = 1.5,
    fontfamily = "serif",
    fontface = "bold",
    #cat.col = c("darkblue", "darkgreen", "orange", "darkorchid4"),
    cat.col= mycols[1:4],
    cat.cex = 1.5,
    cat.pos = 0,
    # 	cat.dist = c(0.05,0.07,0.07,0.08),
    cat.fontfamily = "serif",
    # 	#rotation.degree = 270,
    margin = 0.1
    # 	cat.default.pos='outer',
  )
  
  UnionID<-union(S1[[1]],S2[[1]])
  UnionID<-union(UnionID,S3[[1]])
  UnionID<-union(UnionID,S4[[1]])
  dd<-data.frame(ID=UnionID)
  dd[,categorynames[1]]<-FALSE
  dd[dd$ID %in% S1[[1]],categorynames[1]]<-TRUE
  dd[,categorynames[2]]<-FALSE
  dd[dd$ID %in% S2[[1]],categorynames[2]]<-TRUE
  dd[,categorynames[3]]<-FALSE
  dd[dd$ID %in% S3[[1]],categorynames[3]]<-TRUE
  dd[,categorynames[4]]<-FALSE
  dd[dd$ID %in% S4[[1]],categorynames[4]]<-TRUE
  write.table(dd, file=paste0(output,".merge.xls"), row.names = FALSE, sep="\t", quote=FALSE)

  p<-plot(euler(dd[,2:5],shape="ellipse"),
       fills = list(#fill = c("#fbb4ae", "#b3cde3", "#ccebc5"), alpha = 0.6),
         fill = mycols[1:4], alpha = 0.5),
       labels = list(col = "black", font = 12), 
       edges = list(lex=5),
       quantities = TRUE
  )
  ggsave(paste0(output,".eulerEllipse.png"),plot=p, width=6, height=6, dpi=300, units="in")
  ggsave(paste0(output,".eulerEllipse.pdf"),plot=p, width=6, height=6, dpi=300, units="in")
  
  p2<-plot(euler(dd[,2:5]),
       fills = list(#fill = c("#fbb4ae", "#b3cde3", "#ccebc5"), alpha = 0.6),
         fill = mycols[1:4], alpha = 0.5),
       labels = list(col = "black", font = 12), 
       edges = list(lex=5),
       quantities = TRUE
  )
  ggsave(paste0(output,".euler.png"),plot=p2, width=6, height=6, dpi=300, units="in")
  ggsave(paste0(output,".euler.pdf"),plot=p2, width=6, height=6, dpi=300, units="in")
  
}else if(n==7){
  input1<-as.character(ARGV[1])
  input2<-as.character(ARGV[2])
  input3<-as.character(ARGV[3])
  input4<-as.character(ARGV[4])
  input5<-as.character(ARGV[5])
  output<-as.character(ARGV[6])
  cn<-as.character(ARGV[7])
  
  if(FALSE){
    input1="A.tab"
    input2="B.tab"
    input3="C.tab"
    input4="D.tab"
    input5="E.tab"
    output<-'Venn.5set'
  }
  categorynames<-stringr::str_split(cn,';')[[1]]
  #categorynames<-c("A","B","C","D","E")
  S1<-as.data.frame(read.table(input1,header=TRUE,sep="\t",quote="",stringsAsFactors=FALSE))
  S2<-as.data.frame(read.table(input2,header=TRUE,sep="\t",quote="",stringsAsFactors=FALSE))
  S3<-as.data.frame(read.table(input3,header=TRUE,sep="\t",quote="",stringsAsFactors=FALSE))
  S4<-as.data.frame(read.table(input4,header=TRUE,sep="\t",quote="",stringsAsFactors=FALSE))
  S5<-as.data.frame(read.table(input5,header=TRUE,sep="\t",quote="",stringsAsFactors=FALSE))
  
  S12<-intersect(S1[[1]],S2[[1]])
  S123<-intersect(S12,S3[[1]])
  S1234<-intersect(S123,S4[[1]])
  CommonAcc<-intersect(S1234,S5[[1]])
  CommonAcc<-as.data.frame(CommonAcc)
  print(CommonAcc)
  if(length(CommonAcc)>0){
    write.table(CommonAcc,file=paste(output,".CommonAcc.xls",sep=""),row.names = FALSE,sep="\t")
  }
  venn.plot <- venn.diagram(
    x = list(
      A = S1[[1]],
      B = S2[[1]],
      C = S3[[1]],
      D = S4[[1]],
      E = S5[[1]]
    ),
    filename = paste0(output,'.tif'),
    lwd = 4,
    col = "black",
    category.names=categorynames,
    #fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
    fill = mycols[1:5],
    alpha = 0.30,
    cex = c(1.5, 1.5, 1.5, 1.5, 1.5, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8,
            1, 0.8, 1, 0.55, 1, 0.55, 1, 0.55, 1, 0.55, 1, 0.55, 1, 1, 1, 1, 1, 1.5),
    #cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
    cat.col = mycols[1:5],
    cat.cex = 1,
    cat.fontface = "bold",
    margin = 0.3
  )
  
  UnionID<-union(S1[[1]],S2[[1]])
  UnionID<-union(UnionID,S3[[1]])
  UnionID<-union(UnionID,S4[[1]])
  UnionID<-union(UnionID,S5[[1]])
  dd<-data.frame(ID=UnionID)
  dd[,categorynames[1]]<-FALSE
  dd[dd$ID %in% S1[[1]],categorynames[1]]<-TRUE
  dd[,categorynames[2]]<-FALSE
  dd[dd$ID %in% S2[[1]],categorynames[2]]<-TRUE
  dd[,categorynames[3]]<-FALSE
  dd[dd$ID %in% S3[[1]],categorynames[3]]<-TRUE
  dd[,categorynames[4]]<-FALSE
  dd[dd$ID %in% S4[[1]],categorynames[4]]<-TRUE
  dd[,categorynames[5]]<-FALSE
  dd[dd$ID %in% S5[[1]],categorynames[5]]<-TRUE
  write.table(dd, file=paste0(output,".merge.xls"), row.names = FALSE, sep="\t", quote=FALSE)
  #pdf(paste0(output,".eulerEllipse.pdf"),width=8,height=6)
  p<-plot(euler(dd[,2:6],shape="ellipse"),
       fills = list(#fill = c("#fbb4ae", "#b3cde3", "#ccebc5"), alpha = 0.6),
         fill = mycols[1:5], alpha = 0.5),
       labels = list(col = "#DCDCDC", font = 12), 
       edges = list(lex=4),
       quantities = TRUE
  )
  ggsave(paste0(output,".eulerEllipse.pdf"),plot=p, width=6, height=6, dpi=300, units="in")
  ggsave(paste0(output,".eulerEllipse.png"),plot=p, width=6, height=6, dpi=300, units="in")

  p2<-plot(euler(dd[,2:6]),
       fills = list(#fill = c("#fbb4ae", "#b3cde3", "#ccebc5"), alpha = 0.6),
         fill = mycols[1:5], alpha = 0.5),
       labels = list(col = "#DCDCDC", font = 12), 
       edges = list(lex=4),
       quantities = TRUE
  )
  ggsave(paste0(output,".euler.pdf"),plot=p2, width=6, height=6, dpi=300, units="in")
  ggsave(paste0(output,".euler.png"),plot=p2, width=6, height=6, dpi=300, units="in")

}else{
  cat("Max file is 5")
}