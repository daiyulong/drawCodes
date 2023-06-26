library(survival)
library(survminer)

coxPfilter=0.01                   #显著性过滤标准
inputFile="TCGA.expTime.txt"      #输入文件
setwd("E:\\linshi\\2023\\0608")      #设置工作目录

#读取输入文件
rt=read.table(inputFile, header=T, sep="\t", check.names=F, row.names=1)
rt$futime=rt$futime/365

#对基因进行循环，找出预后相关的基因
outTab=data.frame()
sigGenes=c("futime","fustat")
for(i in colnames(rt[,3:ncol(rt)])){
  #cox分析
  cox <- coxph(Surv(futime, fustat) ~ rt[,i], data = rt)
  coxSummary = summary(cox)
  coxP=coxSummary$coefficients[,"Pr(>|z|)"]
  #保留预后相关的基因
  if(coxP<coxPfilter){
    sigGenes=c(sigGenes,i)
    outTab=rbind(outTab,
                 cbind(id=i,
                       HR=coxSummary$conf.int[,"exp(coef)"],
                       HR.95L=coxSummary$conf.int[,"lower .95"],
                       HR.95H=coxSummary$conf.int[,"upper .95"],
                       pvalue=coxSummary$coefficients[,"Pr(>|z|)"])
    )
    
    #绘制生存曲线
    data=rt[,c("futime", "fustat", i)]
    colnames(data)=c("futime", "fustat", "gene")
    #获取最优cutoff
    res.cut=surv_cutpoint(data, time = "futime", event = "fustat", variables =c("gene"))
    res.cat=surv_categorize(res.cut)
    #比较高低表达生存差异
    diff=survdiff(Surv(futime, fustat) ~gene,data =res.cat)
    pValue=1-pchisq(diff$chisq, df=1)
    #print(pValue)
    if(pValue<0.001){
      pValue="p<0.001"
    }else{
      pValue=paste0("p=",sprintf("%.03f",pValue))
    }
    #输出生存曲线
    fit=survfit(Surv(futime, fustat) ~gene, data = res.cat)
    surPlot=ggsurvplot(fit,
                       data=res.cat,
                       pval=pValue,
                       pval.size=6,
                       legend.title=i,
                       legend.labs=c("high","low"),
                       xlab="Time(years)",
                       palette=c("red", "blue"),
                       break.time.by=1,
                       conf.int=T,
                       risk.table=TRUE,
                       risk.table.title="",
                       risk.table.height=.25)
    pdf(file=paste0("survival.",i,".pdf"), onefile=FALSE, width=6, height =5)
    print(surPlot)
    dev.off()
  }
}

#输出单因素的结果
write.table(outTab,file="TCGA.uniCox.txt",sep="\t",row.names=F,quote=F)

#输出单因素显著基因的表达量
uniSigExp=rt[,sigGenes]
uniSigExp=cbind(id=row.names(uniSigExp),uniSigExp)
write.table(uniSigExp,file="TCGA.uniSigExp.txt",sep="\t",row.names=F,quote=F)


############绘制森林图函数############
bioForest=function(coxFile=null,forestFile=null,forestCol=null){
  #读取输入文件
  rt <- read.table(coxFile,header=T,sep="\t",row.names=1,check.names=F)
  gene <- rownames(rt)
  hr <- sprintf("%.3f",rt$"HR")
  hrLow  <- sprintf("%.3f",rt$"HR.95L")
  hrHigh <- sprintf("%.3f",rt$"HR.95H")
  Hazard.ratio <- paste0(hr,"(",hrLow,"-",hrHigh,")")
  pVal <- ifelse(rt$pvalue<0.001, "<0.001", sprintf("%.3f", rt$pvalue))
  
  #输出图形
  height=nrow(rt)/12.5+5
  pdf(file=forestFile, width = 7,height = height)
  n <- nrow(rt)
  nRow <- n+1
  ylim <- c(1,nRow)
  layout(matrix(c(1,2),nc=2),width=c(3,2.5))
  
  #绘制森林图左边的临床信息
  xlim = c(0,3)
  par(mar=c(4,2.5,2,1))
  plot(1,xlim=xlim,ylim=ylim,type="n",axes=F,xlab="",ylab="")
  text.cex=0.8
  text(0,n:1,gene,adj=0,cex=text.cex)
  text(1.5-0.5*0.2,n:1,pVal,adj=1,cex=text.cex);text(1.5-0.5*0.2,n+1,'pvalue',cex=text.cex,font=2,adj=1)
  text(3,n:1,Hazard.ratio,adj=1,cex=text.cex);text(3,n+1,'Hazard ratio',cex=text.cex,font=2,adj=1,)
  
  #绘制森林图
  par(mar=c(4,1,2,1),mgp=c(2,0.5,0))
  xlim = c(0,max(as.numeric(hrLow),as.numeric(hrHigh)))
  plot(1,xlim=xlim,ylim=ylim,type="n",axes=F,ylab="",xaxs="i",xlab="Hazard ratio")
  arrows(as.numeric(hrLow),n:1,as.numeric(hrHigh),n:1,angle=90,code=3,length=0.05,col="darkblue",lwd=2.5)
  abline(v=1,col="black",lty=2,lwd=2)
  boxcolor = ifelse(as.numeric(hr) > 1, forestCol[1], forestCol[2])
  points(as.numeric(hr), n:1, pch = 15, col = boxcolor, cex=1.6)
  axis(1)
  dev.off()
}

#调用函数，绘制森林图
bioForest(coxFile="TCGA.uniCox.txt",forestFile="forest.pdf",forestCol=c("red","green"))

