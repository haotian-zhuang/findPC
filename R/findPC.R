findPC<-function(sdev,method="All",figure=FALSE,aggregate=NULL){

  if(is.unsorted(-sdev)) {
    stop("'Standard Deviation' should be sorted in decreasing order")
  }

  require(ggplot2)
  require(cowplot)

  x<-1:length(sdev)
  eb<-data.frame(x,sdev)

  # Piecewise Linear Model
  sse<-NULL
  for (k in 1:length(sdev)) {
    D=ifelse(x<=k,0,1)
    x2<-(x-k)*D
    sse[k]=sum(lm(sdev~x+x2)$residuals^2)
  }
  dim_pie<-which.min(sse)
  names(dim_pie)<-"Piecewise Linear Model"

  # Second Derivative (LOESS)
  yyy<-predict(loess(sdev~x,eb),data.frame(x=1:length(sdev)+0.002))
  yy<-predict(loess(sdev~x,eb),data.frame(x=1:length(sdev)+0.001))
  y<-predict(loess(sdev~x,eb),data.frame(x=1:length(sdev)))

  df1<-(yy-y)/0.001;df11<-(yyy-yy)/0.001
  df2<-(df11-df1)/0.001
  dim_sed<-which.max(df2)
  names(dim_sed)<-"Second Derivative (LOESS)"

  # Preceding Residual
  fit<-NULL;slope<-NULL;res<-NULL
  for (i in 1:(length(sdev)-2)) {
    fit[[i]]<-lm(sdev~x,data=eb[c(i+1,length(sdev)),])
    res[i]<-sdev[i]-predict(fit[[i]],newdata=data.frame(x=i))
  }
  den<-density(res)
  rlev<-rle(diff(den$y)>0)$lengths
  cutoff<-sum(rlev[1:2])
  dim_pr<-min(which(res<den$x[cutoff]))
  names(dim_pr)<-"Preceding Residual"

  # Point-Diagonal Distance
  A<-c(1,sdev[1]);B<-c(length(sdev),sdev[length(sdev)]);Dist<-NULL
  for (i in 2:(length(sdev)-1)) {
    C<-c(i,sdev[i]);D<-cbind(rbind(A,B,C),rep(1,3))
    S<-1/2*abs(det(D));Dist[i]<-2*S/dist(rbind(A,B))
  }
  dim_dst<-which.max(Dist)
  names(dim_dst)<-"Point-Diagonal Distance"

  # K-means Clustering
  dim_clu<-min(kmeans(sdev,2)$size)+1
  names(dim_clu)<-"K-means Clustering"

  # Information Dimension
  px<-sdev^2/sum(sdev^2)
  dim_entr<-floor(prod(px^-px))
  names(dim_entr)<-"Information Dimension"

  dim_all<-c(dim_pie,dim_sed,dim_pr,dim_dst,dim_clu,dim_entr)
  dim_mean<-floor(mean(dim_all))
  names(dim_mean)<-"mean"
  dim_median<-floor(median(dim_all))
  names(dim_median)<-"median"
  dim_mode<-as.numeric(names(which.max(table(dim_all))))
  names(dim_mode)<-"mode"


  if(method=="All"){
    ##### Plot function
    if(figure==TRUE){

      g1<-ggplot(eb,aes(x=x,y=sdev))+geom_point()+theme_bw()+
        xlab(NULL)+
        ylab("Standard Deviation")+
        ggtitle("Determine Optimal Number of PCs to Retain Under Multiple Metrics")+
        geom_line(aes(x=x,y=sdev))+
        scale_x_continuous(limits = c(1,length(sdev)),labels = NULL)+
        theme(panel.grid =element_blank())+
        theme(axis.ticks.x = element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line= element_line())+
        theme(plot.title = element_text(hjust = 0.5))


      plotline<-data.frame(dim_all,1:6)
      colnames(plotline)=c("Number","Method")
      g2<-ggplot(plotline,aes(x=Number,y=Method,col=as.factor(Method)))+geom_point(size=2)+theme_bw()+
        xlab("Number of PC")+ylab("Method")+
        scale_x_continuous(breaks = 1:length(sdev),limits = c(1,length(sdev)))+
        scale_y_continuous(limits = c(0,7),labels = NULL)+
        scale_color_discrete(name = "Method",
                             labels = c("Piecewise Linear Model", "Second Derivative (LOESS)", "Preceding Residual",
                                        "Point-Diagonal Distance", "K-means Clustering","Information Dimension"))+
        theme(legend.position = "bottom")+
        theme(legend.title = element_blank())+
        theme(panel.grid =element_blank())+
        theme(axis.ticks.y = element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line.x = element_line())

     g<-plot_grid(g1,g2,nrow = 2,align = "v",rel_heights = c(2,1))
     print(g)
    }
    #####

    if(is.null(aggregate)){
      return(dim_all)
    }

      else if(aggregate=="mean"){
      return(dim_mean)
    } else if(aggregate=="median"){
      return(dim_median)
    } else if(aggregate=="voting"){
  # If all the results are different, voting is median, otherwise voting is mode
  if(length(unique(dim_all))==length(dim_all)){
    return(dim_median)
  } else{
    return(dim_mode)
  }

    }
  }


  else if(method=="Piecewise Linear Model"){
    if(figure==TRUE){
      p1<-ggplot(eb,aes(x=x,y=sdev))+geom_point()+theme_bw()+
        xlab("Number of PC")+ylab("Standard Deviation")+
        geom_line(aes(x=x,y=predict(lm(sdev~x+pmax(0,x-dim_pie)))),col="red")+
        geom_vline(xintercept=dim_pie,lty=2,col="blue")+
        theme(panel.grid =element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line= element_line())
      print(p1)
    }
    return(dim_pie)

  } else if(method=="Second Derivative (LOESS)"){
    if(figure==TRUE){
      p2<-ggplot(eb,aes(x=x,y=sdev))+geom_point()+theme_bw()+
        xlab("Number of PC")+ylab("Standard Deviation")+
        geom_line(aes(x=x,y=predict(loess(sdev~x,eb))),col="red")+
        geom_vline(xintercept=dim_sed,lty=2,col="blue")+
        theme(panel.grid =element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line= element_line())
      print(p2)
    }
    return(dim_sed)

  } else if(method=="Preceding Residual"){
    if(figure==TRUE){
      p3<-ggplot(eb,aes(x=x,y=sdev))+geom_point()+theme_bw()+
        xlab("Number of PC")+ylab("Standard Deviation")+
        geom_line(aes(x=x[dim_pr:length(sdev)],y=predict(fit[[dim_pr]],newdata=data.frame(x=dim_pr:length(sdev)))),
                  data=data.frame(x[dim_pr:length(sdev)],predict(fit[[dim_pr]],newdata=data.frame(x=dim_pr:length(sdev)))),col="red")+
        geom_vline(xintercept=dim_pr,lty=2,col="blue")+
        theme(panel.grid =element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line= element_line())
      print(p3)
    }
    return(dim_pr)

  } else if(method=="Point-Diagonal Distance"){
    if(figure==TRUE){
      p4<-ggplot(eb,aes(x=x,y=sdev))+geom_point()+theme_bw()+
        xlab("Number of PC")+ylab("Standard Deviation")+
        geom_line(aes(x=x,y=sdev),data=eb[c(1,length(sdev)),],col="red")+
        geom_vline(xintercept=dim_dst,lty=2,col="blue")+
        theme(panel.grid =element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line= element_line())
      print(p4)
    }
    return(dim_dst)

  } else if(method=="K-means Clustering"){
    return(dim_clu)

  } else if(method=="Information Dimension"){
    return(dim_entr)

  } else {
    stop("'method' includes 'All (default)','Piecewise Linear Model','Second Derivative (LOESS)',
         'Preceding Residual','Point-Diagonal Distance','K-means Clustering','Information Dimension' options")
  }

}

