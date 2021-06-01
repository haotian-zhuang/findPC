findPC<-function(sdev,method="All",figure=FALSE,aggregate=NULL){

  if(is.unsorted(-sdev)) {
    stop("'Standard Deviation' should be sorted in decreasing order")
  }

  require(ggplot2)

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
     g<-ggplot(eb,aes(x=x,y=sdev))+geom_point()+theme_bw()+
        xlab("Principal Component")+ylab("Standard Deviation")+
        ggtitle("Determine Optimal Number of PCs to Retain Under Multiple Metrics")+
        geom_line(aes(x=x,y=sdev))+
        geom_vline(xintercept=dim_pie,lty=2,col="red")+
        geom_label(aes(x=dim_pie+1,y=6,label="Piecewise Linear Model"),size=5,col="red")+

        geom_vline(xintercept=dim_sed+0.1,lty=2,col="tan")+
        geom_label(aes(x=dim_sed+1,y=5,label="Second Derivative (LOESS)"),size=5,col="tan")+

        geom_vline(xintercept=dim_pr-0.1,lty=2,col="blue")+
        geom_label(aes(x=dim_pr+1,y=4,label="Preceding Residual"),size=5,col="blue")+

        geom_vline(xintercept=dim_dst+0.2,lty=2,col="purple")+
        geom_label(aes(x=dim_dst+1,y=3,label="Point-Diagonal Distance"),size=5,col="purple")+

        geom_vline(xintercept=dim_clu-0.2,lty=2,col="green")+
        geom_label(aes(x=dim_clu+1,y=2,label="K-means Clustering"),size=5,col="green")+

        geom_vline(xintercept=dim_entr,lty=2,col="pink")+
        geom_label(aes(x=dim_entr-1,y=3.5,label="Information Dimension"),size=5,col="pink")
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
    return(dim_pie)
  } else if(method=="Second Derivative (LOESS)"){
    return(dim_sed)
  } else if(method=="Preceding Residual"){
    return(dim_pr)
  } else if(method=="Point-Diagonal Distance"){
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

