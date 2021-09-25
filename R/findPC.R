findPC<-function(sdev,method='perpendicular line',aggregate=NULL,figure=FALSE){

  if(is.unsorted(-sdev)) {
    stop("'sdev' should be sorted in decreasing order")
  }

  require(ggplot2)
  require(cowplot) # arrange multiple plots into a grid

  x<-1:length(sdev)
  eb<-data.frame(x,sdev)

  # piecewise linear model
  sse<-NULL
  for (k in 1:length(sdev)) {
    D=ifelse(x<=k,0,1)
    x2<-(x-k)*D
    sse[k]=sum(lm(sdev~x+x2)$residuals^2)
  }
  dim_plm<-which.min(sse)
  names(dim_plm)<-"Piecewise linear model"

  # first derivative
  df1<-diff(sdev,differences=1)
  den<-density(df1)
  rlev<-rle(diff(den$y)>0)$lengths
  if(length(rlev)<=2) {dim_fid<-max(which(df1<mean(df1)))+1
  } else {
  cutoff<-sum(rlev[-((length(rlev)-1):length(rlev))])
  dim_fid<-max(which(df1<den$x[cutoff]))+1 }
  names(dim_fid)<-"First derivative"

  # second derivative
  df2<-diff(sdev,differences=2)
  df2p<-df2[df2>0]
  den<-density(df2p)
  rlev<-rle(diff(den$y)>0)$lengths
  if(length(rlev)<=2) {dim_sed<-which.max(df2)+1
  } else {
  cutoff<-sum(rlev[1:2])
  dim_sed<-max(which(df2>den$x[cutoff]))+1 }
  names(dim_sed)<-"Second derivative"

  # preceding residual
  fit<-NULL;res<-NULL
  for (i in 1:(length(sdev)-2)) {
    fit[[i]]<-lm(sdev~x,data=eb[(i+1):length(sdev),])
    res[i]<-sdev[i]-predict(fit[[i]],newdata=data.frame(x=i))
  }
  den<-density(res)
  rlev<-rle(diff(den$y)>0)$lengths
  if(length(rlev)<=2) {dim_pr<-max(which(res>mean(res)))+1
  } else {
  cutoff<-sum(rlev[1:2])
  dim_pr<-max(which(res>den$x[cutoff]))+1 }
  names(dim_pr)<-"Preceding residual"

  # perpendicular line
  A<-c(1,sdev[1]);B<-c(length(sdev),sdev[length(sdev)]);Dist<-NULL
  for (i in 2:(length(sdev)-1)) {
    C<-c(i,sdev[i]);D<-cbind(rbind(A,B,C),rep(1,3))
    S<-1/2*abs(det(D));Dist[i]<-2*S/dist(rbind(A,B))
  }
  dim_perl<-which.max(Dist)
  names(dim_perl)<-"Perpendicular line"

  # k-means clustering
  dim_clu<-min(kmeans(sdev,2)$size)+1
  names(dim_clu)<-"K-means clustering"

  dim_all<-c(dim_plm,dim_fid,dim_sed,dim_pr,dim_perl,dim_clu)
  dim_mean<-floor(mean(dim_all))
  names(dim_mean)<-"mean"
  dim_median<-floor(median(dim_all))
  names(dim_median)<-"median"
  dim_mode<-as.numeric(names(which.max(table(dim_all))))
  names(dim_mode)<-"mode"


  if(method=="all"){
    ##### Plot function
    if(figure==TRUE){

      g1<-ggplot(eb,aes(x=x,y=sdev))+geom_point()+theme_bw()+
        xlab(NULL)+
        ylab("Standard deviation")+
        ggtitle("Determine the optimal number of PCs to retain under multiple metrics")+
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
        scale_x_continuous(breaks = seq(5,length(sdev),by=5),limits = c(1,length(sdev)))+
        #scale_x_continuous(breaks = 1:length(sdev),limits = c(1,length(sdev)))+
        scale_y_continuous(limits = c(0,7),labels = NULL)+
        scale_colour_brewer(palette = "Set2",name = "Method",
                            labels = c("Piecewise linear model", "First derivative", "Second derivative", "Preceding residual",
                                       "Perpendicular line", "K-means clustering"))+
        theme(legend.position = "bottom")+
        theme(legend.title = element_blank())+
        theme(panel.grid =element_blank())+
        theme(axis.ticks.y = element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line.x = element_line())

     g<-plot_grid(g1,g2,nrow = 2,align = "v",rel_heights = c(1.5,1))
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


  else if(method=="piecewise linear model"){
    if(figure==TRUE){
      eb$title1<-"Piecewise linear model"
      p1<-ggplot(eb,aes(x=x,y=sdev))+geom_point(col="orange")+theme_bw()+
        xlab("Number of PC")+ylab("Standard deviation")+
        geom_line(aes(x=x,y=predict(lm(sdev~x+pmax(0,x-dim_plm)))),col="skyblue",size=1)+
        geom_vline(xintercept=dim_plm,lty=2,col="royalblue",size=1)+
        theme(panel.grid =element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line= element_line())+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_grid(. ~ title1)+
        theme(strip.background = element_rect(fill="grey"),
              strip.text = element_text(size = 13))
      print(p1)
    }
    return(dim_plm)

  } else if(method=="first derivative"){
    if(figure==TRUE){
      eb$title2<-"First derivative"
      p2<-ggplot(eb,aes(x=x,y=sdev))+geom_point(col="orange")+theme_bw()+
        xlab("Number of PC")+ylab("Standard deviation")+
        geom_line(aes(x=x,y=sdev),data=eb[1:dim_fid,],col="skyblue",size=1)+
        geom_vline(xintercept=dim_fid,lty=2,col="royalblue",size=1)+
        theme(panel.grid =element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line= element_line())+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_grid(. ~ title2)+
        theme(strip.background = element_rect(fill="grey"),
              strip.text = element_text(size = 13))
      print(p2)
    }
    return(dim_fid)

  } else if(method=="second derivative"){
    if(figure==TRUE){
      eb$title3<-"Second derivative"
      p3<-ggplot(eb,aes(x=x,y=sdev))+geom_point(col="orange")+theme_bw()+
        xlab("Number of PC")+ylab("Standard deviation")+
        geom_line(aes(x=x,y=sdev),data=eb[c(dim_sed-1,dim_sed),],col="skyblue",size=1)+
        geom_line(aes(x=x,y=sdev),data=eb[c(dim_sed,dim_sed+1),],col="skyblue",size=1)+
        geom_vline(xintercept=dim_sed,lty=2,col="royalblue",size=1)+
        theme(panel.grid =element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line= element_line())+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_grid(. ~ title3)+
        theme(strip.background = element_rect(fill="grey"),
              strip.text = element_text(size = 13))
      print(p3)
    }
    return(dim_sed)

  } else if(method=="preceding residual"){
    if(figure==TRUE){
      eb$title4<-"Preceding residual"
      p4<-ggplot(eb,aes(x=x,y=sdev))+geom_point(col="orange")+theme_bw()+
        xlab("Number of PC")+ylab("Standard deviation")+
        geom_line(aes(x=x[dim_pr:length(sdev)],y=predict(fit[[dim_pr]],newdata=data.frame(x=dim_pr:length(sdev)))),
                  data=data.frame(x[dim_pr:length(sdev)],predict(fit[[dim_pr]],newdata=data.frame(x=dim_pr:length(sdev)))),col="skyblue",size=1)+
        geom_vline(xintercept=dim_pr,lty=2,col="royalblue",size=1)+
        theme(panel.grid =element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line= element_line())+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_grid(. ~ title4)+
        theme(strip.background = element_rect(fill="grey"),
              strip.text = element_text(size = 13))
      print(p4)
    }
    return(dim_pr)

  } else if(method=="perpendicular line"){
    if(figure==TRUE){
      #lf<-matrix(c(length(sdev)-1,sdev[length(sdev)]-sdev[1],
      #             sdev[length(sdev)]-sdev[1],1-length(sdev)),
      #           nrow = 2,byrow = T)
      #rf<-c((sdev[length(sdev)]-sdev[1])*sdev[dim_perl]+(length(sdev)-1)*dim_perl,
      #           1*sdev[length(sdev)]-sdev[1]*length(sdev))
      #po<-solve(lf,rf)

      eb$title5<-"Perpendicular line"
      p5<-ggplot(eb,aes(x=x,y=sdev))+geom_point(col="orange")+theme_bw()+
        xlab("Number of PC")+ylab("Standard deviation")+
        geom_line(aes(x=x,y=sdev),data=eb[c(1,length(sdev)),],col="skyblue",size=1)+
        geom_vline(xintercept=dim_perl,lty=2,col="royalblue",size=1)+
        theme(panel.grid =element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line= element_line())+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_grid(. ~ title5)+
        theme(strip.background = element_rect(fill="grey"),
              strip.text = element_text(size = 13))
      print(p5)
    }
    return(dim_perl)

  } else if(method=="k-means clustering"){
    if(figure==TRUE){
      eb$title6<-"K-means clustering"
      p6<-ggplot(eb,aes(x=x,y=sdev))+
        geom_point(aes(x=x,y=sdev),eb[1:dim_clu-1,],col="orange")+
        geom_point(aes(x=x,y=sdev),eb[dim_clu:length(sdev),],col="orange")+
        theme_bw()+
        xlab("Number of PC")+ylab("Standard deviation")+
        geom_vline(xintercept=dim_clu,lty=2,col="royalblue",size=1)+
        theme(panel.grid =element_blank())+
        theme(panel.border = element_blank())+
        theme(axis.line= element_line())+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_grid(. ~ title6)+
        theme(strip.background = element_rect(fill="grey"),
              strip.text = element_text(size = 13))
        print(p6)
    }
    return(dim_clu)

  } else {
    stop("'method' includes 'all','piecewise linear model','first derivative',
     'second derivative','preceding residual','perpendicular line (default)','k-means clustering' options")
  }

}
