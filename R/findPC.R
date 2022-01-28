#' findPC
#'
#' Automatically select the number of principal components in single-cell analysis
#'
#' findPC returns a list of results for one or multiple methods with different total numbers of PCs.
#' It also supports the aggregation of results from multiple methods and total numbers of PCs by taking the average, median, or mode.
#' For users who want to visually inspect the results, findPC can also display the elbow plot with the automatically identified elbow points.
#'
#' @param sdev A vector of the standard deviations explained by each principal component
#' @param number A vector including number of PCs used in the function
#' @param method Methods include 'all', 'piecewise linear model', 'first derivative', 'second derivative', 'preceding residual', 'perpendicular line (default)', 'k-means clustering'
#' @param aggregate An option to report the mean, median, or voting (median if all are different, otherwise mode) of the results
#' @param figure Whether to draw a heatmap showing the results
#'
#' @return A numeric value (matrix) including the number of PCs.
#' @export
#' @import ggplot2 reshape2 patchwork grid
#' @author Haotian Zhuang, Zhicheng Ji <haotian.zhuang@@duke.edu>
#' @examples
#' Return the default result (Perpendicular line with 30 PCs)
#' findPC(sdev = sdev)
#'
#' Return the results based on Piecewise linear model with 16,20 and 28 PCs
#' findPC(sdev = sdev,number = c(16,20,28),method = 'piecewise linear model')
#'
#' Return the results based on six methods with 16,20 and 28 PCs
#' findPC(sdev = sdev,number = c(16,20,28),method = 'all')
#'
#' Return voting (median if all are different, otherwise mode) of the results from six methods with 16,20 and 28 PCs
#' findPC(sdev = sdev,number = c(16,20,28),method = 'all',aggregate = 'voting')
#'
#' Return a heatmap based on six methods with 16,20 and 28 PCs
#' findPC(sdev = sdev,number = c(16,20,28),method = 'all',figure = T)

findPC<-function(sdev,number=30,method='perpendicular line',aggregate=NULL,figure=FALSE){

  if(is.unsorted(-sdev)) {
    stop("'sdev' should be sorted in decreasing order")
  }

  if(max(number)>length(sdev)) {
    stop("'number' exceeds the available number of PCs")
  }

  l=length(number)
  result<-matrix(NA,l,6);rownames(result)<-1:l
  for (i in 1:l) {
    result[i,]<-fun(sdev=sdev[1:number[i]])
    rownames(result)[i]<-paste0(number[i],'PCs')
  }
  colnames(result)<-c('Piecewise linear model','First derivative','Second derivative',
                      'Preceding residual','Perpendicular line','K-means clustering')

  if(method=='all'){
    if(figure==T) fig(sdev = sdev,result = result)

  } else if(method=='piecewise linear model'){
    result=result[,1,drop=F]
    if(figure==T) fig(sdev = sdev,result = result)

  } else if(method=='first derivative'){
    result=result[,2,drop=F]
    if(figure==T) fig(sdev = sdev,result = result)

  } else if(method=='second derivative'){
    result=result[,3,drop=F]
    if(figure==T) fig(sdev = sdev,result = result)

  } else if(method=='preceding residual'){
    result=result[,4,drop=F]
    if(figure==T) fig(sdev = sdev,result = result)

  } else if(method=='perpendicular line'){
    result=result[,5,drop=F]
    if(figure==T) fig(sdev = sdev,result = result)

  } else if(method=='k-means clustering'){
    result=result[,6,drop=F]
    if(figure==T) fig(sdev = sdev,result = result)

  } else {
    stop("'method' includes 'all','piecewise linear model',
    'first derivative','second derivative','preceding residual',
    'perpendicular line (default)','k-means clustering' options")}

  dim_mean<-round(mean(result))
  names(dim_mean)<-'mean'
  dim_median<-round(median(result))
  names(dim_median)<-'median'
  dim_mode<-as.numeric(names(which.max(table(result))))
  names(dim_mode)<-'mode'

  if(is.null(aggregate)){
    return(result)
  } else if(aggregate=='mean'){
    return(dim_mean)
  } else if(aggregate=='median'){
    return(dim_median)
  } else if(aggregate=='voting'){
    # If all the results are different, voting is median, otherwise voting is mode
    if(length(unique(as.vector(result)))==length(result)){
      return(dim_median)
    } else{
      return(dim_mode)
    }
  }
}


fun<-function(sdev){

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
  names(dim_plm)<-'Piecewise linear model'

  # first derivative
  df1<-diff(sdev,differences=1)
  den<-density(df1)
  rlev<-rle(diff(den$y)>0)$lengths
  if(length(rlev)<=2) {dim_fid<-max(which(df1<mean(df1)))+1
  } else {
    cutoff<-sum(rlev[-((length(rlev)-1):length(rlev))])
    dim_fid<-max(which(df1<den$x[cutoff]))+1 }
  names(dim_fid)<-'First derivative'

  # second derivative
  df2<-diff(sdev,differences=2)
  df2p<-df2[df2>0]
  den<-density(df2p)
  rlev<-rle(diff(den$y)>0)$lengths
  if(length(rlev)<=2) {dim_sed<-which.max(df2)+1
  } else {
    cutoff<-sum(rlev[1:2])
    dim_sed<-max(which(df2>den$x[cutoff]))+1 }
  names(dim_sed)<-'Second derivative'

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
  names(dim_pr)<-'Preceding residual'

  # perpendicular line
  A<-c(1,sdev[1]);B<-c(length(sdev),sdev[length(sdev)]);Dist<-NULL
  for (i in 2:(length(sdev)-1)) {
    C<-c(i,sdev[i]);D<-cbind(rbind(A,B,C),rep(1,3))
    S<-1/2*abs(det(D));Dist[i]<-2*S/dist(rbind(A,B))
  }
  dim_perl<-which.max(Dist)
  names(dim_perl)<-'Perpendicular line'

  # k-means clustering
  set.seed(2022)
  dim_clu<-min(kmeans(sdev,2)$size)+1
  names(dim_clu)<-'K-means clustering'

  dim_all<-c(dim_plm,dim_fid,dim_sed,dim_pr,dim_perl,dim_clu)
  return(dim_all)
}


fig<-function(sdev,result){

  x<-1:length(sdev)
  eb<-data.frame(x,sdev)

  g1<-ggplot(eb,aes(x=x,y=sdev))+geom_point(size=1,col='orange')+theme_bw()+
    xlab('Number of PC')+
    ylab('Standard deviation')+
    scale_x_continuous(breaks = seq(5,length(sdev),by=5),limits = c(1,length(sdev)))+
    theme(panel.grid =element_blank())+
    theme(panel.border = element_blank())+
    theme(axis.line= element_line())+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(title = element_text(size = rel(1)))

  npc<-result
  NPC<-melt(npc)

  g2<-ggplot(NPC,aes(Var1,Var2,fill=value))+geom_tile()+geom_text(aes(label=value))+
    scale_fill_gradient(low = '#FFCCCC',high = '#FF3333',breaks=range(NPC$value),guide = 'legend')+
    xlab(NULL)+ylab(NULL)+theme_minimal()+
    theme(axis.text.y = element_text(face = 'bold'))+
    theme(axis.text.x = element_text(face = 'bold'))+
    theme(legend.position = 'left')+
    theme(panel.grid = element_blank())+
    scale_y_discrete(position = 'right')+
    theme(legend.key.size = unit(0.2,'inches'))+
    theme(legend.title = element_blank())+
    theme(legend.margin = margin(0,0,0,0,'cm'))

  g<-(g1+g2)+plot_layout(widths = c(3,1))
  grid.draw(g)
}
