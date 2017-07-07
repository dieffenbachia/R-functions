############
#
#     USEFUL FUNCTIONS
#
#     author: Jocelyn Elmes
#
############


#creates strip plots
stripfunction<-function( y, x,xlim,ylim,jitterval=NULL,add=F,at=NULL,pointcol,
                         tick=T,cex.axis,xlabs,ylabs,xaxis.pos,yaxis.pos,xaxis.labels,
                         xaxis.xpos,yaxis.ticks,line,...){
  
  y=as.integer(y)
  x=as.integer(x)
  if(xlim[1] > min(x,na.rm=T))
    xlim[1] <- min(x,na.rm=T)
  if(xlim[2] < max(x,na.rm=T))
    xlim[2] <- max(x,na.rm=T)     
  if(ylim[1] > min(y,na.rm=T))
    ylim[1] <- min(y,na.rm=T)
  if(ylim[2] < max(y,na.rm=T))
    ylim[2] <- max(y,na.rm=T)
  if(missing(xaxis.pos))
    xaxis.pos<-min(y)-1
  if(missing(xaxis.xpos))
    xaxis.xpos<-unique(at)
  
  if(missing(yaxis.pos))
    yaxis.pos<-min(x)-1
  if(missing(yaxis.ticks))
    yaxis.ticks<-seq(ylim[1],ylim[2],1)
  if(missing(xaxis.labels))
    xaxis.labels<-levels(data$x)
  
  if(missing(cex.axis))
    cex.axis<-cex
  if(missing(line))
    line=0
  if(!is.null(at))
    x<-at
  if(add==T){
    if(is.null(jitterval)){
      
      # plot(x,y, xlim = xlim,ylim = ylim,type="n",...)
      points(x,y, xlim = xlim,ylim = ylim,xaxt="n",yaxt="n",xlab="",ylab="",col=pointcol,...)
      # axis(1,at=at,labels=xaxis.labels,tick=tick,cex.axis=cex.axis,...)
      # axis(2,at=yaxis.ticks,tick=tick,cex.axis=cex.axis,...)
      # mtext(side=1,line=line,text=xlabs,cex=cex.axis,adj=c(0.5,NA),...)
      # mtext(side=2,line=line,text=ylabs,cex=cex.axis,adj=c(0.5,NA),...)
      # segments(x0=xlim[1],x1=xlim[2],y0=ylim[1])
      # text(y=xaxis.pos,x=sapply(1:length(xaxis.labels),function(i)xaxis.xpos[i]),
      #      labels=sapply(1:length(xaxis.labels),function(i)xaxis.labels[i]),cex=cex.axis,adj=c(0.5,NA),...)
      
    }else{
      
      # plot(y~jitter(x,jitterval) ,xlim = xlim,ylim = ylim,type="n",...)
      points(y~jitter(x,jitterval), xlim = xlim,ylim = ylim,xaxt="n",yaxt="n",xlab="",ylab="",col=pointcol,...)
      # axis(1,at=at,labels=xaxis.labels,tick=tick,cex.axis=cex.axis,...)
      # axis(2,at=yaxis.ticks,tick=tick,cex.axis=cex.axis,...)
      # mtext(side=1,line=line,text=xlabs,cex=cex.axis,adj=c(0.5,NA),...)
      # mtext(side=2,line=line,text=ylabs,cex=cex.axis,adj=c(0.5,NA),...)
      # segments(x0=xlim[1],x1=xlim[2],y0=ylim[1])
      # text(y=xaxis.pos,x=sapply(1:length(xaxis.labels),function(i)xaxis.xpos[i]),
      #      labels=sapply(1:length(xaxis.labels),function(i)xaxis.labels[i]),cex=cex.axis,adj=c(0.5,NA),...)
      
    }
    
  }else{
    if(is.null(jitterval)){
      
      plot(x,y, xlim = xlim,ylim = ylim,type="p",xaxt="n",yaxt="n",xlab="",ylab="",col=pointcol,...)
      # axis(1,at=at,labels=xaxis.labels,tick=tick,cex.axis=cex.axis,...)
      axis(2,at=yaxis.ticks,tick=tick,cex.axis=cex.axis,...)
      mtext(side=1,line=line,text=xlabs,cex=cex.axis,adj=c(0.5,NA),...)
      mtext(side=2,line=line,text=ylabs,cex=cex.axis,adj=c(0.5,NA),...)
      segments(x0=xlim[1],x1=xlim[2],y0=ylim[1])
      text(y=xaxis.pos,x=sapply(1:length(xaxis.labels),function(i)xaxis.xpos[i]),
           labels=sapply(1:length(xaxis.labels),function(i)xaxis.labels[i]),cex=cex.axis,adj=c(0.5,NA),...)
      
    }else{
      
      plot(y~jitter(x,jitterval) ,xlim = xlim,ylim = ylim,type="p",xaxt="n",yaxt="n",xlab="",ylab="",col=pointcol,...)
      # axis(1,at=at,labels=xaxis.labels,tick=tick,cex.axis=cex.axis,...)
      axis(2,at=yaxis.ticks,tick=tick,cex.axis=cex.axis,...)
      mtext(side=1,line=line,text=xlabs,cex=cex.axis,adj=c(0.5,NA),...)
      mtext(side=2,line=line,text=ylabs,cex=cex.axis,adj=c(0.5,NA),...)
      segments(x0=xlim[1],x1=xlim[2],y0=ylim[1])
      text(y=xaxis.pos,x=sapply(1:length(xaxis.labels),function(i)xaxis.xpos[i]),
           labels=sapply(1:length(xaxis.labels),function(i)xaxis.labels[i]),cex=cex.axis,adj=c(0.5,NA),...)
      
    }
    
  }
}

