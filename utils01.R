#' Wrapper for .Last.value
ans<-function(){
  .Last.value
}

#' Remove all
#'
#' @description Equivalent to \code{rm(list=ls())}
rmall<-function(){
  rm(list=ls(envir=globalenv()),envir=globalenv())
}


#' gen dummy variables from MULTI_NOMINAL factor variables
gendummy<-function(){
  print("cbind(iris,model.matrix(~0+Species,iris))")
}

#' heatmap with ggplot2
#'
#'
heatmap<-function(dt){
  myPalette <- colorRampPalette(brewer.pal(7, "RdYlGn"))
  ggplot(dt,aes(x = x, y = y, fill = value)) +
    geom_tile() +
    scale_fill_gradientn(colours = myPalette(100)) +
    coord_equal() +
    theme_bw()
}


#' Rcolorbrewer help
#'
brewerhelp<-function(){
  display.brewer.all()
  print('myPalette <- colorRampPalette(brewer.pal(7, "RdYlGn"))')

}

#' Vertical Text on X Axis
#'
verticaltext<-function(gplot){
  gplot+theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

### FUNCTION TO CONVERT hclust$merge to D3 format
hcjson<-function(merge){
  
  final<-list()
  toexclude<-c()
  for (i in 1:nrow(merge)){
    left <- merge[i, 1]
    right <- merge[i, 2]
    
    if(left<0 & right<0){   
      final[[i]]=list(name=paste("idx",i),children=list(list(name=paste("leaf",-left),size=1),list(name=paste("leaf",-right),size=1)),size=1)    
    }
    if(left<0 & right>0){   
      final[[i]]=list(name=paste("idx",i),children=list(final[[right]],list(name=paste("leaf",-left),size=1)),size=1)    
      toexclude<-c(toexclude,right)
    }
    if(left>0 & right>0){   
      final[[i]]=list(name=paste("idx",i),children=list(final[[left]],final[[right]]),size=1)   
      toexclude<-c(toexclude,left,right)
    }
  }
  final<-toJSON(list(name="root",children=final[setdiff(c(1:nrow(merge)),toexclude)]))
  final
}
