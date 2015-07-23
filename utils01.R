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
