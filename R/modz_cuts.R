modz_cuts<-function(vec){
  n<-length(vec)
  if(n>100){return(3.5)}
  if(n>10){return(7)}
  if(n<=10){return(12.25)}
}
