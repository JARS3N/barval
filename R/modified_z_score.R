modified_z_score <-function(vec){
  if (length(vec)<2){
return(NULL)}
  # use t-dist to account for smaller sample sizes
  tval=qt(.975,length(vec)-1)
  med <- median(vec)
  MAD <- mad(x=vec,constant = 1/tval)
  abs(vec -  med)/MAD
}
