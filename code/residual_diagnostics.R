
library(ggplot2)
library(cowplot)
# uppdatera funktionen om du vill ändra layout på plottarna
# res_vect = vector with residuals 
# fit_vect = vector with fitted values
# alpha = transparency of points 
# binwidth = binwidth for the histogram
residual_diagnostics<-function(res_vect,fit_vect,alpha=0.3,binwidth=NULL){

  # histogram för residualer
  res_df<-data.frame(residuals=na.omit(res_vect))
  
  # after_stat(density)
  p1<-ggplot(res_df, aes(residuals)) +
    geom_histogram(aes(y = after_stat(density)),binwidth = binwidth) +
    stat_function(
      fun = dnorm, 
      args = list(mean = mean(res_df$residuals), sd = sd(res_df$residuals)), 
      lwd = 1, 
      col = 'blue'
    )
  
  # Fitted values vs residuals
  df_temp<-data.frame(residuals=res_vect,fitted_values=fit_vect)
  p2<-ggplot(data = df_temp,aes(x=fitted_values,y=residuals))+geom_point(alpha=alpha)+
    geom_hline(yintercept=0,col="blue")+xlab("fitted values") +ylab("residuals")
  #p2<-qplot(x = fit_vect,res_vect,ylab="residuals",xlab="fitted values")+
    
  
  # Obervation order vs residuals
  index<-1:length(res_vect)
  df_temp2<-data.frame(index=index,residuals=res_vect)
  p3<-ggplot(data = df_temp2,aes(x=index,y=residuals))+geom_line()+
    geom_hline(yintercept=0,col="blue")
  #p3<-qplot(x = index,res_vect,xlab="index",ylab="residuals",geom="line")+
    
  
  
  # Normal Q-Q plot
  scale_res_vect<-scale(res_vect)
  df <- data.frame(y = scale_res_vect)
  p4 <- ggplot(df, aes(sample = y))+ stat_qq(alpha=alpha,col="blue") + stat_qq_line()+
    ylab("Scaled residuals")
  
  
  plot_final<-plot_grid(p4,p2,p1,p3,nrow = 2)
  
  return(plot_final)
  
}
