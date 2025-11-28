#test_change
a=1+2
print (a)
#Changes are not flowing in - why?

#Define objective function
obj_fun<-function(x)
{
  return(x^2+25*x)
}


#Graphical representation
valuesx<-c(-80:50)
obj_out<-obj_fun(valuesx)
plot(x=valuesx,y=obj_out,typ='l', col=3, xlab = 'values', ylab=' objective function output')

#Define cost function
cost<-function(x){
  return(2*x+25)
}

#Gradient descent function
grad_desc<-function(x = 0.1, alpha = 0.6, conv=0.001, max_iter=100) 
{
  #alpha = learning rate or step size
  #conv = maximum improvement from previous iteration
  #max_iter = stopping iterations

  xtrace <- x
  ftrace <- obj_fun(x)
  diff=obj_fun(x)
  iter=1
  
  #Search for solution until max iterations are reached or improvement is very minimal
  while (iter<max_iter & diff>conv) {
    prev_out<-obj_fun(x)
    x <- x - alpha * cost(x)
    new_out<-obj_fun(x)
    diff=prev_out-new_out
    
    xtrace <- c(xtrace,x)
    ftrace <- c(ftrace,obj_fun(x))
    iter=iter+1
  }
  
  data.frame(
    "x" = xtrace,
    "f_x" = ftrace
  )
}

#Test optimisation function
out<-grad(x=20, alpha=0.1 ,max_iter =100)

#Minimum
print(tail(out,n=1))
#-12.45977 -156.2484

#Plot of output
plot(x=valuesx,y=obj_out,typ='l', col=3, xlab = 'values', ylab=' objective function output')
points(x=out$x,y=out$f_x)
