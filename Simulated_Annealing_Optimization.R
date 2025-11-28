#Simulated Annealing Optimization to find global optimum

#Objective Function
obj_fun<-function(x)
{
  return(x*sin(x))
}

#Graphical representation of objective function
valuesx<-c(1:50)
obj_out<-obj_fun(valuesx)
plot(x=valuesx,y=obj_out,typ='l', col=3)

sprintf("minimum value of objective function: %f",min(obj_out))
sprintf("minimum x: %i",valuesx[obj_out==min(obj_out)])
points(x=valuesx[obj_out==min(obj_out)],y=min(obj_out), col=1, cex=2)

#-------------------------------Output using grad descent--------------------
cost<-function(x){
  return(x*cos(x) + sin(x))
}

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
    print(xtrace)
    print(ftrace)
    print(iter)
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

#Get grad desc output and plot  
out<-grad_desc(x=20, alpha=0.1,max_iter =100)
plot(x=valuesx,y=obj_out,typ='l', col=3, xlab = 'values', ylab=' objective function output')
points(x=out$x,y=out$f_x, cex=1) # Plot optimisation plots

#----------------Optima Using Simulated Annealing------------------
#Find neighbor function
find_neighbor<-function(s){
  s<-sample.int(50,1)
  return(s)
}


#Define simulated annealing function
#obj_func = function to be optimized
#s0 = initial state
#step = step size 
#find neighbor = function to find the next solution to test from the sample space

simulate_annealing_function<-function(obj_fun,s0=0,step=0.01,find_neighbor)
{
  
  #Initialize
  #s stands for state
  #f stands for function value
  #b stands for best
  #c stands for current
  #n stands for neighbor
  
  s_b<-s0
  s_c<-s0
  s_n<-s0
  
  f_c<-obj_fun(s_c)
  f_b<-f_c
  f_n<-f_c
  k=1
  Temp<-(1-step)^k
  
  while (Temp>0.000001)
  {
    #Set Temperature. As iteration increases temperature decreases (slowly)
    Temp<-(1-step)^k
    
    #consider a random neighbor
    s_n<-find_neighbor(s_c)        #Here I have written a custom find neighbor function
    
    f_n<-obj_fun(s_n)                 #Find objective function value for neighbor state
    #print(f_n)
    
    #update current state
    #Accept neighbor or jump to neighbor point only if neighbor is "good" 
    #i.e value of neighbor function is less than current neighbor
    #OR if neighbor is bad then accept it only based on schedule 
    #probability. Higher the temp more chance of poor solution being accepted
    if(( f_n>f_c) | runif(1,0,1)<exp(-(f_n-f_c)/Temp))
    {
      s_c<-s_n
      f_c<-f_n
    }
    
    #update best state
    if(f_n<f_b)
    {
      s_b<-s_n
      f_b<-f_n
    }
    k=k+1
  }
  
  #return best state and best objective function value
  print("best state")
  print(f_b)
  return(c(s_b,f_b))
}

#Call function
s0=20   #Initial starting point
sa_out<-simulate_annealing_function(obj_fun = obj_fun,s0 = s0, find_neighbor = find_neighbor)

#Plot function
plot(x=valuesx,y=obj_out,typ='l', col=3, xlab = 'values', ylab=' objective function output')
points(x=sa_out[1],y=sa_out[2],cex=1.5)
