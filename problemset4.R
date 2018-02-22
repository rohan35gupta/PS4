#Rohan Gupta
#February 22, 2018
#Pol Sci 4626 Problem Set 4

#Getting started
myFunction<-function(doorChoice, carLocation){ #myFunction takes door choice & car location
  if(doorChoice==carLocation){
    x<-TRUE
  }
  else{
    x<-FALSE
  }
  return(x) #Returns TRUE if equal & FALSE if not
}
myFunction(sample(1:3, 1), sample(1:3, 1))