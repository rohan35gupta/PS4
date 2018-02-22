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

#1.
setClass(Class="door", #Defined new S4 class door
         representation = representation(
           chosenDoor = "numeric", #chosenDoor integer (1, 2 or 3)
           carDoor = "numeric", #carDoor integer (1, 2 or 3)
           switch = "logical", #switch boolean indicating whether player adopted strategy of staying with first choice (FALSE) or switching to remaining door (TRUE)
           winner = "logical" #winner boolean indicating whether or not final chosen door same as car door
         ),
         prototype = prototype(
           chosenDoor = c(),
           carDoor = c(),
           switch = c(),
           winner = c()
         )
         )