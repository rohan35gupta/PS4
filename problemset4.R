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

new("door") #Construction function allows user to create door object

setValidity("door", function(object){ #Validation function checks whether values stored in slots appropriately structured
  if(!(object@chosenDoor %in% 1:3)){
    return("@chosenDoor is not a valid value")
  }
  if(!(object@carDoor %in% 1:3)){
    return("@carDoor is not a valid value")
  }
  if(!(object@switch %in% c(TRUE,FALSE))){
    return("@switch is not a valid value")
  }
  if(!(object@winner %in% c(TRUE,FALSE))){
    return("@winner is not a valid value")
  }
}
)

#2.
setGeneric("PlayGame", #Created generic
           function(object="door"){
             standardGeneric("PlayGame")
           })

setMethod("PlayGame", "door", #Created method for door objects called PlayGame
          function(object){
            object@carDoor<-sample(1:3, 1) #Draws random number between 1 & 3 that presents door behind which car hidden & adds to carDoor slot
            first<-sample(1:3, 1) #Draws random number between 1 & 3 that presents door chosen first
            if(object@switch==FALSE){ #If switch=FALSE, random draw added to chosenDoor slot
              object@chosenDoor<-first
            }
            else if(object@switch==TRUE){ #If switch=TRUE, random door chosen such that
              a<-object@carDoor #Does not contain car
              b<-object@chosenDoor #Not first door chosen by contestant
              remove<-1:3
              if(a!=b){
                remove<-remove[-c(a,b)]
              }
              else if(a==b){
                remove<-sample(remove[-a], 1)
              }
              contention<-1:3
              object@chosenDoor<-sample(contention[-remove], 1) #Door removed from contention. Player chooses at random between doors not removed. Added to chosenDoor slot.
            }
            if(object@carDoor==object@chosenDoor){ #Compares two door slots
              object@winner<-TRUE #If same, changes winner to TRUE
            }
            else{
              object@winner<-FALSE #If not same, changes winner to FALSE
            }
          })

#Simulation
#1.
#Ran simulation of game 1,000 times where players choose not to switch. Evaluated percentage of time they win car.
simulationNot<-new("door", chosenDoor = sample(1:3, 1), carDoor = sample(1:3, 1), switch = FALSE, winner = FALSE)
percentage<-0
PlayGame(simulationNot)@winner
for(i in 1:1000){
  if(PlayGame(simulationNot)@winner==TRUE){
    percentage<-percentage+1
  }
}
percentage/1000

#2.
#Ran simulation of game 1,000 times where players choose to switch. Evaluated percentage of time they win car.
simulationChoose<-new("door", chosenDoor = sample(1:3, 1), carDoor = sample(1:3, 1), switch = FALSE, winner = FALSE)
percentage<-0
PlayGame(simulationChoose)@winner
for(i in 1:1000){
  if(PlayGame(simulationChoose)@winner==TRUE){
    percentage<-percentage+1
  }
}
percentage/1000

#3.
#The strategy of choosing to switch is best. It leads to a winner 2/3 of the time, as opposed to 1/3 of the time.