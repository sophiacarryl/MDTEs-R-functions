library("ggplot2")
greet <- function(name, birthday = NULL) {
  paste0("Hi ", name, if (isTRUE(birthday)) "and HAPPY BIRTHDAY"
  )
  
}
greet("Sophia")
# "Hi Sophia"
greet("Sophia", FALSE)
# "Hi Sophia"
greet("Sophia", TRUE)
# "Hi Sophia and HAPPY BIRTHDAY"

trial <- function (name, birthday = NULL){
  paste0("Hi ", name,  if(isTRUE(birthday)) "Happy Birthday", ", how are you doing?"
  )
  
}
trial("Sophia")

newtrial <- function (name, birthday = NULL, age = NULL){
if(isTRUE(birthday) && is.null(age)){
    print(paste0("Hi", " ", name, ", Happy Birthday!!"))
  }
  
else if(isTRUE(birthday) && is.numeric(age)){
    print(paste0("Hi"," ", name, ", Happy", " ", age, " ", "Birthday"))
  }
else if(isFALSE(birthday) && is.numeric(age)){
    print(paste0("Hi"," ", name, ", is it your", " ", age, " " , "birthday?"))
  }
else {
    print(paste0(name, ", How are you doing?"))
  }
}

newtrial("Sophia",TRUE)
# "Hi Sophia, Happy Birthday!!"

newtrial("Sophia",T, 24)
#"Hi Sophia, Happy Birthday!!"

newtrial("Sophia",F, 40)
# "Hi Sophia, is it your 40 birthday?"

newtrial("Sophia")
#"Sophia, How are you doing?"


f2 <- function(x = z) {
  z <- 100
  x
}
f2(1)
