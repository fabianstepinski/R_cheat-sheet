#R_general_cheat-sheet

#Shortcuts
#alt + minus = <-
#ctrl + enter = execute selected part

#BASIC_GRAPHING_____________________________________________________________####

#load basic R dataset
library(datasets)

#look at the first few lines
head(iris)

#get information on commands or datasets
?iris

#plotting
plot(iris$Species) #plot categorical variable
plot(iris$Petal.Length) #plot quantitative variable
plot(iris$Species, iris$Petal.Length) #cat x quant
plot(iris$Petal.Width, iris$Petal.Length) #quant x quant
plot(iris) #matrix for holistic overview

#plotting with options
plot(iris$Petal.Length, iris$Petal.Width,
     col = "#cc0000", #hex code for red
     main = "iris: pethal length vs. petal width", #header
     xlab = "petal length", #x axis lable
     ylab = "petal width" #y axis lable
)

#plot formulas
plot(sin, 0, 2 * pi,
     lwd = 5, #line width
     col = "blue",
     main = "sine curve"
)
plot(dnorm, -pi, pi, 
     lwd = 10,
     main = "standard normal distribution",
     xlab = "z-scores",
     ylab = "density",
     col = "green"
)
plot(exp, 0, 8)

#bar charts - simple is good
#bar charts are the most simple graphics
#for the most simple data
?mtcars
head(mtcars)

#bar chart
barplot(mtcars$cyl) #doesn't work
cylinders <- table(mtcars$cyl) #create table and assign it to variable
barplot(cylinders)

#PROBABILITY________________________________________________________________####

#sample
set.seed(4) #set seed to identify errors
x <- sample(6, 1000, replace = TRUE) #draw randint out of 6, 1000x, with replacing
barplot(table(x))

#roll a d6 1000 times and assign 80% probability to the number 6
x <- sample(6, 1000, replace = TRUE, c(rep(0.2 / 5, 5),0.80))
barplot(table(x),
        xlab = "Number",
        ylab = "Occurance")
#same experiment, fill results into matrix
mat <- matrix(NA, nrow = n, ncol = 2)
for(i in 1:1000){
  mat[i,] <- sample(1:6, size = 2, replace = T)
}

#graph the probabilities of the sum for a roll of two d6
p <- c(1:6, 5:1)/36
plot(2:12, p, type = "h", las = 1, xlab = "sum of two die")

#roll a dice 3 times. what is P("no 4")? - (5/6)^3 = 0.579
sample(1:6, 3, T) #simulation of the question above
#do the experiment x times and count the relative frequency of P("no four")
experiment_amount <- c(100000)
results_with_four <- c(0)
for(roll in 1:experiment_amount){
  result <- sample(1:6, 3, T)
  if(4 %in% result){
    results_with_four <- results_with_four + 1
  }
}
1 - (results_with_four/experiment_amount)
#same experiment
experiment_amount <- c(100000)
simulation_1 <- function(x){
  roll_3 <- sample(1:6, 3, T)
  no_four <- sum(roll_3 == 4) == 0
}
results_without_four <- sapply(1:experiment_amount, simulation_1)
sum(results_without_four)/experiment_amount

#what is the probability to get 7 or more sixes after 10 d6 rolls
n <- 100000 #experiment with 100000 rolls
y <- rep(NA, n)
for(i in 1:n){
  w <- sample(1:6, 10, T) #roll d6 10 times
  count_sixes <- sum(w == 6) #count amount of sixes
  y[i] <- (count_sixes >= 7)
}
100*(sum(y)/n)
#same experiment, with a matrix
n <- 100000 #experiment with 100000 rolls
w <- sample(1:6, 10 * n, T)
mat <- matrix(data = w, ncol = 10)
head(mat) #look at the structure of matrix
number_of_sixes <- apply(mat, 1, function(x){sum(x == 6)})
sum(number_of_sixes >= 7)/nrow(mat)


#for loop: Draw two spheres out of 4 without replacing them, repeat 10x
for (i in 1:10){
  print(sample(x = c("r", "g", "b", "o"), size = 2, replace = FALSE))}

#MISCELLANEOUS______________________________________________________________####

#check the time for used for code
n <- 50000
system.time({
  w <- NULL
  for (i in 1:n) {
    w <- rbind(w, sample(1:6, 2, TRUE))
  }
})