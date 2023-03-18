#R_general_cheat-sheet

#Shortcuts
#alt + minus = <-

#PROBABILITY________________________________________________________________####

#sample
set.seed(4) #set seed to identify errors
x <- sample(6, 1000, replace = TRUE) #draw randint out of 6, 1000x, with replacing
barplot(table(x))

#assign 80% probability to the number 6
x <- sample(6, 1000, replace = TRUE, c(rep(0.2 / 5, 5),0.80))
barplot(table(x),
        xlab = "Number",
        ylab = "Occurance")

#for loop: Draw two spheres out of 4 without replacing them, repeat 10x
for (i in 1:10){
  print(sample(x = c("r", "g", "b", "o"), size = 2, replace = FALSE))}

#
n <- 50000
system.time({
  w <- NULL
  for (i in 1:n) {
    w <- rbind(w, sample(1:6, 2, TRUE))
  }
})

#Introduction to basic graphing

#default plot command:
# basic x-y plotting
# adapts to data types and number of varialbes
#####

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


#####

#bar charts - simple is good
#bar charts are the most simple graphics
#for the most simple data
#####
?mtcars
head(mtcars)

#bar chart
barplot(mtcars$cyl) #doesn't work
cylinders <- table(mtcars$cyl) #create table and assign it to variable
barplot(cylinders)
#####

