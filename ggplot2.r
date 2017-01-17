library(ggplot2)

qplot(Sepal.Length, Petal.Length, data = iris, color = Species, size = Petal.Width, alpha = I(0.7),
      xlab="Sepal Length", ylab="Petal.Length", main="Sepal Length vs. Petal Length")

qplot(Sepal.Length, Petal.Length, data = iris, color = Species, size = Petal.Width, alpha = I(0.7),
      xlab="Sepal Length", ylab="Petal.Length", main="Sepal Length vs. Petal Length", geom="line")

movies <- data.frame(director = c("S", "S", "C", "A", "B", "S", "B", "C", "A", "A", "A", "A"),
                     movie = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                     length1  = c(100,200,300,400,500,600,700,800,900,1000,1100,1200))

qplot(director, data=movies, weight=length1, ylab="# of movies", geom="bar")


qplot(age, circumference, data = Orange, geom = c("point", "line"), colour = Tree)

install.packages("maps")
install.packages("ggplot2")

library(maps)
library(ggplot2)

all_states <- map_data("state")
p <- ggplot()
p <- p + geom_polygon(data=all_states, aes(x=long, y=lat, group=group), color="white")
p
write.csv(all_states, "input_data.csv", row.names=F)

input <- read.csv("input_data.csv", stringsAsFactors = F)
p <- ggplot()
p <- p+ geom_polygon(data=input, aes(x=long, y=lat, group=group), color="white")
p <- p+ geom_point(data=input, aes(x=long, y=lat, size=Yield), color="green")
p <- p+scale_size(name="Yield")
p <- p + geom_text( data=input, hjust=0.5, vjust=-0.5, aes(x=long, y=lat, label=Yield), colour="gold2", size=4 )
p

p <- ggplot()
p <- p+ geom_polygon(data=input, aes(x=long, y=lat, group=group), color="white")
p <- p+ geom_jitter(data=input, position=position_jitter(width=0.5, height=0.5), aes(x=long, y=lat, size=Yield, color=state), color="green")
p <- p+scale_size(name="Yield")
p <- p + geom_text( data=input, hjust=0.5, vjust=-0.5, aes(x=long, y=lat, label=Yield), colour="gold2", size=4 )
p

weather <- read.csv("weather.csv", stringsAsFactors = F)
ggplot(weather, aes(x=date, y=min.temp, color=scenario, group=date))+geom_line()

ggplot(weather, aes(x=date, y=min.temp, color=scenario, group=date))+geom_point()
