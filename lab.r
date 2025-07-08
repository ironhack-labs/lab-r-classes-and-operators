
# Constructor for Circle class
Circle <- function(radius, x = 0, y = 0)  {
  if (!is.numeric(radius) || radius <= 0) {
    stop("Radius must be a positive number.")
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("x and y must be numeric.")
  }
  obj <- list(
    radius = radius,
    x = x,
    y = y
    )
  class(obj) <- "Circle"
  return(obj)
}

# Custom print method for Circle
print.Circle <- function(x, ...) {
  area <- pi * x$radius^2
  cat("Circle with radius:", x$radius, "\n")
  cat("Center at (", x$x, ",", x$y, ")\n", sep = "")
  cat("Area:", round(area, 2), "\n")
}
circumference <- function(object){
    if(!inherits(object, "Circle")){
        stop("circumference() only works with objects of class 'Circle'")
    }
    return(2 * pi * object$radius)    
}

# usage:
my_circle <- Circle(5, 4,4)
print(my_circle)


# custom method
`%>%` <- function(circle1, circle2) {
  if (!inherits(circle1, "Circle") || !inherits(circle2, "Circle")) {
    stop("Both operands must be Circle objects.")
  }
  
  distance <- sqrt((circle2$x - circle1$x)^2 + (circle2$y - circle1$y)^2)
  return(distance <= (circle1$radius + circle2$radius))
}

circle1 <- Circle(radius = 3, x = 0, y = 0)
circle2 <- Circle(radius = 4, x = 5, y = 0)
print(circle1 %>% circle2)

print(circumference(circle1))


# s4 class

setClass(
  "Rectangle",
  slots = list(
    length = "numeric",
    width = "numeric"
  )
)
# Constructor function
Rectangle <- function(length, width) {
  if (!is.numeric(length) || !is.numeric(width) ) {
    stop("Input must be a numeric")
  }
  new("Rectangle", length = length, width = width)
}
# override a method
setMethod("show", 
        "Rectangle", 
        function(object){
                cat("Type:", class(object), "\n")
                cat("Length:", object@length, "\n")
                cat("Width:", object@width, "\n")
                cat("Area:", object@length * object@width, "\n")
})

`%==%` <- function(r1, r2){
    if(!inherits(r1, "Rectangle") || ! inherits(r2, "Rectangle")){
        stop("Both operants must be of class 'Rectangle'")
    }
    return(r1@length * r1@width ==  r2@length * r2@width)
}


my_rectangle <- Rectangle(length = 4, width = 6)
show(my_rectangle)

rectangle1 <- Rectangle(length = 4, width = 6)
rectangle2 <- Rectangle(length = 3, width = 8)
print(rectangle1 %==% rectangle2)