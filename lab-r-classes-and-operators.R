# 1: Create a Custom S3 Class
# Define a custom S3 class called "Circle" that represents a circle. The class should store the radius of the 
Circle <- function(radius) {
  circle <- list(radius = radius)
  class(circle) <- "Circle"
  return(circle)
}

# Write a print.Circle() method to display the radius and area of the circle when the object is printed. Use the formula for the area of a circle: area = π * radius^2.

print.Circle <- function(x, ...) {
  area <- pi * x$radius^2
  
  cat('Type: "Circle object"\n')
  cat("Radius:", x$radius, "\n")
  cat("Area:", area, "\n")
}

my_circle <- Circle(6)
my_circle


# Task 2: Define a Custom Operator
# Define a custom operator %>% that calculates the distance between the centers of two circles and checks if they intersect. The operator should return TRUE if the circles intersect and FALSE otherwise. The formula for the distance between two circles is: distance = sqrt((x2 - x1)^2 + (y2 - y1)^2).

#Assume that the circles are centered at (x1, y1) and (x2, y2), respectively.
#Two circles intersect if the distance between their centers is less than or equal to the sum of their radii.
#Modify the Circle class to include x and y coordinates for the center of the circle.

Circle <- function(radius, x, y) {
  circle <- list(
    radius = radius,
    x = x,
    y = y
  )
  class(circle) <- "Circle"
  return(circle)
}

`%>%` <- function(circle1, circle2) {
  if (!inherits(circle1, "Circle") || !inherits(circle2, "Circle")) {
    stop("Both operands must be Circle objects.")
  }
  
  # Distance between centers
  distance <- sqrt(
    (circle2$x - circle1$x)^2 +
      (circle2$y - circle1$y)^2
  )
  
  # Check intersection
  distance <= (circle1$radius + circle2$radius)
}

circle1 <- Circle(radius = 3, x = 0, y = 0)
circle2 <- Circle(radius = 4, x = 5, y = 0)

circle1 %>% circle2


# Task 3: Write a Function for the Class
# Write a function circumference() that calculates the circumference of a Circle object. The formula for the circumference is: circumference = 2 * π * radius.
# Add this function as a method for the Circle class so that it can be called directly on objects of the class.

circumference <- function(x, ...) {
  UseMethod("circumference")
}

circumference.Circle <- function(x, ...) {
  2 * pi * x$radius
}

my_circle <- Circle(radius = 5)
circumference(my_circle)

# Task 4: Create a Custom S4 Class
# Define a custom S4 class called "Rectangle" that represents a rectangle. The class should store the length and width of the rectangle.
# Write a constructor function Rectangle() that takes the length and width as input and creates an object of the "Rectangle" class.
# Write a show() method for the Rectangle class to display the length, width, and area of the rectangle when the object is printed.

setClass(
  "Rectangle",
  slots = list(
    length = "numeric",
    width  = "numeric"
  )
)

Rectangle <- function(length, width) {
  new("Rectangle", length = length, width = width)
}

setMethod(
  "show",
  "Rectangle",
  function(object) {
    area <- object@length * object@width
    
    cat('Type: "Rectangle object"\n')
    cat("Length:", object@length, "\n")
    cat("Width:", object@width, "\n")
    cat("Area:", area, "\n")
  }
)

my_rectangle <- Rectangle(length = 4, width = 6)
show(my_rectangle)


# Task 5: Define a Custom Operator for the S4 Class
# Define a custom operator %==% that compares two Rectangle objects and checks if they have the same area.
# The operator should return TRUE if the areas are equal and FALSE otherwise.

`%==%` <- function(rect1, rect2) {
  # Check that both objects are Rectangle objects
  if (!is(rect1, "Rectangle") || !is(rect2, "Rectangle")) {
    stop("Both operands must be Rectangle objects.")
  }
  
  # Calculate areas
  area1 <- rect1@length * rect1@width
  area2 <- rect2@length * rect2@width
  
  # Compare areas
  area1 == area2
}

rectangle1 <- Rectangle(length = 4, width = 6)
rectangle2 <- Rectangle(length = 3, width = 8)

rectangle1 %==% rectangle2




