# 1. Constructor function for the S3 class "Circle"
Circle <- function(radius) {
  if (!is.numeric(radius) || radius <= 0) {
    stop("Radius must be a positive number.")
  }
  
  # Create a list object and assign class "Circle"
  circle_obj <- list(radius = radius)
  class(circle_obj) <- "Circle"
  return(circle_obj)
}

# 2. Custom print method for objects of class "Circle"
print.Circle <- function(x, ...) {
  radius <- x$radius
  area <- pi * radius^2
  cat("Circle Object\n")
  cat("--------------\n")
  cat("Radius:", radius, "\n")
  cat("Area  :", round(area, 2), "\n")
}
# Create a Circle object with radius 5
my_circle <- Circle(5)

# Print the Circle object (triggers print.Circle)
print(my_circle)

# 1. Constructor for the modified Circle class (includes x, y)
Circle <- function(radius, x = 0, y = 0) {
  if (!is.numeric(radius) || radius <= 0) {
    stop("Radius must be a positive number.")
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Coordinates x and y must be numeric.")
  }
  
  circle_obj <- list(radius = radius, x = x, y = y)
  class(circle_obj) <- "Circle"
  return(circle_obj)
}

# 2. Custom print method for Circle
print.Circle <- function(x, ...) {
  radius <- x$radius
  area <- pi * radius^2
  cat("Circle Object\n")
  cat("--------------\n")
  cat("Center: (", x$x, ",", x$y, ")\n")
  cat("Radius :", radius, "\n")
  cat("Area   :", round(area, 2), "\n")
}

# 3. Custom operator %>% to check if two circles intersect
`%>%` <- function(c1, c2) {
  if (!inherits(c1, "Circle") || !inherits(c2, "Circle")) {
    stop("Both operands must be Circle objects.")
  }
  
  distance <- sqrt((c2$x - c1$x)^2 + (c2$y - c1$y)^2)
  intersect <- distance <= (c1$radius + c2$radius)
  return(intersect)
}

# 4. Create two circles and test the intersection
circle1 <- Circle(radius = 5, x = 0, y = 0)
circle2 <- Circle(radius = 3, x = 6, y = 0)

# Print circles
print(circle1)
print(circle2)

# Check if they intersect
cat("Do the circles intersect? ", circle1 %>% circle2, "\n")

# 1. Define the Circle class with center (x, y) and radius
Circle <- function(radius, x = 0, y = 0) {
  if (!is.numeric(radius) || radius <= 0) {
    stop("Radius must be a positive number.")
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Coordinates x and y must be numeric.")
  }
  
  circle_obj <- list(radius = radius, x = x, y = y)
  class(circle_obj) <- "Circle"
  return(circle_obj)
}

# 2. Custom print method
print.Circle <- function(x, ...) {
  radius <- x$radius
  area <- pi * radius^2
  cat("Circle Object\n")
  cat("--------------\n")
  cat("Center: (", x$x, ",", x$y, ")\n")
  cat("Radius :", radius, "\n")
  cat("Area   :", round(area, 2), "\n")
}

# 3. Define a generic circumference function
circumference <- function(object, ...) {
  UseMethod("circumference")
}

# 4. Define method for class "Circle"
circumference.Circle <- function(object, ...) {
  return(2 * pi * object$radius)
}

# 5. Create a Circle object
c1 <- Circle(radius = 7, x = 2, y = 3)

# 6. Print the Circle and its circumference
print(c1)
cat("Circumference:", round(circumference(c1), 2), "\n")

# Task 4 - wow!
# 1. Define the S4 class "Rectangle"
setClass(
  "Rectangle",
  slots = list(
    length = "numeric",
    width = "numeric"
  ),
  prototype = list(
    length = 1,
    width = 1
  ),
  validity = function(object) {
    if (object@length <= 0 || object@width <= 0) {
      return("Length and width must be positive numbers.")
    }
    TRUE
  }
)

# 2. Constructor function for Rectangle
Rectangle <- function(length, width) {
  new("Rectangle", length = length, width = width)
}

# 3. Define a show() method for Rectangle class
setMethod(
  "show",
  "Rectangle",
  function(object) {
    area <- object@length * object@width
    cat("Rectangle Object\n")
    cat("----------------\n")
    cat("Length:", object@length, "\n")
    cat("Width :", object@width, "\n")
    cat("Area  :", area, "\n")
  }
)

# 4. Create a Rectangle object and print it
r1 <- Rectangle(length = 8, width = 5)
r1  # This triggers the show() method automatically


# Task 5
# 1. Define the S4 class "Rectangle"
setClass(
  "Rectangle",
  slots = list(
    length = "numeric",
    width = "numeric"
  ),
  prototype = list(
    length = 1,
    width = 1
  ),
  validity = function(object) {
    if (object@length <= 0 || object@width <= 0) {
      return("Length and width must be positive numbers.")
    }
    TRUE
  }
)

# 2. Constructor function
Rectangle <- function(length, width) {
  new("Rectangle", length = length, width = width)
}

# 3. Show method
setMethod(
  "show",
  "Rectangle",
  function(object) {
    area <- object@length * object@width
    cat("Rectangle Object\n")
    cat("----------------\n")
    cat("Length:", object@length, "\n")
    cat("Width :", object@width, "\n")
    cat("Area  :", area, "\n")
  }
)

# 4. Define the custom operator %==%
`%==%` <- function(rect1, rect2) {
  if (!is(rect1, "Rectangle") || !is(rect2, "Rectangle")) {
    stop("Both operands must be Rectangle objects.")
  }
  
  area1 <- rect1@length * rect1@width
  area2 <- rect2@length * rect2@width
  
  return(area1 == area2)
}

# 5. Create two Rectangle objects
r1 <- Rectangle(length = 4, width = 10)
r2 <- Rectangle(length = 5, width = 8)

# 6. Print rectangles
r1
r2

# 7. Compare using %==%
cat("Do the rectangles have the same area? ", r1 %==% r2, "\n")
