# Create a Custom S3 Class
#Define a custom S§ class called "Circle" that represents a circle with a radius.
Circle <- function(radius) {
    if (!is.numeric(radius) || radius <= 0) {
        stop("Radius must be a positive numeric value.")
    }
    structure(list(radius = radius), class = "Circle")
}

# Write a print.Circle method to display the radius and area of the Circle 
print.Circle <- function(x, ...) {
    if (!inherits(x, "Circle")) {
        stop("Object is not of class 'Circle'.")
    }
    area <- pi * (x$radius^2)
    cat(sprintf("Circle with radius: %.2f and area: %.2f\n", x$radius, area))
}
# Example Usage
circle1 <- Circle(5)
print(circle1)

# Define a Custom Operator
# Define a custom operator %>% that calculates the distance between the centers of two circles and checks if they intersect.
`%>%` <- function(circle1, circle2) {
    if (!inherits(circle1, "Circle") || !inherits(circle2, "Circle")) {
        stop("Both operands must be of class 'Circle'.")
    }
    
    # Assuming circles are centered at (0, 0) for simplicity
    x1 <- 0
    y1 <- 0
    x2 <- 0
    y2 <- 0
    
    # Calculate the distance between the centers
    distance <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
    
    # Check if circles intersect
    return(distance <= (circle1$radius + circle2$radius))
}

# Modify the Circle class to include x and y coordinates for the center of the circle.

Circle <- function(radius, x = 0, y = 0) {
    if (!is.numeric(radius) || radius <= 0) {
        stop("Radius must be a positive numeric value.")
    }
    if (!is.numeric(x) || !is.numeric(y)) {
        stop("Coordinates must be numeric values.")
    }
    structure(list(radius = radius, x = x, y = y), class = "Circle")
}

# Write a Function for the Class
# Write a function circumference() that calculates the circumference of a Circle object. 
circumference <- function(circle) {
    if (!inherits(circle, "Circle")) {
        stop("Object is not of class 'Circle'.")
    }
    return(2 * pi * circle$radius)
}
# Add the circumference method to the Circle class
circumference.Circle <- function(circle) {
    if (!inherits(circle, "Circle")) {
        stop("Object is not of class 'Circle'.")
    }
    return(2 * pi * circle$radius)
}
# Example Usage
my_circle <- Circle(radius = 5)
print(circumference(my_circle))

# Create a Custom S4 Class
# Define a custom S4 class called "Rectangle" that represents a rectangle. The class should store the length and width of the rectangle.
setClass("Rectangle",
        slots = list(length = "numeric", width = "numeric"))

# Write a constructor function Rectangle() that takes the length and width as input and creates an object of the "Rectangle" class.
setMethod("initialize", "Rectangle",
        function(.Object, length, width) {
            if (!is.numeric(length) || length <= 0) {
                stop("Length must be a positive numeric value.")
            }
            if (!is.numeric(width) || width <= 0) {
                stop("Width must be a positive numeric value.")
            }
            .Object@length <- length
            .Object@width <- width
            return(.Object)
        })

# Write a show() method for the Rectangle class to display the length, width, and area of the rectangle when the object is printed.
setMethod("show", "Rectangle",
            function(object) {
                area <- object@length * object@width
                cat(sprintf("Type: 'Rectangle' object\nLength: %.2f\nWidth: %.2f\nArea: %.2f\n",
                            object@length, object@width, area))
            })
# Example Usage
my_rectangle <- new("Rectangle", length = 4, width = 6)
show(my_rectangle)


# Define a Custom Operator for the S4 Class 
# Define a custom operator %==% that compares two Rectangle objects and checks if they have the same area.
# The operator should return TRUE if the areas are equal and FALSE otherwise.
`%==%` <- function(rect1, rect2) {
    if (!inherits(rect1, "Rectangle") || !inherits(rect2, "Rectangle")) {
        stop("Both operands must be of class 'Rectangle'.")
    }
    
    area1 <- rect1@length * rect1@width
    area2 <- rect2@length * rect2@width
    
    return(area1 == area2)
}
# Example Usage
rectangle1 <- new("Rectangle", length = 4, width = 6)
rectangle2 <- new("Rectangle", length = 3, width = 8)
rectangle1 %==% rectangle2