# Konstruktor für Circle-Objekte
my_circle <- function(radius) {
  circle <- list(radius = radius)
  class(circle) <- "Circle"  # Setzt die Klasse auf "Circle"
  return(circle)
}

# print.Circle Methode für Circle-Objekte
print.Circle <- function(x) {
  # Berechne die Fläche des Kreises
  area <- pi * (x$radius^2)
  
  # Gib den Radius und die Fläche aus
  cat("Circle with radius:", x$radius, "\n")
  cat("Area:", area, "\n")  # Korrektur des Zeilenumbruchs
}

# Erstelle ein Circle-Objekt mit Radius 5
my_circle_obj <- my_circle(5)  # Achte darauf,

print.Circle(my_circle_obj)


# Konstruktor für Circle-Objekte (mit Koordinaten x, y und radius)
my_circle <- function(radius, x, y) {
  circle <- list(radius = radius, x = x, y = y)  # x, y, und radius werden in das Objekt aufgenommen
  class(circle) <- "Circle"  # Setzt die Klasse auf "Circle"
  return(circle)
}

# print.Circle Methode für Circle-Objekte
print.Circle <- function(x) {
  cat("Circle at (", x$x, ",", x$y, ") with radius:", x$radius, "\n")
}

# Definieren des benutzerdefinierten Operators %>% für die Berechnung und den Schnitt von Kreisen
`%>%` <- function(circle1, circle2) {
  # Berechnung der Distanz zwischen den Mittelpunkten der beiden Kreise
  distance <- sqrt((circle2$x - circle1$x)^2 + (circle2$y - circle1$y)^2)
  
  # Berechnung der Summe der Radien
  sum_radii <- circle1$radius + circle2$radius
  
  # Überprüfen, ob die Kreise sich überschneiden
  if (distance <= sum_radii) {
    return(TRUE)  # Die Kreise überschneiden sich
  } else {
    return(FALSE)  # Die Kreise überschneiden sich nicht
  }
}

# Erstellen von zwei Circle-Objekten
circle1 <- my_circle(0, 0, 5)  # Kreis 1: Zentrum (0, 0) und Radius 5
circle2 <- my_circle(3, 4, 3)  # Kreis 2: Zentrum (3, 4) und Radius 3

# Ausgeben der Kreise
print(circle1)
print(circle2)

# Benutzen des Operators %>% um zu überprüfen, ob die Kreise sich überschneiden
result <- circle1 %>% circle2

# Ausgabe des Ergebnisses
cat("Do the circles intersect?", result, "\n")


circumference <- function(radius) {
  return(2 * 3.14159 * radius)
}

my_circle <- my_circle(5,0,0)
circumference(my_circle$radius)

library(methods)

# 1️⃣ S4-Klasse "Rectangle" definieren
setClass("Rectangle",
         slots = list(
           length = "numeric",
           width = "numeric"
         ))

# 2️⃣ Konstruktor-Funktion für "Rectangle"
Rectangle <- function(length, width) {
  if (length <= 0 || width <= 0) {
    stop("length and width must be positive numbers.")
  }
  new("Rectangle", length = length, width = width)
}

# 3️⃣ show() Methode für "Rectangle" definieren
setMethod(
  "show",
  "Rectangle",
  function(object) {
    area <- object@length * object@width  # Berechnung der Fläche
    cat('Type: "Rectangle object"\n')     # Vorgeschriebener Output
    cat("Length:", object@length, "\n")
    cat("Width:", object@width, "\n")
    cat("Area:", area, "\n")
  }
)

# 4️⃣ Benutzerdefinierter Operator %==% für Flächenvergleich
`%==%` <- function(rectangle1, rectangle2) {
  if (!is(rectangle1, "Rectangle") || !is(rectangle2, "Rectangle")) {
    stop("Both operands must be objects of class 'Rectangle'.")
  }
  
  # Berechnung der Flächen
  area_a <- rectangle1@length * rectangle1@width
  area_b <- rectangle2@length * rectangle2@width  # Fehler korrigiert hier
  
  return(area_a == area_b)
}

# 5️⃣ Test des Operators
rectangle1 <- Rectangle(4,6)
rectangle2 <- Rectangle(6,4)

# Teste den Operator
print(rectangle1 %==% rectangle2)  # TRUE (24 == 24)
