#Loop ou Laço

soma <- 0
for(i in 1:10) {
  soma <- soma + i
}

soma

# loop imprime i

for ( i in 1:12) {
  print(i)
}

for(j in c("a", "b", "c")){
  print(j)
}

# listas

x <- list(1, 1:2)
x[[2]]
x[[2]][2]

lista1 <- list(1, 1:5, "Manoel")

list(1, 1:5, "Manoel",
     data.frame(id=1:3, x=rnorm(3)),
     lista1)

lista_vazia <- list()
