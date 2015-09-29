rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}

expuest <- (round(runif(1, min = 20, max = 300), 0) #  total expuestos

noexp <- round(runif(1, min= 20, max = 300), 0) # total no expuestos

n <- aexp + bnoexp # total de la población fuente

rate <- round(runif(1, min = 0, max = 1), 2) # tasa similar en ambos grupos

b1 <- round(runif(1, min = 20, max = bn), 0) # número de controles expuestos

t1sum <- b1/rate # tiempo de exposición en los controles expuestos

t1 <- rand_vect(, t1sum)

b0 <- bn - b1

t0sum <- b0/rate # tiempo de no exposición en los controles no expuestos

b0
sum(t0)



b0/sum(t0)

b1/sum(t1)

tt <- sum(t0) + sum(t1) # tiempo total de seguimiento en los controles

summary(t0)
summary(t1)
sd(t1)



# -------------

id <- seq(from = 1, to = 2500)
expuestos <- rep(1, times = 500)  # total de expuestos
noexpuest <- rep(0, times = 2000)
expos <- append(expuestos, noexpuest) # tamaño muestra cohorte = 2500

des.exp <- rbinom(length(expuestos), size = 1, prob = 0.60)
des.noexp <- rbinom(length(noexpuest), size = 1, prob = 0.20)

desen <- append(des.exp, des.noexp)

lab.source <- c("id", "exposición", "desenlace")

sourcep <- data.frame(id, expos, desen)

time.exp <- as.integer(length(id[sourcep$expos == 1 & sourcep$desen == 1])/0.80)
time.noexp <- as.integer(length(id[sourcep$expos == 0 & sourcep$desen == 1])/0.20)

rate.ratio <- (length(id[sourcep$expos == 1 & sourcep$desen == 1]) / time.exp) / 
  (length(id[sourcep$expos == 0 & sourcep$desen == 1]) / time.noexp)

b1 <- id[sourcep$desen == 0 & sourcep$expos == 1] # Controles expuestos
b0 <- id[sourcep$desen == 0 & sourcep$expos == 0] # Controles no expuestos

tasa.cexp <- length(b1)/time.exp # tasa controles expuestos
tasa.cexp 

tasa.cnexp <- length(b0)/time.noexp # tasa controles no expuestos
tasa.cnexp

cont1 <- id[sourcep$expos == 1 & sourcep$desen == 0]
cont0 <- id[sourcep$expos == 0 & sourcep$desen == 0]

rate <- round(runif(1, min = 0, max = 0.01), 5) # tasa similar en ambos grupos

t1sum <- as.integer(length(cont1)/rate)

t1 <- rand_vect(length(expuestos), t1sum, sd = (0.1/rate), pos.only = TRUE)

t0sum <- as.integer(length(cont0)/rate)

t0 <- rand_vect(length(noexpuest), t0sum, sd = (0.1/rate), pos.only = TRUE)

time <- append(t1, t0)

sourcep <- cbind(sourcep, time)


length(id[sourcep$expos == 1 & sourcep$desen == 1])
295/0.80


