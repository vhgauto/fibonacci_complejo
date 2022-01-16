# librerías
library(tidyverse)
library(gganimate)

# golden ratio
gold <- (1 + sqrt(5))/2

# fórmula de Binet
# calcula el 'n' Número de Fibonacci, permitiendo que la secuencia sea
# continua, para todos los Reales
# como los exponentes son Reales, los resultados pueden ser Complejos ->
# expreso el número áureo como número Complejo con parte Imaginaria 0
f_Binet <- function(n) {
  ((complex(1, gold, 0))^n - (-1/(complex(1, gold, 0)))^n)/sqrt(5)
}

# vector de exponentes 'n' de la fórmula de Binet
expo <- seq(-3, 7, by = .01)

# aplico la fórmula de Binet al vector de exponentes 'resp'
resp <- f_Binet(expo)

# el vector Complejo 'resp' (respuesta) posee parte Real e Imaginaria
eje_real <- Re(resp)
eje_im <- Im(resp)

# combino los vectores y agrego un 'contador' para animar el gráfico
datos <- data.frame(eje_real, eje_im, contador = 1:length(eje_real))

# secuencia de Fibonacci de números enteros
fib <- c(-1, 0, 1, 2, 3, 5, 8)

# ggplot
p <- ggplot() +
  # ejes
  geom_hline(yintercept = 0, color = "grey80", size = 1) +
  geom_vline(xintercept = 0, color = "grey80", size = 1) +
  # curva
  geom_path(data = datos, aes(eje_real, eje_im, color = contador),
            show.legend = FALSE, size = 1, lineend = "round") +
  # puntos
  geom_point(data = datos, aes(eje_real, eje_im, color = contador),
             show.legend = FALSE, size = 1) +
  # valores de ejes
  geom_text(aes(fib, 0, label = fib), nudge_y = -.1, 
            label.padding = unit(.05, "line"), label.size = NA, size = 3,
            color = "black") +
  # ejes
  geom_text(aes(8.4, .08, label = "R")) +
  geom_text(aes(-.1, 1.6, label = "i")) +
  geom_point(aes(fib, 0), size = 1) +
  # color
  scale_color_viridis_c(direction = -1) +
  # zoom
  coord_cartesian(xlim = c(NA, 8.1)) +
  # tema
  theme_void() +
  # theme
  theme(
    aspect.ratio = 1,
    plot.margin = margin(5, 5, 5, 5)
  )

p

# animación
anim <- p +
  transition_reveal(contador)

# render
animate(
  anim,
  width = 800, height = 800, res = 200, fps = 24, nframes = 150
)

# guardo en formato .gif
anim_save("fibo.gif")

# abro el archivo .gif
system2(command = "open", "fibo.gif")
