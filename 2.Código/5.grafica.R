
# Cargar la biblioteca ggplot2 para crear gráficos más elegantes
library(ggplot2)

# Definir los datos
n = c(1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36,
      37,
      38,
      39,
      40,
      41,
      42)
labels2 = c("Cereales",
            "Azúcar",
            "Café",
            "Especias",
            "Otros Alimentos",
            "Bebidas",
            "Carnes",
            "Pescado",
            "Leche",
            "Huevo",
            "Aceites",
            "Tuberculo",
            "Verduras",
            "Frutas",
            "Alimentos fuera del hogar",
            "Accesorios personales",
            "Calzado",
            "Alquiler",
            "Agua",
            "Atención ambulatoria",
            "Adquisición de vehículo",
            "Combustible",
            "Comunicación",
            "Atención hospitalaria",
            "Cuidado Personal",
            "Cuidados",
            "Educación",
            "Energía",
            "Enseres",
            "Esparcimiento",
            "Impuesto Predio",
            "Mantenimiento de vehículo",
            "Medicinas",
            "Otros Gastos",
            "Parques Turísticos",
            "Refacciones de vehículo",
            "Tabaco",
            "Transferencias a otros hogares",
            "Transporte foraneo",
            "Transporte público",
            "Utensilios",
            "Vestido")
est = c(77.0484914551189 ,
        15.7292207589472,
        21.3968468145855,
        2.05343440194705,
        -153.072389739718,
        3.7420843570187,
        1.52394272014808,
        -14.2629838489708,
        32.5751134821014,
        -1.29393908471588,
        10.7614287382421,
        4.87286026915313,
        49.2081641359765,
        8.53694331539761,
        -383.987253347746,
        -2.00138703062083,
        -8.19097402641359,
        -266.237271263818,
        -24.3449771444046,
        -133.052064586254,
        -153.225235684428,
        -323.295513545115,
        -86.92099008346,
        -52.6449914975808,
        -77.3888767983937,
        -157.913168242139,
        -182.224534152541,
        -93.8479252981952,
        -36.6614637094228,
        -24.8011336490898,
        -28.9062589243245,
        -337.167382832891,
        -3.4295541210227,
        -49.3852459194666,
        -1.53528205902753,
        -13.8718692877763,
        -1.83165914412604,
        -205.027880153484,
        -12.750934100442,
        -2.32891562383056,
        8.89402654321417,
        -17.5248804509934)
lower = c(42.3249673320444 ,
          5.42420482469011,
          11.7417436480227,
          -7.08315484040958,
          -218.734529629582,
          -33.1954891510485,
          -70.5158802043029,
          -48.9802633343838,
          -1.41159553310431,
          -13.9241980818546,
          2.02411467656914,
          -1.64976515139349,
          7.69523595060971,
          -15.2481168014811,
          -555.373782896572,
          -10.4725804312429,
          -38.7679386104288,
          -367.052774958939,
          -56.2346086290926,
          -291.543096744227,
          -270.236564544711,
          -409.023440471722,
          -141.864777939931,
          -110.080404784106,
          -124.342937099501,
          -239.192820942547,
          -370.770096029564,
          -146.267338275896,
          -75.3843486147274,
          -68.6895116281755,
          -71.775238043878,
          -428.579660095746,
          -48.7647972697846,
          -112.579810015323,
          -25.305383357059,
          -34.4175907896342,
          -14.3366594821389,
          -361.782354867199,
          -30.443989882149,
          -60.5964214069882,
          -10.0509798473464,
          -52.3287281615305)
upper = c(111.772015578193 ,
          26.0342366932044,
          31.0519499811482,
          11.1900236443037,
          -87.4102498498541,
          40.6796578650859,
          73.563765644599,
          20.4542956364422,
          66.5618224973071,
          11.3363199124228,
          19.498742799915,
          11.3954856896997,
          90.7210923213433,
          32.3220034322763,
          -212.60072379892,
          6.46980637000119,
          22.3859905576016,
          -165.421767568697,
          7.54465434028337,
          25.4389675717198,
          -36.2139068241449,
          -237.567586618508,
          -31.9772022269888,
          4.79042178894429,
          -30.4348164972858,
          -76.6335155417317,
          6.32102772448147,
          -41.4285123204947,
          2.06142119588175,
          19.0872443299959,
          13.962720195229,
          -245.755105570037,
          41.9056890277392,
          13.8093181763898,
          22.2348192390039,
          6.67385221408152,
          10.6733411938868,
          -48.2734054397694,
          4.94212168126501,
          55.9385901593271,
          27.8390329337747,
          17.2789672595437)

# Crear un data frame con los datos
data <- data.frame(n = n, labels2 = labels2, est = est, lower = lower, upper = upper)

library(ggplot2)
library(viridis)

# Crear un factor para mantener el orden original de labels2
data$labels2 <- factor(data$labels2, levels = labels2)

# Crear el gráfico utilizando ggplot2
ggplot(data, aes(x = labels2, y = est)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.09, color = "black", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Tipo de gasto", y = "Efecto en pesos",
       title = " ",
       caption = " ") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.1, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, family = "sans"),
        legend.title = element_blank()) +
  labs(color = "Legend") +
  guides(color = guide_legend(title = "Estimates"))




# Guardar el gráfico en formato PNG con alta calidad
ggsave(
  "/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.06.19 MODELO_GASTO_CEM_ENIGH/Gráficas/1.Gasto_Agregado.png",
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# Cargar la biblioteca ggplot2 para crear gráficos más elegantes
library(ggplot2)

# Definir los datos
n = c(1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16)
labels2 = c("Cereales" ,
            "Azúcar",
            "Café",
            "Otros Alimentos",
            "Aceites",
            "Verduras",
            "Alimentos fuera del hogar",
            "Alquiler",
            "Adquisición de vehículo",
            "Combustible",
            "Comunicación",
            "Cuidado Personal",
            "Cuidados",
            "Energía",
            "Mantenimiento de vehículo",
            "Transferencias a otros hogares")
est = c(77.0484914551189 ,
        15.7292207589472,
        21.3968468145855,
        -153.072389739718,
        10.7614287382421,
        49.2081641359765,
        -383.987253347746,
        -266.237271263818,
        -153.225235684428,
        -323.295513545115,
        -86.92099008346,
        -77.3888767983937,
        -157.913168242139,
        -93.8479252981952,
        -337.167382832891,
        -205.027880153484)
lower = c(42.3249673320444 ,
          5.42420482469011,
          11.7417436480227,
          -218.734529629582,
          2.02411467656914,
          7.69523595060971,
          -555.373782896572,
          -367.052774958939,
          -270.236564544711,
          -409.023440471722,
          -141.864777939931,
          -124.342937099501,
          -239.192820942547,
          -146.267338275896,
          -428.579660095746,
          -361.782354867199)
upper = c(111.772015578193 ,
          26.0342366932044,
          31.0519499811482,
          -87.4102498498541,
          19.498742799915,
          90.7210923213433,
          -212.60072379892,
          -165.421767568697,
          -36.2139068241449,
          -237.567586618508,
          -31.9772022269888,
          -30.4348164972858,
          -76.6335155417317,
          -41.4285123204947,
          -245.755105570037,
          -48.2734054397694)

# Crear un data frame con los datos
data <- data.frame(n = n, labels2 = labels2, est = est, lower = lower, upper = upper)

library(ggplot2)
library(viridis)

# Crear un factor para mantener el orden original de labels2
data$labels2 <- factor(data$labels2, levels = labels2)

# Crear el gráfico utilizando ggplot2
ggplot(data, aes(x = labels2, y = est)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.09, color = "black", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Tipo de gasto", y = "Efecto en pesos",
       title = " ",
       caption = " ") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.1, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, family = "sans"),
        legend.title = element_blank()) +
  labs(color = "Legend") +
  guides(color = guide_legend(title = "Estimates"))




# Guardar el gráfico en formato PNG con alta calidad
ggsave(
  "/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.06.19 MODELO_GASTO_CEM_ENIGH/Gráficas/1.Gasto_Agregado_representativo.png",
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Cargar la biblioteca ggplot2 para crear gráficos más elegantes
library(ggplot2)

# Definir los datos
n = c(1,
      2,
      3,
      4,
      5,
      6)
labels2 = c("Cereales" ,
            "Azúcar",
            "Café",
            "Otros Alimentos",
            "Aceites",
            "Verduras")
est = c(77.0484914551189 ,
        15.7292207589472,
        21.3968468145855,
        -153.072389739718,
        10.7614287382421,
        49.2081641359765)
lower = c(42.3249673320444 ,
          5.42420482469011,
          11.7417436480227,
          -218.734529629582,
          2.02411467656914,
          7.69523595060971)
upper = c(111.772015578193 ,
          26.0342366932044,
          31.0519499811482,
          -87.4102498498541,
          19.498742799915,
          90.7210923213433)

# Crear un data frame con los datos
data <- data.frame(n = n, labels2 = labels2, est = est, lower = lower, upper = upper)

library(ggplot2)
library(viridis)

# Crear un factor para mantener el orden original de labels2
data$labels2 <- factor(data$labels2, levels = labels2)

# Crear el gráfico utilizando ggplot2
ggplot(data, aes(x = labels2, y = est)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.09, color = "black", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = " ", y = " ",
       title = " ",
       caption = " ") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.1, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(hjust = 1, size = 14),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, family = "sans"),
        legend.title = element_blank()) +
  labs(color = "Legend") +
  guides(color = guide_legend(title = "Estimates")) +
  coord_cartesian(ylim = c(-100, 100), xlim = c(0, 10))




# Guardar el gráfico en formato PNG con alta calidad
ggsave(
  "/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.06.19 MODELO_GASTO_CEM_ENIGH/Gráficas/1.Gasto_Agregado_representativo_nivel3.png",
  width = 4.6,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

# Cargar la biblioteca ggplot2 para crear gráficos más elegantes
library(ggplot2)

# Definir los datos
n = c(1,
      2,
      3,
      4,
      5,
      6,
      7,
      8)
labels2 = c("Alimentos fuera del hogar",
            "Alquiler",
            "Adquisición de vehículo",
            "Combustible",
            "Cuidado Personal",
            "Cuidados de la casa",
            "Energía",
            "Mantenimiento de vehículo")
est = c(-383.987253347746,
        -266.237271263818,
        -153.225235684428,
        -323.295513545115,
        -77.3888767983937,
        -157.913168242139,
        -93.8479252981952,
        -337.167382832891)
lower = c(-555.373782896572,
          -367.052774958939,
          -270.236564544711,
          -409.023440471722,
          -124.342937099501,
          -239.192820942547,
          -146.267338275896,
          -428.579660095746)
upper = c(-212.60072379892,
          -165.421767568697,
          -36.2139068241449,
          -237.567586618508,
          -30.4348164972858,
          -76.6335155417317,
          -41.4285123204947,
          -245.755105570037)

# Crear un data frame con los datos
data <- data.frame(n = n, labels2 = labels2, est = est, lower = lower, upper = upper)

library(ggplot2)
library(viridis)

# Crear un factor para mantener el orden original de labels2
data$labels2 <- factor(data$labels2, levels = labels2)

# Crear el gráfico utilizando ggplot2
ggplot(data, aes(x = labels2, y = est)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.09, color = "black", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Tipo de gasto", y = "Efecto en pesos",
       title = " Gasto a nivel 2",
       caption = " ") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.1, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, family = "sans"),
        legend.title = element_blank()) +
  labs(color = "Legend") +
  guides(color = guide_legend(title = "Estimates"))




# Guardar el gráfico en formato PNG con alta calidad
ggsave(
  "/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.06.19 MODELO_GASTO_CEM_ENIGH/Gráficas/1.Gasto_Agregado_representativo_nivel2.png",
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# Definir los datos
n = c(1,
      2)
labels2 = c("Comunicación",
            "Transferencias a otros hogares",)
est = c(-86.92099008346,
        -205.027880153484)
lower = c(-141.864777939931,
          -361.782354867199)
upper = c(-31.9772022269888,
          -48.2734054397694)

# Crear un data frame con los datos
data <- data.frame(n = n, labels2 = labels2, est = est, lower = lower, upper = upper)

library(ggplot2)
library(viridis)

# Crear un factor para mantener el orden original de labels2
data$labels2 <- factor(data$labels2, levels = labels2)

# Crear el gráfico utilizando ggplot2
ggplot(data, aes(x = labels2, y = est)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.09, color = "black", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Tipo de gasto", y = "Efecto en pesos",
       title = " Gasto a nivel 2",
       caption = " ") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.1, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid = element_blank(),
        plot.caption = element_text(hjust = 0, family = "sans"),
        legend.title = element_blank()) +
  labs(color = "Legend") +
  guides(color = guide_legend(title = "Estimates"))




# Guardar el gráfico en formato PNG con alta calidad
ggsave(
  "/Users/jonathanguallasamin/Dropbox/Tareas_Jonathan/2023.06.19 MODELO_GASTO_CEM_ENIGH/Gráficas/1.Gasto_Agregado_representativo_nivel1.png",
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)
