# ==============================================================================
# HW 1 - Bayesian Dynamic Modeling
# Nome : Bruno Drezza Reis de Souza
# ==============================================================================

# ==========================================
# PROBLEM 4: Normal-Normal Update
# ==========================================

# 1. Dados fornecidos pelo enunciado
mu_0 <- 0          # Média da priori
tau2_0 <- 1        # Variância da priori
sigma <- 0.25      # Desvio padrão populacional conhecido
n <- 15            # Tamanho da amostra
sum_x <- 8.31      # Soma das observações

# Calculando a média amostral (x barra)
x_bar <- sum_x / n

# 2. Trabalhando com Precisões (o inverso da variância)
# Essa é a forma mais fácil de calcular a atualização Normal-Normal
prec_0 <- 1 / tau2_0                  # Precisão da priori
prec_data <- n / (sigma^2)            # Precisão dos dados (informação da amostra)

# A precisão da posteriori é simplesmente a soma das precisões!
prec_1 <- prec_0 + prec_data

# 3. Calculando os parâmetros da Posteriori
# Variância a posteriori
tau2_1 <- 1 / prec_1

# Média a posteriori (média ponderada pelas precisões)
mu_1 <- (prec_0 * mu_0 + prec_data * x_bar) / prec_1

# --- Respostas do Item (i) ---
cat("=== RESULTADOS DO ITEM (i) ===\n")
cat("Média a posteriori (mu_1):", round(mu_1, 4), "\n")
cat("Variância a posteriori (tau2_1):", round(tau2_1, 6), "\n")
cat("Desvio padrão a posteriori:", round(sqrt(tau2_1), 4), "\n\n")

# --- Respostas do Item (ii): Gráficos ---
par(mfrow = c(1, 1))

# Definindo um eixo X que mostre bem ambas as curvas
x_vals <- seq(-3, 3, length.out = 1000)

# Plot da Priori
plot(x_vals, dnorm(x_vals, mean = mu_0, sd = sqrt(tau2_0)), type = "l", 
     col = "blue", lwd = 2, ylab = "Densidade", xlab = expression(mu),
     main = "Atualização Normal-Normal", ylim = c(0, 7))

# Plot da Posteriori sobreposta
lines(x_vals, dnorm(x_vals, mean = mu_1, sd = sqrt(tau2_1)), col = "red", lwd = 2)

# Linha da média amostral (MLE) para referência visual
abline(v = x_bar, col = "green3", lty = 2, lwd = 2)

# Legenda
legend("topleft", legend = c(expression(paste("Priori: ", mu, " ~ N(0, 1)")), 
                             expression(paste("Posteriori: ", mu, " | x ~ N(0.551, 0.004)")), 
                             "Média Amostral (x_bar = 0.554)"),
       col = c("blue", "red", "green3"), lty = c(1, 1, 2), lwd = 2, bty = "n")