library(ggplot2)
library(dplyr)
library(pscl)
library(MASS)

# 1. Wczytaj dane
df <- read.csv("driver_at_fault_sample_final.csv", stringsAsFactors = FALSE)

# 2. Upewnij się, że kolumna ma typ numeryczny
df$total_victims <- as.numeric(df$total_victims)

# 3. Oblicz udział zer
zero_count <- sum(df$total_victims == 0, na.rm = TRUE)
total_count <- sum(!is.na(df$total_victims))
zero_percent <- round(100 * zero_count / total_count, 2)

cat("Liczba zer: ", zero_count, " z ", total_count, " obserwacji\n")
cat("Procent zer: ", zero_percent, "%\n")

# 2. Konwersja total_victims na numeric (na wypadek błędów przy wczytywaniu)
df$total_victims <- as.numeric(df$total_victims)

# 3. Podstawowy wykres słupkowy rozkładu
ggplot(df, aes(x = total_victims)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Rozkład liczby poszkodowanych (total_victims)",
    x = "Liczba poszkodowanych",
    y = "Liczba wypadków"
  ) +
  theme_minimal()

df$total_victims <- as.numeric(df$total_victims)

# Lista wybranych predyktorów
predictors <- c(
  "car_age", "party_age", "collision_severity", "weather_1", "lighting",
  "road_surface", "direction_of_travel", "hit_and_run", "motorcycle_collision",
  "truck_collision"
)

# Formuły do modeli
zinb_formula <- as.formula(paste(
  "total_victims ~", paste(predictors, collapse = " + "),
  "|", paste(predictors, collapse = " + ")
))
count_formula <- as.formula(paste("total_victims ~", paste(predictors, collapse = " + ")))

# 1. Poisson
pois_model <- glm(count_formula, family = "poisson", data = df)

# 2. Negative Binomial
nb_model <- glm.nb(count_formula, data = df)

# 3. ZIP
zip_model <- zeroinfl(zinb_formula, data = df, dist = "poisson")

# 4. ZINB
zinb_model <- zeroinfl(zinb_formula, data = df, dist = "negbin")

# 6. AIC porównanie
cat("AIC porównanie:\n")
cat("Poisson AIC: ", AIC(pois_model), "\n")
cat("NB AIC: ", AIC(nb_model), "\n")
cat("ZIP AIC: ", AIC(zip_model), "\n")
cat("ZINB AIC: ", AIC(zinb_model), "\n")

# 7. Vuong testy
cat("\nVuong test ZIP vs Poisson:\n")
print(vuong(zip_model, pois_model))

cat("\nVuong test ZIP vs NB:\n")
print(vuong(zip_model, nb_model))

cat("\nVuong test ZINB vs NB:\n")
print(vuong(zinb_model, nb_model))

cat("\nVuong test ZINB vs ZIP:\n")
print(vuong(zinb_model, zip_model))

zero.test <- function(x) {
  if(is.table(x)) {
    if(length(dim(x)) > 1) stop ("x must be a 1-way table")
    x <- rep(as.numeric(names(x)), unname(c(x)))
  }
  lambda <- mean(x)
  p0_tilde <- exp(-lambda)
  n0 <- sum(1 * (!(x > 0)))
  n <- length(x)
  numerator <- (n0 - n * p0_tilde)^2
  denominator <- n * p0_tilde * (1 - p0_tilde) - n * lambda * (p0_tilde^2)
  stat <- numerator / denominator
  pvalue <- pchisq(stat, df = 1, lower.tail = FALSE)
  result <- list(statistic = stat, df = 1, prob = pvalue)
  cat(paste("Score test for zero inflation\n\n",
            "\tChi-square =", round(stat, 5), "\n",
            "\tdf = 1\n",
            "\tp-value:", format.pval(pvalue), "\n"))
  print(result)
  
}

zero_test_result <- zero.test(df$total_victims)

# 1. Dopasuj model Poissona
pois_model <- glm(total_victims ~ 1, family = "poisson", data = df)

# 2. Dopasuj model Negatywno-dwumianowy
nb_model <- glm.nb(total_victims ~ 1, data = df)

# 3. Test LR: NB vs Poisson
lrtest_result <- anova(pois_model, nb_model, test = "LRT")
print(lrtest_result)

# Oblicz statystykę overdispersion
dispersion <- sum(residuals(pois_model, type = "pearson")^2) / pois_model$df.residual

# Wyświetl wynik
cat("Overdispersion statistic:", round(dispersion, 3), "\n")


# Dopasowanie modelu Poissona
fm_pois <- glm(total_victims ~ 1, family = poisson, data = df)

# Dopasowanie modelu Negative Binomial
fm_nbin <- glm.nb(total_victims ~ 1, data = df)

# LR test statistic
options(scipen=100)  # aby uniknąć notacji naukowej
LRtest <- 2 * (logLik(fm_nbin) - logLik(fm_pois))

# Wyświetl statystykę LR testu
cat("LR test statistic:", LRtest[1], "\n")

# Oblicz p-value z rozkładu chi-kwadrat z 1 stopniem swobody
p_val <- pchisq(LRtest[1], df = 1, lower.tail = FALSE)

cat("p-value:", p_val, "\n")

# Interpretacja
if(p_val < 0.05) {
  cat("Wynik testu: odrzucamy H0.\n")
  cat("Model Negative Binomial jest statystycznie lepszy niż Poisson.\n")
} else {
  cat("Wynik testu: brak podstaw do odrzucenia H0.\n")
  cat("Model Poissona jest wystarczający.\n")
}

library(pscl)       # zeroinfl
library(lmtest)     # lrtest, waldtest
library(MASS)       # glm.nb
library(vcdExtra)   # zero.test

# 1. Dopasowanie modelu ZINB - bez dodatkowych zmiennych (intercept only)
fm_zinb0 <- zeroinfl(total_victims ~ 1 | 1, data = df, dist = "negbin")
summary(fm_zinb0)

# 2. Dopasowanie modelu ZINB z dodatkowymi predyktorami (tu przykład - możesz podmienić zmienne)
# Zakładam, że masz zmienne takie jak np. car_age, collision_type itd.
# Jeśli nie, zostaw ~ 1 | 1
# Dopasowanie rozbudowanego modelu ZINB z wieloma predyktorami
fm_zinb <- zeroinfl(
  total_victims ~ car_age + party_age + collision_severity + weather_1 | hit_and_run + lighting, 
  data = df, dist = "negbin"
)
summary(fm_zinb)

# Test Warda porównujący model intercept-only do rozbudowanego modelu
waldtest(fm_zinb0, fm_zinb)

# Test LR porównujący model intercept-only do rozbudowanego modelu
lrtest(fm_zinb0, fm_zinb)

# Test zero-inflation dla NB (czy jest nadmiar zer)
zero.test(df$total_victims)

# Modele ZINP (Zero Inflated Poisson) - intercept only
fm_zinp0 <- zeroinfl(total_victims ~ 1 | 1, data = df, dist = "poisson")
summary(fm_zinp0)

# Model ZINP z tymi samymi predyktorami co ZINB
fm_zinp <- zeroinfl(
  total_victims ~ car_age + party_age + collision_severity + weather_1 | hit_and_run + lighting,
  data = df, dist = "poisson"
)
summary(fm_zinp)


# 7. Test LR czy uwzględnienie predyktorów w modelu ZINP jest uzasadnione
lrtest(fm_zinp0, fm_zinp)

# 8. Porównanie AIC ZINB i ZINP - który model jest lepszy?
cat("AIC ZINB model:", AIC(fm_zinb), "\n")
cat("AIC ZINP model:", AIC(fm_zinp), "\n")