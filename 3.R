install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

current_script_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_script_directory)


df_inflation <- read.csv("inflation-years.csv",header = TRUE,sep = ",")
df_inflation$index <- df_inflation$index - 100


df_apartment <- read.csv("cena_1_m2_powierzchni_uzytkowej_budynku_mieszkalnego_oddanego_do_uzytkowania_2.csv",header = TRUE,sep = ";")
df_apartment <- df_apartment %>% filter(Kwartal == 4) %>% select(Rok, Wartosc)
df_apartment <- arrange(df_apartment, Rok)
df_apartment$Wartosc <- as.numeric(gsub("\\s+", "", df_apartment$Wartosc))


df_salary <- read.csv("zarobki.csv",header = TRUE,sep = ";")
df_salary <- df_salary %>% filter(Rok >= 1998)
df_salary <- select(df_salary, Rok, Zarobki)
df_salary$Zarobki <- as.numeric(gsub("\\s+", "", df_salary$Zarobki))


df_salary_inc <- df_salary
df_salary_inc$Wzrost_zarobkow <- c(NA, diff(df_salary$Zarobki))
df_salary_inc$Procentowy_wzrost_zarobkow <- df_salary_inc$Wzrost_zarobkow / df_salary_inc$Zarobki * 100
df_salary_inc <- select(df_salary_inc, Rok, Procentowy_wzrost_zarobkow)


df_apartment_inc <- df_apartment
df_apartment_inc$Wzrost_cen <- c(NA, diff(df_apartment_inc$Wartosc))
df_apartment_inc$Procentowy_wzrost_cen <- df_apartment_inc$Wzrost_cen / df_apartment_inc$Wartosc * 100
df_apartment_inc <- select(df_apartment_inc, Rok, Procentowy_wzrost_cen)


df_m_for_salary <- df_apartment
df_m_for_salary$meters <- df_salary$Zarobki / df_apartment$Wartosc
df_m_for_salary <- select(df_m_for_salary, Rok, meters)


df_real_salary_inc <- df_salary_inc
df_real_salary_inc$Rzeczywisty_wzrost_zarobkow <- df_salary_inc$Procentowy_wzrost_zarobkow - df_inflation$index
df_real_salary_inc <- select(df_real_salary_inc, Rok, Rzeczywisty_wzrost_zarobkow)


plot_dif <- ggplot() +
  geom_line(data = df_inflation, aes(x = year, y = index, color = "Inflacja"), linetype = "solid") +
  geom_line(data = df_salary_inc, aes(x = Rok, y = Procentowy_wzrost_zarobkow, color = "Procentowy wzrost zarobków"), linetype = "solid", size = 1.3) +
  geom_line(data = df_apartment_inc, aes(x = Rok, y = Procentowy_wzrost_cen, color = "Procentowy wzrost cen mieszkań"), linetype = "solid") +
  geom_point(data = df_inflation, aes(x = year, y = index, color = "Inflacja")) +
  geom_point(data = df_salary_inc, aes(x = Rok, y = Procentowy_wzrost_zarobkow, color = "Procentowy wzrost zarobków"), size = 1.3) +
  geom_point(data = df_apartment_inc, aes(x = Rok, y = Procentowy_wzrost_cen, color = "Procentowy wzrost cen mieszkań")) +
  labs(x = "Rok", y = "Wartość", color = "Legenda: ", title = "Inflacja, wzrost cen mieszkań i zarobków w latach 1999-2023") +
  scale_color_manual(values = c("Inflacja" = "blue", "Procentowy wzrost zarobków" = "black", "Procentowy wzrost cen mieszkań" = "green")) +
  theme(legend.position = "top", plot.title = element_text(hjust = 0.5))


plot_salary <- ggplot() +
  geom_line(data = df_real_salary_inc, aes(x = Rok, y = Rzeczywisty_wzrost_zarobkow, color = "Rzeczywisty wzrost zarobkow"), linetype = "solid") +
  geom_point(data = df_real_salary_inc, aes(x = Rok, y = Rzeczywisty_wzrost_zarobkow, color = "Rzeczywisty wzrost zarobkow")) +
  labs(x = "Rok", y = "Wartość", title = "Rzeczywisty wzrost zarobkow") +
  scale_color_manual(values = c("Rzeczywisty wzrost zarobkow" = "black")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


plot_m <- ggplot() +
  geom_line(data = df_m_for_salary, aes(x = Rok, y = meters, color = "Metry kwadratowe za wynagrodzenie"), linetype = "solid") +
  geom_point(data = df_m_for_salary, aes(x = Rok, y = meters, color = "Metry kwadratowe za wynagrodzenie")) +
  labs(x = "Rok", y = "Wartość", title = "Ile metrów kwadratowych mieszkania można kupić za miesięczne wynagrodzenie") +
  scale_color_manual(values = c("Metry kwadratowe za wynagrodzenie" = "black")) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  

print(plot_dif)
print(plot_salary)
print(plot_m)

