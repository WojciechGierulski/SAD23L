library(dplyr)
library(Hmisc)
library(purrr)
library(ggplot2)


df <- read.csv(".../DBW_DB_2024-04-29_1304.csv", sep=";")
df_inf <- read.csv(".../inflation-years.csv", sep=",")
df$wartosc <- gsub(",", ".", df$wartosc)
df$wartosc <- as.numeric(df$wartosc)

df_inf$index <- df_inf$index - 100
df_br <- df %>% filter(id_zmienna == 407) %>% select(nazwa_pozycja_2, wartosc, id_daty)

df_pkb <- df %>% filter(id_zmienna == 1038) %>% select(wartosc, id_daty)


# Dodanie kolumny z wartoscia bezwzgledna nakladu
df_br$wartosc_bezwzgledna <- NA
for (i in 1:nrow(df_br)) {
      pkb <- df_pkb[df_pkb$id_daty == df_br$id_daty[i],]$wartosc
      df_br[i, "wartosc_bezwzgledna"] <- (df_br$wartosc[i] / 100) * pkb
}

# Dodanie kolumny procentowego wzrostu nakładu
min_year <- min(df_br$id_daty)
df_br$wzrost <- NA
for (i in 1:nrow(df_br)) {
      if (df_br$id_daty[i] == min_year) {
            next
      }
      prev_row <- df_br[(df_br$id_daty == (df_br$id_daty[i] - 1)) & df_br$nazwa_pozycja_2 == df_br$nazwa_pozycja_2[i],]
      df_br[i, "wzrost"] <- 100 * (df_br$wartosc_bezwzgledna[i] - prev_row$wartosc_bezwzgledna) / prev_row$wartosc_bezwzgledna
}

df_br <- df_br[!is.na(df_br$wzrost),]
df_inf <- df_inf[df_inf$year %in% unique(df_br$id_daty),]


x_labels <- unique(df_br$id_daty)
colors <- c("Wzrost nakładów na B+R"="red3", "Inflacja roczna"="black")
p = ggplot() +
  geom_line(data=df_br[df_br$nazwa_pozycja_2 == "Ogółem",], aes(y=wzrost, x=id_daty, color="Wzrost nakładów na B+R")) +
  geom_point(data=df_br[df_br$nazwa_pozycja_2 == "Ogółem",], aes(y=wzrost, x=id_daty, color="Wzrost nakładów na B+R")) +
  geom_line(data=df_inf, aes(y=index, x=year, color="Inflacja roczna"), linetype="dashed") +
  labs(x = "Rok",
      y = "[%]",
      color = "") +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = x_labels, labels = x_labels) +
  theme(axis.text.x = element_text(angle = 300, vjust = 0.8, hjust=0))
print(p)


p = ggplot() +
  geom_line(data=df_br, aes(y=wartosc_bezwzgledna, x=id_daty, color=nazwa_pozycja_2, group=nazwa_pozycja_2)) +
  geom_point(data=df_br, aes(y=wartosc_bezwzgledna, x=id_daty, color=nazwa_pozycja_2, group=nazwa_pozycja_2)) +
  labs(x = "Rok",
      y = "Nakłady na B+R [MLN PLN]",
      color = "") +
  theme(axis.text.x = element_text(angle = 300, vjust = 0.8, hjust=0))
print(p)



x_labels <- unique(df_br$id_daty)
colors <- c("Sektor rządowy"="blue3", "Sektor szkolnictwa wyższego"="gold3", "Sektor przedsiębiorstw"="green3", "Inflacja roczna"="black")
p = ggplot() +
  geom_line(data=df_br[df_br$nazwa_pozycja_2 == "Sektor rządowy (GOV)",], aes(y=wzrost, x=id_daty, color="Sektor rządowy")) +
  geom_point(data=df_br[df_br$nazwa_pozycja_2 == "Sektor rządowy (GOV)",], aes(y=wzrost, x=id_daty, color="Sektor rządowy")) +
  geom_line(data=df_br[df_br$nazwa_pozycja_2 == "Sektor szkolnictwa wyższego (HES)",], aes(y=wzrost, x=id_daty, color="Sektor szkolnictwa wyższego")) +
  geom_point(data=df_br[df_br$nazwa_pozycja_2 == "Sektor szkolnictwa wyższego (HES)",], aes(y=wzrost, x=id_daty, color="Sektor szkolnictwa wyższego")) +
  geom_line(data=df_br[df_br$nazwa_pozycja_2 == "Sektor przedsiębiorstw (BES)",], aes(y=wzrost, x=id_daty, color="Sektor przedsiębiorstw")) +
  geom_point(data=df_br[df_br$nazwa_pozycja_2 == "Sektor przedsiębiorstw (BES)",], aes(y=wzrost, x=id_daty, color="Sektor przedsiębiorstw")) +
  geom_line(data=df_inf, aes(y=index, x=year, color="Inflacja roczna"), linetype="dashed") +
  labs(x = "Rok",
      y = "Wzrost nakładów B+R względem poprzedniego roku [%]",
      color = "") +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = x_labels, labels = x_labels) +
  theme(axis.text.x = element_text(angle = 300, vjust = 0.8, hjust=0))
print(p)