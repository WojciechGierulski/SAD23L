install.packages("Hmisc")
install.packages("dplyr")
install.packages("purrr")
install.packages("ggplot2")
install.packages("httpgd")
install.packages("languageserver")

library(dplyr)
library(Hmisc)
library(purrr)
library(ggplot2)

df_inf <- read.csv(".../inflation-months.csv",header = TRUE,sep = ",")
df_inf$inflation_index <- (df_inf$inflation_index - 100)

df <- read.csv(".../DBW_DB_2024-04-24_2204.csv",header = TRUE,sep = ";")

mapping <- list(
  "odzież"=c("koszula męska, bawełna z poliestrem, długi rękaw",
             "półbuty męskie skórzane na podeszwie nieskórzanej za 1 parę",
             "Garnitur męski 2-częściowy, z tkaniny z udziałem wełny za 1 kpl"
             ),
  "media"=c("energia elektryczna dla gospodarstw domowych (taryfa G-11) za 1 kWh",
               "ciepła woda za 1 m3",
               "centralne ogrzewanie lokali mieszkalnych za 1 m2 p u"
               ),
  "zdrowie"=c("Doxycyclinum, kapsułki 0,1 g za 10 szt.",
              "Geriavit, kapsułki za 100 szt.",
              "Ibuprofen, drażetki 0,2 g za 60 szt.",
              "wizyta u lekarza specjalisty",
              "Apap, tabletki, w opakowaniu 12 szt."
              ),
  "higiena"=c("strzyżenie włosów męskich",
              "szampon do włosów za 400 ml",
              "pasta do zębów za 100 ml",
              "Mydło toaletowe w kostce – za 90 g"),
  "transport"=c("bilet jednorazowy na przejazd autobusem miejskim",
                "przejazd taksówką osobową (taryfa dzienna) - za 5 km"),
  "mięso"=c("mięso wołowe z kością (rostbef) za 1 kg",
            "mięso wołowe bez kości (z udźca) za 1 kg",
            "mięso wieprzowe z kością (schab środkowy) za 1 kg",
            "kurczęta patroszone za 1 kg",
            "szynka wieprzowa gotowana za 1 kg",
            "polędwica drobiowa za 1 kg",
            "kiełbasa wędzona za 1 kg",
            "filety z morszczuka mrożone za 1 kg"),
  "nabiał"=c("mleko krowie spożywcze o zawartości tłuszczu 3-3,5%, sterylizowane za 1 l",
             "ser twarogowy półtłusty za 1 kg",
             "masło świeże o zawartości tłuszczu ok. 82,5% za 200 g"),
  "owoce"=c("pomarańcze za 1 kg",
            "jabłka za 1 kg"),
  "warzywa"=c("buraki za 1 kg",
              "marchew za 1 kg",
              "cebula za 1 kg",
              "ziemniaki za 1 kg"),
  "produkty zbożowe"=c("chleb pszenno-żytni za 0.5 kg",
                       "mąka pszenna za 1 kg",
                       "kasza jęczmienna za 0.5 kg"),
  "jajka"=c("jaja kurze świeże za 1 szt"),
  "tłuszcze"=c("masło świeże o zawartości tłuszczu ok. 82,5% za 200 g",
               "olej rzepakowy produkcji krajowej za 1 l")
  )

cat_mapping = list(
  "jedzenie" = c("mięso", "nabiał", "owoce", "warzywa", "produkty zbożowe", "jajka", "tłuszcze"),
  "inne" = c("odzież", "media", "zdrowie", "higiena", "transport")
)

all_categories = unlist(mapping)
filtered_df <- df %>%
  filter(nazwa_pozycja_2 %in% all_categories | nazwa_pozycja_3 %in% all_categories)
filtered_df$produkt <- ifelse(filtered_df$nazwa_pozycja_2 == "", filtered_df$nazwa_pozycja_3, filtered_df$nazwa_pozycja_2)

filtered_df[, "inflation_rate"] <- NA
filtered_df$wartosc <- gsub(",", ".", filtered_df$wartosc)
filtered_df$wartosc <- as.numeric(filtered_df$wartosc)

min_year <- min(filtered_df$id_daty)
min_month <- min(filtered_df[filtered_df$id_daty == min_year,]$id_okres)


for (row in 1:nrow(filtered_df)) {
  curr_row <- filtered_df[row,]
  if (curr_row$id_okres == min_month & curr_row$id_daty == min_year) {
    next
  }
  product <- curr_row$produkt
  if (curr_row$id_okres == 247) {  ## 247 means january
    prev_row <- filtered_df[filtered_df$id_okres == 247+11 & filtered_df$id_daty == curr_row$id_daty - 1 & filtered_df$produkt == product,]
  } else {
    prev_row <- filtered_df[filtered_df$id_okres == curr_row$id_okres - 1 & filtered_df$id_daty == curr_row$id_daty & filtered_df$produkt == product,]
  }
  if (is.na(curr_row$wartosc) == TRUE | is.na(prev_row$wartosc) == TRUE) {
    next
  }
  inflation <- 100 * (curr_row$wartosc - prev_row$wartosc) / prev_row$wartosc
  filtered_df[row, "inflation_rate"] <- inflation
}

filtered_df$date_index = 12 * (filtered_df$id_daty - 2020) + filtered_df$id_okres - 246
filtered_df$date_index = as.numeric(filtered_df$date_index)

date_index_to_str <- function(date_index){
  months = c("12", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11")
  paste(months[date_index %% 12 + 1], "/", toString(ceiling(date_index / 12 + 2019)), sep="")
}

map_category <- function(product) {
  for (cat in names(mapping)) {
    if (product %in% mapping[[cat]]) {
      return(unlist(cat))
    }
  }
}

filtered_df$date_str = unlist(map(filtered_df$date_index, date_index_to_str))
filtered_df$category = unlist(map(filtered_df$produkt, map_category))
df_inf$date_str = unlist(map(df_inf$date_index, date_index_to_str))
filtered_df_notna <- filtered_df[!is.na(filtered_df$inflation_rate),]
filtered_df_notna <- filtered_df_notna[,c("category", "date_index", "date_str", "inflation_rate")]


df_groupped <- filtered_df_notna %>%
  group_by(category, date_index, date_str) %>%
  summarise(mean_inflation_rate = mean(inflation_rate))

df_groupped_date <- filtered_df_notna %>%
  group_by(date_index, date_str) %>%
  summarise(mean_inflation_rate = mean(inflation_rate))


df_groupped$date_index <- as.numeric(df_groupped$date_index)
df_groupped$mean_inflation_rate <- as.numeric(df_groupped$mean_inflation_rate)
df_inf <- rev(df_inf)

df_food <- df_groupped[df_groupped$category %in% cat_mapping$jedzenie,]
x_breaks <- unique(df_food[order(df_food$date_index,decreasing=FALSE),]$date_str)
x_labels <- unique(df_food[order(df_food$date_index,decreasing=FALSE),]$date_str)
x_labels[c(FALSE, TRUE)] <- ""
p = ggplot(df_food) +
  geom_line(aes(y=mean_inflation_rate, x=factor(date_index), color=category, group=category)) +
  geom_point(aes(y=mean_inflation_rate, x=factor(date_index), color=category, group=category)) +
  geom_line(data=df_inf, aes(y=inflation_index, x=factor(date_index), color="black", group="black"), linetype="solid", color="black", linewidth=1.2) +
  scale_x_discrete(labels = x_labels) +
  labs(x="Data",
  y="Uśredniona inflacja w stosunku do poprzedniego miesiąca [%]",
  color = "Kategorie") +
  theme(axis.text.x = element_text(angle = 300, vjust = 0.8, hjust=0))
print(p)

x_breaks <- unique(df_groupped_date[order(df_groupped_date$date_index,decreasing=FALSE),]$date_index)
x_labels <- unique(df_groupped_date[order(df_groupped_date$date_index,decreasing=FALSE),]$date_str)
x_labels[c(FALSE, TRUE)] <- ""
colors <- c("inflacja koszyka" = "red", "inflacja oficjalna" = "black")
p = ggplot(df_groupped_date) +
  geom_line(aes(y=mean_inflation_rate, x=date_index, color="inflacja koszyka")) +
  geom_point(aes(y=mean_inflation_rate, x=date_index, color="inflacja koszyka")) +
  geom_line(data=df_inf, aes(y=inflation_index, x=date_index, color="inflacja oficjalna"), linetype="solid", linewidth=1.2) +
  labs(x = "data",
        y = "Inflacja w stosunku do poprzedniego miesiąca [%]",
        color = "") +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  theme(axis.text.x = element_text(angle = 300, vjust = 0.8, hjust=0))
print(p)

df_other <- df_groupped[df_groupped$category %in% cat_mapping$inne,]
x_labels <- unique(df_other[order(df_other$date_index,decreasing=FALSE),]$date_str)
x_labels[c(FALSE, TRUE)] <- ""
p = ggplot(df_other) +
  geom_line(aes(y=mean_inflation_rate, x=factor(date_index), group=category, color=category)) +
  geom_point(aes(y=mean_inflation_rate, x=factor(date_index), group=category, color=category)) +
  geom_line(data=df_inf, aes(y=inflation_index, x=factor(date_index), color="black", group="black"), linetype="solid", color="black", linewidth=1.2) +
  scale_x_discrete(labels=x_labels) +
  labs(x="data",
  y="Uśredniona inflacja w stosunku do poprzedniego miesiąca [%]",
  color = "Kategorie") +
  theme(axis.text.x = element_text(angle = 300, vjust = 0.8, hjust=0))
print(p)