
library(dplyr)
# ¹®ÀçÀÎ ´ëÅë·É ¿¬¼³¹® ºÒ·¯¿À±â
raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>%
  as_tibble() %>%
  mutate(president = "moon")
# ¹Ú±ÙÇı ´ëÅë·É ¿¬¼³¹® ºÒ·¯¿À±â
raw_park <- readLines("speech_park.txt", encoding = "UTF-8")
park <- raw_park %>%
  as_tibble() %>%
  mutate(president = "park")

# µÎ µ¥ÀÌÅÍ ÇÕÄ¡±â±â
bind_speeches <- bind_rows(moon, park) %>%
  select(president, value)


bind_speeches %>% count(president)

head(bind_speeches)
tail(bind_speeches)

# ±âº»ÀûÀÎ ÀüÃ³¸®
library(stringr)
speeches <- bind_speeches %>%
  mutate(value = str_replace_all(value, "[^°¡-ÆR]", " "),
         value = str_squish(value))
speeches


# ÅäÅ«È­
library(tidytext)
library(KoNLP)
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches

frequency <- speeches %>%
  count(president, word) %>% # ¿¬¼³¹® ¹× ´Ü¾îº° ºóµµ
  filter(str_count(word) > 1) # µÎ ±ÛÀÚ ÀÌ»ó ÃßÃâ
head(frequency)


# dplyr::slice_max() : °ªÀÌ Å« »óÀ§ n°³ÀÇ ÇàÀ» ÃßÃâÇØ ³»¸²Â÷¼ø Á¤·Ä
top10 <- frequency %>%
  group_by(president) %>% # presidentº°·Î ºĞ¸®
  arrange(desc(n)) %>% # »óÀ§ 10°³ ÃßÃâ
  head(10)
top10

top10 <- frequency %>%
  group_by(president) %>% # presidentº°·Î ºĞ¸®
  slice_max(n, n= 10)
top10

print(top10, )

?desc

