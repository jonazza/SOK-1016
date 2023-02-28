#pakker
library(tidyverse) #datamanioulering og ggplot
library(ggthemes) #tema ggplot
library(ggpubr) #ggplot ekstra
library(janitor) #pakke for å hjelpe med dataset
library(httr) #pakke til innlasting av api data fra ssb
library(rjstat) #pakke til lesing av json

url_11_21 <- "https://data.ssb.no/api/v0/no/table/09170/"

query_11_21 <- '{
  "query": [
    {
      "code": "NACE",
      "selection": {
        "filter": "vs:NRNaeringPubAgg",
        "values": [
          "nr23_6",
          "pub2X01_02",
          "pub2X03",
          "pub2X05",
          "nr2X06_09",
          "nr23ind",
          "pub2X35",
          "pub2X36_39",
          "pub2X41_43",
          "pub2X45_47",
          "pub2X49B",
          "pub2X50A",
          "pub2X49A_52",
          "pub2X53",
          "pub2X55_56",
          "pub2X58_63",
          "pub2X64_66",
          "pub2X68A",
          "pub2X68B",
          "pub2X69_75",
          "pub2X77_82",
          "pub2X84",
          "pub2X85",
          "pub2X86_88",
          "pub2X90_97"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "BNPB",
          "BNPB2"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019",
          "2020",
          "2021"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
} '

hent_indeks_11_21.tmp <- url_11_21 %>%
  POST(body = query_11_21, encode = "json")

df11_21 <-  hent_indeks_11_21.tmp %>% #lager datasett
  content("text") %>%
  fromJSONstat() %>%
  as_tibble()

df11_21 <- df11_21 %>% pivot_wider(names_from = næring, values_from = value) %>% #legger sammen enkelte næringer og velger de ønskede kolonnene
  mutate("Industri og bergverksdrift" = Bergverksdrift + Industri) %>%
  mutate("Primærnæringer" = !!sym("Fiske, fangst og akvakultur") + !!sym("Jordbruk og skogbruk")) %>%
  mutate("Tjenester ellers" = !!sym("Informasjon og kommunikasjon") + !!sym("Kultur, underholdning og annen tjenesteyting")) %>%
  mutate("Undervisning" = Undervisning + !!sym("Faglig, vitenskapelig og teknisk tjenesteyting")) %>%
  mutate("Finansiell og forretningsmessig tjenesteyting, eiendomsdrift" = !!sym("Elektrisitets-, gass- og varmtvannsforsyning") + !!sym("Vannforsyning, avløp og renovasjon") + !!sym("Finansierings- og forsikringsvirksomhet") + !!sym("Omsetning og drift av fast eiendom") + !!sym("Boligtjenester, egen bolig") + !!sym("Forretningsmessig tjenesteyting")) %>%
  mutate("Samferdsel" = Rørtransport + !!sym("Utenriks sjøfart") + !!sym("Transport utenom utenriks sjøfart") + !!sym("Post og distribusjonsvirksomhet")) %>%
  mutate("Varehandel, hotell- og restaurantvirksomhet" = !!sym("Overnattings- og serveringsvirksomhet") + !!sym("Varehandel og reparasjon av motorvogner")) %>%
  select(!!sym("Utvinning av råolje og naturgass, inkl. tjenester"), !!sym("Industri og bergverksdrift"), !!sym("Primærnæringer"), !!sym("Tjenester ellers"), !!sym("Helse- og omsorgstjenester"), Undervisning, !!sym("Offentlig administrasjon og forsvar"), !!sym("Finansiell og forretningsmessig tjenesteyting, eiendomsdrift"), Samferdsel, !!sym("Varehandel, hotell- og restaurantvirksomhet"), !!sym("Bygge- og anleggsvirksomhet"), statistikkvariabel, !!sym("Totalt for næringer"),år) %>%
  pivot_longer(c(!!sym("Utvinning av råolje og naturgass, inkl. tjenester"), !!sym("Industri og bergverksdrift"), Primærnæringer, !!sym("Tjenester ellers"), !!sym("Helse- og omsorgstjenester"), Undervisning, !!sym("Offentlig administrasjon og forsvar"), !!sym("Finansiell og forretningsmessig tjenesteyting, eiendomsdrift"), Samferdsel, !!sym("Varehandel, hotell- og restaurantvirksomhet"), !!sym("Bygge- og anleggsvirksomhet"), !!sym("Totalt for næringer")), names_to = "næring", values_to = "value")

facet_names <- c(
  "Bruttoprodukt i basisverdi. Løpende priser (mill. kr)" = "Løpende priser",
  "Bruttoprodukt i basisverdi. Faste 2015-priser (mill. kr)" = "Faste 2015-priser")

df11_21 %>%
  filter(næring != "Totalt for næringer") %>%
  ggplot(aes(x=as.numeric(år), y= as.numeric(value), color=næring)) +
  geom_line() +
  theme_minimal()+
  scale_fill_brewer(palette="Set3") +
  facet_wrap(~ statistikkvariabel, labeller = as_labeller(facet_names))+
  labs(title= "Figur 4: Bruttoprodukt fordelt på næringer", subtitle = "                i perioden 2011-2021", caption = "Kilde: SSB (2022)", y="Bruttoprodukt (mill. kroner)", x = "", color="") +
  theme(legend.text = element_text(size=4))
