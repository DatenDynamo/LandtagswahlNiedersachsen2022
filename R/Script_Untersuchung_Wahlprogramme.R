#
#"Wahlprogramme zur Landtagswahl Niedersachsen 2022"
#Parts of the code based on Nicolas Merz' analysis-zeitonline https://github.com/nicmer/analysis-zeitonline
#
  
  
  # Untersuchung der Wahlprogramme von SPD und GRUENEN Landtagswahl NS 2022
  
  library(tidyverse)
  library(readtext)
  library(tidytext)
  library(manifestoR)
  library(stringr)
  library(quanteda)
  library(nsyllable)
  library(xlsx)
  library(tibble)
  library(dplyr)
  library(stringr)
  library(SnowballC)
  library(magrittr)
  library(wordcloud)
  library(tokenizers)
  library(textreuse)
  library(tm)
  library(stopwords)
  library(udpipe)
  library(quanteda.textstats)
  



mp_setapikey(key.file="manifesto_apikey.txt")
mp_use_corpus_version("2016-6")
  
  #Erstellen einer Übersichtstabelle, welche die den Parteinamen das zugehörige
  #Parteiprogramm, Farbe, Abkürzung, Reihenfolge, Partei ID 
  #(Nach Manifesto-Projekt) zurordnet.
  
parteinamen <- tribble (
  ~doc_id, ~partyabbrev, ~party, ~partycolor, ~partyorder,
  "SPD_utf8.txt","SPD",41320,"#BE232D",1,
  "Koalitionsvertrag_utf8.txt","Koalitionsvertrag",1111,"#BFC7CE",2,
  "GRUENE_utf8.txt","GRUENE",41223,"#6EA500",3
  )

  #Füge die Wahlprogramme in die Übersichtstabelle ein

wahlprogramme2022 <- readtext("Wahlprogramme/*.txt", ignore_missing_files = FALSE, encoding="UTF-8") %>%
    left_join(parteinamen) %>%
    mutate(date=20221009)  %>%
    as_data_frame()

deold <- mp_corpus(countryname=="Germany" & date > 199000 & party != 41952) %>%
  tidy() %>%
  unnest_tokens(word,text,to_lower=FALSE) %>%
  mutate(
    party = ifelse(party==41111 | party == 41112,41113,party),
    party = ifelse(party==41221 | party == 41222,41223,party)
  ) %>%
  left_join(parteinamen) %>% select(party,partyabbrev,date,word,manifesto_id,partycolor) %>%
  rename(doc_id = manifesto_id)

  
  #Länge
  
  #Erstell ein Dataframe mit der Länge der jeweiligen Wahlprogrammen

laenge <- wahlprogramme2022 %>%
  unnest_tokens(word,text,token="words") %>%
  group_by(partyabbrev, date) %>%
  summarize(
    nwords =n()
  ) %>%
  mutate(
    date = as.character(date)
  ) %>%
  left_join(parteinamen) %>%
  mutate(
    now = as.factor(date == 20221009),
    Wahl = as.character(round(as.numeric(date)/100,0))
  )

laenge$partyabbrev_order <- factor(laenge$partyabbrev, levels=c("SPD","Koalitionsvertrag","GRUENE"))

xlabels <- sort(laenge$nwords,decreasing = FALSE)

ggplot(laenge %>% filter(party > 1000),aes(Wahl,nwords)) +
  geom_bar(aes(fill=partyabbrev_order,alpha = now), stat="identity", position = "dodge") + 
 scale_alpha_manual(values=c(0.5,1), guide=FALSE) + 
  scale_fill_manual(values = parteinamen$partycolor, guide=FALSE) +
  facet_wrap(~partyabbrev_order) +
  theme(axis.title.x=element_blank(),
       axis.ticks.x=element_blank()) + 
  scale_y_continuous("Länge der Texte (in Wörtern)") +
  scale_x_discrete(labels= xlabels)

ggplot(laenge, aes(partyabbrev, nwords))+
  geom_bar(stat="identity", position = "dodge", fill = laenge$partycolor) +
  geom_text(aes(label = nwords), vjust = 1.5, family = "serif", color="black") +
  scale_alpha_manual(values=c(0.5,1), guide=FALSE) +
  #facet_wrap(~partyabbrev, scales = "free_x")+
  scale_fill_hue(c=45, l=80) +
  #scale_fill_manual(values = laenge$partycolor, guide=FALSE) +
  scale_y_continuous("Länge der Texte (in Wörtern)") + 
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank())

####### Übereinstimmung Thema Digitalisierung #######

####König und Siewert Keywords
keywords <- c("daten", "data", "digital", "cyber", "internet", "online", "netzpolitik", "netzpolitisch", " ikt-", " ikt ", "it-", "informationstechnologie", "kommunikationstechnologie", "industrie 4.0", "cloud", "e-government", "egovernment", "open government", "opengovernment", "software", "breitband","mobilfunk", "telekommunikation", "networking", "computertechnologie", "programmierung", "webentwicklung", "datenanalyse", "künstliche Intelligenz", "robotik", "blockchain", "virtual reality", "augmented reality", "cybersecurity", "privacy", "datenschutz", "netzneutralität", "streaming", "digitalmarketing", "ecommerce", "smartphone", "tablet", "server", "datenbank", "informationssicherheit", "webdesign", "softwareentwicklung", "softwarearchitektur", "computergrafik", "computeranimation", "cloudcomputing", "cloudstorage", "saas", "paas", "iaas", "smart cities", "smart grid", "industrie 4.0", "5g", "quantencomputing")

SPD <- wahlprogramme2022$text[wahlprogramme2022$partyabbrev=="SPD"]
Koalitionsvertrag <- wahlprogramme2022$text[wahlprogramme2022$partyabbrev=="Koalitionsvertrag"]
GRUENE <- wahlprogramme2022$text[wahlprogramme2022$partyabbrev=="GRUENE"]

count_matches <- function(text, word_list) {
  counts <- sapply(word_list, function(word) str_count(text, word))
  return(counts)
}

Digitalisierung_match <- lapply(list(SPD, Koalitionsvertrag, GRUENE), count_matches, word_list = keywords)
  
results_df <- as.data.frame(Digitalisierung_match)
colnames(results_df) <- c("SPD", "Koalitionsvertrag", "GRUENE")
results_df$word <- keywords
results_long <- gather(results_df, file, count, SPD:GRUENE, factor_key = TRUE)
results_short <- aggregate(count ~ file, data=results_long, sum)
results_short <- mutate(results_short, partycolor = parteinamen$partycolor[order(parteinamen$partyabbrev,decreasing = TRUE)]) 
results_short <- results_short[order(results_short$file, decreasing = TRUE),]
rownames(results_short)<- NULL
results_short$file <- factor(results_short$file, levels =c("GRUENE","Koalitionsvertrag","SPD"))

ggplot(results_short, aes(x = file, y = count)) +
  geom_bar(stat="identity", position = "dodge", fill = results_short$partycolor)+
  geom_text(aes(label = count), vjust = 1.5, family = "serif", color="black") +
#  scale_alpha_manual(values=c(0.5,1), guide=FALSE) +
  scale_y_continuous("Anzahl der Wörter mit\n Digitalisierungsbezug")+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        text=element_text(family = "serif"))


####Sätze mit dem Thema

sentences_lines_filtered <- sentences_lines %>%
  filter(grepl(paste(keywords, collapse="|"), sentence))


sentences_lines <- sentences_lines %>%
  mutate(digitalisierungsbezug = ifelse(grepl(paste(keywords, collapse="|"), sentence), 1, 0))




sentence_counts <- sentences_lines_filtered %>%
  count(partyabbrev)

total_sentences <- sentences_lines %>%
  group_by(partyabbrev) %>%
  summarize(total_sentences = n())
sentence_counts <- sentence_counts %>%
  left_join(total_sentences, by = "partyabbrev") %>% left_join(parteinamen, by = "partyabbrev")

percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}
sentence_counts$Salienz <- percent(sentence_counts$n/sentence_counts$total_sentences.x)


ggplot(sentence_counts, aes(x = partyabbrev, y = n)) +
  geom_bar(stat="identity", position = "dodge", fill = sentence_counts$partycolor)+
  geom_text(aes(label = n), vjust = 1.5, family = "serif", color="black") +
  #  scale_alpha_manual(values=c(0.5,1), guide=FALSE) +
  scale_y_continuous("Anzahl an Sätzen mit\nDigitalisierungsbezug")+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        text=element_text(family = "serif"))

ggplot(sentence_counts, aes(x = partyabbrev, y = Salienz)) +
  geom_bar(stat="identity", position = "dodge", fill = sentence_counts$partycolor)+
  geom_text(aes(label = Salienz), vjust = 1.5, family = "serif", color="black") +
  #  scale_alpha_manual(values=c(0.5,1), guide=FALSE) +
  scale_y_discrete("% Anteil an Sätzen in Texten\nmit Digitalisieurngsbezug")+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        text=element_text(family = "serif"))


####Barcode visualisierung

SPD_sentences <- sentences_lines %>% filter(partyabbrev == "SPD" ) %>% arrange(doc_sentence_id)
GRUENE_sentences <- sentences_lines %>% filter(partyabbrev == "GRUENE" ) %>% arrange(doc_sentence_id)
Koalitionsvertrag_sentences <- sentences_lines %>% filter(partyabbrev == "Koalitionsvertrag" ) %>% arrange(doc_sentence_id)
###################################################
# SPD FARVBCODE
color_scale <- c("0" = "#eae4dd", "1" = "#BE232D")

# SPD PLOT
ggplot(SPD_sentences, aes(x = doc_sentence_id, y = 0, xend = doc_sentence_id, yend = 1, color = factor(digitalisierungsbezug))) +
  geom_segment(size = 3) +
  scale_colour_manual(values = color_scale) +
  theme_void()+
  guides(color = "none")


###########################################
# Gruene FARVBCODE
color_scale <- c("0" = "#eae4dd", "1" = "#6EA500")

# Gruene PLOT
ggplot(GRUENE_sentences, aes(x = doc_sentence_id, y = 0, xend = doc_sentence_id, yend = 1, color = factor(digitalisierungsbezug))) +
  geom_segment(size = 3) +
  scale_colour_manual(values = color_scale) +
  theme_void()+
  guides(color = "none")


# Koaliitosnvererage FARVBCODE
color_scale <- c("0" = "#eae4dd", "1" = "#BFC7CE")

# Koalitionsvertrag PLOT
ggplot(Koalitionsvertrag_sentences, aes(x = doc_sentence_id, y = 0, xend = doc_sentence_id, yend = 1, color = factor(digitalisierungsbezug))) +
  geom_segment(size = 3) +
  scale_colour_manual(values = color_scale) +
  theme_void()+
  guides(color = "none")


############################
################## Kookkurrenz
##########################

########SPD######
text <- SPD_sentences$sentence


# Sätze mit mindestems einem Keyword
sentences <- grep(paste(keywords, collapse = "|"), text, value = TRUE)

#Vorhergehende und nachfolgende Sätze
preceding <- sentences %>% lag(2) %>% na.omit()
current <- sentences
following <- sentences %>% lead(2) %>% na.omit()

# kombinieren in einen Vektor
cooccurrence <- c(preceding, current, following)

# konvertierung in Corpus
cooccurrence_corpus <- corpus(cooccurrence)

# tokenisierung
cooccurrence_tokens <- tokens(cooccurrence_corpus)

# Entfernen von stopwords, nummern und Zeichen
cooccurrence_tokens <- tokens_remove(cooccurrence_tokens, stopwords("german"))

# kollokationen berechnen
collocations <- textstat_collocations(cooccurrence_tokens, size = 2)

# sortieren der ergebnisse nach absolutem Auftrten
collocations <- collocations[order(collocations$count, decreasing = TRUE), ]

#top 20 collocations
tabelle_SPD <- head(collocations[with(collocations, order(count, lambda, decreasing = TRUE)),], 20)

write.table(tabelle_SPD, file = "kookurenz_SPD.txt", sep = ";", quote = FALSE, row.names = F)




####GRUENE

text_gruene <- GRUENE_sentences$sentence


# Sätze mit mindestems einem Keyword
sentences_gruene <- grep(paste(keywords, collapse = "|"), text_gruene, value = TRUE)

#Vorhergehende und nachfolgende Sätze
preciding_gruene <- sentences_gruene %>% lag(2) %>% na.omit()
current_gruene <- sentences_gruene
following_gruene <- sentences_gruene %>% lead(2) %>% na.omit()

# kombinieren in einen Vektor
cooccurrence_gruene <- c(preciding_gruene, current_gruene, following_gruene)

# konvertierung in Corpus
cooccurrence_gruene_corpus <- corpus(cooccurrence_gruene)

# tokenisierung
cooccurrence_gruene_tokens <- tokens(cooccurrence_gruene_corpus)

# Entfernen von stopwords, nummern und Zeichen
cooccurrence_gruene_tokens <- tokens_remove(cooccurrence_gruene_tokens, stopwords("german"))

# kollokationen berechnen
collocations_gruene <- textstat_collocations(cooccurrence_gruene_tokens, size = 2)

# sortieren der ergebnisse nach absolutem Auftrten
collocations_gruene <- collocations_gruene[order(collocations_gruene$count, decreasing = TRUE), ]

#top 20 collocations_gruene
tabelle_gruene <- head(collocations_gruene[with(collocations_gruene, order(count, lambda, decreasing = TRUE)),], 20)

write.table(tabelle_gruene, file = "kookurenz_GRUENE.txt", sep = ";", quote = FALSE, row.names = F)





####Koalitiosnvertrag

text_Koalitionsvertrag <- Koalitionsvertrag_sentences$sentence


# Sätze mit mindestems einem Keyword
sentences_Koalitionsvertrag <- grep(paste(keywords, collapse = "|"), text_Koalitionsvertrag, value = TRUE)

#Vorhergehende und nachfolgende Sätze
preciding_Koalitionsvertrag <- sentences_Koalitionsvertrag %>% lag(2) %>% na.omit()
current_Koalitionsvertrag <- sentences_Koalitionsvertrag
following_Koalitionsvertrag <- sentences_Koalitionsvertrag %>% lead(2) %>% na.omit()

# kombinieren in einen Vektor
cooccurrence_Koalitionsvertrag <- c(preciding_Koalitionsvertrag, current_Koalitionsvertrag, following_Koalitionsvertrag)

# konvertierung in Corpus
cooccurrence_Koalitionsvertrag_corpus <- corpus(cooccurrence_Koalitionsvertrag)

# tokenisierung
cooccurrence_Koalitionsvertrag_tokens <- tokens(cooccurrence_Koalitionsvertrag_corpus)

# Entfernen von stopwords, nummern und Zeichen
cooccurrence_Koalitionsvertrag_tokens <- tokens_remove(cooccurrence_Koalitionsvertrag_tokens, stopwords("german"))

# kollokationen berechnen
collocations_Koalitionsvertrag <- textstat_collocations(cooccurrence_Koalitionsvertrag_tokens, size = 2)

# sortieren der ergebnisse nach absolutem Auftrten
collocations_Koalitionsvertrag <- collocations_Koalitionsvertrag[order(collocations_Koalitionsvertrag$count, decreasing = TRUE), ]

#top 20 collocations_Koalitionsvertrag
tabelle_Koalitionsvertrag <- head(collocations_Koalitionsvertrag[with(collocations_Koalitionsvertrag, order(count, lambda, decreasing = TRUE)),], 20)

write.table(tabelle_Koalitionsvertrag, file = "kookurenz_Koalitionsvertrag.txt", sep = ";", quote = FALSE, row.names = F)

write_file(cooccurrence_gruene, file = "digitaltext_gruene.txt")


#Lesbarkeit

replacement_char <- "XXXXX"
no_sent_endings <- c("Nr\\.","bzw\\.","vgl\\.","Vgl\\.","z\\.B\\.","Abs\\.","Art\\.","u\\.a\\.","z\\.b\\.","Z\\.B\\.","S\\.","regex('(?<=[A-Z])\\.')")
no_sent_ending_xxx <- str_replace_all(no_sent_endings,"(\\.)",replacement_char)                   
replacement_list <- setNames(no_sent_ending_xxx,no_sent_endings)

program_lines <- wahlprogramme2022 %>%
  unnest_tokens(lines,text,token="lines", to_lower = FALSE) %>%
  mutate(corpus_line_id = row_number()) %>%
  mutate(
    heading_order = str_count(lines,"#"),
    lines = ifelse(heading_order > 0, str_sub(lines,start = heading_order+2, end = -1),lines)
  ) %>%
  group_by(doc_id) %>%
  arrange(doc_id,corpus_line_id) %>%
  mutate(doc_line_id = row_number()) %>%
  ungroup() %>%
  left_join(parteinamen)

## sentences
sentences_lines <- program_lines %>%
  mutate(
    lines = str_replace_all(lines,replacement_list),
    lines = str_replace_all(lines,"(?<=[A-Z])\\.",replacement_char),
    lines = str_replace_all(lines,"(?<=[0-9]{1,2})\\.",replacement_char)
  ) %>%
  unnest_tokens(sentence, lines, token = "sentences",to_lower = FALSE) %>%
  mutate(
    sentence = str_replace_all(sentence,replacement_char,".")
  ) %>%
  group_by(doc_id) %>%
  mutate(doc_sentence_id = row_number()) %>% 
  ungroup() %>%
  arrange(doc_id,doc_line_id,doc_sentence_id) %>%
  mutate(corpus_sentence_id = row_number()) %>%
  mutate(
    heading = ifelse(heading_order > 0, sentence,NA),
    heading = zoo::na.locf(heading)
  ) %>%
  left_join(program_lines %>% select(lines,corpus_line_id),by=c("corpus_line_id"="corpus_line_id")) %>%
  rename(paragraph=lines)
program_words <- sentences_lines %>%
  filter(heading_order == 0) %>%
  group_by(partyabbrev) %>%
  unnest_tokens(output = word,input=sentence,token = "words") 

length_words <- program_words %>%
  group_by(partyabbrev) %>%
  mutate(
    word_length = nchar(word),
    nsyls = nsyllable(word)
  ) %>% 
  summarize(
    mean_word = mean(word_length),
    longer_six = mean(word_length > 6),
    longer_sylls = mean(nsyls > 2, na.rm=TRUE)*100
  ) %>%
 ungroup()

doc_stats <- program_words %>%
  group_by(partyabbrev, 'corpus_sentence_id') %>%
  summarize(
    words_sentences = n()
  ) %>%
  group_by(partyabbrev) %>%
  mutate(
    longest_sent = words_sentences == max(words_sentences),
    mean_sent = mean(words_sentences)
  ) %>%
  filter(longest_sent==TRUE) %>%
  left_join(sentences_lines, multiple = "all") %>%
  mutate(sentence = str_replace(sentence,"^-","")) %>%
  left_join(length_words) %>%
  mutate(
    lix = mean_sent + (longer_six * 100),
    nws4 = 0.2656 * mean_sent + 0.2744*longer_sylls - 1.693
  ) %>%
  mutate(
    partyf = factor(partyabbrev, levels=c("SPD","Koalitionsvertrag", "GRUENE"))
  )
laenge$partyabbrev_order <- factor(laenge$partyabbrev, levels=c("SPD","Koalitionsvertrag", "GRUENE"))
ggplot(doc_stats,aes(y=nws4,x=partyf)) +
  geom_bar(stat="identity", aes(fill=partyf)) +
  scale_fill_manual(values=c(parteinamen$partycolor), guide=FALSE) + 
  scale_x_discrete("Wahlprogramme 2022") + 
  scale_y_continuous("Schwierigkeit")
doc_stats %>% mutate(verstaendlichkeit = nws4) %>%
  select(partyabbrev,sentence,mean_sent,mean_word,longer_sylls,verstaendlichkeit) %>%
  as.data.frame() %>% write.xlsx(file = "analyse2017v1.xlsx",sheetName="verstaendlichkeit", append=TRUE) 

### Sentiment



read_senti_scores <- function(filename) {
  results <- read.delim(filename, header = FALSE, encoding="UTF-8") %>%
    cbind(str_split_fixed(.$V3, "[,-]",50),stringsAsFactors = FALSE) %>%
    mutate(
      V1 = str_sub(str_match(V1,".*\\|"),1,-2),
      nr = row_number()
    ) %>%
    select(-V3) %>%
    mutate(nr = as.character(nr)) %>%
    gather(wordstem,word,V1,1:48, -nr,-V2) %>%
    select(word,V2) %>% rename(score=V2) %>%
    filter(word != "") %>%
    arrange(word)
}
positive <- read_senti_scores("Sentiment/SentiWS_v2.0_Positive.txt")
negative <- read_senti_scores("Sentiment/SentiWS_v2.0_Negative.txt")
sentis <- positive %>% bind_rows(negative)
de_metadata <- mp_metadata(countryname=="Germany" & date > 200500 & party != 41952)
de2013 <- mp_corpus(countryname=="Germany" & date > 200500 & party != 41952) %>%
  tidy() %>% left_join(parteinamen) %>%
  unnest_tokens(word,text,token="words")
sentiment_scores <- wahlprogramme2022 %>%
  filter(party > 1000) %>% 
  mutate(date=202210) %>%
  unnest_tokens(word,text,token="words") %>%
# bind_rows(deold) %>%
  group_by(partyabbrev,date) %>%
  mutate(
    doc_length = n()
  ) %>%
  inner_join(sentis)
extrem_sentiment <- sentiment_scores %>%
  group_by(partyabbrev,date) %>%
  count(word,sort=TRUE) %>%
  left_join(sentis) %>%
  left_join(sentiment_scores %>% group_by(partyabbrev,date) %>% summarize(doc_length=mean(doc_length))) %>%
  mutate(
    high_score = (n/doc_length)*score
  ) %>%
  group_by(partyabbrev,date) %>%
  arrange(high_score)
lowest_sentiment <- extrem_sentiment %>%
  top_n(-3)
top_sentiment <- extrem_sentiment %>%
  top_n(3)
sentiment_scores$partyabbrev_order <- factor(sentiment_scores$partyabbrev,
                                             levels = c("SPD","Koalitionsvertrag","GRUENE"))
aggregate_sentiment <- sentiment_scores %>%
  summarize(
    sum_sentiment = sum(score),
    mean_sentiment = mean(score),
    senti_n = n(),
    doclength = mean(doc_length),
    ratio = senti_n/doc_length
  ) %>% left_join(parteinamen) %>%
  left_join(de_metadata) %>%
  ungroup() %>%
  mutate(
    partyabbrev = ifelse(partyabbrev=="LINKE","PDS/LINKE",partyabbrev),
    partyf = as.factor(partyabbrev)
  ) %>% group_by(partyabbrev)
# ggplot(sentiment_scores,aes(x=score,y=partyabbrev_order)) +
#    geom_jitter(aes(color=partyabbrev_order),size=0.3) +
#    scale_color_manual(values=rev(parteinamen$partycolor), guide=FALSE) +
#    scale_y_discrete("") +
#   scale_x_continuous("Sentiment") +
#    facet_grid(~date)
#aggregate_sentiment$partyabbrev_order <- factor(aggregate_sentiment$partyabbrev,
                                                #levels = c("SPD","Koalitionsvertrag","GRUENE"))
ggplot(aggregate_sentiment,aes(y=sum_sentiment/doclength,x=partyabbrev_order)) +
  geom_bar(stat="identity", aes(fill=partyabbrev_order)) +
  scale_fill_manual(values=(parteinamen$partycolor[1:3]), guide=FALSE) +
  coord_flip() +
  scale_x_discrete("Wahlprogramme 2022") +
  scale_y_continuous("Durchschnittliches Sentiment") +
  facet_wrap(~round(date/100))
aggregate_sentiment %>% 
  mutate(sentiment=sum_sentiment/doclength) %>%
  select(partyabbrev,date,sentiment) %>%
  as.data.frame() %>% write.xlsx(file = "analyse2017v1.xlsx",sheetName="sentiment", append=TRUE) 

##Wort häufigkeiten



remove_words <- tibble(word = c(stopwords::stopwords(language="de"),"r","grün","innen","dass","afd","cdu","csu","grüne","linke","spd","fdp","freie","demokraten","vgl","kapitel","sozialdemokrat","sozialdemokratinn"))
words_in_programs <- wahlprogramme2022 %>%
  filter(party > 1000) %>% 
  unnest_tokens(output = word,input = text, token = "words",to_lower=TRUE) %>%
  filter(str_detect(word, "[a-z]+")) %>% 
  group_by(partyabbrev)
total_words <- words_in_programs %>% 
  summarize(total_words = n())

counted_words <- words_in_programs %>%
  anti_join(remove_words) %>%
  mutate(
    word_stem = wordStem(word, language="german"),
    #word_stem = word
  ) %>%
  count(partyabbrev, word_stem) %>%
  filter(!word_stem %in% remove_words$word)
tf_idf_words <- counted_words %>%   
  bind_tf_idf(term = word_stem, document = partyabbrev, n = n) %>%
  group_by(partyabbrev) %>%
  arrange(partyabbrev,-tf_idf) %>%
  top_n(10) 


#("mensch","menschen","wirtschaft","deutschland","arbeit","umwelt",
filter_words <- #c("frei","freiheit","sicher","sicherheit","gleich","gleichheit")
word_freqs <- counted_words %>%
  filter(word_stem %in% filter_words) %>%
  left_join(total_words) %>%
  mutate(share = n*100/total_words) %>%
  select(partyabbrev,share,word_stem) %>%
  spread(partyabbrev,share,total_words)
print(tf_idf_words %>% filter(partyabbrev=="SPD"))
print(tf_idf_words %>% filter(partyabbrev=="GRUENE"))
print(tf_idf_words %>% filter(partyabbrev=="Koalitionsvertrag"))
# menschen
# deutschland
# freiheit
# sicherheit
# eu 
#  filter(n > 5) %>%


print(tf_idf_digital <- counted_words %>% 
  bind_tf_idf(term = word_stem, document = partyabbrev, n = n) %>%
  group_by(partyabbrev) %>%
  arrange(partyabbrev,-tf_idf) %>%
filter(word_stem=="digital"))

tf_idf_words %>% as.data.frame() %>% write.xlsx(file = "analyse2017v1.xlsx",sheetName="tfidf", append=TRUE) 
#most_freq_words <- words_in_programs %>%
#   arrange(partyabbrev, n) %>%
#   group_by(partyabbrev) %>%
#   top_n(10) %>% print()

wahlprogramme2022 %>% as.data.frame() %>% write.csv(file ="wahlprogramme2022.csv")


