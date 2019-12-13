library(tidyverse)
library(NLP)
library(openNLP)

full_data <- read.csv("C:\\Users\\Aaron\\Google Drive\\School Stuff\\Fall 2019\\Data Science Programming I\\DSP1 Semester Project\\Deliverable 1 Materials\\vgMetaFullCleanFinal.csv", TRUE, stringsAsFactors = FALSE)

full_summaries <- full_data$summary


#Performing NER on all summaries results in a vector that is too big for my computers to handle
full_summaries_unique <- as.String(unique(full_summaries))

sent_token_annotator = Maxent_Sent_Token_Annotator()
word_token_annotator = Maxent_Word_Token_Annotator()
pos_tag_annotator = Maxent_POS_Tag_Annotator()



entity_annotator_person = Maxent_Entity_Annotator(kind = 'person')
entity_annotator_organization = Maxent_Entity_Annotator(kind = 'organization')
entity_annotator_location = Maxent_Entity_Annotator(kind = 'location')

anno1 = annotate(full_summaries_unique, list(sent_token_annotator, word_token_annotator))


anno2_person = annotate(full_summaries_unique, entity_annotator_person, anno1)
anno3_person = subset(anno2_person, type == "entity")

entities_person = data.frame(full_summaries_unique[anno3_person])
colnames(entities_person) = c("entity")

anno2_org = annotate(full_summaries_unique, entity_annotator_organization, anno1)
anno3_org = subset(anno2_org, type == "entity")

entities_org = data.frame(full_summaries_unique[anno3_org])
colnames(entities_org) = c("entity")

anno2_loc = annotate(full_summaries_unique, entity_annotator_location, anno1)
anno3_loc = subset(anno2_loc, type == "entity")

entities_loc = data.frame(full_summaries_unique[anno3_loc])
colnames(entities_loc) = c("entity")


entities_person %>%
    count(entity, sort = TRUE)

entities_org %>%
    count(entity, sort = TRUE)

entities_loc %>%
    count(entity, sort = TRUE)

