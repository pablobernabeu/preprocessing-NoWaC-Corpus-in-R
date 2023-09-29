

library(dplyr)  # data wrangling
library(stringr)  # text processing


# NoWaC corpus reference
# 
# Guevara, E. R. (2010). NoWaC: A large web-based corpus for Norwegian. In Proceedings of the 
# NAACL HLT 2010 Sixth Web as Corpus Workshop (pp. 1-7). https://aclanthology.org/W10-1501
# 
# The frequency list was downloaded in March 2023 from 
# https://www.hf.uio.no/iln/english/about/organization/text-laboratory/services/nowac-frequency.html
# Specifically, the list was described as 'frequency list sorted primary alphabetic and 
# secondary by frequency within each character', and the direct URL was the following:    
# https://www.tekstlab.uio.no/nowac/download/nowac-1.1.lemma.frek.sort_alf_frek.txt.gz
# The download required signing in to an institutional network. Next, the downloaded file 
# was unzipped and saved.

corpus = read.delim('nowac-1.1.lemma.frek.sort_alf_frek.txt', 
                    header = FALSE, stringsAsFactors = FALSE, 
                    quote = '', sep = '\t')

head(corpus)

# Preprocess data set
corpus = corpus %>%
  
  # Assign informative names to columns
  rename(word = V1, category = V2) %>%
  
  # Select nouns only, dropping verbs etc. Furthermore, 
  # remove nouns that have any uppercase letters.
  filter(str_detect(category, '^subst'),
         !str_detect(word, '[:upper:]')) %>%
  
  # Rename column
  rename(gender = category) %>%
  
  # Split words and frequencies into separate columns
  mutate( frequency = word %>% str_extract('\\d+') %>% as.numeric(),
          word = word %>% str_remove('^\\s*\\d*|^\\s*') %>% str_remove('^\\s*'),
          
          # Extract gender of each noun into standalone column
          gender = ifelse(gender == 'subst_mask', 'masculine',
                          ifelse(gender == 'subst_fem', 'feminine',
                                 ifelse(gender == 'subst_nøyt', 'neuter', NA))),
          
          # Count number of letters per word
          letters = nchar(word)
  ) %>%
  
  # Only keep entries with gender information
  filter(complete.cases(gender),
         
         # Clean up tokens by keeping only items 
         # with a frequency of 500 or greater.
         frequency >= 500) %>%
  
  # Order columns
  select(word, gender, frequency, letters)

# Many nouns are repeated in the corpus because they are tagged with more than one gender
# (for background, see Rodina & Westergaard, 2015 https://doi.org/10.1017/S1470542714000245, 
# 2021 https://doi.org/10.1017/S1470542719000217). Generally, in these ambiguous cases in
# the present corpus, either entry has a much higher frequency. To resolve these 
# ambiguities, only the entry with the highest frequency will be kept.

corpus = corpus %>%
  arrange(word, desc(frequency)) %>% 
  filter(!duplicated(word))

