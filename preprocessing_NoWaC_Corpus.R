
# The present script can be used to preprocess data from a frequency list of the Norwegian 
# as Web Corpus (NoWaC). 

# Reference of the corpus
# 
# Guevara, E. R. (2010). NoWaC: A large web-based corpus for Norwegian. In Proceedings of the 
# NAACL HLT 2010 Sixth Web as Corpus Workshop (pp. 1-7). https://aclanthology.org/W10-1501

# Before using this script, the frequency list should be downloaded from 
# https://www.hf.uio.no/iln/english/about/organization/text-laboratory/projects/nowac/nowac-frequency.html. 
# The list is described as 'frequency list sorted primary alphabetic and secondary by 
# frequency within each character', and the direct URL is: 
# https://www.tekstlab.uio.no/nowac/download/nowac-1.1.lemma.frek.sort_alf_frek.txt.gz. 
# The download requires signing in to an institutional network. Last, the downloaded file 
# should be unzipped.


library(dplyr)  # data wrangling
library(stringr)  # text processing

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
  
  # Split words and frequencies into separate columns
  mutate( frequency = word %>% str_extract('\\d+') %>% as.numeric(),
          word = word %>% str_remove('^\\s*\\d*|^\\s*') %>% str_remove('^\\s*'),
          
          # Extract gender of each noun into standalone column
          gender = ifelse(category == 'subst_mask', 'masculine',
                          ifelse(category == 'subst_fem', 'feminine',
                                 ifelse(category == 'subst_nøyt', 'neuter', NA))),
          
          # Count number of letters per word
          letters = nchar(word)
  ) %>%
  
  # Only keep entries with gender information
  filter(complete.cases(gender),
         
         # Clean up tokens by keeping only items 
         # with a frequency of 500 or greater.
         frequency >= 500) %>%
  
  # Remove category column, which is superseded 
  # by the gender column.
  select(-category) %>%
  
  # Order columns
  select(word, gender, frequency, letters)

# Many nouns are repeated in the corpus because they are tagged with more than one gender
# (for background, see Rodina & Westergaard, 2015 https://doi.org/10.1017/S1470542714000245, 
# 2021 https://doi.org/10.1017/S1470542719000217). To prevent errors, select only the words 
# that have one consistent gender in the corpus. 

words_with_one_gender = 
  corpus %>% 
  count(word, gender) %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  filter(n == 1) %>%
  pull(word)

corpus = corpus %>%
  filter(word %in% words_with_one_gender)

# To finish resolving duplicated entries, keep only the entry with the highest frequency 
# out of any duplicates.

corpus = corpus %>%
  arrange(word, desc(frequency)) %>% 
  filter(!duplicated(word))

# Remove potential English words. This is done by removing words that coincide with 
# any of the 10,000 words present in the MIT 10,000 Word List 
# (https://www.mit.edu/~ecprice/wordlist.10000).

English_words = read.csv('https://www.mit.edu/~ecprice/wordlist.10000', 
                         col.names = 'word')

corpus = corpus %>%
  filter(!word %in% English_words$word)


