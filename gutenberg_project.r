# Gutenberg Project in R
########################

# Upload needed packages
library(gutenbergr)
library(tidytext)
library(dplyr)

my_mirror<- "http://mirrors.xmission.com/gutenberg/"

df<- gutenberg_metadata

#Pick a book by closeness to my last name
unique(df$author)[startsWith(unique(df$author), "Bo")]


gutenberg_works(author == "Borrow, George")

# Download the book text
book_text <- gutenberg_download(20198, mirror = my_mirror)

# Check for the word count
word_count <- book_text %>%
  unnest_tokens(word, text) %>%  # Split text into individual words
  summarize(total_words = n())  # Count the total number of words

# Print the word count
print(word_count)

# Total words of the book: 234021

# Reassign the value to another variable
my_book<- book_text

# Point 2

# Storing all words in a df "all_words"
all_words<-unnest_tokens(my_book,word,text)

# Count frequency of each words > how ofter each word occur
wordcount<- count(all_words,word,sort=TRUE)

# Extracting bigrams
bigrams<-unnest_tokens(my_book,word,text,token="ngrams", n=2)

# Count bigrams frequency
count_bigrams<- count(bigrams,word,sort=TRUE)

# Point 3

# Test for dependence of bigram <-> phrases

# Remove empty lines
count_bigrams<-count_bigrams[-1,]

count_bigrams[startsWith(count_bigrams$word, "thinking of"),]
count_bigrams[startsWith(count_bigrams$word, "thinking "),]
count_bigrams[endsWith(count_bigrams$word, " of"),]

# Counting the occurrence 

# Sum up all bigrams of "thinking of"
t.o.<- count_bigrams[startsWith(count_bigrams$word, "thinking of"),]$n

# Sum up all bigrams that starts with "thinking" but not "thinking of"
t.noto<- sum(count_bigrams[startsWith(count_bigrams$word,"thinking "),]$n) - t.o.

# Sum up all bigrams that ends with "of" but not "thinking of"
nott.o<- sum(count_bigrams[endsWith(count_bigrams$word, " of"),]$n) - t.o.

# Sum up all bigrams that whether starts nor ends with "thinkg", " of" and "thinking of"
nott.noto<- sum(count_bigrams$n) - t.o. - t.noto - nott.o

# Visualizing: create contingency table 
freq<- matrix(c(t.o., t.noto, nott.o, nott.noto),ncol = 2, byrow = T)

# Creating a mosaic plot from the contingency table
mosaicplot(freq)

# With this plot, we can see than the heights of the bars are very different
# therefore the phrases are not equally distributed

# Chi square test
chisq.test(freq)

# p-value is very low which means the data is depended 
# Approximation might be incorrect as we do not have a lot of data

# Point 5: Entropy of 1000 word parts

entropy<-all_words[1:1000,2]

# From these 1k words we extract characters
char<-unnest_tokens(entropy,token,word,token = "characters")

# Data frame of the characters of 1000 words and theit frequency
df.char<- as.data.frame(count(char,token,sort=TRUE))

# Add a new column with relative frequencies
df.char$relfreq<-df.char$n/sum(df.char$n)

# Frequency is multiplied with relative frequency resulting in entropy
df.char$entropy<- df.char$relfreq*log2(df.char$relfreq)

# Computing overall entropy
all_entropy<- - sum(df.char$entropy)


# Computing for every 1000 words
all_entropy<-c()
for(i in 0:233)
{
  entropy<-all_words[(i*1000+1):(i*1000+1000),2]
  char<-unnest_tokens(entropy,token,word,token = "characters")
  df.char<- as.data.frame(count(char,token,sort=TRUE))
  df.char$relfreq<-df.char$n/sum(df.char$n)
  df.char$entropy<- df.char$relfreq*log2(df.char$relfreq)
  all_entropy<- c(all_entropy, - sum(df.char$entropy))
}

all_entropy
plot(all_entropy)

# Point 7: Confidence interval for the entropies using t-test

# Calculate the mean and standard deviation
mean_entropy<- mean(all_entropy)
sd_entropy<- sd(all_entropy)

# Number of observations for the standard error
n<- length(all_entropy)

# Standard error
se<- sd_entropy/sqrt(n)

# Calculating confidence interval
t_value<- qt(0.975, df = n-1)
lower_ci<- mean_entropy - t_value * se
upper_ci<- mean_entropy + t_value * se

# Point 9: Naive Bayes
# I take the overall text and divide it into
#4 datasets 

24205/4
my_book1<- my_book[1:6051,]
my_book2<- my_book[(6051+1):(2*6051),]
my_book3<- my_book[(2*6051+1):(3*6051),]
my_book4<- my_book[(3*6051+1):(24205),] 

# Preprocess each section
preprocess_section<- function(section) 
{
  section_words<- unnest_tokens(as.data.frame(section), word, text) %>% 
    count(word, sort = TRUE)
  return(section_words)
}

section1_words<- preprocess_section(my_book1)
section2_words<- preprocess_section(my_book2)
section3_words<- preprocess_section(my_book3)
section4_words<- preprocess_section(my_book4)

# Total words per section
total_words_section1<- sum(section1_words$n)
total_words_section2<- sum(section2_words$n)
total_words_section3<- sum(section3_words$n)
total_words_section4<- sum(section4_words$n)

# Vocabulary size for smoothing
global_words<- bind_rows(section1_words, section2_words, section3_words, section4_words)
vocab_size<- length(unique(global_words$word))

# Chosen sentence
sentence<- "How glad I am now that I can read"
sentence_words<- unnest_tokens(data.frame(text = sentence), word, text)

# Function to calculate word probabilities for each section with Laplace smoothing
calculate_word_prob<- function(word, section_words, total_words_section, alpha = 1) 
{
  word_count<- section_words %>% filter(word == !!word) %>% pull(n)
  if (length(word_count) == 0) {
    word_count<- 0
}
  prob<- (word_count + alpha) / (total_words_section + alpha * vocab_size)
  return(prob)
}

# Calculate probabilities for each word in each section
posterior_probs<- data.frame(
  Word = sentence_words$word,
  Section1 = sapply(sentence_words$word, calculate_word_prob, section1_words, total_words_section1),
  Section2 = sapply(sentence_words$word, calculate_word_prob, section2_words, total_words_section2),
  Section3 = sapply(sentence_words$word, calculate_word_prob, section3_words, total_words_section3),
  Section4 = sapply(sentence_words$word, calculate_word_prob, section4_words, total_words_section4)
)

# Compute Naive Bayes likelihood for the sentence in each section
likelihood_section1<- prod(posterior_probs$Section1)
likelihood_section2<- prod(posterior_probs$Section2)
likelihood_section3<- prod(posterior_probs$Section3)
likelihood_section4<- prod(posterior_probs$Section4)

# Assume uniform priors
prior<- 1 / 4

# Compute posterior probabilities using Bayes' rule
posterior_section1<- likelihood_section1 * prior
posterior_section2<- likelihood_section2 * prior
posterior_section3<- likelihood_section3 * prior
posterior_section4<- likelihood_section4 * prior

# Normalize to sum to 1
total_posterior<- posterior_section1 + posterior_section2 + posterior_section3 + posterior_section4
posterior_section1<- posterior_section1 / total_posterior
posterior_section2<- posterior_section2 / total_posterior
posterior_section3<- posterior_section3 / total_posterior
posterior_section4<- posterior_section4 / total_posterior

# Display results
final_posterior<- data.frame(
  Section = c("Section 1", "Section 2", "Section 3", "Section 4"),
  Probability = c(posterior_section1, posterior_section2, posterior_section3, posterior_section4)
)

print(final_posterior)
