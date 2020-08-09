rm(list = ls())
library(tidyverse)

######################################
# 1
######################################

# you can specify the col types using abbreviations
dd <- read_csv("All_UofC_Bio_2011-20.csv", col_types = "ccicicc")

# rename cols for easy access
dd <- dd %>% rename(
  au = Authors,
  au_ids = `Author(s) ID`,
  year = Year,
  journal = `Source title`,
  cits = `Cited by`,
  article = `Document Type`,
  oa = `Access Type`
)

# remove uncited docs (probably editorials, errata, etc)
dd <- dd %>% filter(cits > 0)

######################################
# 2
######################################

# show that the distribution of log citations per year is about normal
# esp for older years

dd %>% ggplot() + 
  aes(x = log(cits),
      fill = year) + 
  geom_histogram() + facet_wrap(~year)

# simple model: only year matters
# for ease of interpretation, remove year from 2010; the coefficient tells us the growth per year
model1 <- summary(lm(log(cits) ~ I(2010 - year), data = dd))

######################################
# 3
######################################

# try adding number of authors
dd <- dd %>% mutate(nau = str_count(au_ids, ";") + 1)
dd <- dd %>% mutate(multi = nau > 12)
model3 <- summary(lm(log(cits) ~ I(2010 - year) + multi, data = dd))
#model3

# add info on article/review
model4 <- summary(lm(log(cits) ~ I(2010 - year) + multi + article, data = dd))
#model4


######################################
# 4
######################################

# add info on journal
# first make into factors
dd <- dd %>% mutate(journal = factor(journal))
model5 <- summary(lm(log(cits) ~ I(2010 - year) + multi + article + relevel(journal, ref = "PLoS ONE"), data = dd))
#model5

mat_coeff <- as.matrix(model5$coefficients)[-(1:4),] # do not plot multi, article, year, intercept
jrn_effects <- as_tibble(mat_coeff) %>%  # make coefficients into matrix
  add_column(journal = rownames(mat_coeff)) %>%  # use the row name as journal name (note that each starts with journal[NAME])
  filter(`Pr(>|t|)` < 10^-6) %>% # only small p-values
  mutate(journal = str_replace_all(journal, "^relevel\\(journal, ref = \"PLoS ONE\"\\)", "")) %>% 
  mutate(journal = ifelse(journal == "Proceedings of the National Academy of Sciences of the United States of America", "PNAS", journal))

# order according to effect
jrn_effects <- jrn_effects %>% arrange(Estimate) %>% mutate(journal = as.character(journal))
jrn_order <- as.character(jrn_effects$journal)
jrn_effects <- jrn_effects %>% mutate(journal = factor(as.character(journal), levels = jrn_order))

jrn_plot <- ggplot(jrn_effects) + 
  aes(x = Estimate, y = journal, fill = sign(Estimate)) + 
  geom_col() + 
  scale_fill_gradient2() + 
  theme(legend.position = "none") +
  ylab("")

######################################
# 5
######################################

# find journals that have published both open access and paywalled articles in a given year
jr_year_oa_test <- dd %>% 
  mutate(oa = ifelse(is.na(oa), "pay", "open")) %>% 
  group_by(journal, year, oa) %>% 
  tally() %>% 
  spread(oa, n, fill = 0) %>% 
  ungroup() %>% 
  filter(pay > 0, open > 0)

# now build data set for testing
testset <- dd %>% inner_join(jr_year_oa_test)
