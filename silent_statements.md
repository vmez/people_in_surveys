Silent Statements
================

``` r
library("tidyverse")
```

``` r
cleaned_data <- data.table::fread("cleaned_data.csv")
```

## Intro

This is part 2.1 of the series called People in Surveys.

I am analysing data from a
[survey](https://www.kaggle.com/kaggle/kaggle-survey-2018#multipleChoiceResponses.csv)
conducted by Kaggle in October 2018, gathering data on the state of
Machine Learning and Data Science as viewed from their subscribers.

In Part 2: Loudest Voices, I focused on the participants who said are
currently in India and USA because together they account for 38.29% of
the participants in this survey, and hence, being the loudest voices of
the state of Data Science and Machine Learning. And with a big
difference, given the third top country is China with 1,644 participants
(India with 4,417 and USA with 4,716) .

## The state of data science and Machine Learning: Silent Statements

Answering location questions can be a funny thing given that for some of
us, the question “Where do you currently reside” is one thing. Which
will be fine if the intention of the analysis is to look at the state of
Data Science and Machine Learning to learn more about what each country
is up to. However, if my location is compared to my level of education,
major…etc the analysis can be misleading for those of us who were born
in one place, studied in two others, worked in a bunch of others, and
now reside elsewhere.

Then in a survey like this one, perhaps I will be inclined to choose “I
wish not to disclose my location”. And that is what some people have
answered.

``` r
cleaned_data %>% group_by(country) %>% count() %>% summary()
```

    ##    country                n         
    ##  Length:58          Min.   :  62.0  
    ##  Class :character   1st Qu.:  86.5  
    ##  Mode  :character   Median : 151.0  
    ##                     Mean   : 411.4  
    ##                     3rd Qu.: 329.2  
    ##                     Max.   :4716.0

``` r
cleaned_data$country <- gsub("United Kingdom of Great Britain and Northern Ireland", "UK and Northern Ireland", cleaned_data$country)
cleaned_data$country <- gsub("I do not wish to disclose my location", "Wish not to disclose", cleaned_data$country)
```

``` r
cleaned_data %>% group_by(country) %>% count() %>% 
  filter(n > 329) %>% 
  ggplot(aes(reorder(country,-n) , n, fill = n)) + geom_col() + 
  ggtitle("In wich country do you currently reside?" ,subtitle = "The graph displays the answers of the top 25% countries") +
  xlab("") + ylab("") +
  coord_flip() + theme_minimal() 
```

![](zoom-into-countries_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

And they stand out given that these participants add up to be in the
same range of those in the top 25%, standing closely to the participants
in Australia and Italy, not far off from Spain. Additionally, there are
quite a lot of people who answered ‘Other’, which fair enough, there
were only 55 countries marked as an option in the survey out of the
[196](https://www.thoughtco.com/number-of-countries-in-the-world-1433445)
existing worldwide.

How many in each
group?

``` r
cleaned_data %>% filter(country %in% c('Other', 'Wish not to disclose', "Australia", "Italy", "Wish not to disclose", "Spain")) %>% 
  group_by(country) %>% tally() %>% arrange(n)
```

    ## # A tibble: 5 x 2
    ##   country                  n
    ##   <chr>                <int>
    ## 1 Australia              330
    ## 2 Italy                  355
    ## 3 Wish not to disclose   394
    ## 4 Spain                  485
    ## 5 Other                 1036

Spain sets them apart. Nonetheless, the conclusions one can make of
Australia’s and Italy’s participation can be applied to those who did
not disclose this information, and represent 1.6% of the participants.

What countries people say are currently in, that have less participants
than those who did not disclose?

``` r
cleaned_data %>% group_by(country) %>% tally() %>%
  filter(n<395) %>%
  ggplot(aes(reorder(country, n), n, fill = n)) + geom_col() +
  ggtitle("The median are those participants who say are from South Africa, being 147", 
          subtitle = "Those who did not disclose are 395. Per reference,'Other'account for 1,036 -beyond this graph") + 
  xlab("") + ylab("") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
```

![](zoom-into-countries_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
If we consider the median is with participants who are located in South
Africa, those who wish not to disclose are quite far ahead.

I wonder if their reason is similar to mine. And I also wonder how
honest are the others. Are people selecting the country where they are
born in or affiliate with instead of saying where they reside? And what
about the people who marked ‘Other’, because they represent 4.34% of the
respondents. Are surveys always offering the same 55 countries? Why
these? Austria for example had 66 respondents. I speculate when I say
that possibly there are at least other 66 people from this 1,036 who may
be from the same country. I also speculate when I say that possibly,
some people opted for ‘Other’ instead of “I do not wish to disclose my
location” as an automatic reaction to the option “Other” in these forms.

I find this interesting. I’ll admit that sometimes I do not provide the
right information about where I am from, my gender, or even my age. This
is true particularly with services from which my interest is to listen
to music, read an article, or just peak in to understand what the
service is. My reasoning is that I do not want to receive targeted
marketing or information that is recommended to me based on the
similarities to those I am boxed with. And to be fair, most of the times
there is not an option to opt-out and still use the service. But I don’t
always need to misinform, mainly because there can be alternatives to
the same service. It’s just a matter of digging them
out.

## What are the other options people do not wish to disclose, aside of their current location?

*135 did not disclose country and salary. *62 did not disclose country
and gender. *25 did not disclose country, salary, and gender. *14 did
not disclose country and their current role. *10 did not disclose
country and the industry they work in. *1 did not disclose country,
salary, gender, and role.

And it can go on branching out to other pairs and finding for example
that 148 participants did not disclose their salary nor the industry
they work in. Why is that? Competition for the ‘Sexiest job in the 21st
century’? Privacy concerns?

How can we make people more comfortable sharing information without
making them feel it’s about them as individuals? And do we have to
rethink surveys, and think first what is it that we want to understand
from people and why? Should these intentions be better communicated to
encourage more transparency and fairness?

When we live in an era when so much attention is given to those whose
data you have off, and dismiss the statements people make by choosing
not to participate, I can’t stop to think that we are missing valuable
perspectives by giving the loudest voices higher preferences.

ps. If you are curious to see a further break down of participants,
bellow are the countries in the lower 25% (1st quartile) and those part
of the 50% (between the 1st and 3rd quartile).

``` r
cleaned_data %>% group_by(country) %>% tally() %>% 
  filter(n < 86.5) %>% 
  ggplot(aes(reorder(country,-n) , n, fill = n)) + geom_col() + 
  ggtitle("Where the lower 25% of those 23,859 participants are in the data", subtitle = "Graph displaying the amount of participants from these countries") +
  xlab("") + ylab("") +
  coord_flip() + theme_minimal()
```

![](zoom-into-countries_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
cleaned_data %>% group_by(country) %>% tally() %>% 
  filter(n > 86.5 & n < 329.2)  %>% 
  ggplot(aes(reorder(country,-n) , n, fill = n)) + geom_col() + 
  ggtitle("Where 50% of those 23,859 participants are in the data", subtitle = "Graph displaying the amount of participants from these countries") +
  xlab("") + ylab("") +
  coord_flip() + theme_minimal()
```

![](zoom-into-countries_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

From those who chose not to disclose their location, did they also not
disclose something else about themselves?

``` r
cleaned_data %>% filter(country %in% "Wish not to disclose",
                        salary %in% "I do not wish to disclose my approximate yearly compensation") %>% tally()
```

    ##     n
    ## 1 135

``` r
cleaned_data %>% filter(country %in% "Wish not to disclose",
                        gender %in% c("Prefer not to say", "Prefer to self-describe")) %>% tally()
```

    ##    n
    ## 1 61

``` r
cleaned_data %>% filter(country %in% "Wish not to disclose",
                        salary %in% "I do not wish to disclose my approximate yearly compensation",
                        gender %in% c("Prefer not to say", "Prefer to self-describe")) %>% tally()
```

    ##    n
    ## 1 25

``` r
cleaned_data %>% filter(country %in% "Wish not to disclose",
                        role %in% 'Other') %>% tally()
```

    ##    n
    ## 1 14

``` r
cleaned_data %>% filter(country %in% 'Wish not to disclose',
                        gender %in% c("Prefer not to say", "Prefer to self-describe"),
                        role %in% 'Other') %>% tally()
```

    ##   n
    ## 1 3

``` r
cleaned_data %>% filter(country %in% "Wish not to disclose",
                        salary %in% "I do not wish to disclose my approximate yearly compensation",
                        gender %in% c("Prefer not to say", "Prefer to self-describe"),
                        role %in% 'Other') %>% tally()
```

    ##   n
    ## 1 1

``` r
cleaned_data %>% filter(country %in% "Wish not to disclose",
                        industry %in% 'Other') %>% tally()
```

    ##    n
    ## 1 10

``` r
cleaned_data %>% filter(country %in% "Wish not to disclose",
                        salary %in% "I do not wish to disclose my approximate yearly compensation",
                        industry %in% 'Other') %>% tally()
```

    ##   n
    ## 1 4

``` r
cleaned_data %>% filter(country %in% "Wish not to disclose",
                        salary %in% "I do not wish to disclose my approximate yearly compensation",
                        gender %in% c("Prefer not to say", "Prefer to self-describe"),
                        industry %in% 'Other') %>% tally()
```

    ##   n
    ## 1 1

``` r
cleaned_data %>% filter(salary %in% "I do not wish to disclose my approximate yearly compensation",
                        industry %in% 'Other') %>% tally()
```

    ##     n
    ## 1 148
