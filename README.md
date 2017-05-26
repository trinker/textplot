textplot   [![Follow](https://img.shields.io/twitter/follow/tylerrinker.svg?style=social)](https://twitter.com/intent/follow?screen_name=tylerrinker)
============


[![Build
Status](https://travis-ci.org/trinker/textplot.svg?branch=master)](https://travis-ci.org/trinker/textplot)
[![Coverage
Status](https://coveralls.io/repos/trinker/textplot/badge.svg?branch=master)](https://coveralls.io/r/trinker/textplot?branch=master)

![](tools/textplot_logo/r_textplot.png)

**textplot** is a suite of text plotting tools that enable the user to
analyze text data via serveral common text plotting methods. Methods
include lexical dispersion plots, word trees, speech networks,
co-occurrence networks, speech Gantt charts, text hilighting, and word
clouds.


Table of Contents
============

-   [Functions](#functions)
-   [Installation](#installation)
-   [Examples](#examples)
    -   [Lexical Dispersion](#lexical-dispersion)
    -   [Word Trees](#word-trees)
    -   [Speech Networks](#speech-networks)
    -   [Co-occurrence Networks](#co-occurrence-networks)
    -   [Speech Gantt Charts](#speech-gantt-charts)
    -   [Text Hilighting](#text-hilighting)
        -   [Regular Expresion Terms](#regular-expresion-terms)
        -   [Token Matching](#token-matching)
        -   [Sentence Matching](#sentence-matching)
    -   [Word Clouds](#word-clouds)
-   [Contact](#contact)

Functions
============


Installation
============

To download the development version of **textplot**:

Download the [zip
ball](https://github.com/trinker/textplot/zipball/master) or [tar
ball](https://github.com/trinker/textplot/tarball/master), decompress
and run `R CMD INSTALL` on it, or use the **pacman** package to install
the development version:

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load_current_gh("trinker/textplot")

Examples
========

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(dplyr, magrittr, textplot)

Lexical Dispersion
------------------

Word Trees
----------

    word_tree(sam_i_am, word = 'I') %>%
        plot()

![](tools/figure/wordtree_1.png)

    word_tree(sam_i_am, word = 'do') %>%
        plot()

![](tools/figure/wordtree_2.png)

    presidential_debates_2012 %>%
        dplyr::filter(person %in% c('ROMNEY', 'OBAMA')) %$%
        word_tree(
            text.var = dialogue, 
            word = 'america', 
            grouping.var = person
        ) %>%
        plot()

<script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>
<script type="text/javascript">
      google.charts.load('current', {packages:['wordtree']});
      google.charts.setOnLoadCallback(drawChart);

      function drawChart() {
        var data = google.visualization.arrayToDataTable(
          [ ['Phrases'],
        ['america is the free enterprise system and freedom and the fact that people can go out there and start a business work on an idea make their own decisions'],
        ['america'],
        ['america roads bridges schools'],
        ['america \'s future is going to bright as well'],
        ['america and putting people back to work'],
        ['america as the land of promise'],
        ['america regardless of where they come from what they look like have a chance to succeed'],
        ['america is our friend'],
        ['america you said russia not al qaida you said russia in the one thousand nine hundred eightys they\'re now calling to ask for their foreign policy back because you know the cold war\'s been over for twenty years'],
        ['america safe over the long haul'],
        ['america has to stand with democracy'],
        ['america to be successful in this region there\'s some things that we\'re going to have to do here at home as well'],
        ['america remains the one indispensable nation'],
        ['america and it is stronger now than when i came into office'],
        ['america and that\'s what my plan does'],
        ['america \'s position in the world and the world notices'],
        ['america will stand with israel'],
        ['america \'s national interest and that will be the case so long as i\'m president'],
        ['america has stood on the right side of history'],
        ['america'],
        ['america is a pacific power that we are going to have a presence there'],
        ['america'],
        ['america continues to be the greatest nation on earth'],
          ]
        );

        var options = {
          wordtree: {
            format: 'implicit',
            word: 'america'
          }
        };

        var chart = new google.visualization.WordTree(document.getElementById('0000000001'));
        chart.draw(data, options);
      }
    </script>
<script type="text/javascript">
      google.charts.load('current', {packages:['wordtree']});
      google.charts.setOnLoadCallback(drawChart);

      function drawChart() {
        var data = google.visualization.arrayToDataTable(
          [ ['Phrases'],
        ['america together and having a discussion on this important topic you pushed through something that you and nancy pelosi and harry reid thought was the best answer and drove it through'],
        ['america telling a patient and a doctor what kind of treatment they can have'],
        ['america to have a stronger more vibrant economy'],
        ['america \'s military'],
        ['america \'s government we were talking about the role of government is not to become the economic player picking winners and losers telling people what kind of health treatment they can receive taking over the health care system that has existed in this country for a long long time and has produced the best health records in the world'],
        ['america faces right now look the reason i\'m in this race is there are people that are really hurting today in this country'],
        ['america'],
        ['america'],
        ['america has been taking over the last four years'],
        ['america'],
        ['america do you want to have for yourself and for your children'],
        ['america \'s middle class working again'],
        ['america \'s young people'],
        ['america twelve million new jobs in four years and rising take home pay'],
        ['america'],
        ['america energy independence within eight years you\'re going to see manufacturing jobs come back'],
        ['america energy independent'],
        ['america and that\'s what i\'m going to do'],
        ['america is to have a true all of the above policy'],
        ['america energy secure'],
        ['america'],
        ['america have been crushed over the last four years'],
        ['america \'s economy going again'],
        ['america \'s workers work in businesses that are taxed as individuals'],
        ['america in five years'],
        ['america'],
        ['america'],
        ['america get good work by getting a stronger economy and by supporting women in the workforce'],
        ['america should have access to contraceptives'],
        ['america without having to go to the the arabs or the venezuelans or anyone else'],
        ['america become energy secure'],
        ['america'],
        ['america'],
        ['america'],
        ['america again'],
        ['america'],
        ['america the most attractive place in the world for businesses of all kinds'],
        ['america the most attractive place for entrepreneurs for people who want to expand their business'],
        ['america again'],
        ['america working again'],
        ['america in particular which is to see a a complete change in the the structure and the the environment in the middle east'],
        ['america long term and we must have a comprehensive strategy to help reject this kind of extremism'],
        ['america'],
        ['america can help that happen'],
        ['america'],
        ['america \'s strength'],
        ['america \'s influence will grow'],
        ['america \'s influence greater today than it was four years ago'],
        ['america has a a responsibility and the privilege of helping defend freedom and promote the principles that that make the world more peaceful'],
        ['america must be strong'],
        ['america must lead'],
        ['america working again and see rising take home pay again and i\'ll do it with five simple steps'],
        ['america'],
        ['america we have just not taken advantage of fully'],
        ['america \'s economy is almost as big as the economy of china'],
        ['america is a huge opportunity for us time zone language opportunities'],
        ['america if they think we\'re headed to the road to greece'],
        ['america'],
        ['america'],
        ['america'],
        ['america with us the president was silent'],
        ['america had been dismissive and derisive'],
        ['america had dictated to other nations'],
        ['america has not dictated to other nations'],
        ['america'],
        ['america \'s commitments around the world and they see what\'s happening and they say well ok'],
        ['america going to be strong'],
        ['america will be very strong'],
        ['america more competitive'],
        ['america the most attractive place in the world for entrepreneurs innovators businesses to grow'],
        ['america decline every year for middle income families now down dollar four thousand three hundred during your term'],
        ['america once again the most attractive place in the world to start businesses to build jobs to grow the economy'],
        ['america \'s going to have that kind of leadership and continue to promote principles of peace to make a world a safer place and make people in this country more confident that their future is secure'],
        ['america \'s going to come back and for that to happen we\'re going to have to have a president who can work across the aisle'],
        ['america as the hope of the earth'],
          ]
        );

        var options = {
          wordtree: {
            format: 'implicit',
            word: 'america'
          }
        };

        var chart = new google.visualization.WordTree(document.getElementById('0000000002'));
        chart.draw(data, options);
      }
    </script>
<h3>
OBAMA
</h3>
    <div id="0000000001" style="width: 900px; height: 600px;"></div>

<h3>
ROMNEY
</h3>
    <div id="0000000002" style="width: 900px; height: 600px;"></div>

![](tools/figure/wordtree_3.png)

Speech Networks
---------------

Co-occurrence Networks
----------------------

Speech Gantt Charts
-------------------

Text Hilighting
---------------

### Regular Expresion Terms

    library(tidyverse)

    map1 <- list(
        `#FF69B4` = c('we(\'[a-z]+)?\\b'),
        `#7CFC00` = c('he(\'[a-z]+)?\\b'),
        yellow = 'you',
        gray70 = '\\bi\\b'
    )

    set.seed(10)
    presidential_debates_2012 %>%
        dplyr::filter(person %in% c('ROMNEY', 'OBAMA')) %>%
        dplyr::group_by(person) %>%
        dplyr::sample_n(15) %$%
        hilight_term(dialogue, map1, list(person)) %>%
        plot()

    ## <style>
    ## mark.ncexktvyrnhfasgeamcuj {
    ##     background-color: #FF69B4;
    ##     color: black;
    ## }
    ## mark.ygmeplmknacmkvtolcfbn {
    ##     background-color: #7CFC00;
    ##     color: black;
    ## }
    ## mark.qnbojygfwmfqfauheemsp {
    ##     background-color: yellow;
    ##     color: black;
    ## }
    ## mark.njvgotaibjkxxzmsluven {
    ##     background-color: gray70;
    ##     color: black;
    ## }
    ## h1 { 
    ##     display: block;
    ##     font-size: 1.2em;
    ##     margin-top: 0.0em;
    ##     margin-bottom: 0.0em;
    ##     margin-left: 0;
    ##     margin-right: 0;
    ##     font-weight: bold;
    ## }
    ## .indented {
    ##     margin-left: 5%%;
    ##     margin-right: 5%%;
    ## }
    ## </style>
    ## <h3>OBAMA</h3>
    ## <p>Secretary Clinton has done an extraordinary job.</p> <p>And when Governor Romney stands here, after a year of campaigning, when during a Republican primary <mark class="ygmeplmknacmkvtolcfbn">he</mark> stood on stage and said <mark class="njvgotaibjkxxzmsluven">I</mark>'m going to give tax cuts <mark class="ygmeplmknacmkvtolcfbn">he</mark> didn't say tax rate cuts, <mark class="ygmeplmknacmkvtolcfbn">he</mark> said tax cuts to everybody, including t<mark class="ygmeplmknacmkvtolcfbn">he</mark> top one percent, <mark class="qnbojygfwmfqfauheemsp">you</mark> should believe him because that's been his history.</p> <p>Make sure <mark class="qnbojygfwmfqfauheemsp">you</mark>r kids can go to college.</p> <p>That's exactly what <mark class="ncexktvyrnhfasgeamcuj">we're</mark> doing.</p> <p>But what it does say is that insurers, <mark class="qnbojygfwmfqfauheemsp">you</mark>'ve got to take everybody.</p> <p>That's going to help Jeremy get a job.</p> <p><mark class="njvgotaibjkxxzmsluven">I</mark>'d get rid of it.</p> <p>What <mark class="njvgotaibjkxxzmsluven">I</mark>'m not for is us ignoring t<mark class="ygmeplmknacmkvtolcfbn">he</mark> other half of t<mark class="ygmeplmknacmkvtolcfbn">he</mark> equation.</p> <p>Loves his family, cares about his faith.</p> <p>And <mark class="ncexktvyrnhfasgeamcuj">we've</mark> been through tough times but <mark class="ncexktvyrnhfasgeamcuj">we</mark> always bounce back because of our character, because <mark class="ncexktvyrnhfasgeamcuj">we</mark> pull together and if <mark class="njvgotaibjkxxzmsluven">I</mark> have t<mark class="ygmeplmknacmkvtolcfbn">he</mark> privilege of being <mark class="qnbojygfwmfqfauheemsp">you</mark>r president for another four years, <mark class="njvgotaibjkxxzmsluven">I</mark> promise <mark class="qnbojygfwmfqfauheemsp">you</mark> <mark class="njvgotaibjkxxzmsluven">I</mark> will always listen to <mark class="qnbojygfwmfqfauheemsp">you</mark>r voices.</p> <p><mark class="qnbojygfwmfqfauheemsp">You</mark> said that, first, <mark class="ncexktvyrnhfasgeamcuj">we</mark> should not have a timeline in Afghanistan.</p> <p>When Governor Romney was asked whether teachers, hiring more teachers was important to growing our economy, Governor Romney said that doesn't grow our economy.</p> <p>In fact <mark class="ncexktvyrnhfasgeamcuj">we've</mark> seen layoffs of hundreds of thousands of teachers over t<mark class="ygmeplmknacmkvtolcfbn">he</mark> last several years, and Governor Romney doesn't think <mark class="ncexktvyrnhfasgeamcuj">we</mark> need more teachers.</p> <p>That's what reporters called it.</p> <p><mark class="ygmeplmknacmkvtolcfbn">He</mark> said, <mark class="njvgotaibjkxxzmsluven">I</mark>'ll get back to <mark class="qnbojygfwmfqfauheemsp">you</mark>.</p>
    ## <h3>ROMNEY</h3>
    ## <p>T<mark class="ygmeplmknacmkvtolcfbn">he</mark> middle class is getting crushed under t<mark class="ygmeplmknacmkvtolcfbn">he</mark> policies of a president who has not understood what it takes to get t<mark class="ygmeplmknacmkvtolcfbn">he</mark> economy working again.</p> <p>It's been two years.</p> <p><mark class="njvgotaibjkxxzmsluven">I</mark> will never know.</p> <p>President Bush didn't.</p> <p>Look, <mark class="njvgotaibjkxxzmsluven">I</mark> look at what's happening around t<mark class="ygmeplmknacmkvtolcfbn">he</mark> world, and <mark class="njvgotaibjkxxzmsluven">i</mark> see Iran four years closer to a bomb.</p> <p>And <mark class="njvgotaibjkxxzmsluven">I</mark> <mark class="njvgotaibjkxxzmsluven">I</mark> don't blame t<mark class="ygmeplmknacmkvtolcfbn">he</mark> administration for t<mark class="ygmeplmknacmkvtolcfbn">he</mark> fact that t<mark class="ygmeplmknacmkvtolcfbn">he</mark> relationship with Pakistan is strained.</p> <p>But my strategy is broader than that.</p> <p>But t<mark class="ygmeplmknacmkvtolcfbn">he</mark> military let's get back to t<mark class="ygmeplmknacmkvtolcfbn">he</mark> military, though.</p> <p><mark class="njvgotaibjkxxzmsluven">I</mark> ran t<mark class="ygmeplmknacmkvtolcfbn">he</mark> Olympics and balanced t<mark class="ygmeplmknacmkvtolcfbn">he</mark> budget.</p> <p><mark class="njvgotaibjkxxzmsluven">I</mark> came through small business.</p> <p><mark class="ncexktvyrnhfasgeamcuj">We're</mark> blessed with terrific soldiers, and extraordinary technology and intelligence.</p> <p>But let me let me come back <mark class="ncexktvyrnhfasgeamcuj">we</mark> can come back.</p> <p><mark class="qnbojygfwmfqfauheemsp">You</mark> took General Motors bankrupt.</p> <p>There are a number of things that sound good, but frankly, <mark class="ncexktvyrnhfasgeamcuj">we</mark> just can't afford them.</p> <p><mark class="njvgotaibjkxxzmsluven">I</mark> <mark class="njvgotaibjkxxzmsluven">I</mark> was <mark class="njvgotaibjkxxzmsluven">I</mark> was someone who ran businesses for twenty five years, and balanced t<mark class="ygmeplmknacmkvtolcfbn">he</mark> budget.</p>

### Token Matching

### Sentence Matching

Word Clouds
-----------

Contact
=======

You are welcome to:    
- submit suggestions and bug-reports at: <https://github.com/trinker/textplot/issues>    
- send a pull request on: <https://github.com/trinker/textplot/>    
- compose a friendly e-mail to: <tyler.rinker@gmail.com>    
