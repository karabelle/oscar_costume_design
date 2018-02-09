# oscar_costume_design

## Where did I get the data?
First, I looked at the awards referenced in fivethirtyeight.com's predictions and whether they had a costume design category which had been awarded for at least 5 years consistently.

Then to find a few more possiblities, I looked at past Oscar winning costume designers like Sandy Powell and Colleen Atwood on both Wikipedia and IMDB to see which other movie costume design awards they had won.

## What is in costume_dataset.csv?

`Year`
Four digit year - based on when the movie came out and therefor it's year of eligibility for the Oscars.
i.e. The Young Victoria won an Oscar for Costume Design at the Oscars ceremony held in 2010, but was released in 2009.
This file only contains information going back to 2000, instead of the 25 years fivethirtyeight used.

`Award` 
A character string.

|Data Value|Organization|Award Category/Name|Range of Years with Winners|Next Ceremony|
|----------|------------|-------------------|---------------------------|-------------|
|BAFTA|British Academy of Film and Television Arts||2000 - 2017|N|
|CCA|Critic's Choice Awards||2009 - 2017||
|CDGA-C|Costume Designer's Guild of America|Excellence in Contemporary Film|2000 - 2016|02/20/2018|
|CDGA-P|Costume Designer's Guild of America|Excellence in Period Film|2005 - 2016*|02/20/2018|
|CDGA-F|Costume Designer's Guild of America|Excellence in Sci-Fi / Fantasy Film|2005 - 2016|02/20/2018|
|LVFCS|Las Vegas Film Critic's Society||2000 - 2017*||
|OF&TA|Online Film & Television Awards||2000 - 2016|02/11/2018|
|PFCS|Phoenix Film Critic's Society||2000 - 2017||
|Satellite|International Press Academy||2000 - 2016|02/10/2018|

*CDGA-P & CDGA-F: These categories were combined until 2005, but the winners and nominees for 2000 - 2004 have been marked as being one or the other based on the genre of the film for ease of analysis.*
*LVFCS: Only includes winners, no nominees.*

`Movie`
A character string. May contain non-ASCII characters.

`Winner`
A single character or blank.
If winners have been announced, then "Y" denotes the winner and all other nominees are marked with "N". If winners have not been announced, all movies have a blank in this column.
