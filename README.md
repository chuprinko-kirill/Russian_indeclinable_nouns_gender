# Вариативность рода несклоняемых существительных в русском (for English scroll further)

В этом репозитроии хранятся корпусные данные, иллюстрирующие вариативность выбора рода у несклоняемых существительных в современном русском языке. Данные собирались из Генерального Интернет-Корпуса Русского Языка [(ГИКРЯ)](http://www.webcorpora.ru/), подкорпуса LiveJournal.

# Russian indeclinable nouns' gender variation

The corpus data on gender assignment variation in contemporary Russian can be found here. The data were collectd from LiveJournal subcorpus of General Internet Corpus of Russian [GICR](http://www.webcorpora.ru/en/). Some scripts for the data's statistical analysis are also shared here.

The main dataset available in this repository at the moment is .xlsx format file (default format for Microsoft Excel since the version of 2007) with data on common inanimate indeclinable nouns. We plan to add more data on other indeclinable nouns type (as acronyms, animate common nouns, inanimate personal nouns speficially toponyms). We will update downloaded files in order to improve their accessability for non native Russian speakers. We would like also to update them in case when we detect errors in the collected data, so any contribution (reporting detected errors or sharing more apropriate data if you would like to share) will be apreciated.

Common structure for our data files is as follow:

- first page of the Excel book has the pivot table allowing to quickly look through the list of studied words and the quantity of collected instances for each agreement pattern
- second page contains raw data from corpus, including available metadata and demographic data. We also add some grammar markup: gender feature, case.
- third page, "lookup" contains markup for each studied noun (stress position, hypernym information, final vowel etc)
