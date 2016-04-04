--  Dump all tables to csv files, for backup. SQLite3 syntax

.output author.csv
select * from Author;

.output collection.csv
select * from Collection;

.output series.csv
select * from Series;

.output title.csv
select * from Title;

.output author_collection.csv
select * from AuthorCollection;

.output author_series.csv
select * from AuthorSeries;

.output author_title.csv
select * from AuthorTitle;

.output series_title.csv
select * from SeriesTitle;

.output collection_title.csv
select * from CollectionTitle;

-- end of file
