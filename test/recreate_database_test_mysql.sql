drop database test;

create database test;
use test;

--  path is relative to build directory
source ../../src/create_schema_mysql.sql;

-- end of file
