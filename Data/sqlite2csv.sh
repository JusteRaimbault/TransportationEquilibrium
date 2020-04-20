#!/bin/bash
DB=$1
OUT=$2
sqlite3 $OUT <<!
.headers on
.mode csv
.output out.csv
select * from data;
	!
