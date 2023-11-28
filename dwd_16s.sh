#!/bin/bash


mkdir 16S_DB
export KRAKEN2_DB_NAME=16S_DB
export KRAKEN2_THREAD_CT=4
./16s_intl.sh
