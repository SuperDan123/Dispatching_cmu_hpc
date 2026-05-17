#!/bin/bash

Rscript "main/00_get_data/get_aggregate_data.R"
python "main/00_get_data/download_meshdata.py"