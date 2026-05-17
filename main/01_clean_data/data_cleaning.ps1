# python "main/01_clean_data/01_make_rename_files_old_data.py"
# stata do "main/01_clean_data/02_read_data_old.do"
Rscript "main/01_clean_data/03_clean_data_aggregate.R"     
Rscript "main/01_clean_data/04_clean_data_establishment.R"
Rscript "main/01_clean_data/05_make_municipal_merger_history.R"
Rscript "main/01_clean_data/06_update_commuting_zone.R"
Rscript "main/01_clean_data/07_make_adjacent_municipalites.R"

# Report
Rscript -e "rmarkdown::render('report/01_clean_data/data_codebook.Rmd')"