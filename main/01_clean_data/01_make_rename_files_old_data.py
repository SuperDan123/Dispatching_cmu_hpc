
import pandas as pd
import os


sheet_list = ["H22", "H23_ippan", "H23_tokutei", "H24", "H25", "H26", "H27_old", "H27_new"]

for s in sheet_list:

	df = pd.read_excel("rawdata/variable_names/variable_name_old_data.xlsx", sheet_name = s)
	varname_list = df["English"].to_list()

	if not os.path.exists("main/rename_old"):
		os.makedirs("main/rename_old")

	file_name = "main/rename_old/rename_" + s + ".do"
	with open(file_name, mode = "w") as f:

		for i, varname in enumerate(varname_list):
			line = "rename v" + str(i + 1) + " " + varname + " \n"
			f.write(line)
