import urllib.request
import os

common_path = "rawdata/meshdata/"
common_url = "https://www.stat.go.jp/data/mesh/csv/"

if not os.path.exists("rawdata/meshdata"):
	os.makedirs("rawdata/meshdata")

for pref in range(1, 48):
    print("------ {0} -----".format(pref))
    
    if pref == 1:
        for i in range(1, 4):
            urllib.request.urlretrieve(common_url + "01-" + str(i) + ".csv", common_path + "1_" + str(i) + ".csv")
    elif pref <= 9:
        urllib.request.urlretrieve(common_url + "0" + str(pref) + ".csv", common_path + str(pref) + ".csv")
    else:
        urllib.request.urlretrieve(common_url + str(pref) + ".csv", common_path + str(pref) + ".csv")
