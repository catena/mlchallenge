import csv
import re

i = 0
with open("data/Training.csv", "r") as infile, open("data/Training.vw", "wb") as outfile:
  reader = csv.reader(infile)
  reader.next()
  for line in reader:
      vw_line = "1 4.0" if line[20] == "1" else "-1"
      vw_line += " |f acc_len:" + line[0]
      vw_line += " messages:" + line[1]
      vw_line += " day.mins:" + line[2]
      vw_line += " eve.mins:" + line[3]
      vw_line += " night.mins:" + line[4]
      vw_line += " intl.mins:" + line[5]
      vw_line += " custserv.calls:" + line[6]
      vw_line += " intl.plan:" + line[7]
      vw_line += " msg.plan:" + line[8]
      vw_line += " day.calls:" + line[9]
      vw_line += " day.charge:" + line[10]
      vw_line += " eve.calls:" + line[11]
      vw_line += " eve.charge:" + line[12]
      vw_line += " night.calls:" + line[13]
      vw_line += " night.charge:" + line[14]
      vw_line += " intl.calls:" + line[15]
      vw_line += " intl.charge:" + line[16]
      outfile.write(vw_line + "\n") 
  
with open("data/Testing.csv", "r") as infile, open("data/Testing.vw", "wb") as outfile:
  reader = csv.reader(infile)
  reader.next()
  for line in reader:
      vw_line = "1 |f acc_len:" + line[0]
      vw_line += " messages:" + line[1]
      vw_line += " day.mins:" + line[2]
      vw_line += " eve.mins:" + line[3]
      vw_line += " night.mins:" + line[4]
      vw_line += " intl.mins:" + line[5]
      vw_line += " custserv.calls:" + line[6]
      vw_line += " intl.plan:" + line[7]
      vw_line += " msg.plan:" + line[8]
      vw_line += " day.calls:" + line[9]
      vw_line += " day.charge:" + line[10]
      vw_line += " eve.calls:" + line[11]
      vw_line += " eve.charge:" + line[12]
      vw_line += " night.calls:" + line[13]
      vw_line += " night.charge:" + line[14]
      vw_line += " intl.calls:" + line[15]
      vw_line += " intl.charge:" + line[16]
      outfile.write(vw_line + "\n") 

