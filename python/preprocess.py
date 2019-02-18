#!/usr/bin/env python3


import sys
import re




def preprocess(filecontent):
  # Remove /* .. */ but keep newlines
  lastpos = 0
  lenfile = len(filecontent)
  inside = False
  newlinesinignored = 0
  outcontent = ""
  while lastpos < lenfile and lastpos != -1:
    if not inside:
      pos = filecontent.find("/*", lastpos)
      newlines = ""
      for i in range(0,newlinesinignored):
        newlines = newlines + "\n"
      outcontent = outcontent + newlines + filecontent[lastpos:pos]
      inside = True
    else:
      pos = filecontent.find("*/", lastpos)
      ignoredcontent = filecontent[lastpos:pos]
      newlinesinignored = ignoredcontent.count("\n")
      inside = False
    if pos != -1:
      lastpos = pos+2
    else:
      lastpos = -1

  # Remove // comments
  outcontent2 = ""
  endcommentm = re.compile(r"^(.*?)\/\/.*$")
  lines = outcontent.split("\n")
  first = True
  for l in lines:
    matches = endcommentm.match(l)
    if matches != None:
      outline = matches.group(1)
    else:
      outline = l
    if first:
      outcontent2 = outline
    else:
      outcontent2 = outcontent2 + "\n" + outline
    first = False
  return outcontent2


#with open("input.sv") as fp:
#  filecontent = fp.read()
#  o = preprocess(filecontent)
#  print(o)
  
