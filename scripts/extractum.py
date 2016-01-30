#This script uses the encoding utf-8

import sys,string,re, math

#User inputs age, gidit ratio, gender scale ratio, and interviewer name manually

age = sys.argv[1]

digit = float(sys.argv[2])

gender = float(sys.argv[4])

interviewer = sys.argv[3]

#logdigit = math.log(digit,2)

lines = sys.stdin.readlines()

#This assumes that the interviewee name appears in the second field of the first line; ELAN output needs to be set up to output the interviewee lines first, or else this is not helpful.

name = lines[0].split()[1]
#name = "CT"

interviewee = []

#All transcribed instances of um/uh, ignoring case

filledpause = re.compile("^[Uu][mh]$")

#annotations for pause preceding 

precpause = re.compile("(\{BR\})|(\{CG\})|(\{LS\})|(\{LG\})|([\.\,])$")

#annotation for pause following 

folpause = re.compile("^(\{BR\})|(\{CG\})|(\{LS\})|(\{LG\})|([\.\,])")

#regex for a filled pause with a comma or period following; this is used to insert a space between the um/uh and the comma/period.

commaregex = re.compile("([Uu][mh])([,\.])")

#Inserts space between um/uh and comma/period

def comma(matchobj):
    return ("%s %s" % (matchobj.group(1),matchobj.group(2)))


#Remember to output everything in lower case

#Makes a list of all lines spoken by the interviewee, ignoring interviewer speech

for li in lines:
    if li.split()[1] == name:
        interviewee.append(li)

#Get a wordcount, before splitting off commas/periods. The -4 is to remove the metadata fields; this assumes that ELAN is outputting 4 metadata fields at the beginning of each line.

wordcount = 0

for line in interviewee:
    words = line.split()
    wordcount = wordcount+len(words)-4



for line in interviewee:
#Add a space before following comma or period to make life easier
    line = commaregex.sub(comma,line)
    words = line.split()
    ii = 0
    for w in words:
        matches = filledpause.search(w)
        if matches != None:
            sys.stdout.write("%s,%s," % (name,matches.group(0).lower()))
#Again, the ii==4 is assuming ELAN has output 4 columns of metadata; if this changes you, you have to change this. It's there so that um/uh at the beginning of an utterance is coded as having a preceding pause.
            if (ii == 4) or (precpause.search(words[ii-1]) != None):
                sys.stdout.write("P,")
            else:
                sys.stdout.write("S,")
            if (ii == len(words)-1) or (folpause.search(words[ii+1]) != None):
                sys.stdout.write("P,")
            else:
                sys.stdout.write("S,")
            sys.stdout.write("%s,%s,%s,%s,%s\n" % (age, gender, digit, interviewer, wordcount))
        ii = ii+1
