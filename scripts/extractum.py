#This script uses the encoding utf-8

import sys,string,re, math

#User inputs age, gidit ratio, gender scale ratio, and interviewer name manually

age = sys.argv[1]

digit = float(sys.argv[2])

gender = float(sys.argv[4])

#Assumed to be Kinsey number:

sexuality = sys.argv[5]

#Assumed to be binary:

tomboy = sys.argv[6]

interviewer = sys.argv[3]

#logdigit = math.log(digit,2)

lines = sys.stdin.readlines()

#This assumes that the interviewee name appears in the second field of the first line; ELAN output needs to be set up to output the interviewee lines first, or else this is not helpful.

#Assumes the name of the interviewee can be found in the first field of the first line. Make sure ELAN output is configured accordingly.

name = lines[0].split()[0]
#name = "CT"

interviewee = []

#All transcribed instances of um/uh, ignoring case. Note that this also includes "er", because that is theoretically ambiguous in UK dialects. "em" shouldn't actually occur in the transcriptions at all, but it's included just in case.

filledpause = re.compile("^(([Uu][mh])|([Ee][rm]))$")

#annotations for pause preceding 

precpause = re.compile("((\{BR\})|(\{CG\})|(\{LS\})|(\{LG\})|([\.\,]))$")

#annotation for pause following ; note that the extra set of parens is important so that the caret scopes over the disjunction

folpause = re.compile("^((\{BR\})|(\{CG\})|(\{LS\})|(\{LG\})|([\.\,]))")

#regex for a filled pause with a comma or period following; this is used to insert a space between the um/uh and the comma/period.

commaregex = re.compile("(([Uu][mh])|([Ee][rm]))([,\.])")

#Inserts space between um/uh and comma/period

def comma(matchobj):
    return ("%s %s" % (matchobj.group(1),matchobj.group(4)))


#Remember to output everything in lower case

#Makes a list of all lines spoken by the interviewee, ignoring interviewer speech

#Assumes the name of the interviewee can be found in the first field of the first line. Make sure ELAN output is configured accordingly.

for li in lines:
    if li.split()[0] == name:
        interviewee.append(li)

#Get a wordcount, before splitting off commas/periods. The -3 is to remove the metadata fields; this assumes that ELAN is outputting 3 metadata fields at the beginning of each line.

wordcount = 0

for line in interviewee:
    words = line.split()
    wordcount = wordcount+len(words)-3



for line in interviewee:
#Add a space before following comma or period to make life easier
    line = commaregex.sub(comma,line)
    words = line.split()
    ii = 0
    for w in words:
        matches = filledpause.search(w)
        if matches != None:
            sys.stdout.write("%s,%s," % (name,matches.group(0).lower()))
#Again, the ii==3 is assuming ELAN has output 3 columns of metadata; if this changes you, you have to change this. It's there so that um/uh at the beginning of an utterance is coded as having a preceding pause.
            if (ii == 3) or (precpause.search(words[ii-1]) != None):
                sys.stdout.write("P,")
            else:
                sys.stdout.write("S,")
            if (ii == len(words)-1) or (folpause.search(words[ii+1]) != None):
                sys.stdout.write("P,")
            else:
                sys.stdout.write("S,")
            sys.stdout.write("%s,%s,%s,%s,%s,%s,%s\n" % (age, digit, interviewer, wordcount, gender, sexuality, tomboy))
        ii = ii+1
