"""
This program was adapted from the Stanford NLP class SpamLord homework assignment.
    The code has been rewritten and the data modified, nevertheless
    please do not make this code or the data public.
This version has two patterns that were suggested in comments
    in order to get you started .
"""
import sys
import os
import re
import pprint

"""
TODO
For Part 1 of our assignment, add to these two lists of patterns to match
examples of obscured email addresses and phone numbers in the text.
For optional Part 3, you may need to add other lists of patterns.
"""
# email .edu patterns

# each regular expression pattern should have exactly two sets of parentheses.
#   the first parenthesis should be around the someone part
#   the second parenthesis should be around the somewhere part
#   in an email address whose standard form is someone@somewhere.edu
epatterns = []
epatterns.append('([A-Za-z.]+)@([A-Za-z.]+)\.edu') #orignal line
epatterns.append('([A-Za-z.]+)\s+@\s+([A-Za-z.]+)\.edu') #original line
epatterns.append('([A-Za-z.]+)@([A-Za-z.]+)\.EDU') #captial letter 'EDU'
epatterns.append('([A-Za-z]+)\sWHERE\s([A-Za-z]+)\sDOM\sedu') # gets 'WHERE' for '@' after name and 'DOM' for 'domain/dot'
epatterns.append('([A-Za-z.]+)\sat\s([A-Za-z.]+)\.edu') # 'name at email.edu'
epatterns.append('([A-Za-z.]+)<del>@([A-Za-z.]+)\.edu') # name '<del>' @ email.edu
epatterns.append('([A-Za-z.]+)\s<at symbol>\s([A-Za-z.]+)\.edu') # '<at symbol>' instead of '@'
epatterns.append('([A-Za-z.]+)\s\(followed\sby\s\"@?(@?[A-Za-z.]+)\.edu') # 'name (followed by "@email.edu")'

##Patterns that don't work, they only call more False Positives, but match in RegEx

#epatterns.append('([A-Za-z.]+)@([A-Za-z.]+)\.com') # for .com emails
#epatterns.append('([-A-Za-z]+?)\-+@\-+([-A-Za-z]+?)\.[-edu]+?') # '-' in front of letters
#epatterns.append('([A-Za-z.]+)\s+at\s+([A-Za-z.]+)\sdt\sedu') #'dt' instead of '.'
#epatterns.append('([A-Za-z.]+)\sat\s([A-Za-z\sdot]+)\sedu')# 'at/dot' instead of '@' and '.'
#epatterns.append('([A-Za-z.]+)\sAT\s([A-Za-z\sDOT]+)\sedu') # captial 'AT' and 'DOT'


# phone patterns
# each regular expression pattern should have exactly three sets of parentheses.
#   the first parenthesis should be around the area code part XXX
#   the second parenthesis should be around the exchange part YYY
#   the third parenthesis should be around the number part ZZZZ
#   in a phone number whose standard form is XXX-YYY-ZZZZ
ppatterns = []
ppatterns.append('(\d{3})-(\d{3})-(\d{4})') #original line
ppatterns.append('(\d{3})\s?\-?\s(\d{3})\s\-?\s(\d{4})') #modified original, possible space between number group and '-' (51-56 TP) get rid of the possible \s (32 to 6 FP)
ppatterns.append('\W(\d{3})\W\s?-?(\d{3})-(\d{4})') # parenthesis around first group and possible '-' after,(56 to 64 TP), also possible space after parenthesis(went from 64 to 103 TP)
ppatterns.append('\+1\s\(?(\d{3})\)?\s?(\d{3})\s?(\d{4})') # Adds for a '+1\s' before first group and just spaces between groups, if we add possible '\-?' then it creates more FP (TP 102-104)



""" 
This function takes in a filename along with the file object and
scans its contents against regex patterns. It returns a list of
(filename, type, value) tuples where type is either an 'e' or a 'p'
for e-mail or phone, and value is the formatted phone number or e-mail.
The canonical formats are:
     (name, 'p', '###-###-#####')
     (name, 'e', 'someone@something')
If the numbers you submit are formatted differently they will not
match the gold answers

TODO
For Part 3, if you have added other lists, you should add
additional for loops that match the patterns in those lists
and produce correctly formatted results to append to the res list.
"""
def process_file(name, f):
    # note that debug info should be printed to stderr
    # sys.stderr.write('[process_file]\tprocessing file: %s\n' % (path))
    res = []
    for line in f:
        # you may modify the line, using something like substitution
        #    before applying the patterns

        # email pattern list
        for epat in epatterns:
            # each epat has 2 sets of parentheses so each match will have 2 items in a list
            matches = re.findall(epat,line)
            for m in matches:
                # string formatting operator % takes elements of list m
                #   and inserts them in place of each %s in the result string
                # email has form  someone@somewhere.edu
                #email = '%s@%s.edu' % m
                email = '{}@{}.edu'.format(m[0],m[1])
                res.append((name,'e',email))
                ## email has option of domain being .com


        # phone pattern list
        for ppat in ppatterns:
            # each ppat has 3 sets of parentheses so each match will have 3 items in a list
            matches = re.findall(ppat,line)
            for m in matches:
                # phone number has form  areacode-exchange-number
                #phone = '%s-%s-%s' % m
                phone = '{}-{}-{}'.format(m[0],m[1],m[2])
                res.append((name,'p',phone))
    return res

"""
You should not edit this function.
"""
def process_dir(data_path):
    # save complete list of candidates
    guess_list = []
    # save list of filenames
    fname_list = []

    for fname in os.listdir(data_path):
        if fname[0] == '.':
            continue
        fname_list.append(fname)
        path = os.path.join(data_path,fname)
        f = open(path,'r', encoding='latin-1')
        # get all the candidates for this file
        f_guesses = process_file(fname, f)
        guess_list.extend(f_guesses)
    return guess_list, fname_list

"""
You should not edit this function.
Given a path to a tsv file of gold e-mails and phone numbers
this function returns a list of tuples of the canonical form:
(filename, type, value)
"""
def get_gold(gold_path):
    # get gold answers
    gold_list = []
    f_gold = open(gold_path,'r', encoding='latin-1')
    for line in f_gold:
        gold_list.append(tuple(line.strip().split('\t')))
    return gold_list

"""
You should not edit this function.
Given a list of guessed contacts and gold contacts, this function
    computes the intersection and set differences, to compute the true
    positives, false positives and false negatives. 
It also takes a dictionary that gives the guesses for each filename, 
    which can be used for information about false positives. 
Importantly, it converts all of the values to lower case before comparing.
"""
def score(guess_list, gold_list, fname_list):
    guess_list = [(fname, _type, value.lower()) for (fname, _type, value) in guess_list]
    gold_list = [(fname, _type, value.lower()) for (fname, _type, value) in gold_list]
    guess_set = set(guess_list)
    gold_set = set(gold_list)

    # for each file name, put the golds from that file in a dict
    gold_dict = {}
    for fname in fname_list:
        gold_dict[fname] = [gold for gold in gold_list if fname == gold[0]]

    tp = guess_set.intersection(gold_set)
    fp = guess_set - gold_set
    fn = gold_set - guess_set

    pp = pprint.PrettyPrinter()
    #print 'Guesses (%d): ' % len(guess_set)
    #pp.pprint(guess_set)
    #print 'Gold (%d): ' % len(gold_set)
    #pp.pprint(gold_set)

    print ('True Positives (%d): ' % len(tp))
    # print all true positives
    pp.pprint(tp)
    print ('False Positives (%d): ' % len(fp))
    # for each false positive, print it and the list of gold for debugging
    for item in fp:
        fp_name = item[0]
        pp.pprint(item)
        fp_list = gold_dict[fp_name]
        for gold in fp_list:
            s = pprint.pformat(gold)
            print('   gold: ', s)
    print ('False Negatives (%d): ' % len(fn))
    # print all false negatives
    pp.pprint(fn)
    print ('Summary: tp=%d, fp=%d, fn=%d' % (len(tp),len(fp),len(fn)))

"""
You should not edit this function.
It takes in the string path to the data directory and the gold file
"""
def main(data_path, gold_path):
    guess_list, fname_list = process_dir(data_path)
    gold_list =  get_gold(gold_path)
    score(guess_list, gold_list, fname_list)

"""
commandline interface assumes that you are in the directory containing "data" folder
It then processes each file within that data folder and extracts any
matching e-mails or phone numbers and compares them to the gold file
"""
if __name__ == '__main__':
    print ('Assuming ContactFinder.py called in directory with data folder')
    main('data/dev', 'data/devGOLD')
