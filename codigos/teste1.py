'''


Testando 1..2..3..
'''

import os
import urllib2

csvfile = '/tmp/links.csv'
targetdir = '/tmp/so'

with open(csvfile) as links:
    for link in links:
        filename = link.split('/')[-1].strip()
        filepath = os.path.join(targetdir, filename)
        print 'Downloading %s \n\t .. to %s' % (link.strip(), filepath)
        with open(filepath, 'w') as data:
            xlsfile = urllib2.urlopen(link)
            data.writelines(xlsfile)