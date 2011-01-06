#!/usr/bin/env python2.6

import tinyurl
import urllib
from optparse import OptionParser


class LetMeGoogleThatForYou(object):
    """LMGTFY helper with link shortening via TinyUrl
    """

    def googleThatForMe(self, text):
        """Return shortened URL to LMGTFY search

        @param text: text to search for
        @return: shortened URL
        """
        url = 'http://lmgtfy.com/?%s' % (urllib.urlencode({'q': text}))
        return tinyurl.create_one(url)

def main():
    parser = OptionParser(usage="%prog query", version="%prog 0.1")
    (opts, args) = parser.parse_args()
    if len(args) == 0:
        parser.error("no search query supplied")
    query = " ".join(args)
    res = LetMeGoogleThatForYou().googleThatForMe(query)
    print res.encode('utf8')

if __name__ == '__main__':
    main()

