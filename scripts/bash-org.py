#!/usr/bin/env python2.6

import urllib, urllib2
from pyquery import PyQuery
import re
from optparse import OptionParser
from sys import stderr, exit


class BashOrg(object):
    """Citations, search over bash.org
    """
    _encoding = 'utf-8'
    
    def _fetch_page(self, url):
        return PyQuery(url=url, opener=lambda url: unicode(urllib2.urlopen(url).read(), self._encoding))

    def _quote_to_text(self, quote_el):
        """Converts quote element to plain text
        """
        fixed_html = quote_el.html().replace('<', '&lt;').replace('>', '&gt;')
        return PyQuery(fixed_html).text()        

    def cite(self, num):
        """Get quote by number
        """
        page = self._fetch_page("http://bash.org/?quote=%d" % num)
        quote = page("p.qt")
        if not quote:
            return None
        return self._quote_to_text(quote)

    # def search(self, text, results = 3):
    #     """Search quotes for given text

    #     :param text: text to search (ascii or unicode)
    #     :param result: number of results to return
    #     :return: 
    #     """
    #     url = "http://bash.org.ru/?%s" % urllib.urlencode({'text': text.encode('windows-1251')})
    #     page = self._fetch_page(url)
    #     # If there is a span with "error" class having "!" in itself
    #     if True in page("span.error").map(lambda i, e: "!" in PyQuery(e).text()):
    #         return []
    #     # Whoa...
    #     matches = [(e.parent()("span").text(),          # votes
    #                 int(e.parent()("a").eq(0).text()),  # quote number
    #                 self._quote_to_text(e))             # quote text
    #                for m in page("#quotes div.q div.vote + div")
    #                for e in (PyQuery(m),)]
    #     return sorted(matches, reverse=True)[0:results]

def main():
    parser = OptionParser(usage="usage: %prog [options] [TEXT]", version="%prog 0.1")
    parser.add_option("-s", "--search", dest="search", action="store_true",
                      help="search for TEXT in quotes", default=False)
    parser.add_option("-n", "--number", dest="num", type="int", metavar="#",
                      help="fetch quote by number")
    (opts, args) = parser.parse_args()
    text = unicode(" ".join(args), 'utf8')
    bo = BashOrg()
    if opts.search:
        results = bo.search(text)
        for votes, num, quote in results:
            line = "#%d [%s]\n%s\n" % (num, votes, quote)
            print line.encode('utf8')
    elif opts.num:
        cite = bo.cite(opts.num)
        if cite:
            print cite.encode('utf8')
    else:
        parser.error("no action specified")

if __name__ == '__main__':
    try:
        main()
    except Exception, e:
        print >>stderr, e
        exit(1)
