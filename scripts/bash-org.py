#!/usr/bin/env python2.6

import urllib, urllib2
from pyquery import PyQuery
import re
from optparse import OptionParser
from sys import stderr, exit


class BashOrg(object):
    """Citations, search over bash.org
    """
    _encoding = 'iso-8859-1'

    def _fetch_page(self, url):
        return PyQuery(url=url, opener=lambda url: unicode(urllib2.urlopen(url).read(), self._encoding))

    def _quote_to_text(self, quote_el):
        """Converts quote element to plain text
        """
        fixed_html = quote_el.html().replace('<br/>', '').replace('<', '&lt;').replace('>', '&gt;')
        return PyQuery(fixed_html).text()

    def cite(self, num):
        """Get quote by number
        """
        page = self._fetch_page("http://bash.org/?%s" % urllib.urlencode({'quote': num}))
        quote = page("p.qt")
        if not quote:
            return None
        return self._quote_to_text(quote)

    def search(self, text, results = 5):
        """Search quotes for given text

        :param text: text to search (ascii or unicode)
        :param result: number of results to return
        :return:
        """
        url = "http://bash.org/?%s" % urllib.urlencode({'search': text.encode('utf-8'),
                                                        'sort': 0,
                                                        'show': results})
        page = self._fetch_page(url)
        # Whoa...
        matches = [(re.search(r'\((-?[0-9]+)\)', e.prev().text()).group(1),  # votes
                    int(e.prev()("a b").text()[1:]),                         # quote number
                    self._quote_to_text(e))                                  # quote text
                   for m in page("p.qt")
                   for e in (PyQuery(m),)]
        return matches[:results]

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
