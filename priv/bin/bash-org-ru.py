#!/usr/bin/env python2.7

import urllib, urllib2
from pyquery import PyQuery
import re
from optparse import OptionParser
from sys import stderr, exit


class BashOrgRu(object):
    """Citations, search over bash.org.ru
    """
    _encoding = 'windows-1251'
    _ua = 'Mozilla/5.0'
    
    def _fetch_page(self, url):
        headers = {'User-Agent': self._ua}
        return PyQuery(url=url, opener=lambda url: unicode(
                urllib2.urlopen(urllib2.Request(url, None, headers)).read(),
                self._encoding))

    def _quote_to_text(self, quote_el):
        """Converts quote element to plain text
        """
        fixed_html = (quote_el.html()
                      .replace('<br/>', '\n')
                      .replace('<', '&lt;')
                      .replace('>', '&gt;'))
        return PyQuery(fixed_html).text()

    def cite(self, num):
        """Get quote by number

        :param num: quote number
        :return: str
        """
        page = self._fetch_page("http://bash.im/quote/%d" % num)
        if page("#bar"):
            return None
        quote = page(".quote .text").eq(0)
        return self._quote_to_text(quote)

    def search(self, text, results = 3):
        """Search quotes for given text

        :param text: text to search (ascii or unicode)
        :param result: number of results to return
        :return: list((number, votes, text))
        """
        url = "http://bash.im/?%s" % urllib.urlencode(
            {'text': text.encode('windows-1251')})
        page = self._fetch_page(url)
        if not page(".search-results"):
            return []
        # Whoa...
        matches = [
            (int(e('.actions .id').eq(0).text().lstrip('#')),  # quote number
             int(e('.actions .rating').eq(0).text()),          # votes
             self._quote_to_text(e('.text').eq(0)))            # quote text
            for m in page('.quote') for e in (PyQuery(m),) if e('.text')]
        return sorted(matches, reverse=True)[0:results]

def main():
    parser = OptionParser(usage="usage: %prog [options] [TEXT]",
                          version="%prog 0.2")
    parser.add_option("-s", "--search", dest="search", action="store_true",
                      help="search for TEXT in quotes", default=False)
    parser.add_option("-n", "--number", dest="num", type="int", metavar="#",
                      help="fetch quote by number")
    (opts, args) = parser.parse_args()
    text = unicode(" ".join(args), 'utf8')
    bor = BashOrgRu()
    if opts.search:
        results = bor.search(text)
        for num, votes, quote in results:
            line = "#%d [%d]\n%s\n" % (num, votes, quote)
            print line.encode('utf8')
    elif opts.num:
        cite = bor.cite(opts.num)
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
