#!/usr/bin/env python2.6

import urllib, urllib2
import json, re
from optparse import OptionParser
from sys import stderr, exit


class Wiki(object):
    """Performs lookups and searches over Wikipedia using it's WikiMedia API
    """

    def __init__(self, lang):
        """        
        @param lang: language code 
        @type  lang: str
        """
        self._lang = lang

    # List of filters that get applied to Wikipedia text. Order IS dependent
    _page_filters = [# Strip templates/infoboxes
                     (r"(?s){{.*?}}",                 r""),
                     # Replace wiki-links with their captions
                     (r"(?s)\[\[[^\]]+?\|(.+?)\]\]",  r"\1"),
                     # Replace external links with their captions
                     (r"(?s)\[(\S+://\S+) (.+?)\]",   r"\2 (\1)"),
                     # Replace wiki-pages with their names
                     (r"(?s)\[\[(.+?)\]\]",           r"\1"),
                     # Replace non-empty <ref> and other tags with []
                     (r"(?s)</.+?>",                  r"]"),
                     (r"(?s)<.+?>",                   r"["),
                     # Strip pederastic Wiki markup
                     (r"(?m)\'\'\'|\'\'|__|^:\s",     r""),
                     # Strip off empty braces left after all this fucking
                     (r"\[\]|\(\)",                   r""),
                     # Strip trailing newline
                     (r"\n$",                         r""),
                     # Strip blank lines and turn paragraphs into itemization
                     (r"(?m)^\s*",                    r"- "),
                    ]

    def _dewikify(self, text):
        """Removes wiki formatting from text
        
        @param text: wiki-formatted text
        @type  text: str
        @return: plain text
        """
        for pat, repl in self._page_filters:
            text = re.sub(pat, repl, text)
        return text

    def _make_request(self, **params):
        """Performs WikiMedia API request
        
        @param params: dict of parameters
        @return: parsed reply object (str, list or dict)
        """
        params['format'] = 'json'
        url = 'http://%s.wikipedia.org/w/api.php?%s' % (self._lang, urllib.urlencode(params))
        resp = urllib2.urlopen(url)
        return json.load(resp)

    def get_page(self, topic):
        """Fetches page (first section only) from Wikipedia as plain text
        
        @param topic: topic name
        @type  topic: str
        @return: tuple (URL, page title, page text)
        """
        reply = self._make_request(action='query', redirects=1, titles=topic,
                                   prop='info|revisions',
                                    inprop='url',
                                    rvprop='content', rvlimit=1, rvsection=0
                                  )
        # `pages' contains dict with one element (requested page)
        pages = reply['query']['pages']
        if '-1' in pages:
            return None
        page = pages.values()[0]
        page_html = page['revisions'][0]['*']  # Fucking monkeys
        return (page['fullurl'], page['title'], self._dewikify(page_html))

    def search(self, text):
        """Performs search over Wikipedia pages (both titles and content)
        
        @param text: text to search
        @type  text: str
        @return: list of tuples (URL, title, snippet)
        """
        short_repl = self._make_request(action='query', list='search', 
                                          srredirects=1, srprop='snippet', srsearch=text
                                       )
        long_repl  = self._make_request(action='query', generator='search',
                                          gsrredirects=1, gsrsearch=text,
                                        prop='info',
                                          inprop='url'
                                       )
        pages = dict((page['title'], page) for page in long_repl['query']['pages'].values())
        shorts = dict((s['title'], s) for s in short_repl['query']['search'])
        matches = [(page['fullurl'], title, self._dewikify(shorts[title]['snippet']))
                   for (title, page) in pages.iteritems() if title in shorts]
        return matches


def main():
    parser = OptionParser(usage="usage: %prog [options] topic", version="%prog 0.1")
    parser.add_option("-l", "--lang", dest="lang", help="language", metavar="LANG", default="en")
    parser.add_option("-s", "--search", dest="search", action="store_true", help="perform search", default=False)
    (opts, args) = parser.parse_args()
    if len(args) == 0:
        parser.error("no topic supplied")
    topic = " ".join(args)
    w = Wiki(opts.lang)
    if opts.search:
        matches = w.search(topic)
        for url, title, snippet in matches:
            line = "- %s: %s\n(%s)" % (title, snippet, url) 
            print line.encode('utf8')
    else:
        page = w.get_page(topic)
        if page is not None:
            url, title, text = page
            print "Wikipedia topic: %s" % title.encode('utf8')
            print text.encode('utf8')
            print "... %s" % url

if __name__ == '__main__':
   try:
        main()
   except Exception, e:
       print >>stderr, e
       exit(1)
