#!/usr/bin/env python2.6

import urllib, urllib2
import json, re
import htmlentitydefs
from optparse import OptionParser
from sys import stderr, exit
from collections import defaultdict


class Google(object):
    """Google Search, Calculator and Translate services
    """
    def __init__(self, referer, lang = "en"):
        """Create new Services instance
        
        @param referer: referer URL to use
        @param lang: language to search in
        """
        self._referer = referer
        self._lang = lang

    def _make_REST_request(self, service, method, **params):
        """Performs request to Google REST services
        
        @param params: dict of parameters
        @return: parsed reply object (str, list or dict)
        """
        params['v'] = 1.0
        url = 'http://ajax.googleapis.com/ajax/services/%s/%s?%s' % (service, method, urllib.urlencode(params))
        req = urllib2.Request(url, headers={'Referer': self._referer})
        resp = urllib2.urlopen(req)
        return json.load(resp)

    # List of filters to apply to google results HTML
    _postproc = [# <sup> raises into power
                 (r'<sup>',    '^'),
                 # Strip all other tags (<b>, etc)
                 (r'<[^>]*>',  '')]

    def _dehtmlize(self, text):
        """Converts HTML to text, stripping unneded tags and replacing entities
        """
        for pat, repl in self._postproc:
            text = re.sub(pat, repl, text)
        for entity, cp in htmlentitydefs.name2codepoint.iteritems():
            text = text.replace('&%s;' % entity, unichr(cp))
        return text

    def search(self, text, results = 5):
        """Performs Google Search
        
        @param text: text to search
        @param results: number of results to return
        @return: (total matches, [(URL, title, snippet)])
        """
        repl = self._make_REST_request('search', 'web',
                                       q=text,
                                       rsz="large" if (results > 4) else "small",
                                       hl=self._lang)
        matches = [(urllib.unquote(res['url']), res['titleNoFormatting'], self._dehtmlize(res['content']))
                   for res in repl['responseData']['results'][0:results]]
        match_count = len(matches) and int(repl['responseData']['cursor']['estimatedResultCount'])
        return (match_count, matches)

    def fight(self, *words):
        """Google Fight between specified words (compares popularity)
        
        @param words: sequence of words to compare
        @return: (winner (one of words), winner's match count, looser's (worst) match count)
        """
        counts = [self.search(word, 1)[0] for word in words]
        winpts, losepts = max(counts), min(counts)
        winner = words[counts.index(winpts)]
        return (winner, winpts, losepts)

    def translate(self, text, langpair):
        """Translate text from one language to another
        
        @param text: text to translate
        @param langpair: language pair (e.g. 'en|ru')
        @return: translated text
        """
        repl = self._make_REST_request('language', 'translate',
                                       q=text, langpair=langpair)
        return repl['responseData']['translatedText']

    # Find what seems to be Calculator result
    _calc_draft_match = r'(?s).*calc_img\.gif(?P<match>.*)</h2>'
    # Accurately rip out the answer
    _calc_fine_match  = r'.*<b>(?P<match>.*)</b>'

    def calc(self, expr):
        """Google Calculator service
        
        @param expr: expression to calculate
        @type  expr: str
        @return: value (str) or None
        """
        tld = defaultdict(lambda: self._lang, en="com")[self._lang]
        params = urllib.urlencode({'num': 1, 'ie': 'utf-8', 'oe': 'utf-8', 'q': expr})
        url = 'http://www.google.%s/search?%s' % (tld, params)
        # You get back simpler and smaller html if you pretend to be Lynx
        ua = 'Lynx/2.8.6rel.4 libwww-FM/2.14'
        req = urllib2.Request(url, headers={'Referer': self._referer, 'User-Agent': ua})
        resp = unicode(urllib2.urlopen(req).read(), 'utf-8')
        draft = re.match(self._calc_draft_match, resp)
        if draft:
            fine = re.match(self._calc_fine_match, draft.group('match'))
            if fine:
                ans = self._dehtmlize(fine.group('match'))
                return ans
        return None


def main():
    parser = OptionParser(usage="usage: %prog [options] query", version="%prog 0.1")
    parser.add_option("-l", "--lang", dest="lang", default="en",
                      help="language to search in", metavar="LANG")
    parser.add_option("-n", "--results", dest="results", type="int",
                      help="number of results to display", metavar="COUNT", default=5)
    parser.add_option("-f", "--fight", dest="fight", metavar="OPPONENT",
                      help="compare by popularity with OPPONENT")
    parser.add_option("-c", "--calc", dest="calc", action="store_true", default=False,
                      help="evaluate query using Google Calculator")
    parser.add_option("-t", "--translate", dest="langpair", metavar="LANGPAIR",
                      help="translate query from one language to another (e.g. en-ru)")
    (opts, args) = parser.parse_args()
    if len(args) == 0:
        parser.error("no search query supplied")
    query = " ".join(args)
    g = Google('http://tweedle-dee.org/', opts.lang)
    if opts.langpair:
        res = g.translate(query, opts.langpair.replace('-', '|'))
        print res.encode('utf8')
    elif opts.fight:
        res = "%s wins! (%d vs %d)" % g.fight(query, opts.fight) 
        print res
    elif opts.calc:
        ans = g.calc(query)
        if ans:
            print ans.encode('utf8')
    else:
        (count, matches) = g.search(query, opts.results)
        print "Google search for '%s': %d result(s)" % (query, count)
        for url, title, snippet in matches:
            line = "- %s: %s\n(%s)" % (title, snippet, url) 
            print line.encode('utf8')

if __name__ == '__main__':
   try:
        main()
   except Exception, e:
       print >>stderr, e
       exit(1)
