#!/usr/bin/env python2.6

import urllib, urllib2
import json, re
import htmlentitydefs
from optparse import OptionParser
from pyquery import PyQuery
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

    def _make_HTML_request(self, path, params):
        """Performs ordinary request and fetches HTML

        @param url: URL part with no parameters and no leading /
        @param params: request parameters (dict)
        """
        tld = defaultdict(lambda: self._lang, en="com")[self._lang]
        url = 'http://www.google.%s/%s?%s' % (tld, path, urllib.urlencode(params))
        # You get back simpler and smaller html if you pretend to be Lynx
        ua = 'Lynx/2.8.6rel.4 libwww-FM/2.14'
        req = urllib2.Request(url, headers={'Referer': self._referer, 'User-Agent': ua})
        resp = unicode(urllib2.urlopen(req).read(), 'utf-8')
        return resp


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

    def dict(self, word, lang_from, lang_to, results = 5):
        """Lookup word in dictionary

        @param word: word to lookup
        @param lang_from: source language
        @param lang_to: target language
        @param results: number of results to fetch
        @return: [{'pos': part-of-speech, 'tr': list of translations}]

        WARNING this is unofficial Google API
        """
        params = {'callback': 'eval', 'q': word, 'sl': lang_from, 'tl': lang_to}
        url = 'http://www.google.com/dictionary/json?%s' % (urllib.urlencode(params))
        req = urllib2.Request(url, headers={'Referer': self._referer})
        resp = urllib2.urlopen(req).read()
        data = json.loads(re.search(r'eval\(({.*}),.*\)', resp).group(1))
        res = []
        for e in data['primaries'][0]['entries']:
            trans = {'pos': '?', 'tr': []}
            for l in e['labels']:
                if l['title'] == "Part-of-speech": trans['pos'] = l['text']
            for ee in e['entries'][:results]:
                if ee['type'] == 'meaning': trans['tr'].append(ee['terms'][0]['text'])
            res.append(trans)
        return res

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
        resp = self._make_HTML_request('search', {'num': 1, 'ie': 'utf-8', 'oe': 'utf-8', 'q': expr})
        draft = re.match(self._calc_draft_match, resp)
        if draft:
            fine = re.match(self._calc_fine_match, draft.group('match'))
            if fine:
                ans = self._dehtmlize(fine.group('match'))
                return ans
        return None

    def define(self, term, results = 5):
        """Google Define service

        @param term: term to search for
        @param results: number of results to return
        """
        resp = self._make_HTML_request('search', {'ie': 'utf-8', 'oe': 'utf-8', 'q': 'define:%s' % term, 'defl': self._lang})
        body = PyQuery(resp)
        items = body("ul.std li").map(lambda i, e: (PyQuery(PyQuery(e).html().split("<br/>")[0]).text(),
                                                    "=".join((PyQuery(e)("a").attr('href') or "").split('=')[1:]) or None))
        return items[:results]


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
    parser.add_option("-D", "--dict", dest="dict", metavar="LANGPAIR",
                      help="lookup word in dictionary w.r.t. langpair (e.g. en-ru)")
    parser.add_option("-d", "--define", dest="define", action="store_true", default=False,
                      help="use Google Define on the query")
    (opts, args) = parser.parse_args()
    if len(args) == 0:
        parser.error("no search query supplied")
    query = " ".join(args)
    g = Google('http://tweedle-dee.org/', opts.lang)
    if opts.langpair:
        res = g.translate(query, opts.langpair.replace('-', '|'))
        print res.encode('utf8')
    elif opts.dict:
        [lang_from, lang_to] = opts.dict.split('-')
        res = g.dict(query, lang_from, lang_to)
        for trans in res:
            line = "(%s) %s" % (trans['pos'], ', '.join(trans['tr']))
            print line.encode('utf8')
    elif opts.fight:
        res = "%s wins! (%d vs %d)" % g.fight(query, opts.fight) 
        print res
    elif opts.calc:
        ans = g.calc(query)
        if ans:
            print ans.encode('utf8')
    elif opts.define:
        res = g.define(query)
        for definition, url in res:
            line = url and ("- %s\n(%s)" % (definition, url)) or ("- %s" % definition)
            print line.encode('utf8')
    else:
        (count, matches) = g.search(query, opts.results)
        print "Google search for '%s': %d result(s)" % (query, count)
        for url, title, snippet in matches:
            line = "- %s: %s\n(%s)" % (title, snippet, url) 
            print line.encode('utf8')

if __name__ == '__main__':
   #try:
        main()
   #except Exception, e:
   #    print >>stderr, e
   #    exit(1)
