#!/usr/bin/env python2.7

import argparse
import gzip
import json
import StringIO
import urllib2


class ZenRusRates(object):
  """Query USD/EUR exhange rates from a zenrus-alike site."""

  def _make_request(self):
    url = 'http://joyreactor.cc/kursSource'
    resp = urllib2.urlopen(urllib2.Request(url))
    buf = StringIO.StringIO(resp.read())
    text = gzip.GzipFile(fileobj=buf)
    return json.load(text)

  def query_rates(self):
    data = self._make_request()
    if len(data) >= 3:
      return data[0], data[1]
    else:
      return 0, 0


def main():
  parser = argparse.ArgumentParser(description='query USD/EUR to RUR rates')
  args = parser.parse_args()
  btce = ZenRusRates()
  usd, eur = btce.query_rates()
  print usd
  print eur


if __name__ == '__main__':
  main()
