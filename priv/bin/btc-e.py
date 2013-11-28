#!/usr/bin/env python2.7

import argparse
import json
import urllib2


class BtcE(object):
  """Btc-e.com API"""

  def _make_request(self, rate, command):
    url = 'https://btc-e.com/api/2/%s/%s' % (rate, command)
    resp = urllib2.urlopen(urllib2.Request(url))
    return json.load(resp)

  def query_rate(self, rate, fields):
    data = self._make_request(rate, 'ticker')
    if 'ticker' not in data:
      return []
    return [data['ticker'].get(k, '') for k in fields]


def main():
  parser = argparse.ArgumentParser(description='query btc-e.com rates')
  parser.add_argument('-r', '--rate', dest='rate', default='btc_usd',
                    help='rate to query')
  parser.add_argument('-f', '--fields', dest='fields', default=['buy', 'sell'],
                      nargs='+', help='fields to query')
  args = parser.parse_args()
  btce = BtcE()
  values = btce.query_rate(args.rate, args.fields)
  for value in values:
    print value


if __name__ == '__main__':
  main()
