#!/usr/bin/perl -w

use REST::Google::Translate;

REST::Google::Translate->http_referer('http://tweedle-dee.org');

my $text = $ARGV[0];
my $lang = $ARGV[1];

my $res = REST::Google::Translate->new(
	'q' => $text,
	langpair => $lang
	);

die "response status failure" if $res->responseStatus != 200;

my $translated = $res->responseData->translatedText;

printf "Translation: %s\n", $translated;
