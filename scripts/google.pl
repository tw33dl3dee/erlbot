#!/usr/bin/perl -wCSDA

use strict;
use REST::Google;
use Data::Dumper;
use Text::Iconv;

REST::Google->service('http://ajax.googleapis.com/ajax/services/search/web');
REST::Google->http_referer('http://tweedle-dee.org');

eval
{
	my $iconv = Text::Iconv->new("utf-8", "utf-8");
	my $srch = $iconv->convert($ARGV[0]);
	
	my $res = REST::Google->new('q' => $srch);
	if ($res->responseStatus != 200)
	{				
		print "Google down o_0\n";
	}
	else
	{
		my $data = $res->responseData;
		print "$ARGV[0]: ".($data->{cursor}->{estimatedResultCount} or 0)." results.\n";
		foreach (@{$data->{results}})
		{
			$_->{content} =~ s/<(([^ >]|\n)*)>//g,
			print $_->{content}." (".$_->{url}.")\n";
		}
	}
};
if ($@)
{
	print $@."\n";
}
