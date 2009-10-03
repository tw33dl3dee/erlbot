#!/usr/bin/perl -wCSDA

use strict;
use IO::All;
use HTML::DOM;
use Text::Iconv;
use Data::Dumper;

eval 
{
	my $quotenum = $ARGV[0];
	my $tmp_name = "/tmp/yadbot.tmp.html";

	unlink($tmp_name);
	io($tmp_name) < io("http://bash.org.ru/quote/$quotenum")
		or die "Timeout";

	my $dom = new HTML::DOM; # charset => "utf-8";
	$dom->parse_file($tmp_name);

	print("No such quote, idiot.\n"), exit if $dom->getElementById("bar");

	my @divs1 = $dom->getElementById("quotes")->getElementsByTagName("div");
	my $cont = $divs1[3]->innerHTML;
	$cont =~ s/\<br\>/\n/g;

	print "Bash.Org.Ru quote #$quotenum:\n";
	print $cont."\n";
	#print $_."\n" foreach split /\n/, $cont;
};
if ($@)
{
	print $@."\n";
}
