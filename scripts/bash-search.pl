#!/usr/bin/perl -wCSDA

use strict;
use IO::All;
use HTML::DOM;
use Text::Iconv;
use Data::Dumper;

my $MAX_QUOTES = 3;

eval 
{
	my $iconv = Text::Iconv->new("utf-8", "windows-1251");
	my $srch = $iconv->convert($ARGV[0]);
	my $tmp_name = "/tmp/yadbot.tmp.html";

	unlink($tmp_name);
	io($tmp_name) < io("http://bash.org.ru/?text=$srch")
		or die "Timeout";

	my $dom = new HTML::DOM;
	$dom->parse_file($tmp_name);

	print "TOP $MAX_QUOTES Bash.Org.Ru results for ".$ARGV[0].":\n";

	my @spans = $dom->getElementsByTagName("span");
	foreach (@spans) {
		print("No results :(\n"), exit if ($_->className =~ /error/ and $_->innerHTML =~ /!$/)
	}

	my @results = ();
	my @divs1 = $dom->getElementById("quotes")->getElementsByTagName("div");
	foreach (@divs1) {
		next unless $_->className eq 'q';
		my @divs2 = $_->getElementsByTagName('div');
		$divs2[2] or next;
		my $text = $divs2[2]->innerHTML . "\n";
		my $num = $_->getElementsByTagName('a')->[0]->innerHTML;
		my $vote = $_->getElementsByTagName('span')->[0]->innerHTML;
		$text =~ s/\<br\>/\n/g;
		push @results, {text => $text, num => $num, vote => $vote };
	}

	my @sorted_res = sort { $b->{vote} <=> $a->{vote} } @results;

	foreach (@sorted_res[0..$MAX_QUOTES-1]) {
		next unless $_;
		print "\n#".$_->{num}." ".$_->{vote}.":\n".$_->{text}."---";
	}
	print "---\n";
};
if ($@)
{
	print $@."\n";
}
