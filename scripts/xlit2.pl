#!/usr/bin/perl

use strict;
use Lingua::RU::Jcuken qw(jcu2qwe qwe2jcu);
use Lingua::DetectCyrillic;
use Text::Iconv;

#binmode STDIN, ":utf8";
#binmode STDOUT, ":utf8";

my $count = @ARGV ? $ARGV[0] : -1;

my $d = Lingua::DetectCyrillic->new(MaxTokens => 15);

while ($count-- && defined($_ = <STDIN>)) {
	exit if /[:=]-[DdPp]/;
	exit if /0?:-?[\)\(]/;
	exit if /[hf]tt?p:/;

	my $s = qwe2jcu($_, 'utf-8');
	my $i1 = Text::Iconv->new("utf-8", "koi8-r")->convert($s);
	my $i2 = Text::Iconv->new("utf-8", "windows-1251")->convert($s);
	my ($enc1, $lang1, $charcount1, $alg1) = $d->Detect($i1);
	my ($enc2, $lang2, $charcount2, $alg2) = $d->Detect($i2);

	#print $_;
	#print "$enc1, $lang1, $charcount1 (vs ".length($i1)."), $alg1\n";
	#print "$enc2, $lang2, $charcount2 (vs ".length($i2)."), $alg2\n";

	#utf8::downgrade $s;

	print $s if 
		$lang1 eq 'Rus' && $lang2 eq 'Rus' && 
		$enc1 eq 'koi8-r' && $enc2 eq 'windows-1251' 
		&& ($charcount1 > 0.5*length($i1)) && ($charcount2 > 0.5*length($i2));
}
