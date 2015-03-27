addf = /usr/bin/sed \
	-e 's,~/checkout/Grail/grammars/big_french_drt.pl,../grammars/big_french_drt,g'
addgl = /usr/bin/sed \
        -e 's/chart.pl/grail_light.pl/g'
addglc = /usr/bin/sed \
        -e 's/chart.pl/grail_light_cr.pl/g'


all: grail_light.pl grail_light_cr.pl

chart_lp.pl: chart.pl
	/usr/bin/sed -e 's/%LP//g' chart.pl > chart_lp.pl
chart_cr.pl: chart.pl
	/usr/bin/sed -e 's/%CR//g' chart.pl > chart_cr.pl

grail_light.pl: chart_lp.pl
	$(addf) chart_lp.pl | $(addgl) | tail +2 > grail_light.pl
grail_light_cr.pl: chart_cr.pl
	$(addf) chart_cr.pl | $(addglc) | tail +2 > grail_light_cr.pl
