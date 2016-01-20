addf = /usr/bin/sed \
	-e 's,~/checkout/Grail/grammars/big_french_drt.pl,../grammars/big_french_drt,g'
addgl = /usr/bin/sed \
        -e 's/chart.pl/grail_light.pl/g'
addglc = /usr/bin/sed \
        -e 's/chart.pl/grail_light_cr.pl/g'
addglnd = /usr/bin/sed \
        -e 's/grail_light.pl/grail_light_nd.pl/g'


all: grail_light.pl grail_light_cr.pl grail_light_nd.pl parser.tcl parser_cr.tcl

chart_lp.pl: chart.pl
	/usr/bin/sed -e 's/%LP//g' chart.pl > chart_lp.pl
chart_cr.pl: chart.pl
	/usr/bin/sed -e 's/%CR//g' chart.pl > chart_cr.pl

grail_light.pl: chart_lp.pl
	$(addf) chart_lp.pl | $(addgl) | tail +2 > grail_light.pl
grail_light_nd.pl: grail_light.pl
	/usr/bin/sed -e 's/%output_proofs(nd)./output_proofs(nd)./g' grail_light.pl | /usr/bin/sed -e 's/output_proofs(chart)./%output_proofs(chart)./g' | $(addglnd)  > grail_light_nd.pl
grail_light_cr.pl: chart_cr.pl
	$(addf) chart_cr.pl | $(addglc) | tail +2 > grail_light_cr.pl

parser.tcl: gui.tcl
	/usr/bin/sed -e 's/#LP //g' gui.tcl > parser.tcl
parser_cr.tcl: gui.tcl
	/usr/bin/sed -e 's/#CR //g' gui.tcl > parser_cr.tcl

