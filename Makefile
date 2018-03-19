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

news: news1.pl news2.pl news3.pl news4.pl news5.pl news6.pl news7.pl news8.pl news9.pl news10.pl news11.pl news12.pl news13.pl news14.pl news15.pl news16.pl news17.pl news18.pl news19.pl news20.pl news21.pl news22.pl news23.pl news24.pl news25.pl news26.pl news27.pl news28.pl news29.pl news30.pl news31.pl news32.pl news33.pl news37.pl news38.pl news39.pl

news1.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 7 to 11 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news1.pl
news2.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 12 to 22 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news1.pl
news3.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl to 6 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news3.pl
news4.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 39 to 44 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news4.pl
news5.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 23 to 38 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news5.pl
news6.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 54 to 68 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news6.pl
news7.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 69 to 82 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news7.pl
news8.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 83 to 95 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news8.pl
news9.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 96 to 100 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news9.pl
news10.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 101 to 108 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news10.pl
news11.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 109 to 117 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news11.pl
news12.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 118 to 133 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news12.pl
news13.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 134 to 149 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news13.pl
news14.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 150 to 159 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news14.pl
news15.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 160 to 174 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news15.pl
news16.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 175 to 188 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news16.pl
news17.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 189 to 210 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news17.pl
news18.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 211 to 219 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news18.pl
news19.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 220 to 231 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news19.pl
news20.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 232 to 241 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news20.pl
news21.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 242 to 260 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news21.pl
news22.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 261 to 277 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news22.pl
news23.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 278 to 299 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news23.pl
news24.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 300 to 308 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news24.pl
news25.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 309 to 321 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news25.pl
news26.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 322 to 338 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news26.pl
news27.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 344 to 349 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news27.pl
news28.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 339 to 343 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news28.pl
news29.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 350 to 374 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news29.pl
news30.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 375 to 388 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news30.pl
news31.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 389 to 405 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news31.pl
news32.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 419 to 436 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news32.pl
news33.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 439 to 468 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news33.pl
news37.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 469 to 483 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news37.pl
news38.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 484 to 492 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news38.pl
news39.pl: annodis.pl grail_light_cr.pl
	rm semantics.pl
	./grail_light_cr.pl from 493 to 529 annodis.pl	
	egrep 'semantics.+ reduced' semantics.pl > news39.pl

debat_light.pos: debat_light_tok.txt
	pos --model /Users/moot/checkout/models/ext_french_pos_tt --input debat_light_tok.txt > debat_light.pos

super10.txt: debat_light.pos
	 msuper -input debat_light.pos --ofmt "%w|%p|%S \n" --model /Users/moot/checkout/models/french_tt/ --beta 0.1 > super10.txt
super50.txt: debat_light.pos
	 msuper -input debat_light.pos --ofmt "%w|%p|%S \n" --model /Users/moot/checkout/models/french_tt/ --beta 0.05 > super50.txt
super100.txt: debat_light.pos
	 msuper -input debat_light.pos --ofmt "%w|%p|%S \n" --model /Users/moot/checkout/models/french_tt/ --beta 0.01 > super100.txt

super10_nolem.pl: super10.txt
	./supertag2pl super10.txt > super10_nolem.pl
super50_nolem.pl: super50.txt
	./supertag2pl super50.txt > super50_nolem.pl
super100_nolem.pl: super100.txt
	./supertag2pl super100.txt > super100_nolem.pl

super10.pl: super10_nolem.pl
	./lefff.pl super10_nolem.pl
super50.pl: super50_nolem.pl
	./lefff.pl super50_nolem.pl
super100.pl: super100_nolem.pl
	./lefff.pl super100_nolem.pl


