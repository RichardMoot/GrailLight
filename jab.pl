:- dynamic sent/2.

sent(1, A) :-
	prob_parse(
		   [ si('Il', cls-pro:per, il, [np-0.99999666]),
		     si(brilgue,
			v-ver:pres,
			brilgue,
			
			[ dl(0, np, s)-0.97565836,
			  dr(0, dl(0, np, s), np)-0.017163998
			]),
		     si(:, ponct-pun, :, [dr(0, dl(0, s, s), s)-0.9956388]),
		     si(les, det-det:art, les, [dr(0, np, n)-0.9999995]),
		     si(tôves, nc-nom, tôves, [n-0.999998]),
		     si(lubricilleux,
			adj-adj,
			lubricilleux,
			[dl(0, n, n)-0.99999595]),
		     si(se, clr-pro:per, se, [cl_r-0.99999976]),
		     si(gyrent,
			v-ver:pres,
			gyrent,
			[dl(0, cl_r, dl(0, np, s))-0.99954706]),
		     si(en,
			p-prp,
			en,
			
			[ dr(0, dl(0, dl(0, np, s), dl(0, np, s)), dl(0, np, s_ppres))-0.9956442
			]),
		     si(vrillant,
			vpr-ver:ppre,
			vriller,
			
			[ dl(0, np, s_ppres)-0.9565086,
			  dr(0, dl(0, np, s_ppres), np)-0.015979122
			]),
		     si(dans,
			p-prp,
			dans,
			[dr(0, dl(1, s, s), np)-0.9496303, dr(0, pp, np)-0.049595945]),
		     si(le, det-det:art, le, [dr(0, np, n)-1.0]),
		     si(guave, nc-nom, guave, [n-0.9999994]),
		     si('.', ponct-pun, '.', [dl(0, s, txt)-0.999824])
		   ],
		   A).
sent(2, A) :-
	prob_parse(
		   [ si('Enmîmés',
			vpp-ver:pper,
			enmîmés,
			
			[ dl(0, np, s_pass)-0.93101263,
			  dl(0, n, n)-0.020358838,
			  np-0.019982053
			]),
		     si(sont, v-ver:pres, être, [dr(0, dl(0, np, s), np)-0.9961145]),
		     si(les, det-det:art, les, [dr(0, np, n)-0.99999976]),
		     si(gougebosqueux, nc-nom, gougebosqueux, [n-0.9999995]),
		     si(et, cc-kon, et, [dr(0, dl(0, np, np), np)-0.9992424]),
		     si(le, det-det:art, le, [dr(0, np, n)-0.99999845]),
		     si(mômerade, et-nom, mômerade, [n-0.99984646]),
		     si(horsgrave, adj-adj, horsgrave, [dl(0, n, n)-0.998995]),
		     si('.', ponct-pun, '.', [dl(0, s, txt)-0.99969304])
		   ],
		   A).

