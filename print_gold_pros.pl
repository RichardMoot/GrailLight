proof_file('../TLGbank/chart_proofs/aa1_proofs.pl', 'aa1_gold_pros.txt').
proof_file('../TLGbank/chart_proofs/ap1_proofs.pl', 'ap1_gold_pros.txt').
proof_file('../TLGbank/chart_proofs/aq2_proofs.pl', 'aq2_gold_pros.txt').
proof_file('../TLGbank/chart_proofs/as2_proofs.pl', 'as2_gold_pros.txt').
proof_file('../TLGbank/chart_proofs/at_proofs.pl', 'at_gold_pros.txt').
proof_file('../TLGbank/chart_proofs/300_proofs.pl', '300_gold_pros.txt').
proof_file('../TLGbank/chart_proofs/1000_proofs.pl', '1000_gold_pros.txt').
proof_file('../TLGbank/chart_proofs/8000_proofs.pl', '8000_gold_pros.txt').
proof_file('../TLGbank/chart_proofs/annodis_proofs.pl', 'annodis_gold_pros.txt').
proof_file('../TLGbank/chart_proofs/frwiki1_proofs.pl', 'frwiki1_gold_pros.txt').
proof_file('../TLGbank/chart_proofs/frwiki2_proofs.pl', 'frwiki2_gold_pros.txt').

portray(f(List)) :-
	!,
	print_pros_list(List).
portray(p(I,A,B)) :-
	!,
	format('(~@ ~p ~p)', [write_index(I),A,B]).

write_index(0) :-
	write('ZERO').
write_index(1) :-
	write('ONE').

print_pros_list([P|Ps]) :-
	format('(X ~@', [print_pros(P)]),
	print_pros_list1(Ps).
print_pros_list1([P|Ps]) :-
	format(' ~@', [print_pros(P)]),
	print_pros_list1(Ps).
print_pros_list1([]) :-
	write(')').


print_pros(f(List)) :-
	!,
	print_pros_list(List).
print_pros(p(I,A,B)) :-
	!,
	format('(~@ ~@ ~@)', [write_index(I),print_pros(A),print_pros(B)]).
print_pros(W) :-
	format('(w ~p)', [W]).

computed_pros(OutFile) :-
        format('~NCreating ~w...', [OutFile]),
	flush_output,
        print_all_pros('proofs.pl', OutFile),
        format('done', []),
	flush_output,
        fail.
computed_pros(_).
	

all_gold_pros :-
        proof_file(InFile, OutFile),
        format('~NCreating ~w...', [OutFile]),
	flush_output,
        print_all_pros(InFile, OutFile),
        format('done', []),
	flush_output,
        fail.
all_gold_pros.


print_all_pros(InFile, OutFile) :-
	abolish(proof/2),
	compile(InFile),
	tell(OutFile),
	print_all_pros1,
	told.

print_all_pros1 :-
	proof(_, rule(_,Pros,_,_)),
	format('~@~n', [print_pros(Pros)]),
	fail.
print_all_pros1.
