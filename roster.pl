menu(X) :-
    write('\tClass roster management system'), nl,
    write('\t=============================='), nl,
    write('\t   MENU'), nl,
    write('\t=============================='), nl,
    write('\t0. Reset roster'), nl,
    write('\t1. Load roster from file'), nl,
    write('\t2. Store roster to file'), nl,
    write('\t3. Display roster sorted by ID'), nl,
    write('\t4. Add student to roster'), nl,
    write('\t5. Remove student from roster'), nl,
    write('\t6. Exit'), nl,
    write('\tEnter your choice (followed by a \'.\'): '),
    read(Sel),
    process(Sel, X).

process(0, X) :-
    nl,
    write('Roster Reset (now empty).'), nl,
    menu([]).

process(1, X) :-
    write('Enter a filename to load roster from : '),
    read(File),
    nl, see(File),
    read(F),
    seen,
    menu(F).


process(2, X) :-
    write('Enter name of file to store roster to: '),
    read(File),
    nl, tell(File),
    write(X),
    write('.'),
    told,
    menu(X).

process(3, X) :-
    nl,
    write('\tDisplay roster, sorted by ID: '),nl,
    so_rt(X, N), %N
    displayr(1, N), %N
    nl, menu(X).

    
process(4, X) :-
    nl,
    read_student_info([A, B, C]),
    nl, nl, menu([[A,B,C] | X]).

process(5, X) :-
    write('Remove student from roster.'), nl,
    write('Enter student name or ID'),
    read(R),
    remove(R, X, N),
    menu(N).


process(6, _) :-
    nl,
    write('Good-bye!'), nl, !.


process(_, X) :-
    nl,
    write('Invalid choice'), nl,
    menu(X).


read_student_info([A, B, C]) :-
  write('\tStudent ID: '),
  read(A),
  write('\tStudent Name: '),
  read(B),
  write('\tStudent Grade: '),
  read(C).


displayr(I, []) :- write('Roster is empty'), nl.
    displayr(I, [V]) :- display_format(I, V).
    displayr(I, [X|G]) :-
    display_format(C,X),
    P is I+1,
    displayr(P, G).

display_format(I, [A,B,C]) :-
    write('No.'),
    write(I),
    write(': ID = '),
    write(A),
    write(', Name='),
    format('~s', [B]),
    write(', Grade='),
    write(C), nl.
     
remove(_, [], []). 
remove(R, [[R|T]|U], U). 
remove(R, [[_,R|T]|U], U). 
remove(R, [Y|X], [Y|L]) :- remove(R, X, U).

takeout(Item, [Item|L], L).
takeout(Item, [X|L], [X|L1]) :- takeout(Item, L, L1).

perm([], []).
perm([X|Y], Z) :- perm(Y, W), takeout(X, Z, W).

is_sorted([]).
is_sorted([_]).
is_sorted([ [A|AT], [U|UT]|T]) :- A < U, is_sorted([ [U|UT]|T]).
      
so_rt(Y, NL) :-
    perm(Y, NL),
    is_sorted(NL).
 




    
