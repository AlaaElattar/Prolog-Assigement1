:-[students_courses].

%Task1

add_tail([],X,[X]).
add_tail([H|T],X,[H|L]):-add_tail(T,X,L).

inList(X,[X|T]).
inList(X,[H|T]):-
	inList(X,T).	
	
students(X,Y,R):-
	student(Z,X,Y1),
	\+ inList([Z,Y1],R),
	add_tail(R,[Z,Y1],L1),
	students(X,Y,L1).

students(_,X,X).

studentsInCourse(X,Y):-
	students(X,Y,[]).

%---------------------------------------------------------------

%Task2

counter(X,R,OLD,F):-
	student(Z,X,Y1),
	\+ inList([Z,Y1],R),
	NewOlD is OLD+1,
	add_tail(R,[Z,Y1],L1),
	counter(X,L1,NewOlD,F).

counter(_,_,OLD,OLD).	

numStudents(X,F):-
	OLD is 0,
	counter(X,[],OLD,F).


%---------------------------------------------------------------

%Task3

maxMix(X,Y,New):-
     X>Y,New is X;
     Y>X,New is Y.
 
 
maxIDStudent(X,R,Max,Y):-
	student(X,Z,Y1),
	\+ inList([Z,Y1],R),
	add_tail(R,[Z,Y1],L1),
	maxMix(Y1,Max,Newmax),
	NewMax is Newmax,
	maxIDStudent(X,L1,NewMax,Y).
 
maxIDStudent(_,_,NewMax,NewMax).
 
maxStudentGrade(X,Y):-
	Max is 0,
	maxIDStudent(X,[],Max,Y).


%---------------------------------------------------------------
	
%Task4

swap(A,B):-
    A=:=0, B = 'zero';
    A=:=1, B = 'one';
    A=:=2, B = 'two';
    A=:=3, B = 'three';
    A=:=4, B = 'four';
    A=:=5, B = 'five';
    A=:=6, B = 'six';
    A=:=7, B = 'seven';
    A=:=8, B = 'eight';
    A=:=9, B = 'nine'.


reverse([], Y, R) :-
    R = Y.
reverse([H|T] , Y, R) :-
    reverse(T, [H|Y], R).

convert(0,R,R).
convert(X,R,Y):-
    NewX is X mod 10,
    NewX2 is X//10,
    swap(NewX,W),
	NewH = W,
    add_tail(R,NewH,L1),
	\+ inList([NewH],R),
    convert(NewX2,L1,Y).
 
 
gradeInDigits(ID,COURSE,R,DIGIT):-
    student(ID,COURSE,G1),
    convert(G1,R,RDIGIT),
    reverse(RDIGIT,[],DIGIT).
 
 
gradeInWords(ID, COURSE, DIGIT):-
    gradeInDigits(ID,COURSE,[],DIGIT).

%---------------------------------------------------------------
	
%Task5

memberList([H|_],H).
memberList([_|T],K):-
			memberList(T,K).
			
deletMember([H|T],T).

getgrad(Y,New,GR):-
		student(Y,New,GR).

passsss1(Y,List1,C,R1,Courses):-
		memberList(List1,Mem),
		deletMember(List1,List2),
		NewList =List2,
		Newmem = Mem,
		(student(Y,Newmem,G),G>=50->Newc is C+1,passsss1(Y,NewList,Newc,R1,Courses);
		add_tail(R1,Newmem,L2),passsss1(Y,NewList,C,L2,Courses)).
				
passsss1(_,[],C,X,V):-
			(C>0->V=X).

preList(X,R,List):-
     prerequisite(Y,X),
	 add_tail(R,Y,L1),
	 \+ inList([Y],R),
	 preList(Y,L1,List).	 
preList(_,X,X).

remainingCourses(Y,X,Courses):-
          preList(X,[],List),
		  reverse(List,[],RList),
		  passsss1(Y,RList,0,[],Courses).
