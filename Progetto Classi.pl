:- dynamic(class/3).
:- dynamic(class_field/4).
:- dynamic(instance_field/4).
:- dynamic(inst/2).

getFirst([],[]) :- !.
getFirst([N],N) :- !.
getFirst([N | Ns], N) :- !.

def_class(Name, Parents) :-
    (\+ class(Name, _, _)),
    write(1),
    atom(Name),
    !,
    is_list(Parents),
    assert(class(Name, Parents, [])),
    is_class(Name).


def_class(Name, Parents, Parts) :-
    (\+ class(Name, _, _)),
    write(10),
    atom(Name),
    write(11),
    !,
    is_list(Parents),
    write(12),
    is_list(Parts),  
    write(13), 
    maplist(check_part(Name), Parts),
    write(14),
    assert(class(Name, Parents, Parts)).

%%%dove cazzo la metto sta merda!!!!!
is_class(Name) :-
    atom(Name),
    class(Name, _, _),
    findall([Name, Parents, Parts], class(Name, Parents, Parts), Class), 
    length(Parents, N),
    N  \= 0,
    write(N),
    nth0(0, Class, [Name, Parents, Parts]),   
    maplist(is_class, Parents),
    !.

is_class(Name) :-
    atom(Name),
    class(Name, _, _),
    findall([Name, Parents, Parts], class(Name, Parents, Parts), Class),    
    length(Parents, N),
    N =@= 0,
    write(N),
    maplist(is_class, Parents),
    !.
/*
field(Field_Name, Field_Value) :-
    atom(Field_Name),
    !.

field(Field_Name, Field_Value, Field_Type) :-
    atom(Field_Name),
    atom(Field_Type),
    !.

method(Method_Name, Args, Body) :-
    is_list(Args),
    atom(Method_Name),
    !.
*/
/*check_part(field(Field_Name, Field_Value)) :- */


check_part(Class_Name, field(Field_Name, Field_Value)) :-
   /*is_class(Class_Name),*/
   atom(Field_Name),
   write(15),
   (\+ class_field(Class_Name, Field_Name, _, _)),   
   !,
   assert(class_field(Class_Name, Field_Name, Field_Value, string)).


check_part(Class_Name, field(Field_Name, Field_Value, Field_Type)) :-
    /*is_class(Class_Name),*/
    atom(Field_Name),
    atom(Field_Type),
    write(20),
    (\+ class_field(Class_Name, Field_Name, _, _)), 
    !,
    assert(class_field(Class_Name, Field_Name, Field_Value, Field_Type)).


check_part(Class_Name, method(Method_Name, Args, Body)) :-
    /*is_class(Class_Name),*/
    atom(Method_Name),
    is_list(Args),
    assert(method(Method_Name, Args, Body)).


/*make(Instance_Name, Class_Name, Instance_Fields) :-
    atom(Instance_Name),
    %%is_class(Class_Name),
    copy_attr(Instance_Name, Instance_Fields, Class_Name),
    write(Instance_Fields),
    findall([Class_Name, Parents, Parts], class(Class_Name, Parents, Parts), Classes),
    getFirst(Classes,[Name, Parents, Parts]),  
    maplist(copy_attr(Instance_Name,Instance_Fields),Parents),
    assert(inst(Instance_Name, Class_Name)),
    !.*/

make(Instance_Name, Class_Name, Instance_Fields) :-
    make(Instance_Name,Class_Name),
    findall([Instance_Name, N, V, T], instance_field(Instance_Name, N, V, T), Fields),
    sort(Fields, Sorted_Fields),
    sort(Instance_Fields, Sorted_Instance_Fields),
    modify_fields(Instance_Name, Sorted_Instance_Fields, Sorted_Fields),
    write(Fields).


make(Instance_Name, Class_Name) :-
    atom(Instance_Name),  
    (\+ inst(Instance_Name,_)),
    %%is_class(Class_Name),  
    copy_attr(Instance_Name, Class_Name),  
    findall([Class_Name, Parents, Parts], class(Class_Name, Parents, Parts), Classes),
    getFirst(Classes,[Name, Parents, Parts]),  
    write("Parents: " + Parents),
    %%maplist(copy_attr(Instance_Name),Parents), 
    something(Instance_Name,Parents),
    assert(inst(Instance_Name, Class_Name)),   
    !.


something(Instance_Name, []) :-
    !.
something(Instance_Name, [P | Ps]) :-
    copy_attr(Instance_Name, P),   
    findall([P, Parents, Parts], class(P, Parents, Parts), Classes),
    getFirst(Classes,[Name, Parents, Parts]),
    append(Ps, Parents, Result),
    write("\n"),
    write("Result " + Result),
    write("\n"),
    something(Instance_Name, Result),
    !.  
/*copy_attr(Instance_Name, Class_Name) :-
    class(Class_Name,_,_),    
    write(3),
    findall([N, V, T], class_field(Class_Name, N, V, T), Class_Fields),   
    sort(Class_Fields, Sorted_Class_Fields),   
    copy_assert(Instance_Name, Sorted_Class_Fields).*/

copy_attr(Instance_Name, Class_Name) :-  
    class(Class_Name,_,_),    
    findall([N, V, T], class_field(Class_Name, N, V, T), Class_Fields),   
    sort(Class_Fields, Sorted_Class_Fields),   
    copy_assert(Instance_Name, Sorted_Class_Fields).


/*copy_attr(Instance_Name, Instance_Fields, Class_Name) :-
    findall([N, V, T], class_field(Class_Name, N, V, T), Class_Fields),
    sort(Class_Fields, Sorted_Class_Fields),
    sort(Instance_Fields, Sorted_Istance_Fields),
    copy_assert(Instance_Name, Sorted_Class_Fields, Sorted_Istance_Fields).*/


copy_assert(_,[]) :- !.

copy_assert(Instance_Name, [F | Fs]) :-
    [N, V, T] = F,
    (\+ instance_field(Instance_Name,N, _, _)),  
    assert(instance_field(Instance_Name, N, V, T)),
    copy_assert(Instance_Name, Fs).

copy_assert(Instance_Name, [F | Fs]) :-   
    copy_assert(Instance_Name, Fs).


/*copy_assert(_,[],[]) :- !.

copy_assert(Instance_Name, [F | Fs], []) :-
    copy_assert(Instance_Name, [F | Fs]).

copy_assert(Instance_Name, [F | Fs], [NF | NFs]) :-
    [CFN, CFV, CFT] = F,
    [IFN, IFV] = NF,
    CFN =@= IFN,
    !,
    assert(instance_field(Instance_Name, IFN, IFV, CFT)),
    copy_assert(Instance_Name, Fs, NFs).

copy_assert(Instance_Name, [F | Fs],[NF | NFs]) :-
    [CFN, CFV, CFT] = F,
    [IFN, IFV] = NF,
    CFN \= IFN,
    !,
    assert(instance_field(Instance_Name, CFN, CFV, CFT)),
    copy_assert(Instance_Name, Fs, [NF | NFs]).*/


modify_fields(Instance_Name,[],[]) :-
    !.

modify_fields(Instance_Name,[],Fs) :-
    !.

modify_fields(Instance_Name, [IF | IFs], [F | Fs]) :-
    [N, FN, FV, FT] = F,
    [IFN, IFV] = IF,
    FN =@= IFN,
    !,
    retract(instance_field(Instance_Name, FN, FV, FT)),
    assert(instance_field(Instance_Name, IFN, IFV, FT)),
    modify_fields(Instance_Name, IFs, Fs).
    
modify_fields(Instance_Name, [IF | IFs], [F | Fs]) :-
    [N, FN, FV, FT] = F,
    [IFN, IFV] = IF,
    FN \= IFN,
    !,
    modify_fields(Instance_Name, [IF | IFs], Fs).

/* DEBUG */

testing(Name, Surname) :-
    write("\n"),
    write(Name),  
    write(Surname),
    write("\n").

initialize():-
    call(def_class(a,[],[field(a,"",string),field(zampe,0,int)])),
    call(def_class(b,[a],[field(b,"",string),field(zampe,0,int)])),
    call(def_class(animale,[b],[field(specie,"",string),field(zampe,0,int)])),
    call(def_class(persona,[b],[field(nome,"persona",string),field(cognome,"",string)])),
    call(def_class(cane,[persona,animale],[field(nome,"cane",string),field(anni,0,int)])),
    !.

/* END DEBUG */